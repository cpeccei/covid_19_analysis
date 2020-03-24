rm(list = ls())

library(tidyverse)
library(lubridate)

options(stringsAsFactors = FALSE, scipen = 999)
# If more than print_min rows, print only print_max rows.
options(tibble.print_max = 20, tibble.print_min = 20)

get_data <- function(event) {
    url <- str_c(
        "https://raw.githubusercontent.com/CSSEGISandData/",
        "COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/",
        "time_series_covid19_", event, "_global.csv"
    )
    url %>%
        read_csv() %>%
        rename(
            province_or_state = "Province/State",
            country_or_region = "Country/Region",
            lat = Lat,
            lon = Long
        ) %>%
        pivot_longer(-(1:4), names_to = "date", values_to = "count") %>%
        mutate(
            date = mdy(date),
            event = event
        )
}

d <- c("confirmed", "deaths") %>%
    map_dfr(get_data)

# Columns of d:
# -------------
# province_or_state
# country_or_region
# lat
# lon
# date
# count
# event

# Roll up provinces to the country level and create columns
# for counts of Confirmed, Deaths and Recovered
dc <- d %>%
    group_by(country_or_region, event, date) %>%
    summarize(count = sum(count)) %>%
    ungroup() %>%
    pivot_wider(names_from = event, values_from = count)

min_deaths <- 50
top_countries_by_deaths <- dc %>%
    top_n(1, date) %>%
    top_n(10, deaths) %>%
    # Only include countries that have at least 2 x min_deaths
    filter(deaths > min_deaths * 2) %>%
    pull(country_or_region)

# Use min_deaths to compute data for plotting
g <- dc %>%
    filter(
        country_or_region %in% top_countries_by_deaths,
        deaths >= min_deaths
    ) %>%
    group_by(country_or_region) %>%
    # Take first 14 days since first day with min_deaths
    slice(1:14) %>%
    mutate(
        x = row_number() - 1,
        is_last = x == max(x)
    ) %>%
    ungroup()

p <- ggplot(g, aes(x, deaths, color = country_or_region)) +
    geom_line(size = 1) +
    labs(
        title = "Total deaths from COVID-19",
        subtitle = paste("Includes data up until", max(dc$date)),
        x = paste("Days since", scales::ordinal(min_deaths), "death"),
        y = "Total deaths",
        color = "Country"
    ) +
    scale_x_continuous(breaks = seq(0, 14, 2)) +
    scale_y_continuous(labels = scales::comma) +
    # Place a dot on the last data point for each country
    geom_point(data = filter(g, is_last), size = 4)

ggsave(file = "../output/covid_deaths_first_14_days.png",
    width = 9, height = 7)

# ======================================================================

g <- dc %>%
    filter(
        country_or_region %in% top_countries_by_deaths,
        deaths >= min_deaths
    ) %>%
    group_by(country_or_region) %>%
    mutate(
        x = row_number() - 1,
        is_last = x == max(x)
    ) %>%
    ungroup()

p <- ggplot(g, aes(x, deaths, color = country_or_region)) +
    geom_line(size = 1) +
    labs(
        title = "Total deaths from COVID-19",
        subtitle = paste("Includes data up until", max(dc$date)),
        x = paste("Days since", scales::ordinal(min_deaths), "death"),
        y = "Total deaths",
        color = "Country"
    ) +
    scale_y_continuous(labels = scales::comma) +
    # Place a dot on the last data point for each country
    geom_point(data = filter(g, is_last), size = 4)

ggsave(file = "../output/covid_deaths_all_days.png", width = 9, height = 7)
