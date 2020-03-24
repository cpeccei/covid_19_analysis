#!/bin/bash

cd /home/ec2-user/repos/covid_19_analysis/src
Rscript analysis.R
cd ..
git add .
git commit -m 'Update graphs'
git push

