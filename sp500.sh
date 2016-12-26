#!/bin/bash


python2.7 -m SimpleHTTPServer 6161 &

COUNTER=0
while [  $COUNTER -lt 120 ]; do
 git pull
 R -e 'rmarkdown::render("bestpicks.Rmd")'
 Rscript sp500.R
 echo The counter is $COUNTER
 sleep 7200
 let COUNTER=COUNTER+1 
done
