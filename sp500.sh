#!/bin/bash


python2.7 -m SimpleHTTPServer 6161 &

COUNTER=0
while [  $COUNTER -lt 10 ]; do
 Rscript sp500.R
 python2.7 csv2html.py top.csv > top.html
 echo The counter is $COUNTER
 sleep 7200
 let COUNTER=COUNTER+1 
done
