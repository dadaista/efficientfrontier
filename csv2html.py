#!/usr/bin/python

import sys
import os
import csv
import string

if len( sys.argv ) < 2 :
    sys.stderr.write( sys.argv[ 0 ]  + 
                      ": usage - "   + 
                      sys.argv[ 0 ]  + " [.csv file name]\n" )
    sys.exit()

if not os.path.exists(sys.argv[ 1 ]):
    sys.stderr.write( sys.argv[ 1 ] + " not found \n" )
    sys.exit()



sys.stdout.write( "<html>" )
sys.stdout.write( "<body>" )

sys.stdout.write( "<table>" )

with open( sys.argv[ 1 ], 'rb') as csvfile:
    table_string = ""
    reader       = csv.reader( csvfile )
    
    for row in reader:
        table_string += "<tr>" + \
                          "<td>" + \
                              string.join( row, "</td><td>" ) + \
                          "</td>" + \
                        "</tr>\n"
    
    sys.stdout.write( table_string )


sys.stdout.write( "</table>" ) 
import time
sys.stdout.write( "<p>" ) 

sys.stdout.write(time.strftime("%d/%m%/%Y %H:%M:%S"))
sys.stdout.write( "</p>" ) 

sys.stdout.write( "</body>" )
sys.stdout.write( "</html>" )
   