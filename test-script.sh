#!/bin/bash
for filename in tests/*; do
   echo $filename
   ./bawk.native -a $filename -f ./input.txt
done
