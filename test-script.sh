#!/bin/bash
for filename in tests/*; do
   echo $filename
   ./bawk.native $filename
done
