#!/bin/bash
for filename in tests/*; do
   ./bawk.native $filename
done
