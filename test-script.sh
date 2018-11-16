#!/bin/bash
substr="fail-*"
for filename in tests/*.bawk; do
  v=${filename::-5}
  if [[ ${filename:${#filename}-5} = ".bawk" ]];
  then
    echo $filename
    if [[ ${filename:0:10} = "tests/fail" ]];
      then
        ./bawk.native -s $filename -f ./input.txt 2> $v.err
      else
        ./bawk.native -s $filename -f ./input.txt > $v.out
    fi
  fi
done
