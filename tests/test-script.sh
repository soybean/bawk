#!/bin/bash
for filename in .; do
   ocamlbuild bawk.native ./bawk.native $filename
done
