#!/bin/bash

# reconstruct FILENAME from input
filename="prob"$1".ml"
echo $filename

from="solutions/pending/"$filename
to="solutions/done/"$filename

# move to done
mv $from "solutions/done"

# add to git
git rm $from
git add $to

# update README
make clean
make
./table
make clean
git add README.md
