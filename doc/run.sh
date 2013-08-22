#!/usr/bin/env bash

name="titanic"
idir=$PWD/$name.rmd2html
if [ -d $idir ]; then
    echo "Clean-up previous structure"
    rm -rf $idir
fi
mkdir -p mkdir -p $name.rmd2html
ln -s $PWD/images $idir/images
./rmd2html.R $name.Rmd

echo "### Please open up created HTML or PDF files"
echo "PDF : $idir/$name.pdf"
echo "HTML: $idir/$name.html"
