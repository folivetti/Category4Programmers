#!/bin/bash
pandoc -t beamer --template=template.tex -F ./minted.py -o $1.tex $1.md
pdflatex --shell-escape $1.tex
pdflatex --shell-escape $1.tex
rm $1.aux $1.log $1.nav $1.out $1.snm $1.tex $1.toc $1.vrb
