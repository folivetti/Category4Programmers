#!/bin/bash
#pandoc -t beamer --highlight-style=pygments --template=template.tex --pdf-engine=xelatex -o $1.pdf $1.md
#pandoc -t beamer --listings --template=template.tex --pdf-engine=xelatex -o $1.pdf $1.md
pandoc -t beamer --template=template.tex -F ./minted.py -o $1.tex $1.md
pdflatex --shell-escape $1.tex
pdflatex --shell-escape $1.tex
rm $1.aux $1.log $1.nav $1.out $1.snm $1.tex $1.toc $1.vrb
#pandoc -t beamer -F ./minted.py --template=template.tex --pdf-engine-opt=-output-directory=temp --pdf-engine-opt=-shell-escape -o $1.pdf $1.md
