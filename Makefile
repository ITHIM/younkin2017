younkin2017.pdf: ./younkin2017.Rnw ./lib/ITHIM.bib ./lib/materialsAndMethods.Rnw ./lib/materialsAndMethods2.Rnw ./lib/abstract.Rnw ./lib/introduction.Rnw ./lib/portland.Rnw ./lib/results.Rnw ./lib/ITHIM.bib
	R -e 'library("knitr"); setwd("~/younkin2017"); knit("~/younkin2017/younkin2017.Rnw")'
	pdflatex younkin2017.tex
	bibtex younkin2017.aux
	pdflatex younkin2017.tex
	pdflatex younkin2017.tex

clean:
	rm -rf ./*.aux
	rm -rf ./*.bbl
	rm -rf ./*.blg
	rm -rf ./*.log
	rm -rf ./*.out
	rm -rf ./*.tex
	rm -rf ./figure/
	rm -rf ./figures/
