younkin2017.pdf: ./younkin2017.Rnw ./lib/ITHIM.bib ./lib/materialsAndMethods.Rnw ./lib/abstract.Rnw ./lib/introduction.Rnw ./lib/portland.Rnw ./lib/results.Rnw ./lib/ATUS.Rnw ./lib/health.Rnw ./lib/burden.Rnw ./lib/ATUS.Rnw ./lib/appendix.Rnw ./lib/ITHIM.bib ./data/gbd_Manuscript_2011-2015.csv
	R -e 'library("knitr"); setwd("~/younkin2017"); knit("~/younkin2017/younkin2017.Rnw")'
	pdflatex younkin2017.tex
	bibtex younkin2017.aux
	pdflatex younkin2017.tex
	pdflatex younkin2017.tex
#	cp -v younkin2017.tex younkin2017.pdf ~/box/syounkin/ITHIM/

clean:
	rm -rf ./*.aux
	rm -rf ./*.bbl
	rm -rf ./*.blg
	rm -rf ./*.log
	rm -rf ./*.out
	rm -rf ./*.tex
	rm -rf ./figure/
	rm -rf ./figures/

ATUS:
	wget http://www.bls.gov/tus/special.requests/atuscps_2011.zip
	wget http://www.bls.gov/tus/special.requests/atuscps_2012.zip
	wget http://www.bls.gov/tus/special.requests/atuscps_2013.zip
	wget http://www.bls.gov/tus/special.requests/atuscps_2014.zip
	wget http://www.bls.gov/tus/special.requests/atuscps_2015.zip
	wget http://www.bls.gov/tus/special.requests/atuscps_2016.zip
	wget http://www.bls.gov/tus/special.requests/atussum_2011.zip
	wget http://www.bls.gov/tus/special.requests/atussum_2012.zip
	wget http://www.bls.gov/tus/special.requests/atussum_2013.zip
	wget http://www.bls.gov/tus/special.requests/atussum_2014.zip
	wget http://www.bls.gov/tus/special.requests/atussum_2015.zip
	wget http://www.bls.gov/tus/special.requests/atussum_2016.zip
	unzip atuscps_2011.zip
	unzip atuscps_2012.zip
	unzip atuscps_2013.zip
	unzip atuscps_2014.zip
	unzip atuscps_2015.zip
	unzip atuscps_2016.zip
	unzip atussum_2011.zip
	unzip atussum_2012.zip
	unzip atussum_2013.zip
	unzip atussum_2014.zip
	unzip atussum_2015.zip
	unzip atussum_2016.zip
