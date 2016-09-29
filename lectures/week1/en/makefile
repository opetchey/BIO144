FILE=lecture1_en

all:
	R CMD Sweave $(FILE).Rnw
	pdflatex $(FILE)
	bibtex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)
	evince $(FILE).pdf &

clean: 
	rm $(FILE).bbl $(FILE).aux $(FILE).log $(FILE).out $(FILE).blg 
