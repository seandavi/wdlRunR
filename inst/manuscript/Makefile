all: main.pdf wdlrunr_manuscript.pdf

wdlrunr_manuscript.pdf: wdlrunr_manuscript.tex citations.bib
	rm wdlrunr_manuscript.b*; xelatex wdlrunr_manuscript.tex; biber --bblsafecharsset=full wdlrunr_manuscript; xelatex wdlrunr_manuscript.tex; xelatex wdlrunr_manuscript.tex; xelatex wdlrunr_manuscript.tex

arch.pdf: wdlrunr.dot
	dot -Tpdf wdlrunr.dot -o arch.pdf

%.tex: %.Rnw
	Rscript -e 'knitr::knit("$<")'
