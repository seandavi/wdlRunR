all: main.pdf cromwell_manuscript.pdf

cromwell_manuscript.pdf: cromwell_manuscript.tex citations.bib
	rm cromwell_manuscript.b*; xelatex cromwell_manuscript.tex; biber --bblsafecharsset=full cromwell_manuscript; xelatex cromwell_manuscript.tex; xelatex cromwell_manuscript.tex; xelatex cromwell_manuscript.tex

main.pdf: main.tex citations.bib arch.pdf
	rm main.b*; xelatex main.tex; bibtex main; xelatex main.tex; xelatex main.tex; xelatex main.tex

arch.pdf: wdlrunr.dot
	dot -Tpdf wdlrunr.dot -o arch.pdf

%.tex: %.Rnw
	Rscript -e 'knitr::knit("$<")'
