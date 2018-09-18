compile:
	pdflatex figures/*.tex
	pdflatex --output-dir=figures figures/figure-state-draw.tex
	pdflatex --shell-escape slides.tex

clean:
	rm -rf _minted-slides screenlog.0 slides.aux slides.log slides.nav slides.out slides.snm slides.sta figures/*.aux figures/*.log figures/*.pdf *.aux *.log figure-state-draw.pdf
