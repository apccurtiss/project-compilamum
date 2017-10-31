TEX = pdflatex
NAME = paper
TFLAGS = --jobname=$(NAME) -file-line-error
SUP = -interaction=batchmode

.PHONY:all err clean

all: $(NAME).pdf

err:  paper.tex *.tex
	$(TEX) $(TFLAGS) paper.tex

$(NAME).pdf: paper.tex *.tex
	$(TEX) $(TFLAGS) $(SUP) paper.tex

clean:
	rm $(NAME).pdf $(NAME).aux $(NAME).log

