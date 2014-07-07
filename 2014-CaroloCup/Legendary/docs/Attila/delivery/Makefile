NAME=delivery
MY_FAVOURITE_PDF_READER=mupdf


all: build view

view:
	$(MY_FAVOURITE_PDF_READER) $(NAME).pdf

build:
	texi2pdf -c $(NAME).tex
