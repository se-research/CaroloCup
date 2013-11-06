SRC_DIR=src/
LIB_DIR=lib/
BUILD_DIR=build/

PDF=$(BUILD_DIR)/Report.pdf

REPORT=$(SRC_DIR)/Report.tex

SRC=$(SRC_DIR)/Bib.bib \
		$(SRC_DIR)/Introduction.tex \
		$(SRC_DIR)/Theory.tex \
		$(SRC_DIR)/Results.tex \
		$(SRC_DIR)/Discussion.tex \
		$(SRC_DIR)/Conclusion.tex \
		$(SRC_DIR)/Report.tex \

LIB=$(LIB_DIR)/beamerthemechalmers.sty \
		$(LIB_DIR)/chalmers-letter.cls \
		$(LIB_DIR)/chalmers-thesis.cls \
		$(LIB_DIR)/chalmers-thesisletter.cls \

DEFAULT=-output-format pdf \
				-file-line-error \
				-halt-on-error \
				-output-directory $(BUILD_DIR) \

view: build
	mupdf $(PDF)

build: $(SRC) $(LIB)
	mkdir $(BUILD_DIR); \
	touch $(BUILD_DIR)
	lualatex $(DEFAULT) $(REPORT)
	cp $(SRC_DIR)/Bib.bib $(BUILD_DIR)/
	biber $(BUILD_DIR)/Report
	lualatex $(DEFAULT) $(REPORT)

draft: $(SRC)
	lualatex $(DEFAULT) $(REPORT)

clean: $(BUILD_DIR)
	rm -rv $(BUILD_DIR)
