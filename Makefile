# Makefile
#
# Converts Markdown to other formats (HTML, PDF, EPUB) using Pandoc
#
# a) Run "make" (or "make all") to convert to all other formats
# b) Run "make clean" to delete converted files
#
# pandoc docs/part1.md -o docs/part1.pdf --from markdown --template eisvogel --listings
#
PANDOC=pandoc
IFORMAT=markdown
MATHJAX="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
TEMPLATE_TEX=eisvogel.latex
TEMPLATE_HTML=template.html
PNG_IMAGES=$(patsubst %.pdf,%.png,$(wildcard img/*.pdf))

PANDOC_OPTIONS=--standalone --toc --toc-depth=2 --mathjax=$(MATHJAX) --highlight-style pygments --listings
PANDOC_HTML_OPTIONS=--to html5 --template $(TEMPLATE_HTML)
PANDOC_PDF_OPTIONS=--template $(TEMPLATE_TEX)
PANDOC_EPUB_OPTIONS=--to epub3


SRC=docs/part1.md \
    docs/part2.md
OBJ=$(SRC:.md=.html)

%.html: %.md
	$(PANDOC) --from $(IFORMAT) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o $@ $<

%.pdf: %.md
	$(PANDOC) --from $(IFORMAT) $(PANDOC_OPTIONS) $(PANDOC_PDF_OPTIONS) -o $@ $<
