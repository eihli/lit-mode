##
# Project Title
#
# @file
# @version 0.1

all: lit-mode.el docs/index.html

docs/google-code-prettify:
	srcweave-format-init docs

lit-mode.el: index.lit
	srcweave --tangle ./ $<

docs/index.html README.md: index.lit
	srcweave --weave ./docs/ --formatter srcweave-format $<
	pandoc --from html --to gfm docs/index.html -o README.md

.PHONY:
clean:
	rm -f docs/src/lit-mode.el
	rm -f docs/index.html

# end
