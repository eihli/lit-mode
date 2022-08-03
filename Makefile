##
# Project Title
#
# @file
# @version 0.1

all: docs/src/lit-mode.el docs/index.html

docs/google-code-prettify:
	srcweave-format-init docs

docs/src/lit-mode.el: index.lit
	srcweave --tangle ./docs/src/ $<

docs/index.html: index.lit
	srcweave --weave ./docs/ --formatter srcweave-format $<

.PHONY:
clean:
	rm -f docs/src/lit-mode.el
	rm -f docs/index.html

# end
