default:
	sbcl --load build.lisp --eval "(sb-ext:save-lisp-and-die \"bokbok\" :toplevel #'main :executable t :purify t)"

run:
	sbcl --load build.lisp --eval "(progn (main-with-swank) (quit))"
