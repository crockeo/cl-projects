.SILENT:

default:
	sbcl --load "build.lisp" --eval "(save-image)"

run:
	sbcl --load "build.lisp" --eval "(main:main)"

check:
	sbcl --load "build.lisp" --eval "(quit)"
