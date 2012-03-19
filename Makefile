include ../lisp.cf

asdf:
	rm -rf asdf-package/*
	sbcl --load build-asdf
