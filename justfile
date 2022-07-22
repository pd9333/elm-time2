#!/usr/bin/env just --justfile

test:
  elm-verify-examples
  elm-test

doc:
  elm-doc-preview

# Local Variables:
# mode: makefile
# End:
# vim: set ft=make :
