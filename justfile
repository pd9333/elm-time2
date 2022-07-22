#!/usr/bin/env just --justfile

test:
  elm-verify-examples
  elm-test

# Local Variables:
# mode: makefile
# End:
# vim: set ft=make :
