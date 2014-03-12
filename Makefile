deps:
	git clone git@github.com:purescript/purescript-transformers.git
	git clone git@github.com:purescript/purescript-arrows.git

all: lib test

lib:
	mkdir -p js/Text/Pretty
	psc src/Text/Pretty/PatternArrows.purs.hs \
	  purescript-transformers/src/Control/Monad/Identity.purs \
	  purescript-transformers/src/Control/Monad/State.purs \
	  purescript-transformers/src/Control/Monad/Trans.purs \
	  purescript-transformers/src/Control/Monad/State/Trans.purs \
	  purescript-arrows/src/Control/Arrow.purs.hs \
	  -o js/Text/Pretty/PatternArrows.js \
	  -e js/Text/Pretty/PatternArrows.e.purs.hs \
	  --module Text.Pretty.PatternArrows --tco --magic-do

test:
	mkdir -p js/
	psc src/Text/Pretty/PatternArrows.purs.hs \
	  purescript-transformers/src/Control/Monad/Identity.purs \
	  purescript-transformers/src/Control/Monad/State.purs \
	  purescript-transformers/src/Control/Monad/Trans.purs \
	  purescript-transformers/src/Control/Monad/State/Trans.purs \
	  purescript-arrows/src/Control/Arrow.purs.hs \
	  examples/Examples.purs.hs \
	  -o js/Examples.js \
	  --module Main --tco --magic-do --main
