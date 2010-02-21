#OPT=--preference='parsec > 3'

all:
	cabal configure
	cabal build

install:
	cabal install $(OPT)

clean: 
	rm -rf dist config.log config.h config.status
