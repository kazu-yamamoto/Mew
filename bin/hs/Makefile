BASE = smew
SRC = $(BASE).hs
CLN = $(BASE).hi $(BASE).o $(BASE)

all: $(BASE)

$(BASE): $(SRC)
	ghc --make -Wall $(SRC)
	strip $(BASE)

clean: 
	rm -rf $(CLN)
