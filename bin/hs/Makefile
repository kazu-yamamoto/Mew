BASE = smew
SRC = $(BASE).hs
CLN = $(BASE).hi $(BASE).o $(BASE)

all: $(BASE)

$(BASE): $(SRC)
	ghc --make $(SRC)
	strip $(BASE)

clean: 
	rm -rf $(CLN)
