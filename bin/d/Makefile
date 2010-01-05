# You NEED TO make a link "sqlite3.a" to its entity.
# (e.g /usr/lib/libsqlite3.dylib on Mac)

TGT = smew
OBJ = smew.o Sqlite3DB.o SearchEngine.o
LIB = sqlite3.a

DC = dmd

.SUFFIXES: .d
.d.o:
	$(DC) -c $<

all: $(TGT)

$(TGT): $(OBJ)
	$(DC) -of$@ $(OBJ) $(LIB)

clean:
	rm -f $(TGT) $(OBJ)
