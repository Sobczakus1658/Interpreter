GHC        = ghc -Wincomplete-patterns
GHC_OPTS   = -package array -package containers -package mtl

.PHONY: all clean

all: interpreter

interpreter: interpreter.hs RunProgram.hs TypeChecker.hs ExceptionsAndTypes.hs
	$(GHC) $(GHC_OPTS) --make interpreter.hs RunProgram.hs TypeChecker.hs ExceptionsAndTypes.hs -main-is Interpreter -o interpreter

clean:
	-rm -f *.hi *.o interpreter