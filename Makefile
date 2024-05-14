GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc
GHC_OPTS   = -package array -package containers -package mtl


interpreter : interpreter.hs RunProgram.hs TypeChecker.hs
	${GHC} ${GHC_OPTS} $@

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi
