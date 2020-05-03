all:
	happy -gca ParGrammar.y
	alex -g LexGrammar.x
	ghc --make run_interpreter.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocGrammar.* LexGrammar.* ParGrammar.* LayoutGrammar.* Interpreter.* PrintGrammar.* run_interpreter.* AbsGrammar.* interpreter ErrM.* SharedString.* ComposOp.* grammar.dtd XMLGrammar.* Makefile*
	
