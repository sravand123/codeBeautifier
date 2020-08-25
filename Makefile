#Make file to build the project
all: parse
%.lex.sml: %.lex
	mllex $<
%.grm.sml: %.grm
	mlyacc $<

.PHONY: all clean test

parse: tiger.sml parse.mlb tiger.grm.sml tiger.lex.sml beautify.sml
	mlton parse.mlb
clean: 
	rm parse
	rm tiger.lex.sml
	rm tiger.grm.sml
	rm tiger.grm.sig
	rm tiger.grm.desc
test:  parse
	${CURDIR}/parse test2.tiger

