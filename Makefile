ERL=erl
ERLC=erlc
ERLC_OPT=+debug_info -W -o ebin

all: tools src_src

clean_gen:
	rm -f src/herml_scan.erl
	rm -f src/herml_parse.erl

tests: src_src src_tests
	${ERL} -pa ebin -pa ebin_tests -noshell -s init stop -eval 'test_suite:test().'

ebin:
	mkdir ebin

ebin_tests:
	mkdir ebin_tests

special: tools src/herml_scan.erl src/herml_parse.erl

ebin_tools:
	mkdir -p tools/ebin

tools: ebin_tools
	cd tools/src;erl -make
	cp tools/src/leexinc.hrl tools/ebin

src_src: ebin src/herml_parse.erl src/herml.app
	cd src;erl -make

src_tests: ebin_tests
	cd tests;erl -make

src/herml.app: ebin
	cp src/herml.app ebin

src/herml_scan.erl: tools
	${ERL} -pa tools/ebin -noshell -s init stop -eval 'leex:file("src/herml_scan.xrl", [{outdir, "src"}])'

src/herml_parse.erl: src/herml_scan.erl
	${ERL} -noshell -s init stop -eval 'yecc:file("src/herml_parse.yrl")'

clean: clean_gen
	rm -f ebin/*
	rm -f ebin_tests/*
	rm -f tools/ebin/*
