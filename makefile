BIN=./bin
NULL=/dev/null
EXE=./bin/minimize
TEST=./test

generate_run: Main.hs
	@mkdir -p ${BIN}
	@ghc Main.hs -o ${EXE} -hidir ${BIN} -odir ${BIN}

run: generate_run
	@${EXE}

test: Test.hs
	@mkdir -p ${TEST}
	@ghc Test.hs -o ${TEST}/test -hidir ${TEST} -odir ${TEST}

clean:
	@rm -rf ./bin
