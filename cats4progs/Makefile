build:
	stack $@

.PHONY: test
test:
	stack $@

clean:
	stack $@

run: build
	stack exec cats4progs

repl:
	stack ghci --main-is cats4progs:exe:cats4progs
