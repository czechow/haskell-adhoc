build:
	stack $@

.PHONY: test
test:
	stack $@

clean:
	stack $@

run: build
	stack exec table2tree

repl:
	stack ghci --main-is table2tree:exe:table2tree
