TARGET=sbox

build:
	stack --verbose $@

.PHONY: test
test:
	stack $@

clean:
	stack $@

run: build
	stack exec $(TARGET)

bench: build
	stack exec $(TARGET) -- --output bench.html

repl:
	stack ghci --main-is $(TARGET):exe:$(TARGET)
