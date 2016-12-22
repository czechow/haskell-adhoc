TARGET=fakelivy

build:
	stack $@

.PHONY: test
test:
	stack $@

clean:
	stack $@

run: build
	stack exec $(TARGET)-exe

bench: build
	stack exec $(TARGET) -- --output bench.html

repl:
	stack ghci --main-is $(TARGET):exe:$(TARGET)-exe
