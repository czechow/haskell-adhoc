TARGET=cndt

build:
	stack $@

.PHONY: test
test:
	stack $@

clean:
	stack $@

run: build
	stack exec $(TARGET)

repl:
	stack ghci --main-is $(TARGET):exe:$(TARGET)
