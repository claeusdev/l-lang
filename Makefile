.PHONY: run build clean

run:
	stack run -- ${ARGS}

build:
	stack build

clean:
	stack clean

