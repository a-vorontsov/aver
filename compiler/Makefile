default: clean build

build:
	@dune build
	@mkdir -p bin/
	@cp -f ./_build/install/default/bin/averc ./bin/averc

install: build
	@mkdir -p ../bin/
	@cp -f ./bin/averc ../bin/averc

clean:
	@rm -rf ./bin/
	@dune clean
