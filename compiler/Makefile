default: build

build:
	dune build
	@mkdir -p bin/
	@cp -f ./_build/install/default/bin/averc ./bin/averc

install:
	@mkdir -p ../bin/
	@chmod +x ./bin/averc
	@cp -f ./bin/averc ../bin/averc

clean:
	@rm -rf ./bin/
	dune clean
