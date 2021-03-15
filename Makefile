default: build

build:
	@cd compiler && make
	@cd vm && make

build-jit:
	@cd compiler && make
	@cd vm && make jit

install:
	@cd compiler && make install
	@cd vm && make install

clean:
	@cd compiler && make clean
	@cd vm && make clean
	@rm -rf ./bin