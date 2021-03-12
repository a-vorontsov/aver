default: build

build:
	@cd vm && make
	@cd compiler && make

install:
	@cd vm && make install
	@cd compiler && make install

clean:
	@cd vm && make clean
	@cd compiler && make clean
	@rm -rf ./bin