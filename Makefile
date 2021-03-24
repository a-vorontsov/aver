default: build

.PHONY: compiler vm
.SILENT: clean

build: compiler vm

compiler:
	@cd compiler && make

vm: compiler
	@cd vm && make

jit: compiler
	@cd vm && make jit

install:
	@cd compiler && make install
	@cd vm && make install

clean:
	@cd compiler && make clean
	@cd vm && make clean
	@rm -rf ./bin