include ../.env

default: no-jit

no-jit:
	@mkdir -p bin/
	$(PYTHON) ${RPYTHON} --batch main.py
	@mv ./aver-nojit ./bin/

jit:
	@mkdir -p bin/
	$(PYTHON) ${RPYTHON} --batch -Ojit main.py
	@mv ./aver ./bin/

install:
	@mkdir -p ../bin/
	@cp ./bin/aver* ../bin/

clean:
	@rm -r ./bin/