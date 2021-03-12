PYPYPATH?=~/pypy
RPYTHON?=$(PYPYPATH)/rpython/bin/rpython

default: no-jit

no-jit:
	@mkdir -p bin/
	@PYTHONPATH=$(PYTHONPATH):$(PYPYPATH):. $(RPYTHON) --batch main.py
	@mv ./aver-nojit ./bin/

jit:
	@mkdir -p bin/
	@PYTHONPATH=$(PYTHONPATH):$(PYPYPATH):. $(RPYTHON) --batch -Ojit main.py
	@mv ./aver ./bin/

install:
	@mkdir -p ../bin/
	@cp ./bin/aver* ../bin/

clean:
	@rm -r ./bin/