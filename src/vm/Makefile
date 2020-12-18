PYPYPATH?=~/pypy
RPYTHON?=$(PYPYPATH)/rpython/bin/rpython

no-jit:
	@mkdir -p bin/
	PYTHONPATH=$(PYTHONPATH):$(PYPYPATH):. $(RPYTHON) --batch main.py
	@mv ./aver-nojit ./bin/

jit:
	@mkdir -p bin/
	PYTHONPATH=$(PYTHONPATH):$(PYPYPATH):. $(RPYTHON) --batch -Ojit main.py
	@mv ./aver ./bin/

.PHONY: no-jit jit