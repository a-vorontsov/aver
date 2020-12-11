PYPYPATH?=~/pypy
RPYTHON?=$(PYPYPATH)/rpython/bin/rpython

aver-no-jit:
	@mkdir -p bin/
	PYTHONPATH=$(PYTHONPATH):$(PYPYPATH):. $(RPYTHON) --batch main.py
	@mv ./aver-nojit ./bin/

aver-jit:
	@mkdir -p bin/
	PYTHONPATH=$(PYTHONPATH):$(PYPYPATH):. $(RPYTHON) --batch -Ojit main.py
	@mv ./aver ./bin/

.PHONY: aver-no-jit aver-jit