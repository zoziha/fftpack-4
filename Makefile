all: 
	$(MAKE) -f makefile --directory=src
	$(MAKE) -f makefile --directory=test

test:
	$(MAKE) -f makefile --directory=test