compile:
	ghc --make -O sortbench-criterion
test:
	python sortbench.py
clean:
	rm -f *.txt *.o *.bin *~ *.stat *.hi *.prof
