#OFLAGS = -O
OFLAGS = -O2
# -pgmc ccache-gcc
OFLAGS613 = -Odph -O2 -funbox-strict-fields -fllvm -optlo-disable-inlining -optlo-basicaa -optlo-basiccg -optlo-count-aa -optlo-domfrontier -optlo-domtree -optlo-globalsmodref-aa -optlo-memdep -optlo-no-aa -optlo-postdomtree -optlo-codegenprepare  -optlo-functionattrs -optlo-block-placement -optlo-constmerge -optlo-constprop -optlo-die -optlo-dse -optlo-globaldce -optlo-globalopt -optlo-indvars -optlo-inline -optlo-ipconstprop -optlo-ipsccp -optlo-lcssa -optlo-loop-deletion -optlo-loop-index-split -optlo-loop-unroll -optlo-loop-unswitch -optlo-loopsimplify -optlo-mem2reg -optlo-memcpyopt -optlo-scalarrepl -optlo-tailcallelim

CFILES = sort.cpp


compile:
	ghc --make $(CFILES) $(OFLAGS) sortbench-criterion -pgml g++

compile613:
	$(GHC613BIN) --make $(CFILES) $(OFLAGS613) sortbench-criterion -pgml g++ -v -v -v -v

test:
	python sortbench.py
clean:
	rm -f *.txt *.o *.bin *~ *.stat *.hi *.prof hello
