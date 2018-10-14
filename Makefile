all:
	cabal new-build

clean:
	rm dist dist-newstyle -rf
	rm -f {,src/}*.{o,hi,dyn_hi,dyn_o,hs~} *~ *.json *.hp *.lprof *.bin
cls:
	echo -en '\ec'
