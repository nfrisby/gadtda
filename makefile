all:
	ghc -fforce-recomp -O2 --make Increase                   && \
ghc -O2 -c Increase.hs                   -ddump-simpl -dsuppress-all > catch-slow.hs && \
ghc -fforce-recomp -O2 --make Increase -DFAST            && \
ghc -O2 -c Increase.hs -DFAST            -ddump-simpl -dsuppress-all > catch-SC-not-forced.hs && \
ghc -fforce-recomp -O2 --make Increase -DFAST -DFORCE-SC && \
ghc -O2 -c Increase.hs -DFAST -DFORCE-SC -ddump-simpl -dsuppress-all > catch-SC-forced.hs && \
rm *.o *.hi

clean:
	rm *.o *.hi catch-*.hs

# running this command
#
# ghc -fforce-recomp -O2 --make Increase && ghc -O2 -c Increase.hs -ddump-simpl -dsuppress-all -dverbose-core2core
#
# shows that spec-constr is firing
