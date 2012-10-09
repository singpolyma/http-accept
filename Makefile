GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHShttp-accept-$(VERSION).a dist/http-accept-$(VERSION).tar.gz

install: dist/build/libHShttp-accept-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Network/HTTP/Accept.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/http-accept/index.html README

README: http-accept.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/http-accept/index.html: dist/setup-config Network/HTTP/Accept.hs
	cabal haddock --hyperlink-source

dist/setup-config: http-accept.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHShttp-accept-$(VERSION).a: dist/setup-config Network/HTTP/Accept.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/http-accept-$(VERSION).tar.gz: README dist/setup-config Network/HTTP/Accept.hs
	cabal check
	cabal sdist
