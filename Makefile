build:
	cabal install --overwrite-policy=always

build-old:
	cabal install -w ghc-8.2.2 --overwrite-policy=always
