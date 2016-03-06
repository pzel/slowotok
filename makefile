.PHONY: build deploy run test

build:
	@stack build --pedantic

run: build
	@stack exec slowotok-exe -- +RTS -s

deploy: build
	@rsync -crzq .stack-work/install/x86_64-linux/lts-3.18/7.10.2/bin/slowotok-exe $(T):/tmp/slowotok-exe
	@rsync -crzq  data/* $(T):slowotok/data
	@rsync -crzq  static/* $(T):slowotok/static
	@ssh $(T) 'pkill slowotok || /bin/true'
	@ssh $(T) 'cp /tmp/slowotok-exe ~/slowotok/'
	@ssh -n -f $(T) "sh -c 'cd slowotok; nohup ./slowotok-exe > slowotok.log 2>&1 &'"

test:
	@stack test
