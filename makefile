.PHONY: build deploy

build:
	@stack build --pedantic

deploy: build
	@rsync -crzq .stack-work/install/x86_64-linux/lts-3.18/7.10.2/bin/slowotok-exe brzoza:/tmp/slowotok-exe
	@rsync -crzq  data/* brzoza:slowotok/data
	@ssh brzoza 'pkill slowotok || /bin/true'
	@ssh brzoza 'cp /tmp/slowotok-exe ~/slowotok/'
	@ssh -n -f brzoza "sh -c 'cd slowotok; nohup ./slowotok-exe > /dev/null 2>&1 &'"

