build-watch:
	stack build --file-watch

build-linux:
	stack --stack-yaml linux/stack.yml build --copy-bins

build-image: build-linux
	docker build -f linux/Dockerfile-release -t jedi/splitbot .
