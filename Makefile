build-compilation-image:
	docker build -f linux/Dockerfile-build -t jedi/splitbot-build .

build-linux: build-compilation-image
	stack --stack-yaml linux/stack.yml install

build-release-image: build-linux
	docker build -f linux/Dockerfile-release -t jedi/splitbot .
