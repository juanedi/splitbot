build-watch:
	stack build --file-watch

build-linux:
	stack --stack-yaml linux/stack.yml build --copy-bins

create-image: build-linux
	@[ "${DOCKER_IMAGE_NAME}" ] || (echo "DOCKER_IMAGE_NAME variable not set"; exit 1)
	docker build -f linux/Dockerfile-release -t ${DOCKER_IMAGE_NAME} .

release-prepare: create-image
	@[ "${TAG}" ] || (echo "TAG variable not set"; exit 1)
	docker tag ${DOCKER_IMAGE_NAME} ${DOCKER_IMAGE_NAME}:${TAG}

release-publish:
	@[ "${DOCKER_IMAGE_NAME}" ] || (echo "DOCKER_IMAGE_NAME variable not set"; exit 1)
	@[ "${TAG}" ] || (echo "TAG variable not set"; exit 1)
# Verify that the tag was created
	docker inspect ${DOCKER_IMAGE_NAME}:${TAG}
# Push 'latest' build
	docker push ${DOCKER_IMAGE_NAME}
# Push version tag
	docker push ${DOCKER_IMAGE_NAME}:${TAG}
	git tag -a ${TAG} -m "release version ${TAG}"
	git push origin ${TAG}
