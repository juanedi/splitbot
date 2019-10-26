build-watch:
	stack build --file-watch

release:
	@[ "${TAG}" ] || (echo "TAG variable not set"; exit 1)
	git tag -a ${TAG} -m "release version ${TAG}"
	git push origin ${TAG}
