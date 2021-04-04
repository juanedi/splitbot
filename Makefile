watch:
	watchexec --exts hs -r -- 'cabal build && cabal run splitbot -- --polling'

push-release:
	@[ "${TAG}" ] || (echo "TAG variable not set"; exit 1)
	git tag -a ${TAG} -m "release version ${TAG}"
	git push origin ${TAG}
	@echo The GH tag was pushed! 🎉
	@echo Check out https://circleci.com/dashboard to see how the build/publish process is going.
