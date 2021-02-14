build:
	ghc \
    --make \
    -odir _build \
    -hidir _build \
    -XOverloadedStrings \
    -fwarn-unused-imports -Wno-name-shadowing \
    -threaded -rtsopts -with-rtsopts=-N \
    -isrc \
    -o bin/splitbot \
    app/Main.hs

watch:
	watchexec --exts hs -r -- 'make build && bin/splitbot --polling'

push-release:
	@[ "${TAG}" ] || (echo "TAG variable not set"; exit 1)
	git tag -a ${TAG} -m "release version ${TAG}"
	git push origin ${TAG}
	@echo The GH tag was pushed! 🎉
	@echo Check out https://circleci.com/dashboard to see how the build/publish process is going.
