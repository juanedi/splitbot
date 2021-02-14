export PATH="$gnutar/bin:$gzip/bin:$ghc/bin:$coreutils/bin"

set -euo pipefail

mkdir -p $out/bin

tar -xzf $src
cd splitbot-$tag

ghc \
    --make \
    -odir _build \
    -hidir _build \
    -XOverloadedStrings \
    -fwarn-unused-imports -Wno-name-shadowing \
    -threaded -rtsopts -with-rtsopts=-N \
    -isrc \
    -o $out/bin/splitbot \
    app/Main.hs
