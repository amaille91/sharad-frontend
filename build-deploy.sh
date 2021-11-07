#!/bin/bash -e

cabal v2-build --ghcjs all
echo "Copying /home/dev/workspace/sharad-haskell/sharad-frontend/dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/sharad-frontend-0.1.0.0/x/sharad-frontend/build/sharad-frontend/sharad-frontend.jsexe/* in ../../sandbox/static/"
cp /home/dev/workspace/sharad-haskell/sharad-frontend/dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/sharad-frontend-0.1.0.0/x/sharad-frontend/build/sharad-frontend/sharad-frontend.jsexe/* ../../sandbox/static/
echo "Copy done"
echo "Copying miso/static/* in ../../sandbox/static/"
cp miso/static/* ../../sandbox/static/
echo "Copy done"
