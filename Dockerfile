FROM ubuntu:18.04
RUN \
  PACKAGES="ghc cabal-install" && \
  apt-get update && \
  apt-get -y install $PACKAGES && \
  cabal update
