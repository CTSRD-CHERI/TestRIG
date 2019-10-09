FROM ubuntu:18.04
ARG CABALFILE
RUN \
  PACKAGES="ghc cabal-install build-essential" && \
  apt-get update && \
  apt-get -y install $PACKAGES && \
  groupadd -g 1001 jenkins && \
  useradd -ms /bin/bash -u 1001 -g 1001 jenkins
USER jenkins
WORKDIR /home/jenkins
RUN echo "$CABALFILE" > tmp.cabal && \
    cabal update && \
    cabal install --only-dependencies && \
    rm tmp.cabal
