FROM ubuntu:18.04
RUN \
  PACKAGES="ghc cabal-install" && \
  apt-get update && \
  apt-get -y install $PACKAGES && \
  useradd -ms /bin/bash -u 1001 -U -g 1001 jenkins
USER jenkins
WORKDIR /home/jenkins
RUN cabal update
