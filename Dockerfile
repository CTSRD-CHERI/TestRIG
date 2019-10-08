FROM ubuntu:18.04
RUN \
  PACKAGES="ghc cabal-install" && \
  apt-get update && \
  apt-get -y install $PACKAGES && \
  useradd -ms /bin/bash jenkins
USER jenkins
WORKDIR /home/jenkins
RUN cabal update
