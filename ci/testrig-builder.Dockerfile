FROM ubuntu:20.04 AS testrig-builder

# create a jenkins user
RUN \
  groupadd -g 1001 jenkins && \
  useradd -ms /bin/bash -u 1001 -g 1001 jenkins

# work from the jenkins user home directory
WORKDIR /home/jenkins

# install packages as root
ENV PACKAGES="ghc cabal-install build-essential wget opam libgmp-dev z3 m4 pkg-config zlib1g-dev verilator python3 gcc g++ device-tree-compiler libfontconfig libxft2"
RUN \
  apt-get update && \
  DEBIAN_FRONTEND="noninteractive" TZ="Europe/London" apt-get -y -qq install $PACKAGES && \
  ldconfig

# switch to jenkins user
USER jenkins

# install BSV
ADD bsc-install-focal.tar.xz /home/jenkins/
ENV BLUESPECDIR=/home/jenkins/bsc-install/lib/
ENV BLUESPEC=/home/jenkins/bsc-install/
ENV PATH=/home/jenkins/bsc-install/bin/:$PATH

# install opam and rems repo
RUN \
  opam init --disable-sandboxing -y && \
  opam switch create 4.12.0 -y && \
  eval `opam config env -y` && \
  opam repository add rems https://github.com/rems-project/opam-repository.git -y

# install sail
RUN \
  opam update -y && \
  opam install sail -y && \
  echo ". /home/jenkins/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" > /home/jenkins/sourceme.sh

# install cabal packages
COPY vengines/QuickCheckVEngine/QCVEngine.cabal .
RUN \
  cabal update && \
  cabal install --only-dependencies && \
  rm *.cabal
