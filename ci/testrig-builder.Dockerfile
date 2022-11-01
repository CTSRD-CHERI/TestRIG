FROM ubuntu:20.04 AS testrig-builder

# create a jenkins user
RUN \
  groupadd -g 1001 jenkins && \
  useradd -ms /bin/bash -u 1001 -g 1001 jenkins

# work from the jenkins user home directory
WORKDIR /home/jenkins

# install packages as root
ENV PACKAGES="build-essential wget opam libgmp-dev z3 m4 pkg-config zlib1g-dev verilator python3 pip gcc g++ device-tree-compiler libfontconfig libxft2 libtcl8.6 curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5"
RUN \
  apt-get update && \
  DEBIAN_FRONTEND="noninteractive" TZ="Europe/London" apt-get -y -qq install $PACKAGES && \
  ldconfig && \
  pip install pyyaml

# switch to jenkins user
USER jenkins

# install BSV
ADD bsc-install-focal.tar.xz /home/jenkins/
ENV PATH=/home/jenkins/bsc-install/bin/:$PATH

# install opam and rems repo
RUN \
  opam init --disable-sandboxing -y --compiler=4.08.0 && \
  eval `opam config env -y` && \
  opam repository add rems https://github.com/rems-project/opam-repository.git -y

# install sail
RUN \
  git clone --branch sail2 https://github.com/rems-project/sail.git && \
  opam update -y && \
  cd sail && \
  opam pin add . -y && \
  cd .. && \
  eval `opam config env -y` && \
  sail -v

# install sailcov and source script
RUN \
  eval `opam config env -y` && \
  make -C sail/sailcov && \
  echo ". /home/jenkins/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" > /home/jenkins/sourceme.sh

# install rust
RUN \
  curl https://sh.rustup.rs -sSf | sh -s -- -y
  
# install ghc and cabal
RUN \  
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- -y

# build sail coverage library
RUN \
  eval `opam config env -y` && \
  . /home/jenkins/.cargo/env && \
  make -C $OPAM_SWITCH_PREFIX/share/sail/lib/coverage

# install cabal packages
# COPY vengines/QuickCheckVEngine/QCVEngine.cabal .
#RUN \
#  cd vengines/QuickCheckVEngine/ &&\
#  . /home/jenkins/.ghcup/env && \
#  cabal configure && \
#  cabal update && \
#  cabal install --only-dependencies && \
#  cd ../../
