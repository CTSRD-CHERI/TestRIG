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

# install opam and rems repo
RUN \
  opam init --disable-sandboxing -y && \
  opam switch 4.12.0 -y && \
  eval `opam config env -y` && \
  opam repository add rems https://github.com/rems-project/opam-repository.git -y

# install sail
RUN \
  opam update -y && \
  opam install sail -y && \
  echo ". /home/jenkins/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" > /home/jenkins/sourceme.sh

# install BSV
ENV BSVURL="https://s3.wasabisys.com/bluespec/downloads/Bluespec-2019.05.beta2-debian9stretch-amd64.tar.gz"
RUN \
  wget $BSVURL && \
  tar -xzf Bluespec-2019.05.beta2-debian9stretch-amd64.tar.gz && \
  rm -r Bluespec-2019.05.beta2-debian9stretch-amd64.tar.gz
ENV BLUESPECDIR=/home/jenkins/Bluespec-2019.05.beta2-debian9stretch-amd64/lib/
ENV BLUESPEC_LICENSE_FILE=27001@lmserv-bluespec.cl.cam.ac.uk
ENV PATH=/home/jenkins/Bluespec-2019.05.beta2-debian9stretch-amd64/bin/:$PATH

# install cabal packages
COPY vengines/QuickCheckVEngine/QCVEngine.cabal .
RUN \
  cabal update && \
  cabal install --only-dependencies && \
  rm *.cabal
