FROM ctsrd/testrig-builder as testrig

# work from the jenkins user home directory
WORKDIR /home/jenkins

# copy TestRIG repo from host checkout
COPY --chown=jenkins TestRIG ./TestRIG

RUN \
  . /home/jenkins/sourceme.sh && \
  . /home/jenkins/.ghcup/env && \
  cd TestRIG && \
  make clean && \
  SAILCOV=1 \
  make -j 8 QCVEngine \
  flute-rv64xcheri \
  sail \
  sail-rv64-cheri \
  toooba-rv64xcheri
  #piccolo-rv32xcheri
  #ibex-rv32ic-cheri
  #rvbs
  #rvbs-rv32ICZicsrZifencei
  #rvbs-rv32ICZicsrZifenceiXcheri
  #rvbs-rv32IZicsrZifencei
  #rvbs-rv32IZicsrZifenceiXcheri
  #rvbs-rv64ICZicsrZifencei
  #rvbs-rv64ICZicsrZifenceiXcheri
  #rvbs-rv64IZicsrZifencei
  #rvbs-rv64IZicsrZifenceiXcheri
  #sail-rv32
  #sail-rv32-cheri
  #sail-rv64
  #spike
  #spike-cheri
