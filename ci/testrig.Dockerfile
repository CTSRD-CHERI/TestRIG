FROM ctsrd/testrig-builder as testrig

# work from the jenkins user home directory
WORKDIR /home/jenkins

# copy TestRIG repo from host checkout
COPY --chown=jenkins TestRIG ./TestRIG

RUN \
  . /home/jenkins/sourceme.sh && \
  cd TestRIG && \
  make -j 8 QCVEngine \
  ibex-rv32ic-cheri \
  flute-rv64xcheri \
  qemu \
  rvbs \
  rvbs-rv32ICZicsrZifencei \
  rvbs-rv32ICZicsrZifenceiXcheri \
  rvbs-rv32IZicsrZifencei \
  rvbs-rv32IZicsrZifenceiXcheri \
  rvbs-rv64ICZicsrZifencei \
  rvbs-rv64ICZicsrZifenceiXcheri \
  rvbs-rv64IZicsrZifencei \
  rvbs-rv64IZicsrZifenceiXcheri \
  sail \
  sail-rv32 \
  sail-rv32-cheri \
  sail-rv64 \
  sail-rv64-cheri \
  spike \
  spike-cheri
  #piccolo-rv32xcheri
