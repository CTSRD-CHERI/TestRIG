FROM ctsrd/testrig-builder as testrig

RUN \
  . /home/jenkins/sourceme.sh && \
  git clone --branch test --depth 1 https://github.com/CTSRD-CHERI/TestRIG.git && \
  cd TestRIG && \
  git submodule update --init --recursive && \
  make -j 8 QCVEngine \
  piccolo-rv32xcheri \
  piccolo-rv64xcheri \
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
  #ibex-rv32ic-cheri \
