#-
# Copyright (c) 2018 Alexandre Joannou
# Copyright (c) 2019 Marno van der Maas
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

all: vengines riscv-implementations

clean: clean-vengines clean-riscv-implementations

# Verification Engines
################################################################################
vengines: QCVEngine

# Scripts expect the older build directory, so we have to use v1-build with newer cabal binaries
QCVEngine: Makefile
	cd vengines/QuickCheckVEngine &&\
	if cabal --help 2>&1 | grep v1-build > /dev/null; then \
	  (cabal v1-install --only-dependencies && cabal v1-configure && cabal v1-build); \
	else \
	  (cabal install --only-dependencies && cabal configure && cabal build); \
	fi

sail-generator:
	cd vengines/sail-riscv-test-generation &&\
	$(MAKE)

.PHONY: clean-vengines clean-QCVEngine

clean-vengines: clean-QCVEngine clean-sail-generator

clean-QCVEngine:
	cd vengines/QuickCheckVEngine &&\
	cabal clean

clean-sail-generator:
	cd vengines/sail-riscv-test-generation &&\
	$(MAKE) clean

# RISCV implementations
################################################################################
riscv-implementations: spike sail

piccolo-rv32xcheri:
	$(MAKE) -C riscv-implementations/Piccolo/builds -f Resources/Build_all.mk ARCH=RV32IMUxCHERI_RVFI_DII SIM=bluesim build

piccolo-rv64xcheri:
	$(MAKE) -C riscv-implementations/Piccolo/builds -f Resources/Build_all.mk ARCH=RV64IUxCHERI_RVFI_DII SIM=bluesim build

piccolo-rv32: piccolo-rv32xcheri #for now, just testing CHERI implementation.
piccolo-rv64: piccolo-rv64xcheri #for now, just testing CHERI implementation.

flute-rv32xcheri:
	$(MAKE) -C riscv-implementations/Flute/builds -f Resources/Build_all.mk ARCH=RV32IMUxCHERI_RVFI_DII SIM=bluesim build

flute-rv64xcheri:
	$(MAKE) -C riscv-implementations/Flute/builds -f Resources/Build_all.mk ARCH=RV64ACDFIMSUxCHERI_RVFI_DII SIM=bluesim build

flute-rv32: flute-rv32xcheri #for now, just testing CHERI implementation.
flute-rv64: flute-rv64xcheri #for now, just testing CHERI implementation.

toooba-rv64xcheri:
	$(MAKE) -C riscv-implementations/Toooba/builds/RV64ACDFIMSUxCHERI_Toooba_RVFI_DII_verilator compile simulator

toooba-rv64: toooba-rv64xcheri #for now, just testing CHERI implementation.

rvbs: rvbs-rv32IZicsrZifencei

rvbs-rv32IZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 rvfi-dii

rvbs-rv32ICZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 RVC=1 rvfi-dii

rvbs-rv32IZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 rvfi-dii

rvbs-rv32ICZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 RVC=1 rvfi-dii

rvbs-rv64IZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 rvfi-dii

rvbs-rv64ICZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 RVC=1 rvfi-dii

rvbs-rv64IZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 rvfi-dii

rvbs-rv64ICZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 RVC=1 rvfi-dii

SPIKE_DIR=riscv-implementations/riscv-isa-sim

$(SPIKE_DIR)/build/Makefile:
	cd $(SPIKE_DIR) && mkdir -p build && cd build && \
	  ../configure --prefix=`pwd` --enable-rvfi-dii --disable-cheri

spike: $(SPIKE_DIR)/build/Makefile
	$(MAKE) -C $(SPIKE_DIR)/build install

$(SPIKE_DIR)/build-misaligned/Makefile:
	cd $(SPIKE_DIR) && mkdir -p build-misaligned && cd build-misaligned && \
	  ../configure --prefix=`pwd` --enable-rvfi-dii --disable-cheri --enable-misaligned

spike-misaligned: $(SPIKE_DIR)/build-misaligned/Makefile
	$(MAKE) -C $(SPIKE_DIR)/build-misaligned install

$(SPIKE_DIR)/build-cheri/Makefile:
	cd $(SPIKE_DIR) && mkdir -p build-cheri && cd build-cheri && \
	  ../configure --prefix=`pwd` --enable-rvfi-dii --enable-cheri

spike-cheri: $(SPIKE_DIR)/build-cheri/Makefile
	$(MAKE) -C $(SPIKE_DIR)/build-cheri install

$(SPIKE_DIR)/build-cheri-misaligned/Makefile:
	cd $(SPIKE_DIR) && mkdir -p build-cheri-misaligned && cd build-cheri-misaligned && \
	  ../configure --prefix=`pwd` --enable-rvfi-dii --enable-cheri --enable-misaligned

spike-cheri-misaligned: $(SPIKE_DIR)/build-cheri-misaligned/Makefile
	$(MAKE) -C $(SPIKE_DIR)/build-cheri-misaligned install


QEMU_DIR=riscv-implementations/qemu

$(QEMU_DIR)/build/config-host.mak:
	cd $(QEMU_DIR) && mkdir -p build && cd build && \
	  ../configure --prefix=`pwd`/install --enable-debug --enable-sanitizers \
	    --disable-vnc --disable-sdl --disable-gtk --disable-opengl --disable-cocoa \
	    --disable-strip --disable-linux-aio --disable-kvm --disable-werror --disable-pie \
	    --disable-linux-user --disable-bsd-user --disable-xen --disable-docs --disable-rdma \
	    --disable-capstone \
	    --enable-rvfi-dii --target-list=riscv64cheri-softmmu,riscv64-softmmu,riscv32-softmmu

qemu: $(QEMU_DIR)/build/config-host.mak
	$(MAKE) -C $(QEMU_DIR)/build

sail: sail-rv32

sail-rv32:
	ARCH=RV32 $(MAKE) -C riscv-implementations/sail-riscv c_emulator/riscv_rvfi_RV32

sail-rv64:
	ARCH=RV64 $(MAKE) -C riscv-implementations/sail-riscv c_emulator/riscv_rvfi_RV64

sail-rv32-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv c_emulator/cheri_riscv_rvfi_RV32

sail-rv64-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv c_emulator/cheri_riscv_rvfi_RV64

ibex-rv32ic-cheri:
	$(MAKE) -C riscv-implementations/ibex/verilator

.PHONY: clean-riscv-implementations clean-rvbs clean-sail

clean-riscv-implementations: clean-rvbs clean-spike clean-sail clean-piccolo clean-flute clean-toooba

clean-rvbs: clean-rvbs-rv32IZicsrZifencei

clean-rvbs-rv32IZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 mrproper-rvfi-dii

clean-rvbs-rv32ICZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 RVC=1 mrproper-rvfi-dii

clean-rvbs-rv32IZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 mrproper-rvfi-dii

clean-rvbs-rv32ICZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 RVC=1 mrproper-rvfi-dii

clean-rvbs-rv64IZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 mrproper-rvfi-dii

clean-rvbs-rv64ICZicsrZifencei:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 RVC=1 mrproper-rvfi-dii

clean-rvbs-rv64IZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 mrproper-rvfi-dii

clean-rvbs-rv64ICZicsrZifenceiXcheri:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVZICSR=1 RVZIFENCEI=1 RVXCHERI=1 RVC=1 mrproper-rvfi-dii

clean-spike:
	rm -rf riscv-implementations/riscv-isa-sim/build
	rm -rf riscv-implementations/riscv-isa-sim/build-cheri
	rm -rf riscv-implementations/riscv-isa-sim/build-fesvr

clean-sail:
	$(MAKE) -C riscv-implementations/sail-riscv clean

clean-sail-rv32-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv clean

clean-sail-rv64-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv clean

clean-piccolo:
	rm -rf riscv-implementations/Piccolo/builds/RV*

clean-flute:
	rm -rf riscv-implementations/Flute/builds/RV*

clean-toooba:
	if [ -e riscv-implementations/Toooba/builds/RV64ACDFIMSUxCHERI_Toooba_RVFI_DII_verilator ]; then \
	    $(MAKE) -C riscv-implementations/Toooba/builds/RV64ACDFIMSUxCHERI_Toooba_RVFI_DII_verilator clean; \
	fi

clean-ibex:
	$(MAKE) -C riscv-implementations/ibex/verilator clean
