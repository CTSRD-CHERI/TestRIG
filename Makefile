#-
# SPDX-License-Identifier: BSD-2-Clause
#
# Copyright (c) 2018 Alexandre Joannou
# Copyright (c) 2019 Marno van der Maas
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#

all: vengines riscv-implementations

clean: clean-vengines clean-riscv-implementations

# Verification Engines
################################################################################
vengines: QCVEngine

QCVENGINE_BIN_DIR=$(CURDIR)/vengines/QuickCheckVEngine/bin
QCVEngine: Makefile
	( cd $(CURDIR)/vengines/QuickCheckVEngine && \
	  cabal install --only-dependencies --overwrite-policy=always && \
	  cabal build )
	$(eval QCVENGINE_BUILT = $(shell \
	  cd $(CURDIR)/vengines/QuickCheckVEngine && \
	  cabal list-bin QCVEngine ))
	mkdir -p $(QCVENGINE_BIN_DIR)
	cp $(QCVENGINE_BUILT) $(QCVENGINE_BIN_DIR)

sail-generator:
	cd vengines/sail-riscv-test-generation &&\
	$(MAKE)

.PHONY: clean-vengines clean-QCVEngine

clean-vengines: clean-QCVEngine clean-sail-generator

# Clean the older cabal build location to match the QCVEngine build recipe
clean-QCVEngine:
	cd vengines/QuickCheckVEngine &&\
	if cabal --help 2>&1 | grep v1-build > /dev/null; then \
	  (cabal v1-clean); \
	else \
	  (cabal clean); \
	fi

clean-sail-generator:
	cd vengines/sail-riscv-test-generation &&\
	$(MAKE) clean

# RISCV implementations
################################################################################
riscv-implementations: sail

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
	$(MAKE) -C riscv-implementations/Toooba/builds/RV64ACDFIMSUxCHERI_Toooba_RVFI_DII_bluesim compile simulator

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
	ARCH=RV32 HPM_PLATFORM=EXAMPLE $(MAKE) -C riscv-implementations/sail-riscv c_emulator/riscv_rvfi_RV32

sail-rv64:
	ARCH=RV64 HPM_PLATFORM=EXAMPLE $(MAKE) -C riscv-implementations/sail-riscv c_emulator/riscv_rvfi_RV64

sail-rv32-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv c_emulator/cheri_riscv_rvfi_RV32

sail-rv64-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv c_emulator/cheri_riscv_rvfi_RV64

ibex-rv32ic-cheri:
	$(MAKE) -C riscv-implementations/ibex/verilator

muntjac-rv64imac:
	$(MAKE) -C riscv-implementations/muntjac

.PHONY: clean-riscv-implementations clean-rvbs clean-sail

clean-riscv-implementations: clean-rvbs clean-spike clean-sail clean-piccolo clean-flute clean-toooba clean-muntjac clean-qemu clean-sail-rv32-cheri clean-sail-rv64-cheri

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
	if [ -e riscv-implementations/Toooba/builds/RV64ACDFIMSUxCHERI_Toooba_RVFI_DII_bluesim ]; then \
	    $(MAKE) -C riscv-implementations/Toooba/builds/RV64ACDFIMSUxCHERI_Toooba_RVFI_DII_bluesim clean; \
	fi

clean-ibex:
	$(MAKE) -C riscv-implementations/ibex/verilator clean

clean-muntjac:
	$(MAKE) -C riscv-implementations/muntjac clean

clean-qemu:
	rm -rf $(QEMU_DIR)/build
