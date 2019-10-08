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

QCVEngine:
	cd vengines/QuickCheckVEngine &&\
	cabal configure &&\
	cabal build

sail-generator:
	cd vengines/sail-riscv-test-generation &&\
	make

.PHONY: clean-vengines clean-QCVEngine

clean-vengines: clean-QCVEngine clean-sail-generator

clean-QCVEngine:
	cd vengines/QuickCheckVEngine &&\
	cabal clean

clean-sail-generator:
	cd vengines/sail-riscv-test-generation &&\
	make clean

# RISCV implementations
################################################################################
riscv-implementations: spike sail

piccolo-rv32xcheri:
	$(MAKE) -C riscv-implementations/Piccolo/builds -f Resources/Build_all.mk ARCH=RV32IMUxCHERI SIM=bluesim RVFI_DII=RVFI_DII build

piccolo-rv64xcheri:
	$(MAKE) -C riscv-implementations/Piccolo/builds -f Resources/Build_all.mk ARCH=RV64IUxCHERI SIM=bluesim RVFI_DII=RVFI_DII build

piccolo-rv32: piccolo-rv32xcheri #for now, just testing CHERI implementation.
piccolo-rv64: piccolo-rv64xcheri #for now, just testing CHERI implementation.

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

spike:
	cd riscv-implementations/riscv-isa-sim &&\
	rm -rf build && mkdir build && cd build && ../fesvr/configure --prefix=`pwd` && make install &&\
	../configure --with-fesvr=`pwd` --prefix=`pwd` --enable-rvfi-dii &&\
	make install && cp libfesvr.so lib/

spike-cheri:
	cd riscv-implementations/riscv-isa-sim &&\
	rm -rf build && mkdir build && cd build && ../fesvr/configure --prefix=`pwd` && make install &&\
	../configure --with-fesvr=`pwd` --prefix=`pwd` --enable-rvfi-dii --enable-cheri --enable-cheri128 --enable-mergedrf &&\
	make install && cp libfesvr.so lib/

spike-cheri-fast:
	cd riscv-implementations/riscv-isa-sim/build &&\
	make install

spike-cheri-standalone:
	cd riscv-implementations/riscv-isa-sim &&\
	rm -rf standalone-build && mkdir standalone-build && cd standalone-build && ../fesvr/configure --prefix=`pwd` && make install &&\
	../configure --with-fesvr=`pwd` --prefix=`pwd` --enable-cheri --enable-cheri128 --enable-mergedrf --enable-misaligned &&\
	make install && cp libfesvr.so lib/

sail: sail-rv32

sail-rv32:
	ARCH=RV32 $(MAKE) -C riscv-implementations/sail-riscv c_emulator/riscv_rvfi

sail-rv64:
	ARCH=RV64 $(MAKE) -C riscv-implementations/sail-riscv c_emulator/riscv_rvfi

sail-rv32-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv c_emulator/cheri_riscv_rvfi_RV32

sail-rv64-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv c_emulator/cheri_riscv_rvfi_RV64

ibex-rv32ic-cheri:
	$(MAKE) -C riscv-implementations/ibex/verilator

.PHONY: clean-riscv-implementations clean-rvbs clean-sail

clean-riscv-implementations: clean-rvbs clean-spike clean-sail clean-piccolo

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

clean-sail:
	$(MAKE) -C riscv-implementations/sail-riscv clean

clean-sail-rv32-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv clean

clean-sail-rv64-cheri:
	$(MAKE) -C riscv-implementations/sail-cheri-riscv clean

clean-piccolo:
	rm -rf riscv-implementations/Piccolo/builds/RV*

clean-ibex:
	$(MAKE) -C riscv-implementations/ibex/verilator clean
