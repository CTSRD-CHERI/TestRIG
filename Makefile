#-
# Copyright (c) 2018 Alexandre Joannou
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
vengines: QCVengine

QCVengine:
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
riscv-implementations: rvbs spike

rvbs: rvbs-rv32i

rvbs-rv32i:
	$(MAKE) -C riscv-implementations/RVBS rvfi-dii

rvbs-rv32ic:
	$(MAKE) -C riscv-implementations/RVBS RVC=1 rvfi-dii

rvbs-rv32ixcheri:
	$(MAKE) -C riscv-implementations/RVBS RVXCHERI=1 rvfi-dii

rvbs-rv64i:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 rvfi-dii

rvbs-rv64ic:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVC=1 rvfi-dii

rvbs-rv64ixcheri:
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVXCHERI=1 rvfi-dii

spike:
	cd riscv-implementations/riscv-isa-sim &&\
	rm -rf build && mkdir build && cd build && ../fesvr/configure --prefix=`pwd` && make install &&\
	../configure --with-fesvr=`pwd` --prefix=`pwd` --enable-rvfi-dii --enable-misaligned &&\
	make install && cp libfesvr.so lib/

spike-cheri:
	cd riscv-implementations/riscv-isa-sim &&\
	rm -rf build && mkdir build && cd build && ../fesvr/configure --prefix=`pwd` && make install &&\
	../configure --with-fesvr=`pwd` --prefix=`pwd` --enable-rvfi-dii --enable-cheri --enable-cheri128 --enable-mergedrf --enable-misaligned &&\
	make install && cp libfesvr.so lib/

sail:
	$(MAKE) -C riscv-implementations/sail-riscv c_emulator/riscv_rvfi

.PHONY: clean-riscv-implementations clean-rvbs clean-sail

clean-riscv-implementations: clean-rvbs clean-spike clean-sail

clean-rvbs:
	$(MAKE) -C riscv-implementations/RVBS mrproper-rvfi-dii
	$(MAKE) -C riscv-implementations/RVBS RVC=1 mrproper-rvfi-dii
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 mrproper-rvfi-dii
	$(MAKE) -C riscv-implementations/RVBS XLEN=64 RVC=1 mrproper-rvfi-dii

clean-spike:
	rm -rf riscv-implementations/riscv-isa-sim/build

clean-sail:
	$(MAKE) -C riscv-implementations/sail-riscv clean
