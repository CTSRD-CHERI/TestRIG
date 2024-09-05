#! /usr/bin/env python3
# PYTHON_ARGCOMPLETE_OK
#-
# SPDX-License-Identifier: BSD-2-Clause
#
# Copyright (c) 2018-2019 Alexandre Joannou
# Copyright (c) 2019 Peter Rugg
# Copyright (c) 2019 Marno van der Maas
# Copyright (c) 2020 Alex Richardson
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

import argparse
import os
import os.path as op
import re
import signal
import subprocess as sub
import sys
import time
import socket
import typing
from dataclasses import dataclass
from typing import Optional

## misc paths variables ##
scripts_path=op.dirname(op.realpath(__file__))
testrig_root_path=op.join(scripts_path, op.pardir, op.pardir)
implementations_path=op.join(testrig_root_path, "riscv-implementations")
vengines_path=op.join(testrig_root_path, "vengines")

################################
# Parse command line arguments #
################################################################################

def auto_int(x):
  return int(x, 0)

def auto_pos_int(x):
  val = int(x, 0)
  if val <= 0:
    raise argparse.ArgumentTypeError("argument must be a positive int. Got {:d}.".format(val))
  return val

def auto_write_fd(fname):
  return open(fname, 'w')

def std_ext(ext_name):
  return ["", ext_name]

def z_ext(ext_name):
  return ["", "Z"+ext_name]

def x_ext(ext_name):
  return ["", "X"+ext_name]

known_rvfi_dii = {'spike', 'rvbs', 'sail', 'piccolo', 'flute', 'toooba', 'ibex', 'muntjac', 'qemu', 'manual', 'none'}
known_vengine = {'QCVEngine', 'QCVEngine-docker'}
multi_letter_exts = ["_".join(filter(None, [e0, e1, e2, e3]))
                     for e0 in z_ext("icsr")
                     for e1 in z_ext("ihpm")
                     for e2 in z_ext("ifencei")
                     for e3 in x_ext("cheri")]
known_architectures = sorted(set([e0 + e1 + e2 + e3 + e4 + e5 + e6 + e7 + e8
                                  for e0 in ["rv32i", "rv64i"]
                                  for e1 in std_ext("m")
                                  for e2 in std_ext("s")
                                  for e3 in std_ext("a")
                                  for e4 in std_ext("f")
                                  for e5 in std_ext("d")
                                  for e6 in std_ext("c")
                                  for e7 in std_ext("n")
                                  for e8 in multi_letter_exts]
                                 + [e0 + e1 + e2 + e3
                                    for e0 in ["rv32g", "rv64g"]
                                    for e1 in std_ext("c")
                                    for e2 in std_ext("n")
                                    for e3 in x_ext("cheri")]
                                 ))
#print(known_architectures)
known_generators = {'internal', 'sail', 'manual'}

parser = argparse.ArgumentParser(description='Runs a TestRIG configuration')

# model args
parser.add_argument('-a', '--implementation-A', metavar='IMP', choices=known_rvfi_dii,
  default='sail',
  help="The implementation A to use. (one of {:s})".format(str(known_rvfi_dii)))
parser.add_argument('--implementation-A-port', metavar='PORT', type=auto_int, default=0,
  help="The port to use for implementation A's rvfi-dii server")
parser.add_argument('--implementation-A-log', metavar='PATH',
  default=None, type=auto_write_fd,
  #nargs='?', const=sub.PIPE,
  help="Turn on logging for implementation A's rvfi-dii server (optionally specifying a file path)")
# implementation args
parser.add_argument('-b', '--implementation-B', metavar='IMP', choices=known_rvfi_dii,
  default='sail',
  help="The implementation B to use. (one of {:s})".format(str(known_rvfi_dii)))
parser.add_argument('--implementation-B-port', metavar='PORT', type=auto_int, default=0,
  help="The port to use for implementation B's rvfi-dii server")
parser.add_argument('--implementation-B-log', metavar='PATH',
  default=None, type=auto_write_fd,
  #nargs='?', const=sub.PIPE,
  help="Turn on logging for implementation B's rvfi-dii server (optionally specifying a file path)")
# verification engine args
parser.add_argument('-e', '--verification-engine', metavar='VENG', choices=known_vengine,
  default='QCVEngine',
  help="The verification engine to use. (one of {:s})".format(str(known_vengine)))
# general configuration args
parser.add_argument('-s', '--spawn-delay', metavar='DELAYSEC', default=5, type=auto_int,
  help="Specify a number of seconds to wait between server creation and verification engine startup.")
parser.add_argument('-v', '--verbosity', metavar='VERB', type=auto_int,
  default=1, help="Increase verbosity level by adding more \"v\".")
parser.add_argument('-S', '--save-dir', metavar='SAVEDIR', type=str,
  help="Keep running, saving each failure to directory provided.")
parser.add_argument('-n', '--number-of-tests', metavar='NTESTS', type=auto_int,
  default=100, help="Runs the verification engine for NTESTS tests.")
parser.add_argument('-t', '--trace-file', metavar='FILENAME', type=str,
  help="Runs the test specified in FILENAME")
parser.add_argument('-d', '--trace-dir', metavar='DIRNAME', type=str,
  help="Runs the tests contained in DIRNAME")
parser.add_argument('--path-to-rvbs-dir', metavar='PATH', type=str,
  default=op.join(implementations_path, "RVBS/output/"),
  help="The PATH to the rvbs executable directory")
parser.add_argument('--path-to-spike', metavar='PATH', type=str,
  default=None, help="The PATH to the spike executable")
parser.add_argument('--path-to-qemu', metavar='PATH', type=str,
  default=None, help="The PATH to the qemu executable")
parser.add_argument('--path-to-piccolo', metavar='PATH', type=str,
  default=op.join(implementations_path, "Piccolo/builds/RV32IMUxCHERI_RVFI_DII_Piccolo_bluesim/exe_HW_sim"),
  help="The PATH to the Piccolo executable")
parser.add_argument('--path-to-flute', metavar='PATH', type=str,
  default=op.join(implementations_path, "Flute/builds/RV64ACDFIMSUxCHERI_RVFI_DII_Flute_bluesim/exe_HW_sim"),
  help="The PATH to the Flute executable")
parser.add_argument('--path-to-toooba', metavar='PATH', type=str,
  default=op.join(implementations_path, "Toooba/builds/RV64ACDFIMSUxCHERI_Toooba_RVFI_DII_bluesim/exe_HW_sim"),
  help="The PATH to the Toooba executable")
parser.add_argument('--path-to-ibex', metavar='PATH', type=str,
  default=op.join(implementations_path, "ibex/build/lowrisc_ibex_ibex_testrig_0/default-verilator/Vibex_top_sram"),
  help="The PATH to the Ibex executable")
parser.add_argument('--path-to-muntjac', metavar='PATH', type=str,
  default=op.join(implementations_path, "muntjac/bin/muntjac_core"),
  help="The PATH to the Muntjac executable")
parser.add_argument('--path-to-QCVEngine', metavar='PATH', type=str,
  default=op.join(vengines_path, "QuickCheckVEngine/bin/QCVEngine"),
  help="The PATH to the QCVEngine executable")
parser.add_argument('--path-to-sail-riscv-dir', metavar='PATH', type=str,
  default=None,  # This value is set to None so that later it can be set depending on whether CHERI is enabled or not.
  help="The PATH to the directory containing the sail executable. Examples: riscv-implementations/sail-riscv and riscv-implementations/sail-cheri-riscv")
parser.add_argument('-r', '--architecture', type=str.lower, metavar='ARCH', choices=list(map(str.lower, known_architectures)),
  default='rv32i',
  help="""The architecture to verify, where ARCH is a non case sensitive string
  of the form 'rv{32,64}g[c][n]', or 'rv{32,64}i[m][a][f][d][n]' optionally followed
  by an '_'-separated list of one or more of {Zicsr, Zifencei, Xcheri}
  appearing in that order (e.g. rv64ifcXcheri, rv64imd,
  rv32imafZicsr_Zifencei_Xcheri ...)""")
parser.add_argument('--verification-archstring', type=str.lower, metavar='ARCH',
  help="""The architecture string to pass to the verification engine, (defaults to the value of --architecture).
  Setting a different value here (such as rv64xcheri) allows verifying only a subset of the extensions without
  runnining all the integer tests before.""")
parser.add_argument('--test-include-regex', type=str, metavar='regex',
  help="""A regex describing the subset of tests to run on the verification engine, (defaults to all tests).""")
parser.add_argument('--test-exclude-regex', type=str, metavar='regex',
  help="""A regex describing the subset of tests to exclude (overriding test-include-regex) on the verification engine, (defaults to no tests).""")
parser.add_argument('--csr-include-regex', type=str, metavar='regex',
  help="""A regex describing the subset of CSRs to include in tests, (defaults to all CSRs).""")
parser.add_argument('--csr-exclude-regex', type=str, metavar='regex',
  help="""A regex describing the subset of CSRs to exclude (overriding csr-include-regex) on the verification engine, (defaults to no CSRs).""")
parser.add_argument('--support-misaligned', action=argparse.BooleanOptionalAction, default=False,
  help="""Enable misaligned memory accesses""")
parser.add_argument('--generator', metavar='GENERATOR', choices=known_generators,
  default='internal',
  help="The instruction generator to use. (one of {:s})".format(str(known_generators)))
parser.add_argument('--path-to-generator', metavar='PATH', type=str,
  default=op.join(vengines_path, "sail-riscv-test-generation/main.native"),
  help="The PATH to the instruction generation (not needed for internal or manual generators)")
parser.add_argument('--generator-port', metavar='PORT', default=5002, type=auto_int,
  help="Use instruction generator on given port.")
parser.add_argument('--generator-log', metavar='PATH', default=None, type=auto_write_fd,
  help="Log instruction generator output")
parser.add_argument('--timeout', type=int, default=0, help="Timeout after N secods")
parser.add_argument('-j', '--parallel-jobs', metavar='JOBS', default=1, type=auto_int,
  help="Spawn the VEngine and implementations multiple times for parallel jobs")
parser.add_argument('-l', '--parallel-log', action='count', default=0,
  help="Enable parallel logging in ./parallel-logs. Note that this may use lots of space")
parser.add_argument('--relaxed-comparison', action='count', default=0,
  help="Compare a reduced set of RVFI fields in VEngine")
parser.add_argument('--strict-comparison', action='count', default=0,
  help="Compare all RVFI fields in VEngine")
parser.add_argument('--no-shrink', action='count', default=0,
  help="Disable VEngine test case shrinking")
parser.add_argument('--no-save', action='count', default=0,
  help="Don't ask to save files")
parser.add_argument('--continue-on-fail', action='count', default=0,
  help="Continue when encountering a failure")
parser.add_argument('--test-len', metavar='LEN', default=None, type=auto_int,
  help="Tell vengine to generate tests up to LEN instructions long")
parser.add_argument('--supported-features', metavar='FEAT', type=str,
  help="Specify supported features to vengine, separated by '_'. Each feature should begin with 'X'.")

# Use argcomplete to provide bash tab completion (https://github.com/kislyuk/argcomplete)
try:
  import argcomplete
  argcomplete.autocomplete(parser)
except ImportError:
  argcomplete = None
args = parser.parse_args()

###########
# helpers #
###########

class ISA_Configuration:
  has_xlen_32 = False
  has_xlen_64 = False
  support_misaligned = False
  archstring = None

  def has(self, ext_name: str):
    assert not ext_name.lower().startswith('x')
    assert not ext_name.lower().startswith('z')
    return self.ext_map.get(ext_name.lower(), False)

  @property
  def has_cheri(self):
    return self.has("cheri")

  @property
  def has_icsr(self):
    return self.has("icsr")

  @property
  def has_ifencei(self):
    return self.has("ifencei")

  def __init__(self, archstring):
    parts = list(filter(None, re.split("[_zx]", archstring.lower())))
    self.archstring = archstring
    if parts[0][:4] == 'rv32':
      self.has_xlen_32 = True
    elif parts[0][:4] == 'rv64':
      self.has_xlen_64 = True
    else:
      print("ERROR: ISA string must start with rv32 or rv64")
      exit(-1)
    self.std_extensions = parts[0][4:]
    self.ext_map = {}
    for letter in self.std_extensions:
      if letter in ('i', 'm', 's', 'a', 'f', 'd', 'c', 'n'):
        self.ext_map[letter] = True
      elif letter == 'g':
        # G enables imafd+icsr+ihpm+ifencei
        for ext in ('i', 'm', 's', 'a', 'f', 'd', 'icsr', 'ihpm', 'ifencei'):
          self.ext_map[ext] = True
        self.ext_map['g'] = True
      else:
        print("ERROR: Unknown standard extension '"+letter+"'")
        exit(-1)
    self.extensions = parts[1:]
    for extension in self.extensions:
      if extension in ('icsr', 'ifencei', 'ihpm', 'cheri'):
        self.ext_map[extension] = True
      else:
        print("ERROR: Extension "+extension+" not currently supported")
        exit(-1)

  def get_rvbs_name(self):
    result = "rvbs-"
    if self.has_xlen_32:
      result += "rv32"
    elif self.has_xlen_64:
      result += "rv64"
    if self.has("i"):
      result += "I"
    if self.has("c"):
      result += "C"
    if self.has("m"):
      result += "M"
    if self.has("a"):
      print("ERROR: A extenstion is currently not supported by RVBS.")
      exit(-1)
    if self.has("f"):
      print("ERROR: F extenstion is currently not supported by RVBS.")
      exit(-1)
    if self.has("d"):
      print("ERROR: D extenstion is currently not supported by RVBS.")
      exit(-1)
    if self.has_icsr:
      result += "Zicsr"
    if self.has_ifencei:
      result += "Zifencei"
    if self.has_cheri:
      result += "Xcheri"
    result += "-rvfi-dii"
    return result

  def get_spike_arch(self):
    result = ""
    if self.has_xlen_32:
      result = "rv32"
    elif self.has_xlen_64:
      result = "rv64"
    if self.has("i"):
      result += "i"
      if not (self.has_icsr and self.has_ifencei):
        print("WARNING: enabling I in spike also automatically enables icsr and ifencei extenstions.")
    if self.has("m"):
      result += "m"
    if self.has("a"):
      result += "a"
    if self.has("f"):
      result += "f"
    if self.has("d"):
      result += "d"
    if self.has("c"):
      result += "c"
    if self.has_cheri:
      print("Make sure you have build Spike with CHERI with 'make spike-cheri'")
    return result

  def get_qemu_cpu(self):
    # See cpu.c: static Property riscv_cpu_properties[]
    # Only e is off by default
    supported_qemu_exts = list("iegmafdc") + ["Counters", "Zifencei"]
    # Explicitly disable QEMU extensions that are on by default and selectively enable
    ext_map = {k: k + "=false" for k in supported_qemu_exts}
    # TestRIG expects s,u,Zicsr to be on by default:
    ext_map["s"] = "s=true"
    ext_map["u"] = "u=true"
    print("WARNING: enabling s and u extensions by default in QEMU.")
    if not self.has_icsr:
      ext_map["Zicsr"] = "Zicsr=true"
      print("WARNING: enabling Zicsr extension by default in QEMU.")
    # TODO: mmu/pmp/priv_spec?
    # DEFINE_PROP_STRING("priv_spec", RISCVCPU, cfg.priv_spec),
    # DEFINE_PROP_BOOL("mmu", RISCVCPU, cfg.mmu, true),
    # DEFINE_PROP_BOOL("pmp", RISCVCPU, cfg.pmp, true),
    result = ""
    if self.has_xlen_32:
      result = "rv32"
    elif self.has_xlen_64:
      result = "rv64"
    for ext, value in self.ext_map.items():
      # Note: QEMU expects ,Xcheri=true to be passed
      if ext == "cheri":
        ext = "Xcheri"
      if ext == "icsr":
        ext = "Zicsr"
      if ext == "ihpm":
        ext = "Counters"
      if ext == "ifencei":
        ext = "Zifencei"
      ext_map[ext] = ext + "=" + str(value).lower()
    result += "," + ",".join(ext_map.values())
    return result

  def get_sail_name(self):
    result = "riscv_rvfi"
    #if self.has_icsr:
    #  print("ERROR: Sail currently does not support CSRs.")
    #  exit(-1)
    #TODO check if there are other configurations that Sail does not yet support and throw an error.
    if self.has_cheri:
      result = "cheri_" + result
    if self.has_xlen_32:
      result += "_RV32"
    elif self.has_xlen_64:
      result += "_RV64"
    return result


def verboseprint(lvl, msg):
  if args.verbosity >= lvl:
    print(msg)

def input_y_n(prompt):
  s = input(prompt)
  return s.lower() in ["", "y", "ye", "yes"]

#########################
# spawn rvfi_dii server #
#########################

def spawn_rvfi_dii_server(name, port, log, isa_def):
  # few common variables
  use_log = open(os.devnull, "w")
  if log:
    use_log = log
  if 'x' in isa_def.archstring:
    # x Splits the standard RISC-V exenstions (e.g. rv32i) from non-standard ones like CHERI
    extension = isa_def.archstring.split('x', maxsplit=1)[1]
  else:
    # No extension specified in the architecture string
    extension = ""

  env2 = os.environ.copy()
  cmd = []
  ##############################################################################
  if name == 'spike':
    if args.path_to_spike is None:
      args.path_to_spike = op.join(implementations_path, "riscv-isa-sim/build")
      if isa_def.has_cheri:
        args.path_to_spike += "-cheri"
      if isa_def.support_misaligned:
        args.path_to_spike += "-misaligned"
      args.path_to_spike += "/spike"
    cmd = [args.path_to_spike, "--rvfi-dii-port", str(port),
           "--isa={:s}".format(isa_def.get_spike_arch()), "-m0x80000000:0x10000"]
    if "LD_LIBRARY_PATH" in env2:
      env2["LD_LIBRARY_PATH"] = "%s:%s" % (env2["LD_LIBRARY_PATH"], op.dirname(args.path_to_spike))
    else:
      env2["LD_LIBRARY_PATH"] = op.dirname(args.path_to_spike)

    if log:
      cmd += ["-l"]

    if extension != "" and extension != "cheri":
      cmd += ["--extension={:s}".format(extension)]
  ##############################################################################
  elif name == 'qemu':
    if args.path_to_qemu is None:
      args.path_to_qemu = op.join(implementations_path, "qemu/build")
      # TODO: allow testing against non-CHERI versions
      if isa_def.has_xlen_32:
        args.path_to_qemu += "/riscv32-softmmu/qemu-system-riscv32"
      else:
        args.path_to_qemu += "/riscv64cheri-softmmu/qemu-system-riscv64cheri"
    cmd = [args.path_to_qemu, "--rvfi-dii-port", str(port),
           "-cpu", isa_def.get_qemu_cpu(), "-bios", "none"]
    if not isa_def.support_misaligned:
      sys.exit("FATAL: --support-misaligned must be passed for QEMU since there is no way of turning it off")
    if log:
      cmd += ["-D", "/dev/stderr", "-d", "instr"]
  ##############################################################################
  elif name == 'rvbs':
    env2["RVFI_DII_PORT"] = str(port)
    print("starting RVBS server.")
    cmd = [op.join(args.path_to_rvbs_dir, isa_def.get_rvbs_name())]
    if log:
      cmd += ["+itrace"]
  ##############################################################################
  elif name == 'piccolo':
    env2["RVFI_DII_PORT"] = str(port)
    cmd = [args.path_to_piccolo, "+v2" if log else "+v0"]
  ##############################################################################
  elif name == 'flute':
    env2["RVFI_DII_PORT"] = str(port)
    cmd = [args.path_to_flute, "+v2" if log else "+v0"]
  ##############################################################################
  elif name == 'toooba':
    env2["RVFI_DII_PORT"] = str(port)
    cmd = [args.path_to_toooba]
  ##############################################################################
  elif name == 'sail':
    if args.path_to_sail_riscv_dir is None:
      args.path_to_sail_riscv_dir = op.join(implementations_path, "sail-")
      if isa_def.has_cheri:
        args.path_to_sail_riscv_dir += "cheri-"
      args.path_to_sail_riscv_dir += "riscv/c_emulator/"
    full_sail_sim = op.join(op.dirname(op.realpath(__file__)), args.path_to_sail_riscv_dir, isa_def.get_sail_name())
    cmd = [full_sail_sim]
    if not isa_def.has("c"):
      #cmd += ["--disable-compressed"]
      cmd += ["-C"]
    if isa_def.support_misaligned:
      #cmd += ["--enable-misaligned"]
      cmd += ["-m"]
    #cmd += ["--rvfi-dii", str(port)]
    cmd += ["-r", str(port)]
    cmd += ["-i"]
    cmd += ["--c-hints-expand"]
  ##############################################################################
  elif name == 'ibex':
    cmd = [args.path_to_ibex, str(port), "3"]
  ##############################################################################
  elif name == 'muntjac':
    env2["RVFI_DII_PORT"] = str(port)
    cmd = [args.path_to_muntjac]
  ##############################################################################
  elif name == 'manual':
    return None
  ##############################################################################
  elif name == 'none':
    return None
  ##############################################################################
  else:
    print("Unknown rvfi-dii server {:s}".format(name))
    return None
  ##############################################################################
  print("running rvfi-dii server as: ", " ".join(cmd))
  p = sub.Popen(cmd, env=env2, stdin=None, stdout=use_log, stderr=use_log)
  print('spawned {:s} rvfi-dii server on port: {:d} ({})'.format(name, port, cmd))
  return p

#############################
# spawn verification engine #
#############################

def spawn_vengine(name, mport, iport, arch, log):
  if name == 'QCVEngine-docker':
    cmd = ["docker", "run", "--rm", "-t", "-p", f"{mport}:{mport}", "-p", f"{iport}:{iport}",
           "ctsrd/testrig", "./TestRIG/vengines/QuickCheckVEngine/dist/build/QCVEngine/QCVEngine",
           # Use the host IP addresses in the docker container:
           "-A", "host.docker.internal", "-B", "host.docker.internal"]
    useQCVEngine = True
  elif name == 'QCVEngine':
    cmd = [args.path_to_QCVEngine]
    useQCVEngine = True
  else:
    cmd = ["false"]
    useQCVEngine = False

  if useQCVEngine:
    if args.supported_features is not None:
      QCVEngineArch = str(arch) + "_" + args.supported_features
    else:
      QCVEngineArch = str(arch)
    cmd += ['-a', str(mport), '-b', str(iport), '-r', QCVEngineArch]
    cmd += ['-n', str(args.number_of_tests)]
    cmd += ['-v', str(args.verbosity)]
    relaxed = True
    if args.generator != 'internal':
      cmd += ['-i', str(args.generator_port)]
    if args.trace_file:
      print("using trace_file {:s}".format(args.trace_file))
      cmd += ['-t', args.trace_file]
    if args.trace_dir:
      print("using trace_dir {:s}".format(args.trace_dir))
      cmd += ['-d', args.trace_dir]
    if args.save_dir:
      if not os.path.isdir(args.save_dir):
        sys.exit("--save-dir does not exist: " + os.path.abspath(args.save_dir))
      cmd += ['-s', args.save_dir]
    if args.timeout:
      cmd += ['--timeout', str(int(args.timeout * 1000000))]
    if args.no_shrink:
      cmd += ['-S']
    if args.relaxed_comparison:
      relaxed = True
    if args.strict_comparison:
      relaxed = False
    if args.no_save:
      cmd += ['--no-save']
    if args.continue_on_fail:
      cmd += ['--continue-on-fail']
    if args.test_len:
      cmd += ['-L', str(args.test_len)]
    if args.test_include_regex:
      cmd += ['-I', args.test_include_regex]
    if args.test_exclude_regex:
      cmd += ['-x', args.test_exclude_regex]
    if args.csr_include_regex:
      cmd += ['--csr-include-regex', args.csr_include_regex]
    if args.csr_exclude_regex:
      cmd += ['--csr-exclude-regex', args.csr_exclude_regex]
    if args.implementation_A == 'none' or args.implementation_B == 'none':
      cmd += ['--single-implementation']
    if not relaxed:
      cmd += ['--strict-comparison']
    print("running qcvengine as: ", " ".join(cmd))
    if log is None:
      p = sub.Popen(cmd)
    else:
      p = sub.Popen(cmd, stdout=log)
    return p
  else:
    print("Unknown verification engine {:s}".format(name))

#######################################
# spawn instruction generation engine #
#######################################

def spawn_generator(name, arch, log):
  if name == "sail":
    if log:
      use_log = log
    elif args.verbose > 0:
      use_log = sys.stdout
    else:
      use_log = open(os.devnull, "w")

    if 'x' in arch:
      # x Splits the standard RISC-V exenstions (e.g. rv32i) from non-standard ones like CHERI
      isa = arch.split('x')[0]
    else:
      # No extension specified in the architecture string
      isa = arch
    cmd = [args.path_to_generator, '-p', str(args.generator_port)]
    if 'c' not in isa:
      cmd += ['-no_compressed']

    print("running sail generator as: ", " ".join(cmd))
    generator = sub.Popen(cmd, stdout=use_log, stderr=use_log)
    print('spawned sail instruction generator on port: {:d}'.format(args.generator_port))
    return generator
  else:
    return None

#################
# main function #
#################


@dataclass
class LogfileConfiguration:
  impl_a_log: Optional[typing.TextIO]
  impl_b_log: Optional[typing.TextIO]
  generator_log: Optional[typing.TextIO]
  error_log: Optional[typing.TextIO]


def main():
  def kill_procs(servA, servB, gen, vengine):
    for a in servA:
      if a != None:
        print("killing implementation A's rvfi-dii server")
        a.kill()
    for b in servB:
      if b != None:
        print("killing implementation B's rvfi-dii server")
        b.kill()
    for g in generator:
      if g != None:
        print("killing generator")
        g.kill()
    for v in vengine:
      if v != None:
        v.kill()

  def handle_SIGINT(sig, frame):
    kill_procs(a, b, generator, e)
    exit(0)

  signal.signal(signal.SIGINT, handle_SIGINT)

  # Lists of process handles, sockets being temporarily held, and ports allocated to each process
  a = []
  asocks = []
  aports = []
  b = []
  bsocks = []
  bports = []
  e = []
  generator = []
  isa_def = ISA_Configuration(args.architecture)
  isa_def.support_misaligned = args.support_misaligned
  if args.relaxed_comparison and args.strict_comparison:
    print('Cannot do both relaxed and strict comparison')
    exit(-1)
  # Sanitise inputs based on implementation restrictions
  force_misaligned_support = False
  force_misaligned_no_support = False
  implementations = [args.implementation_A, args.implementation_B]
  if "ibex" in implementations:
    force_misaligned_support = True
  if force_misaligned_support and force_misaligned_no_support:
    print("ERROR: implementations need incompatible misaligned support")
    exit(1)
  if force_misaligned_support:
    print("Forcing support for misaligned since an implementation requires it")
    isa_def.support_misaligned = True
  if force_misaligned_no_support:
    print("Forcing no support for misaligned since an implementation requires it")
    isa_def.support_misaligned = False
  # Allow --verification-archstring to override architecture
  vengine_archstring = args.verification_archstring if args.verification_archstring else args.architecture
  try:
    for job in range(args.parallel_jobs):
      # Open a socket to allocate a free port
      asock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      asock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
      asock.bind(('', 0))
      asocks.append(asock)
      bsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      bsock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
      bsock.bind(('', 0))
      bsocks.append(bsock)
    if (args.parallel_jobs > 1):
      try:
        if (args.parallel_log):
          os.mkdir('parallel-logs')
      except FileExistsError:
        pass  # do nothing

    logs: "list[LogfileConfiguration]" = []
    for job in range(args.parallel_jobs):
      if args.parallel_jobs == 1:
        logs.append(LogfileConfiguration(
          impl_a_log=args.implementation_A_log,
          impl_b_log=args.implementation_B_log,
          generator_log=args.generator_log,
          error_log=None))
      else:
        # Ignore user-supplied arguments since they don't make sense for multiple jobs (TODO print error if they are supplied?)
        if args.parallel_log:
          logs.append(LogfileConfiguration(
            impl_a_log=auto_write_fd('parallel-logs/a' + str(job)),
            impl_b_log=auto_write_fd('parallel-logs/b' + str(job)),
            generator_log=auto_write_fd('parallel-logs/g' + str(job)),
            error_log=auto_write_fd('parallel-logs/v' + str(job))))
        else:
          logs.append(LogfileConfiguration(None, None, None, None))

      if (args.implementation_A_port != 0):
        aports.append(args.implementation_A_port)
      else:
        aports.append(asocks[job].getsockname()[1])
      asocks[job].close()
      if (args.implementation_B_port != 0):
        bports.append(args.implementation_B_port)
      else:
        bports.append(bsocks[job].getsockname()[1])
      bsocks[job].close()

      a.append(spawn_rvfi_dii_server(args.implementation_A, aports[job],
                                     logs[job].impl_a_log, isa_def))
      b.append(spawn_rvfi_dii_server(args.implementation_B, bports[job],
                                     logs[job].impl_b_log, isa_def))

    time.sleep(args.spawn_delay)  # small delay to give time to the spawned servers to be ready to listen

    for job in range(args.parallel_jobs):
      if a[job] is not None and a[job].poll() is not None:
        print("ERROR: Implementation A failed to start!")
        print(" ".join(a[job].args), "failed with exit code", a[job].poll())
        exit(1)
      if b[job] is not None and b[job].poll() is not None:
        print("ERROR: Implementation B failed to start!")
        print(" ".join(b[job].args), "failed with exit code", b[job].poll())
        exit(1)
      e.append(spawn_vengine(args.verification_engine, aports[job], bports[job],
                             vengine_archstring, logs[job].error_log))

    # TODO support non-standard generator in parallel builds
    generator.append(spawn_generator(args.generator, args.architecture,
                                     logs[0].generator_log))

    # Periodic non-blocking loop over processes to terminate those that finish early
    alive = args.parallel_jobs
    while alive > 1:
      alive = 0
      time.sleep(5) # Wait for 5 seconds in between polls
      for job in range(args.parallel_jobs):
        if e[job].poll() == None:
          alive += 1
        else:
          # Kill the process, since it is done
          kill_procs([a[job]],[b[job]],[None],[e[job]])
          a[job] = None
          b[job] = None
          # Keep a handle to the vengine so we can get the returncode later

    retMax = 0
    for job in range(args.parallel_jobs):
      e[job].wait()
      retMax = max(retMax,e[job].returncode)

    print('run terminated')
    exit(retMax)
  finally:
    time.sleep(2)
    kill_procs(a, b, generator, e)

if __name__ == "__main__":
  main()
