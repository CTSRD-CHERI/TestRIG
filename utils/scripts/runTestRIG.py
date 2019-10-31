#! /usr/bin/env python3

#-
# SPDX-License-Identifier: BSD-2-Clause
#
# Copyright (c) 2018-2019 Alexandre Joannou
# Copyright (c) 2019 Peter Rugg
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

import argparse
import os
import signal
import os.path as op
import subprocess as sub
import time
import sys
import re

################################
# Parse command line arguments #
################################################################################

def auto_int (x):
  return int(x,0)

def auto_pos_int (x):
  val = int(x,0)
  if val <= 0:
    raise argparse.ArgumentTypeError("argument must be a positive int. Got {:d}.".format(val))
  return val

def auto_write_fd (fname):
  return open(fname, 'w')

def std_ext (ext_name):
  return ["", ext_name]

def z_ext (ext_name):
  return ["", "Z"+ext_name]

def x_ext (ext_name):
  return ["", "X"+ext_name]

known_rvfi_dii = set({'spike', 'rvbs', 'sail', 'piccolo', 'ibex', 'manual'})
known_vengine  = set({'QCVEngine'})
multi_letter_exts = ["_".join(filter(None, [e0,e1,e2])) for e0 in z_ext("icsr")
                                                        for e1 in z_ext("ifencei")
                                                        for e2 in x_ext("cheri")]
known_architectures = sorted(set([e0+e1+e2+e3+e4+e5+e6
                                   for e0 in ["rv32i", "rv64i"]
                                   for e1 in std_ext("m")
                                   for e2 in std_ext("a")
                                   for e3 in std_ext("f")
                                   for e4 in std_ext("d")
                                   for e5 in std_ext("c")
                                   for e6 in multi_letter_exts]
                                 + [e0+e1+e2
                                   for e0 in ["rv32g", "rv64g"]
                                   for e1 in std_ext("c")
                                   for e2 in x_ext("cheri")]
                                ))
#print(known_architectures)
known_generators = set({'internal', 'sail', 'manual'})

parser = argparse.ArgumentParser(description='Runs a TestRIG configuration')

# model args
parser.add_argument('-a', '--implementation-A', metavar='IMP', choices=known_rvfi_dii,
  default='sail',
  help="The implementation A to use. (one of {:s})".format(str(known_rvfi_dii)))
parser.add_argument('--implementation-A-port', metavar='PORT', type=auto_int, default=5000,
  help="The port to use for implementation A's rvfi-dii server")
parser.add_argument('--implementation-A-log', metavar='PATH',
  default=None, type=auto_write_fd,
  #nargs='?', const=sub.PIPE,
  help="Turn on logging for implementation A's rvfi-dii server (optionally specifying a file path)")
# implementation args
parser.add_argument('-b', '--implementation-B', metavar='IMP', choices=known_rvfi_dii,
  default='spike',
  help="The implementation B to use. (one of {:s})".format(str(known_rvfi_dii)))
parser.add_argument('--implementation-B-port', metavar='PORT', type=auto_int, default=5001,
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
parser.add_argument('-s', '--spawn-delay', metavar='DELAYSEC', default=1, type=auto_int,
  help="Specify a number of seconds to wait between server creation and verification engine startup.")
parser.add_argument('-v', '--verbose', action='count', default=0,
  help="Increase verbosity level by adding more \"v\".")
parser.add_argument('-S', '--save-dir', metavar= 'SAVEDIR', type=str,
  help="Keep running, saving each failure to directory provided.")
parser.add_argument('-n', '--number-of-tests', metavar= 'NTESTS', type=auto_int,
  default=100, help="Runs the verification engine for NTESTS tests.")
parser.add_argument('-t', '--trace-file', metavar= 'FILENAME', type=str,
  help="Runs the test specified in FILENAME")
parser.add_argument('-d', '--trace-dir', metavar= 'DIRNAME', type=str,
  help="Runs the tests contained in DIRNAME")
parser.add_argument('--path-to-rvbs-dir', metavar='PATH', type=str,
  #default='rvbs-rv32i-rvfi-dii',
  default=op.join(op.dirname(op.realpath(__file__)), "../../riscv-implementations/RVBS/output/"),
  help="The PATH to the rvbs executable directory")
parser.add_argument('--path-to-spike', metavar='PATH', type=str,
  default=None, help="The PATH to the spike executable")
parser.add_argument('--path-to-piccolo', metavar='PATH', type=str,
  default=op.join(op.dirname(op.realpath(__file__)), "../../riscv-implementations/Piccolo/builds/RV32IMUxCHERI_RVFI_DII_Piccolo_bluesim/exe_HW_sim"),
  help="The PATH to the Piccolo executable")
parser.add_argument('--path-to-ibex', metavar='PATH', type=str,
  default=op.join(op.dirname(op.realpath(__file__)), "../../riscv-implementations/ibex/verilator/obj_dir/Vibex_core_avalon"),
  help="The PATH to the Ibex executable")
parser.add_argument('--path-to-QCVEngine', metavar='PATH', type=str,
  #default='QCVEngine',
  default=op.join(op.dirname(op.realpath(__file__)), "../../vengines/QuickCheckVEngine/dist/build/QCVEngine/QCVEngine"),
  #default=op.join(op.dirname(op.realpath(__file__)), "../../vengines/QuickCheckVEngine/dist-newstyle/build/x86_64-linux/ghc-8.6.3/QCVEngine-0.1.0.0/x/QCVEngine/build/QCVEngine/QCVEngine"),
  help="The PATH to the QCVEngine executable")
parser.add_argument('--path-to-sail-riscv-dir', metavar='PATH', type=str,
  default=None, #This value is set to None so that later it can be set depending on whether CHERI is enabled or not.
  help="The PATH to the directory containing the sail executable. Examples: riscv-implementations/sail-riscv and riscv-implementations/sail-cheri-riscv")
parser.add_argument('-r', '--architecture', type = str.lower, metavar='ARCH', choices=map(str.lower, known_architectures),
  default='rv32i',
  help="""The architecture to verify, where ARCH is a non case sensitive string
  of the form 'rv{32,64}g[c]', or 'rv{32,64}i[m][a][f][d]' optionally followed
  by an '_'-separated list of one or more of {Zicsr, Zifencei, Xcheri}
  appearing in that order (e.g. rv64ifcXcheri, rv64imd,
  rv32imafZicsr_Zifencei_Xcheri ...)""")
parser.add_argument('--generator', metavar='GENERATOR', choices=known_generators,
  default='internal',
  help="The instruction generator to use. (one of {:s})".format(str(known_generators)))
parser.add_argument('--path-to-generator', metavar='PATH', type=str,
  default=op.join(op.dirname(op.realpath(__file__)), "../../vengines/sail-riscv-test-generation/main.native"),
  help="The PATH to the instruction generation (not needed for internal or manual generators)")
parser.add_argument('--generator-port', metavar='PORT', default=5002, type=auto_int,
  help="Use instruction generator on given port.")
parser.add_argument('--generator-log', metavar='PATH', default=None, type=auto_write_fd,
  help="Log instruction generator output")

args = parser.parse_args()

###########
# helpers #
###########

class ISA_Configuration:
  has_xlen_32 = False
  has_xlen_64 = False
  has_i = False
  has_m = False
  has_a = False
  has_f = False
  has_d = False
  has_c = False
  has_icsr = False
  has_ifencei = False
  has_cheri = False

  def __init__(self, archstring):
    parts = list(filter(None, re.split("[_zx]", archstring.lower())))
    if parts[0][:4] == 'rv32':
      self.has_xlen_32 = True
    elif parts[0][:4] == 'rv64':
      self.has_xlen_64 = True
    else:
      print("ERROR: ISA string must start with rv32 or rv64")
      exit(-1)
    for letter in parts[0][4:]:
      if letter == 'i':
        self.has_i = True
      elif letter == 'm':
        self.has_m = True
      elif letter == 'a':
        self.has_a = True
      elif letter == 'f':
        self.has_f = True
      elif letter == 'd':
        self.has_d = True
      elif letter == 'c':
        self.has_c = True
      elif letter == 'g':
        self.has_i = True
        self.has_m = True
        self.has_a = True
        self.has_f = True
        self.has_d = True
        self.has_icsr = True
        self.has_ifencei = True
      else:
        print("ERROR: Unknown standard extension '"+letter+"'")
        exit(-1)
    for extension in parts[1:]:
      if extension == "icsr":
        self.has_icsr = True
      elif extension == "ifencei":
        self.has_ifencei = True
      elif extension == "cheri":
        self.has_cheri = True
      else:
        print("ERROR: Extension "+extension+" not currently supported")
        exit(-1)

  def get_rvbs_name(self):
    result = "rvbs-"
    if self.has_xlen_32:
      result += "rv32"
    elif self.has_xlen_64:
      result += "rv64"
    if self.has_i:
      result += "I"
    if self.has_c:
      result += "C"
    if self.has_m:
      result += "M"
    if self.has_a:
      print("ERROR: A extenstion is currently not supported by RVBS.")
      exit(-1)
    if self.has_f:
      print("ERROR: F extenstion is currently not supported by RVBS.")
      exit(-1)
    if self.has_d:
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
    if self.has_i:
      result += "i"
      if not (self.has_icsr and self.has_ifencei):
        print("WARNING: enabling I in spike also automatically enables icsr and ifencei extenstions.")
    if self.has_c:
      result += "c"
    if self.has_m:
      result += "m"
    if self.has_a:
      result += "a"
    if self.has_f:
      result += "f"
    if self.has_d:
      result += "d"
    if self.has_cheri:
      print("Make sure you have build Spike with CHERI with 'make spike-cheri'")
    return result

  def get_sail_name(self):
    result = "riscv_rvfi"
    if self.has_icsr:
      print("ERROR: Sail currently does not support CSRs.")
      exit(-1)
    #TODO check if there are other configurations that Sail does not yet support and throw an error.
    if self.has_cheri:
      result = "cheri_" + result
      if self.has_xlen_32:
        result += "_RV32"
      elif self.has_xlen_64:
        result += "_RV64"
    return result


def verboseprint(lvl,msg):
  if args.verbose >= lvl:
    print(msg)

def input_y_n(prompt):
  s = input(prompt)
  return s.lower() in ["", "y", "ye", "yes"]

#########################
# spawn rvfi_dii server #
#########################

def spawn_rvfi_dii_server(name, port, log, arch="rv32i"):
  ## few common variables
  isa_def = ISA_Configuration(arch)
  use_log = open(os.devnull,"w")
  if log:
    use_log = log
  if 'x' in arch:
    # x Splits the standard RISC-V exenstions (e.g. rv32i) from non-standard ones like CHERI
    [isa, extension] = arch.split('x')
  else:
    # No extension specified in the architecture string
    [isa, extension] = [arch, ""]

  env2 = os.environ.copy()
  cmd = []
  ##############################################################################
  if (name == 'spike'):
    # This is a bit of a hack, but necessary for now, since the Spike implentation doesn't support underscores in the ISA string parsing.
    if '_' in isa:
      newIsa = isa.split('_')[0]
    else:
      newIsa = isa
    if args.path_to_spike is None:
      args.path_to_spike = "../../riscv-implementations/riscv-isa-sim/build"
      if isa_def.has_cheri:
        args.path_to_spike += "-cheri"
      args.path_to_spike += "/spike"
    cmd = [op.join(op.dirname(op.realpath(__file__)), args.path_to_spike), "--rvfi-dii-port", str(port),"--isa={:s}".format(isa_def.get_spike_arch()), "-m0x80000000:0x10000"]
    if "LD_LIBRARY_PATH" in env2:
      env2["LD_LIBRARY_PATH"] = "%s:%s" % (env2["LD_LIBRARY_PATH"], op.dirname(args.path_to_spike))
    else:
      env2["LD_LIBRARY_PATH"] = op.dirname(args.path_to_spike)

    if log:
      cmd += ["-l"]

    if extension != "" and extension != "cheri":
      cmd += ["--extension={:s}".format(extension)]
  ##############################################################################
  elif (name == 'rvbs'):
    env2["RVFI_DII_PORT"] = str(port)
    print("starting RVBS server.")
    cmd = [op.join(args.path_to_rvbs_dir, isa_def.get_rvbs_name())]
    if log:
      cmd += ["+itrace"]
  ##############################################################################
  elif (name == 'piccolo'):
    env2["RVFI_DII_PORT"] = str(port)
    cmd = [args.path_to_piccolo]
  ##############################################################################
  elif (name == 'sail'):
    if args.path_to_sail_riscv_dir is None:
      args.path_to_sail_riscv_dir = "../../riscv-implementations/sail-"
      if isa_def.has_cheri:
        args.path_to_sail_riscv_dir += "cheri-"
      args.path_to_sail_riscv_dir += "riscv/c_emulator/"
    full_sail_sim = op.join(op.dirname(op.realpath(__file__)), args.path_to_sail_riscv_dir, isa_def.get_sail_name())
    if isa_def.has_c:
      cmd = [full_sail_sim, "-r", str(port)]
    else:
      cmd = [full_sail_sim, "-C", "-r", str(port)]
  ##############################################################################
  elif (name == 'ibex'):
    cmd = [args.path_to_ibex, 'localhost', str(port)]
  ##############################################################################
  elif (name == 'manual'):
    return None
  ##############################################################################
  else:
    print("Unknown rvfi-dii server {:s}".format(name))
    return None
  ##############################################################################
  print("running rvfi-dii server as: ", " ".join(cmd))
  p = sub.Popen(cmd, env=env2, stdin=None, stdout=use_log, stderr=use_log)
  print('spawned {:s} rvfi-dii server on port: {:d}'.format(name, port))
  return p

#############################
# spawn verification engine #
#############################

def spawn_vengine(name, mport, iport, arch):
  if (name == 'QCVEngine'):
    cmd = [args.path_to_QCVEngine, '-a', str(mport), '-b', str(iport), '-r', str(arch)]
    cmd += ['-n', str(args.number_of_tests)]
    if args.verbose > 0:
      cmd += ['-v']
    if (args.generator != 'internal'):
      cmd += ['-i', str(args.generator_port)]
    if (args.trace_file):
      print("using trace_file {:s}".format(args.trace_file))
      cmd += ['-t', args.trace_file]
    if (args.trace_dir):
      print("using trace_dir {:s}".format(args.trace_dir))
      cmd += ['-d', args.trace_dir]
    if (args.save_dir):
      cmd += ['-s', args.save_dir]
    print("running qcvengine as: ", " ".join(cmd))
    p = sub.Popen(cmd)
    return p
  else:
    if generator:
      generator.kill()
    print("Unknown verification engine {:s}".format(name))

#######################################
# spawn instruction generation engine #
#######################################

def spawn_generator(name, arch, log):
  if name == "sail":
    if log:
      use_log = log
    elif args.verbose > 0:
      use_log = os.sys.stdout
    else:
      use_log = open(os.devnull,"w")

    if 'x' in arch:
      # x Splits the standard RISC-V exenstions (e.g. rv32i) from non-standard ones like CHERI
      [isa, extension] = arch.split('x')
    else:
      # No extension specified in the architecture string
      [isa, extension] = [arch, ""]
    cmd = [args.path_to_generator, '-p', str(args.generator_port)]
    if not ('c' in isa.split('x')[0]):
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

def main():
  def kill_procs(servA, servB, gen, vengine):
    if servA:
      print("killing implementation A's rvfi-dii server")
      servA.kill()
    if servB:
      print("killing implementation B's rvfi-dii server")
      servB.kill()
    if generator:
      print("killing generator")
      gen.kill()
    if vengine:
      print("killing vengine")
      vengine.kill()

  def handle_SIGINT(sig, frame):
    kill_procs(a,b,generator,e)
    exit(0)

  signal.signal(signal.SIGINT, handle_SIGINT)

  a = None
  b = None
  e = None
  generator = None
  try:
    a = spawn_rvfi_dii_server(args.implementation_A, args.implementation_A_port, args.implementation_A_log, args.architecture)
    b = spawn_rvfi_dii_server(args.implementation_B, args.implementation_B_port, args.implementation_B_log, args.architecture)

    time.sleep(args.spawn_delay) # small delay to give time to the spawned servers to be ready to listen

    e = spawn_vengine(args.verification_engine, args.implementation_A_port, args.implementation_B_port, args.architecture)
    generator = spawn_generator(args.generator, args.architecture, args.generator_log)

    e.wait()
  except:
    print("Exception happened: " + sys.exc_info()[0])
  else:
    print('run terminated')
    kill_procs(a,b,generator,e)
    exit(0)

if __name__ == "__main__":
  main()
