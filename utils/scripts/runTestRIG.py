#! /usr/bin/env python3

#-
# SPDX-License-Identifier: BSD-2-Clause
#
# Copyright (c) 2018 Alexandre Joannou
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

known_rvfi_dii = set({'spike','rvbs'})
known_vengine  = set({'QCVEngine'})

parser = argparse.ArgumentParser(description='Runs a TestRIG configuration')

# model args
parser.add_argument('-a', '--implementation-A', metavar='IMP', choices=known_rvfi_dii,
  default='rvbs',
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
parser.add_argument('-n', '--number-of-tests', metavar= 'NTESTS', type=auto_int,
  default=100, help="Runs the verification engine for NTESTS tests.")
parser.add_argument('--path-to-rvbs', metavar='PATH', type=str,
  #default='rvbs-rv32i-rvfi-dii',
  default=op.join(op.dirname(op.realpath(__file__)), "../../riscv-implementations/RVBS/output/rvbs-rv32i-rvfi-dii"),
  help="The PATH to the rvbs executable")
parser.add_argument('--path-to-spike', metavar='PATH', type=str,
  default=op.join(op.dirname(op.realpath(__file__)), "../../riscv-implementations/riscv-isa-sim/build/spike"),
  help="The PATH to the spike executable")
parser.add_argument('--path-to-QCVEngine', metavar='PATH', type=str,
  #default='QCVEngine',
  default=op.join(op.dirname(op.realpath(__file__)), "../../vengines/QuickCheckVEngine/dist/build/QCVEngine/QCVEngine"),
  help="The PATH to the QCVEngine executable")

args = parser.parse_args()

###########
# helpers #
###########

def verboseprint(lvl,msg):
  if args.verbose >= lvl:
    print(msg)

def input_y_n(prompt):
  s = input(prompt)
  return s.lower() in ["", "y", "ye", "yes"]

#########################
# spawn rvfi_dii server #
#########################

def spawn_rvfi_dii_server(name, port, log):
  ## few common variables
  use_log = open(os.devnull,"w")
  if log:
    use_log = log
  env2 = os.environ.copy()
  cmd = []
  ##############################################################################
  if (name == 'spike'):
    cmd = [args.path_to_spike, "--rvfi-dii-port", str(port),"--isa=RV32I","--progsize=0","-m0x80000000:0x10000"]
    if log:
      cmd += ["-l"]
  ##############################################################################
  elif (name == 'rvbs'):
    env2["RVFI_DII_PORT"] = str(port)
    cmd = [args.path_to_rvbs]
    if log:
      cmd += ["+itrace"]
  ##############################################################################
  else:
    print("Unknown rvfi-dii server {:s}".format(name))
    return None
  ##############################################################################
  p = sub.Popen(cmd, env=env2, stdin=None, stdout=use_log)
  print('spawned {:s} rvfi-dii server on port: {:d}'.format(name, port))
  return p

#############################
# spawn verification engine #
#############################

def spawn_vengine(name, mport, iport):
  if (name == 'QCVEngine'):
    cmd = [args.path_to_QCVEngine, '-a', str(mport), '-b', str(iport)]
    cmd += ['-n', str(args.number_of_tests)]
    if args.verbose > 0:
      cmd += ['-v']
    p = sub.Popen(cmd)
    return p
  else:
    print("Unknown verification engine {:s}".format(name))

#################
# main function #
#################

def main():
  a = spawn_rvfi_dii_server(args.implementation_A, args.implementation_A_port, args.implementation_A_log)
  b = spawn_rvfi_dii_server(args.implementation_B, args.implementation_B_port, args.implementation_B_log)

  def kill_rvfi_dii_servers():
    if a:
      print("killing implementation A's rvfi-dii server")
      a.kill()
    if b:
      print("killing implementation B's rvfi-dii server")
      b.kill()

  def handle_SIGINT(sig, frame):
    kill_rvfi_dii_servers()
    exit(0)

  signal.signal(signal.SIGINT, handle_SIGINT)

  time.sleep(args.spawn_delay) # small delay to give time to the spawned servers to be ready to listen
  e = spawn_vengine(args.verification_engine, args.implementation_A_port, args.implementation_B_port)
  e.wait()
  print('verification engine run terminated')
  kill_rvfi_dii_servers()
  exit(0)

if __name__ == "__main__":
  main()
