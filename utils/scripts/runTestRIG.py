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
import os.path as op
import subprocess as sub

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

known_rvfi_dii = set({'spike','rvbs'})
known_vengine  = set({'QCTestRIG'})

parser = argparse.ArgumentParser(description='Runs a TestRIG configuration')

parser.add_argument('-m', '--model', metavar='MODEL', choices=known_rvfi_dii,
  default='spike',
  help="The model to use. (one of {:s})".format(str(known_rvfi_dii)))
parser.add_argument('-i', '--implementation', metavar='IMPL', choices=known_rvfi_dii,
  default='rvbs',
  help="The implementation to use. (one of {:s})".format(str(known_rvfi_dii)))
parser.add_argument('-e', '--verification-engine', metavar='VENG', choices=known_vengine,
  default='QCTestRIG',
  help="The verification engine to use. (one of {:s})".format(str(known_vengine)))
parser.add_argument('-v', '--verbose', action='count', default=0,
  help="Increase verbosity level by adding more \"v\".")
parser.add_argument('-n', '--number-of-tests', metavar= 'NTESTS', type=auto_int,
  default=100, help="Runs the verification engine for NTESTS tests.")
parser.add_argument('--path-to-rvbs', metavar='PATH', type=str,
  default='rvbs-rv32i-rvfi-dii',
  help="The PATH to the rvbs executable")
parser.add_argument('--path-to-QCTestRIG', metavar='PATH', type=str,
  default='QCTestRIG',
  help="The PATH to the QCTestRIG executable")

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

def spawn_rvfi_dii_server(name, port):
  if (name == 'spike'):
    print('TODO spawn spike rvfi-dii server')
  elif (name == 'rvbs'):
    itrace_path = 'rvbs-{:d}-itrace'.format(port)
    itrace = open(itrace_path,'w')
    env2 = os.environ.copy()
    env2["RVFI_DII_PORT"] = str(port)
    cmd = [args.path_to_rvbs, "+itrace"]
    sub.Popen(cmd, env=env2, stdin=None, stdout=itrace, stderr=itrace)
    print('spawned rvbs rvfi-dii server on port: {:d}, trace file: {:s}'.format(
      port, itrace_path))
  else:
    print("Unknown rvfi-dii server {:s}".format(name))
    exit(0)

#############################
# spawn verification engine #
#############################

def spawn_vengine(name):
  if (name == 'QCTestRIG'):
    cmd = [args.path_to_QCTestRIG, '-m', '5000', '-i', '5001']
    cmd += ['-n', str(args.number_of_tests)]
    if args.verbose > 0:
      cmd += ['-v']
    sub.run(cmd)
  else:
    print("Unknown verification engine {:s}".format(name))
    exit(0)

#################
# main function #
#################

def main():
  spawn_rvfi_dii_server(args.model, 5000)
  spawn_rvfi_dii_server(args.implementation, 5001)
  spawn_vengine(args.verification_engine)
  exit(0)

if __name__ == "__main__":
  main()
