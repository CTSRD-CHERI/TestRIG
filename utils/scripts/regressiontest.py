#! /usr/bin/env python3
# PYTHON_ARGCOMPLETE_OK
# -
# SPDX-License-Identifier: BSD-2-Clause
#
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
import asyncio
import datetime
import junitparser
import shlex
import subprocess
import tempfile
import sys
import typing
from enum import Enum
from pathlib import Path


class AnsiColour(Enum):
  black = b"\x1b[1;30m"
  red = b"\x1b[1;31m"
  green = b"\x1b[1;32m"
  yellow = b"\x1b[1;33m"
  blue = b"\x1b[1;34m"
  magenta = b"\x1b[1;35m"
  cyan = b"\x1b[1;36m"
  white = b"\x1b[1;37m"
  reset = b"\x1b[0m"


DEBUG = False
def debug(*args, **kwargs):
  if DEBUG:
    print(*args, **kwargs, file=sys.stderr, flush=True)

def print_coloured(*args, colour: AnsiColour, **kwargs):
  sys.stderr.buffer.write(colour.value)
  print(*args, "\x1b[0m", **kwargs, file=sys.stderr, flush=True)

def info(*args, **kwargs):
  print_coloured(*args, colour=AnsiColour.blue, **kwargs)

def error(*args, **kwargs):
  print_coloured(*args, colour=AnsiColour.red, **kwargs)

async def run_testrig(args: argparse.Namespace, remaining_args: list, output_dir: str):
  trace_rootdir = Path(args.trace_dir)
  if not trace_rootdir.is_dir():
    sys.exit(str(trace_rootdir) + " does not exist!")
  xunit_output = Path(args.xunit_output)
  if not xunit_output.parent.is_dir():
    sys.exit("invalid xunit output file: " + str(xunit_output))
  global DEBUG
  if args.debug:
    DEBUG = True
  command = [str(Path(__file__).parent / "runTestRIG.py"),
             "--trace-dir", str(trace_rootdir),
             "--save-dir", output_dir,
             "-a", args.reference_impl,
             "-b", args.test_impl,
             "--no-shrink",
             ] + remaining_args
  if args.timeout:
    command += ["--timeout", str(args.timeout)]

  info("Running '", " ".join(shlex.quote(s) for s in command), "'", sep="")
  process = await asyncio.create_subprocess_exec(command[0], *command[1:], stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.STDOUT)
  xml = junitparser.JUnitXml(name="Regression test " + args.test_impl + " vs " + args.reference_impl)
  testsuite = junitparser.TestSuite(name=args.trace_dir)
  xml.add_testsuite(testsuite)
  current_test = None
  current_output = []  # type: typing.List[str]
  starttime = datetime.datetime.utcnow()

  def add_test_result(junit_result=None):
    nonlocal current_test
    nonlocal current_output
    if current_test is None:
      current_test = junitparser.TestCase("UNKNOWN TEST CASE")
    if junit_result is not None:
      current_test.result = junit_result
    current_test.time = (datetime.datetime.utcnow() - starttime).total_seconds()
    if current_output:
      current_test.system_out = "".join(current_output)
    testsuite.add_testcase(current_test)
    current_test = None
    current_output = []

  async def fatal_error(error_message):
    process.terminate()
    process.kill()
    remaining_stdout = await process.stdout.read()
    debug("Remaining output: ", remaining_stdout)
    current_output.append(remaining_stdout.decode("utf-8"))
    add_test_result(junit_result=junitparser.Error(message=error_message))
    error(error_message)

  while True:
    # No output for N seconds means something went wrong...
    output = b""
    try:
      output = await asyncio.wait_for(process.stdout.readline(), 60)
    except asyncio.TimeoutError:
      try:
        output = await asyncio.wait_for(process.stdout.readuntil(separator=b'\r'), 10)
      except asyncio.TimeoutError:
        await fatal_error("TIMEOUT!")
        break
    if not output and process.stdout.at_eof():
      info("EOF")
      break
    debug("==>TR: \x1b[1;33m", output, "\x1b[0m", sep="")
    if output.startswith(b'Reading trace from '):
      # start of testcase
      assert current_test is None, "Reading new test before last one finished?"
      starttime = datetime.datetime.utcnow()
      trace_file = output[len(b'Reading trace from '):].rstrip().decode("utf-8")
      relpath = Path(trace_file).relative_to(trace_rootdir)
      debug("Starting test", relpath, " from", trace_file)
      info("==== Testing:", relpath, "... ", end="\n" if DEBUG else "")
      current_test = junitparser.TestCase(str(relpath))
      current_output = []
      continue
    if output.startswith(b'+++ OK, passed'):
      # End of testcase
      if current_test.result is not None:
        assert isinstance(current_test.result, junitparser.Skipped), "unexpected test result"
        print_coloured("SKIPPED", colour=AnsiColour.yellow)
      else:
        print_coloured("OK", colour=AnsiColour.green)
      assert output == b"+++ OK, passed 1 test.\n", b"output format changed? " + output
      assert current_test is not None
      add_test_result(current_test.result)
      continue
    elif output == b'Failure.\n':
      assert isinstance(current_test.result, junitparser.Failure), "Didn't see ''*** Failed!' message?"
      add_test_result(current_test.result)
      error("FAILED!")

    # Not a marker message -> add to current test output
    if current_output is not None:
      current_output.append(output.decode("utf-8"))

    # Check if test failed:
    if output.startswith(b"*** Failed!"):
      assert current_test is not None
      current_test.result = junitparser.Failure(message=output.strip().decode("utf-8"))
    elif output.startswith(b"Error:"):
      error(output.decode("utf-8"))
      assert current_test is not None
      current_test.result = junitparser.Failure(message=output.strip().decode("utf-8"))
      if output.startswith(b"Error: implementation A timeout.") or output.startswith(b"Error: implementation B timeout."):
        if current_test.result is None:
          current_test.result = junitparser.Error(message=output.strip().decode("utf-8"))
        continue
      else:
        await fatal_error("Unknown error: " + output.decode("utf-8"))
        break
    elif output.startswith(b"Warning:"):
      if output.startswith(b"Warning: reporting success since implementations not running"):
        debug("implementations not running!")
        if current_test.result is None:
          current_test.result = junitparser.Skipped(message=output.strip().decode("utf-8"))
        continue
      await fatal_error("Unknown warning!")
      break

  await process.wait()
  xml.update_statistics()
  print("SUMMARY:")
  print("Total tests:", xml.tests)
  print("Successful: ", xml.tests - xml.failures - xml.errors - xml.skipped)
  print("Failed:     ", xml.failures)
  print("ERRORS:     ", xml.errors)
  print(xml)
  if xml.failures != 0:
    print("Minimized cases: ")
    subprocess.check_call(["find", str(output_dir)], cwd=str(output_dir))
  if str(xunit_output) != "/dev/null":
    xml.write(filepath=str(xunit_output), pretty=True)


def main():
  parser = argparse.ArgumentParser(description='Runs TestRIG regression test',
                                   formatter_class=argparse.ArgumentDefaultsHelpFormatter)
  parser.add_argument("--trace-dir", "-d", required=True)
  parser.add_argument('--reference-impl', '-a', '--implementation-A', metavar='IMP', default='sail',
                      help="The referece (CHERI-)RISCV implementation to use for this regression test.")
  parser.add_argument('---test-impl', '-b', '--implementation-B', metavar='IMP', required=True,
                      help="The (CHERI-)RISCV implementation to be tested.")
  parser.add_argument('--xunit-output', '--output', default="/dev/null", help="Output XUnit XML file.")
  parser.add_argument('--debug', action="store_true", help="Print debug output.")
  parser.add_argument('--timeout', type=int, default=0, help="Timeout after N secods")
  args, remaining = parser.parse_known_args()
  with tempfile.TemporaryDirectory() as output_dir:
    asyncio.run(run_testrig(args, remaining, output_dir))

if __name__ == '__main__':
  main()
