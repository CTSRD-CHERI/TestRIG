#! /usr/bin/env python3

import re
import argparse
import subprocess
import tempfile
import sys
from pathlib import Path
import shutil
import sqlite3
from multiprocessing import Pool
from tqdm import tqdm

from Coverage import CoverTypes
from utils import *

testrig_root = "../../"
sail_dir = f"{testrig_root}/riscv-implementations/sail-cheri-riscv"

def check_divergence(context, sail_dut_file, new_sail_content, example_label):
    with tempfile.TemporaryDirectory(prefix="rigcover-") as sail_build_dir:
        impl_path = f"{sail_build_dir}/riscv-implementations/"
        Path(impl_path).mkdir(parents=True)
        shutil.copytree(sail_dir, f"{impl_path}/sail-cheri-riscv")
        shutil.copy(f"{testrig_root}/Makefile", sail_build_dir)
        with open(f"{impl_path}/sail-cheri-riscv/{sail_dut_file}", "w") as f:
            f.write(new_sail_content)
        makeresult = subprocess.run(["make", "-C", sail_build_dir, "sail-rv64-cheri"], capture_output=True)
        if makeresult.returncode != 0:
            context.log("Build failed!")
            return (False, None)
        context.log("Build success")
        args = [ "-r", "rv64icxcheri"
               , "--test-include-regex", "caprandom"
               , "-n", str(context.args.depth)
               ]
        context.log(f"Running with args {' '.join(args)}")
        command = [ "python3"
                  , f"{testrig_root}/utils/scripts/runTestRIG.py"
                  , "-a", "sail"
                  , "-b", "sail"
                  , "--path-to-dut", f"{impl_path}/sail-cheri-riscv/c_emulator"
                  , "--force-rvfi-v1"
                  , "-S", context.dir
                  ] + args
        context.indent()
        testresult = subprocess.run(command, capture_output=True)
        m = re.search("Writing counterexample file to: ([^\\r\\n]*\\.S)", testresult.stdout.decode(sys.stdout.encoding))
        if m:
          newfile = f"{context.args.tracedir}/{example_label}.S"
          shutil.move(f"{m[1]}", newfile)
          context.log(f"Found counterexample: {example_label}.S", force_print=True)
          ret = (True, newfile)
        else:
          context.log("No counterexample found?")
          if testresult.returncode:
              context.log("Warning: QCVEngine returned nonzero without finding a counterexample", force_print=True)
              context.log(testresult.stderr.decode(sys.stderr.encoding), force_print=True)
          ret = (True, None)
        context.unindent()
        return ret

def createTable(context, name, fields, fieldTypes, addId = False, foreignText=""):
    idField = "id integer PRIMARY KEY AUTOINCREMENT, " if addId else ""
    try:
        context.sql(f"CREATE TABLE {name}({idField}{','.join([fields[i] + ' ' + fieldTypes[i] for i in range(len(fields))])}{foreignText})")
        return True
    except sqlite3.OperationalError as e:
        context.log(f"Error creating {name}: {e}")
        return False

def tableExists(context, name):
    return context.sql(f'SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type="table" AND name="{name}")').fetchone()[0] == 1

def doRun(runArgs):
    [entry, Cover, sail_content, sail_path, args] = runArgs
    context = Context(args)
    cover = Cover(context)
    modif_sail, label = cover.getRun(sail_content, entry[2:])
    context.indent()
    built, counterexample = check_divergence(context, sail_path, modif_sail, label)
    context.unindent()
    context.sql(f"INSERT INTO {cover.name}_runs VALUES (?, ?, ?, ?)",
        [entry[0], context.args.depth, built, counterexample])

def main(args):
    context = Context(args)

    Path(args.tracedir).mkdir(parents=True, exist_ok=True)

    coverTypes = [C(context) for C in CoverTypes if args.train is None or C(None).name == args.train]

    for cover in coverTypes:
        context.log(f"Covertype {cover.name}", force_print=True)
        codeFields = ["file", "startindex", "endindex", "linenum"] + cover.extraFields
        codeFieldTypes = ["text", "int", "int", "int"] + cover.extraFieldTypes
        runsFields = ["codeid", "depth", "builds", "counterexample"]
        runsFieldTypes = ["int", "int", "bool", "text"]
        codeTable = f"{cover.name}_code"
        runsTable = f"{cover.name}_runs"
        if args.train is not None:
             if tableExists(context, codeTable) or tableExists(context, runsTable):
                 if args.retrain:
                     try:
                         context.sql(f"DROP TABLE {codeTable}")
                         context.sql(f"DROP TABLE {runsTable}")
                     except sqlite3.OperationalError as e:
                         pass
                 else:
                     context.log(f"Error: table {codeTable} or {runsTable} already exists. Run with --retrain to overwrite", force_print=True)
                     exit(1)
             createTable(context, codeTable, codeFields, codeFieldTypes, addId = True)
             createTable(context, runsTable, runsFields, runsFieldTypes, addId = False,
                 foreignText=f", FOREIGN KEY (codeid) references {codeTable}(id)")

        for sail_path in args.sail_paths:
            context.log(f"Processing {sail_path}", force_print=True)
            with open(f"{sail_dir}/{sail_path}", "r") as sail_file:
                sail_content = sail_file.read()
            sail_content = strip_comments(sail_content)
            if args.train is not None:
                entries = cover.train(sail_content)
                for entry in entries:
                    entry = [sail_path] + entry
                    context.sql(f"INSERT into {codeTable} ({','.join(codeFields)}) VALUES ({','.join(['?' for e in entry])})", entry)
            else:
                try:
                    entries = context.sql(f"SELECT * FROM {codeTable} WHERE file == '{sail_path}'").fetchall()
                except sqlite3.OperationalError as e:
                    context.log("Error getting code table. Need to train first?", force_print=True)
                    context.log(str(e), force_print=True)
                    exit(1)
                jobs = []
                for entry in entries:
                    oldBuildFails = context.sql(f"SELECT * FROM {cover.name}_runs WHERE codeId = {entry[0]} AND builds = FALSE").fetchall()
                    oldBuildCounters = context.sql(f"SELECT * FROM {cover.name}_runs WHERE codeId = {entry[0]} AND builds = TRUE AND counterexample IS NOT NULL").fetchall()
                    if oldBuildFails or oldBuildCounters:
                        continue
                    jobs.append([entry, type(cover), sail_content, sail_path, args])
                with Pool(args.j) as p:
                    list(tqdm(p.imap_unordered(doRun, jobs), total = len(jobs)))

parser = argparse.ArgumentParser(
               prog="RigCover"
             , description="Measure coverage from perturbing the Sail model"
             )
parser.add_argument('sail_paths', nargs='+')
parser.add_argument('--db', required=False, default='rigcover.db')
parser.add_argument('-v', '--verbose', action='store_true')
parser.add_argument('--depth', required=False, default=100)
parser.add_argument('--train', metavar='covertype', choices=[C(None).name for C in CoverTypes])
parser.add_argument('--retrain', action='store_true')
parser.add_argument('--logdir', required=False, default="runlogs")
parser.add_argument('--tracedir', required=False, default="traces")
parser.add_argument('-j', required=False, default=1, type=int)

# Use argcomplete to provide bash tab completion (https://github.com/kislyuk/argcomplete)
try:
  import argcomplete
  argcomplete.autocomplete(parser)
except ImportError:
  argcomplete = None
args = parser.parse_args()

if __name__ == "__main__":
    main(args)
