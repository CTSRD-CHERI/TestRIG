#! /usr/bin/env python3

import re
import argparse
import subprocess
import tempfile
import time
import sys
from pathlib import Path
import shutil
import sqlite3

from CoverIf import CoverIf
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
        makeresult = subprocess.run(["make", "-C", sail_build_dir, "sail-rv64-cheri"])
        if makeresult.returncode != 0:
            context.log("Build failed!")
            return (False, None)
        context.log("Build success")
        args = [ "-r", "rv64icxcheri"
               , "--test-include-regex", "caprandom"
               , "-n", str(context.depth)
               ]
        context.log(f"Running with args {' '.join(args)}")
        command = [ "python3"
                  , f"{testrig_root}/utils/scripts/runTestRIG.py"
                  , "-a", "sail"
                  , "-b", "sail"
                  , "--path-to-dut", f"{impl_path}/sail-cheri-riscv/c_emulator"
                  , "-S", context.dir
                  ] + args
        context.indent()
        testresult = subprocess.run(command, capture_output=True)
        m = re.search("Writing counterexample file to: ([^\\r\\n]*\\.S)", testresult.stdout.decode(sys.stdout.encoding))
        if m:
          newfile = f"{context.dir}/{example_label}.S"
          shutil.move(f"{m[1]}", newfile)
          context.log(f"Found counterexample: {example_label}.S")
          ret = (True, newfile)
        else:
          context.log("No counterexample found?")
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

def main(args):
    with sqlite3.connect(args.db) as db:
        context = Context(args.verbose, db, args.depth)

        coverTypes = [CoverIf(context)]

        for cover in coverTypes:
            codeFields = ["file", "startindex", "endindex", "linenum"] + cover.extraFields
            codeFieldTypes = ["text", "int", "int", "int"] + cover.extraFieldTypes
            runsFields = ["codeid", "depth", "builds", "counterexample"]
            runsFieldTypes = ["int", "int", "bool", "text"]
            codeTable = f"{cover.name}_code"
            runsTable = f"{cover.name}_runs"
            if args.train:
                 createTable(context, codeTable, codeFields, codeFieldTypes, addId = True)
                 createTable(context, runsTable, runsFields, runsFieldTypes, addId = False,
                     foreignText=f", FOREIGN KEY (codeid) references {codeTable}(id)")

            for sail_path in args.sail_paths:
                with open(f"{sail_dir}/{sail_path}", "r") as sail_file:
                    sail_content = sail_file.read()
                sail_content = strip_comments(sail_content)
                if args.train:
                    entries = cover.train(sail_content)
                    for entry in entries:
                        entry = [sail_path] + entry
                        context.sql(f"INSERT into {codeTable} ({','.join(codeFields)}) VALUES ({','.join(['?' for e in entry])})", entry)
                else:
                    try:
                        entries = context.sql(f"SELECT * FROM {codeTable} WHERE file == '{sail_path}'").fetchall()
                    except sqlite3.OperationalError as e:
                        context.log("Error getting code table. Need to train first?")
                        context.log(str(e))
                        exit(1)
                    for entry in entries:
                        print(entry)
                        entry = entry # Trim filename and ID
                        modif_sail, label = cover.getRun(sail_content, entry[2:])
                        context.indent()
                        built, counterexample = check_divergence(context, sail_path, modif_sail, label)
                        context.unindent()
                        context.sql(f"INSERT INTO {runsTable} VALUES (?, ?, ?, ?)",
                            [entry[0], context.depth, built, counterexample])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
                        prog="RigCover"
                      , description="Measure coverage from perturbing the Sail model"
                      )
    parser.add_argument('sail_paths', nargs='+')
    parser.add_argument('--db', required=False, default='rigcover.db')
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('--depth', required=False, default=100)
    parser.add_argument('--train', action = 'store_true')

    main(parser.parse_args())
