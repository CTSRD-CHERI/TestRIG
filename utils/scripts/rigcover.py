#! /usr/bin/env python3

import re
import argparse
import subprocess
import tempfile
import time
import sys
from pathlib import Path
import shutil
import duckdb

testrig_root = "../../"
sail_dir = f"{testrig_root}/riscv-implementations/sail-cheri-riscv"
ws = r"([^a-zA-Z0-9_]|^|$)"

class Context:
    def __init__(self, verbose, db, depth):
        self.verbose = verbose
        self.dir = time.strftime("run-%Y%m%d-%H%M%S")
        subprocess.run(["mkdir", self.dir])
        self._indent = 0
        self.db = db
        self.depth = depth

    def log(self, message):
        with open(f"{self.dir}/log.txt", "a+") as l:
            l.write((" " * self._indent) + message + "\n");
        if self.verbose:
            print(message)

    def indent(self):
        self._indent += 2

    def unindent(self):
        self._indent -= 2

def check_divergence(sail_dut_file, new_sail_content, example_label, context, update):
    with tempfile.TemporaryDirectory(prefix="rigcover-") as sail_build_dir:
        impl_path = f"{sail_build_dir}/riscv-implementations/"
        Path(impl_path).mkdir(parents=True)
        shutil.copytree(sail_dir, f"{impl_path}/sail-cheri-riscv")
        shutil.copy(f"{testrig_root}/Makefile", sail_build_dir)
        with open(f"{impl_path}/sail-cheri-riscv/{sail_dut_file}", "w") as f:
            f.write(new_sail_content)
        makeresult = subprocess.run(["make", "-C", sail_build_dir, "sail-rv32-cheri"])
        if makeresult.returncode != 0:
            context.db.execute(update, [context.depth, False, None])
            context.log("Build failed!")
            return
        context.log("Build success")
        args = [ "-r", "rv32icxcheri"
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
          context.db.execute(update, [context.depth, True, newfile])
        else:
          context.log("No counterexample found?")
          context.db.execute(update, [context.depth, True, None])
        context.unindent()

def line_num(content, index):
    return len([c for c in content[:index] if c == "\n"]) + 1

def sanitise(text):
    new_text = ""
    in_label = False
    for c in text:
        if re.match("[a-zA-Z0-9]", c):
             new_text += c
             in_label = True
        else:
            if in_label:
                in_label = False
                new_text += "_"
    return new_text

def find_all_branches(sail_dut_file, sail_content, context):
    try:
        context.db.execute("CREATE TABLE branches(charindex int, thenoffset int, linenum int, cond text, PRIMARY KEY(charindex))")
        context.db.execute("CREATE TABLE exclude(branchindex int, FOREIGN KEY(branchindex) REFERENCES branches(charindex))")
    except duckdb.duckdb.CatalogException:
        context.log("Branches table already exists")
        return
    for m in re.finditer(f"{ws}if{ws}", sail_content):
        line = line_num(sail_content, m.span()[0])
        context.log(f"found 'if' at {m.span()[0] + 1} (line {line})")
        context.indent()
        remaining = sail_content[m.span()[1]-1:]
        n = re.search(f"{ws}then{ws}", remaining)
        if n is None:
            context.log("no matching 'then': skipping")
        else:
            context.log(f"found 'then' at {n.span()[0] + 1}")
            old_cond = remaining[:n.span()[0] + 1]
            context.db.execute("INSERT INTO branches VALUES (?, ?, ?, ?)", [m.span()[1]-1, n.span()[0]+1, line, old_cond])
        context.unindent()

def run_all_branches(sail_dut_file, sail_content, context, predicate):
    find_all_branches(sail_dut_file, sail_content, context)
    try:
        context.db.execute("CREATE TABLE runs(branchindex int, newcond bool, depth int, builds bool, counterexample text, FOREIGN KEY(branchindex) REFERENCES branches(charindex))")
    except duckdb.duckdb.CatalogException:
        context.log("Runs table already exists")
    for (charindex, thenoffset, linenum, old_cond) in context.db.sql(f"SELECT charindex, thenoffset, linenum, cond from branches where charindex IN ({predicate})").fetchall():
        for new_cond in ["true", "false"]:
            context.log(f"Replacing old condition with {new_cond}")
            new_content = f"{sail_content [:charindex]} {new_cond} /* {old_cond} */ {sail_content[charindex+thenoffset:]}"
            counterexample_label = f"{charindex}-{linenum}-{sanitise(old_cond)}-{new_cond}"
            context.log(f"Label: counterexample_label")
            context.indent()
            check_divergence(sail_dut_file, new_content, counterexample_label, context,
                             f"INSERT INTO runs VALUES ({charindex}, {new_cond}, ?, ?, ?)")
            context.unindent()

def strip_comments(content):
    nest_level = 0
    line_comment = False
    new_content = ""
    while content:
        if len(content) >= 2 and content[:2] == "//" and nest_level == 0:
            line_comment = True
            content = content[2:]
            continue
        elif content[:1] == "\n":
            new_content += "\n"
            content = content[1:]
            line_comment = False
            continue
        elif len(content) >= 2 and content[:2] == "/*" and not line_comment:
            nest_level += 1
            content = content[2:]
        elif len(content) >= 2 and content[:2] == "*/" and not line_comment:
            nest_level -= 1
            content = content[2:]
        else:
            if nest_level == 0 and not line_comment:
                new_content += content[0]
            content = content[1:]
    return new_content

def main(args):
    with open(f"{sail_dir}/{args.sail_path}", "r") as sail_file:
        sail_content = sail_file.read()

    sail_content = strip_comments(sail_content)

    db = duckdb.connect(args.db)

    context = Context(args.verbose, db, args.depth)

    run_all_branches(args.sail_path, sail_content, context, args.predicate)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
                        prog="RigCover"
                      , description="Measure coverage from perturbing the Sail model"
                      )
    parser.add_argument('sail_path')
    parser.add_argument('--db', required=False, default='rigcover.db')
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('--predicate', required=False, default='SELECT charindex from branches where charindex not in (select * from exclude)')
    parser.add_argument('--depth', required=False, default=100)

    main(parser.parse_args())
