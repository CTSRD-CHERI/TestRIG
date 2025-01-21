import time
import sqlite3
from shortuuid import ShortUUID
from pathlib import Path

ws = r"[ \t]"
intV = r"[0-9]*"

def line_num(content, index):
    return len([c for c in content[:index] if c == "\n"]) + 1

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

class Context:
    def __init__(self, args):
        self.dir = f"{args.logdir}/{time.strftime('run-%Y%m%d-%H%M%S') + ShortUUID().random(length=8)}"
        Path(self.dir).mkdir(parents=True, exist_ok=False)
        self._indent = 0
        self.db = sqlite3.connect(args.db)
        self.cur = self.db.cursor()
        self.args = args

    def log(self, message, force_print=False):
        with open(f"{self.dir}/log.txt", "a+") as l:
            l.write((" " * self._indent) + message + "\n");
        if self.args.verbose or force_print:
            print(message)

    def indent(self):
        self._indent += 2

    def unindent(self):
        self._indent -= 2

    def sql(self, *args, **kwargs):
        self.log(args[0])
        x = self.cur.execute(*args, **kwargs)
        self.db.commit()
        return x
