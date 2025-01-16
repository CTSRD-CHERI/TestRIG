import sqlite3
import argparse
import html
import re
import uuid

testrig_root = "../../"
sail_dir = f"{testrig_root}/riscv-implementations/sail-cheri-riscv"

css = """
<style>
.tooltip {
  position: relative;
  display: inline-block;
}

.tooltip .tooltiptext {
  visibility: hidden;
  width: auto;
  height: auto;
  background-color: black;
  color: #fff;
  text-align: center;
  border-radius: 6px;
  padding: 5px 0;

  position: absolute;
  z-index: 1;
}

.tooltip:hover .tooltiptext {
  visibility: visible;
}
</style>
"""

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

def outputHtml(code):
  with open("myfile.html", "w") as htmlfile:
    htmlfile.write(f'<html> {css} <body> <code style="white-space: preserve nowrap"> {code} </code> </body> </html>')

def tooltip(prose, tip):
  return f'<div class="tooltip">{prose}<span class="tooltiptext">{tip}</span></div>'

def main(args):
  sep = uuid.uuid4()
  with open(f"{sail_dir}/{args.sail_path}", "r") as sail_file:
    sail_content = sail_file.read()

  sail_content = strip_comments(sail_content)

  db = sqlite3.connect(args.db)
  cur = db.cursor()

  branches = cur.execute("SELECT charindex, thenoffset, cond from branches order by charindex desc").fetchall()

  for i, b in enumerate(branches):
    sail_content = sail_content[:b[0]] + f"{sep}IF{i};" + sail_content[b[0]:]

  sail_content = html.escape(sail_content)

  idx = 0
  while True:
    m = re.search(f"{sep}IF([0-9]*);", sail_content[idx:])
    if not m:
      break
    idx += m.start()
    b = branches[int(m.groups()[0])]
    runs = cur.execute(f"SELECT newcond, builds, depth, counterexample from runs where branchindex = {b[0]}").fetchall()
    counters = [f'<a href="{r[3]}">{r[0] == 1}</a>' for r in runs if r[1] and r[3] is not None]
    buildfails = ["No build" for r in runs if not r[1]]
    counterfails = [f"{r[0] == 1} (NA after {r[2]})" for r in runs if r[1] and r[3] is None]
    bg = "gray"
    if [r for r in runs if r[0] and r[1] and r[3] is not None] and [r for r in runs if not r[0] and r[1] and r[3] is not None]:
        bg = "green"
    elif counterfails:
        bg = "red"
    elif buildfails:
        bg = "blue"
    sail_content = sail_content[:idx] + sail_content[idx+m.end()-m.start():]
    len_diff = len(html.escape(b[2])) - len(b[2])
    sail_content = sail_content[:idx-2] + tooltip(f'<span style="background-color: {bg};">{sail_content[idx-2:idx+b[1]+len_diff-1]}</span>', ", ".join(counters + buildfails + counterfails)) + sail_content[idx+b[1]+len_diff-1:]

  outputHtml(sail_content)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
                        prog="ShowCoverage"
                      , description="Show coverage output from RigCover"
                      )
    parser.add_argument('sail_path')
    parser.add_argument('--db', required=False, default='rigcover.db')

    main(parser.parse_args())
