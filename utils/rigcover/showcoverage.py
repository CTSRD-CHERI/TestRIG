import sqlite3
import argparse
import html
import re
import uuid

from Coverage import CoverTypes
from utils import *

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

def outputHtml(code):
  with open("myfile.html", "w") as htmlfile:
    htmlfile.write(f'<html> {css} <body> <code style="white-space: preserve nowrap"> {code} </code> </body> </html>')

def tooltip(prose, tip):
  return f'<div class="tooltip">{prose}<span class="tooltiptext">{tip}</span></div>'

def main(args):
  sep = uuid.uuid4()
  coverTypes = [c(None) for c in CoverTypes]
  with open(f"{sail_dir}/{args.sail_path}", "r") as sail_file:
    sail_content = sail_file.read()

  sail_content = strip_comments(sail_content)

  db = sqlite3.connect(args.db)
  cur = db.cursor()

  points = []
  for coverI, cover in enumerate(coverTypes):
      cover_points = cur.execute(f"SELECT id, startindex, endindex from {cover.name}_code").fetchall()

      points += [(startindex, f"{sep};{coverI};{i};START;") for i, startindex, endindex in cover_points] \
              + [(endindex  , f"{sep};{coverI};{i};END;"  ) for i, startindex, endindex in cover_points]

  points = sorted(points, key=lambda a: -a[0])

  for index, tok in points:
      sail_content = sail_content[:index] + tok + sail_content[index:]

  sail_content = html.escape(sail_content)

  idx = 0
  while True:
    mS = re.search(f"{sep};({intV});({intV});START;", sail_content[idx:])
    if not mS:
      break
    coverI = int(mS.groups()[0])
    codeId = int(mS.groups()[1])
    cover = coverTypes[coverI]
    mE = re.search(f"{sep};{coverI};{codeId};END;", sail_content[idx:])
    assert mE, f"START without END: {codeId} {sail_content[idx+mS.start():idx+mS.start()+200]}"
    pre_content = sail_content[:idx + mS.start()]
    in_content = sail_content[idx + mS.end() : idx + mE.start()]
    post_content = sail_content[idx + mE.end():]
    idx += mS.start()
    runs = cur.execute(f"SELECT depth, builds, counterexample FROM {cover.name}_runs WHERE codeId = {codeId}").fetchall()
    counters = [f'<a href="{r[2]}">counterexample</a>' for r in runs if r[1] and r[2] is not None]
    buildfails = ["No build" for r in runs if not r[1]]
    counterfails = [f"(NA after {r[0]})" for r in runs if r[1] and r[2] is None]
    bg = "gray"
    if counters:
        bg = "green"
    elif counterfails:
        bg = "red"
    elif buildfails:
        bg = "#aed6f1"
    sail_content = pre_content \
                 + tooltip(f'<span style="background-color: {bg};">{in_content}</span>', ", ".join(counters + buildfails + counterfails)) \
                 + post_content

  outputHtml(sail_content)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
                        prog="ShowCoverage"
                      , description="Show coverage output from RigCover"
                      )
    parser.add_argument('sail_path')
    parser.add_argument('--db', required=False, default='rigcover.db')

    main(parser.parse_args())
