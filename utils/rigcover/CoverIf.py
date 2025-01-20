import re
from utils import *

def sanitiseCond(text):
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

class CoverIf():
    extraFields = ["oldcond", "newcond"]
    extraFieldTypes = ["text", "boolean"]

    def __init__(self, context):
        self.context = context
        self.name = "CoverIf"

    def train(self, sail_content):
        ret = []
        for m in re.finditer(f"{ws}if{ws}", sail_content):
            startindex = m.span()[1] - 1
            line = line_num(sail_content, m.span()[0])
            self.context.log(f"found 'if' at {m.span()[0] + 1}")
            self.context.indent()
            remaining = sail_content[m.span()[1]-1:]
            n = re.search(f"{ws}then{ws}", remaining)
            if n is None:
                self.context.log("no matching 'then': skipping")
            else:
                endindex = startindex + n.span()[0]+1
                self.context.log(f"found 'then' at {endindex}")
                old_cond = remaining[:n.span()[0] + 1]
                for new_cond in [True, False]:
                    ret.append([startindex, endindex, line, old_cond, new_cond])
            self.context.unindent()
        return ret

    def getRun(self, sail_content, entry):
        [startindex, endindex, line, old_cond, new_cond] = entry
        new_cond = "true" if new_cond else "false"
        self.context.log(f"Replacing old condition with {new_cond}")
        new_content = f"{sail_content [:startindex]} {new_cond} /* {old_cond} */ {sail_content[endindex:]}"
        counterexample_label = f"{startindex}-{line}-{sanitiseCond(old_cond)}-{new_cond}"
        self.context.log(f"Label: counterexample_label")
        return (new_content, counterexample_label)
