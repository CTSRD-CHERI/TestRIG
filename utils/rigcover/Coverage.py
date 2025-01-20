import re
from utils import *

def abbrevSail(text):
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
        counterexample_label = f"{startindex}-{line}-{abbrevSail(old_cond)}-{new_cond}"
        self.context.log(f"Label: counterexample_label")
        return (new_content, counterexample_label)

class CoverAssign():
    extraFields = []
    extraFieldTypes = []

    def __init__(self, context):
        self.context = context
        self.name = "CoverAssign"

    def train(self, sail_content):
        ret = []
        for m in re.finditer(f"{ws}={ws}", sail_content):
            startindex = m.start()
            while startindex >= 0 and sail_content[startindex] != '\n':
                startindex -= 1
            startindex += 1
            endindex = startindex
            while endindex < len(sail_content) and sail_content[endindex] != ';':
                endindex += 1
            endindex += 1
            sail_line = sail_content[startindex:endindex]
            if "union clause" in sail_line or "function" in sail_line:
                continue
            line = line_num(sail_content, startindex)
            self.context.log(f"found assign at {startindex}")
            ret.append([startindex, endindex, line])
        return ret

    def getRun(self, sail_content, entry):
        [startindex, endindex, line] = entry
        self.context.log(f"Commenting assign")
        new_content = f"{sail_content [:startindex]} /* {sail_content[startindex:endindex]} */ {sail_content[endindex:]}"
        counterexample_label = f"{startindex}-{line}-assign-{sail_content[startindex:endindex]}"
        self.context.log(f"Label: counterexample_label")
        return (new_content, counterexample_label)

CoverTypes = [CoverAssign]
