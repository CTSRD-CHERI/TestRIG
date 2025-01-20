ws = r"([^a-zA-Z0-9_]|^|$)"
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
