import subprocess
import re

from libqtile import widget

def get_sysmon(index: int = 0) -> str:
    out = subprocess.check_output(
        ["bash", "/home/nasreddin/.local/bin/system-monitor"],
        text=True
    ).splitlines()
    m = [
        match.group(1)
        for line in out
        if line and (match := re.search(r"<txt>(.*)</txt>", line))
    ]
    return m[index]

sysmon = widget.GenPollText(
    func=get_sysmon,
    update_interval=2,
)
