import subprocess
from libqtile import widget

def get_sysmon():
    out = subprocess.check_output(
        ["bash", "/home/nasreddin/.local/bin/system-monitor"],
        text=True
    )
    import re
    m = re.search(r"<txt>(.*)</txt>", out)
    return m.group(1) if m else "???"

sysmon = widget.GenPollText(
    func=get_sysmon,
    update_interval=2,
)
