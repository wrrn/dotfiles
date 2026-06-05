#!/usr/bin/env python3
"""Countdown timer to 17:30 for waybar."""

import json
from datetime import datetime, timedelta

TARGET_HOUR = 17
TARGET_MINUTE = 30

now = datetime.now()
target = now.replace(hour=TARGET_HOUR, minute=TARGET_MINUTE, second=0, microsecond=0)

if target < now:
    target += timedelta(days=1)

remaining = target - now
total_seconds = int(remaining.total_seconds())
hours = total_seconds // 3600
minutes = (total_seconds % 3600) // 60
seconds = total_seconds % 60

if hours > 0:
    time_str = f"{hours}h{minutes}m"
else:
    time_str = f"{minutes}m"

# Alert state when under 10 minutes
css_class = "ending" if total_seconds < 600 else ""

text = time_str

output = {
    "text": text,
    "class": css_class,
    "tooltip": f"{hours}h {minutes}m until {TARGET_HOUR:02d}:{TARGET_MINUTE:02d}",
}

print(json.dumps(output))
