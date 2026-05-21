#!/usr/bin/env python3
"""
Click handler for the Waybar Focusmate module.

Tries to focus an already-open Focusmate window via niri, in priority order:
  1. The Focusmate Chrome PWA (matched by app_id prefix)
  2. Any browser tab/window whose title looks like a live Focusmate session
  3. Any other window titled "Focusmate" (e.g. the dashboard tab)

If none of those exist, launches the Focusmate Chrome PWA. As a last
resort (no Chrome on PATH), opens the dashboard URL with xdg-open.

Deliberately avoids matching:
  - OBS scenes named "Focusmate"
  - Slack channels titled "Focusmate"
  - Chrome DevTools windows pointing at app.focusmate.com
"""
from __future__ import annotations

import json
import subprocess
import sys

PWA_EXTENSION_ID = "bakidganjjbjihpnhkebieendhopjlbh"
PWA_APP_ID_PREFIX = f"chrome-{PWA_EXTENSION_ID}"
CHROME_BINARIES = ("google-chrome-stable", "google-chrome", "chromium")
DASHBOARD_URL = "https://www.focusmate.com/dashboard"
EXCLUDE_APP_SUBSTRINGS = ("slack", "obs", "obsproject")


def score(w: dict) -> int | None:
    """Lower score = better match. None means "not a Focusmate window"."""
    app = (w.get("app_id") or "").lower()
    title = (w.get("title") or "")
    tlow = title.lower()

    # Reject obvious false positives first.
    if "devtools" in tlow:
        return None
    if any(s in app for s in EXCLUDE_APP_SUBSTRINGS):
        return None

    # The PWA window — best possible match.
    if app.startswith(PWA_APP_ID_PREFIX):
        return 0

    if "focusmate" in tlow:
        # A live session window typically has a countdown in the title, e.g.
        # "Focusmate - 51:24 until end - Focusmate". Prefer those over the
        # generic homepage tab.
        if any(c.isdigit() for c in title):
            return 1
        return 2

    return None


def list_windows() -> list[dict]:
    try:
        out = subprocess.run(
            ["niri", "msg", "--json", "windows"],
            capture_output=True, text=True, check=True, timeout=2,
        ).stdout
    except (subprocess.SubprocessError, FileNotFoundError):
        return []
    try:
        data = json.loads(out)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def main() -> int:
    candidates = [(s, w) for w in list_windows() if (s := score(w)) is not None]
    candidates.sort(key=lambda sw: sw[0])

    if candidates:
        wid = candidates[0][1].get("id")
        if wid is not None:
            subprocess.run(
                ["niri", "msg", "action", "focus-window", "--id", str(wid)],
                check=False,
            )
            return 0

    # Launch the PWA. Mirrors the Exec= line in
    # ~/.local/share/applications/chrome-<id>-Default.desktop, so behaviour
    # matches double-clicking the app icon.
    import shutil
    for binary in CHROME_BINARIES:
        if shutil.which(binary):
            subprocess.Popen(
                [
                    binary,
                    "--profile-directory=Default",
                    f"--app-id={PWA_EXTENSION_ID}",
                ],
                start_new_session=True,
            )
            return 0

    subprocess.run(["xdg-open", DASHBOARD_URL], check=False)
    return 0


if __name__ == "__main__":
    sys.exit(main())
