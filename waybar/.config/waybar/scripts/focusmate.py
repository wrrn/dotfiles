#!/usr/bin/env python3
"""
Waybar custom module: current Focusmate session countdown.

Polled every second by Waybar; outputs Waybar JSON describing the active
session, or `{}` when idle (so the module hides via `hide-empty-text: true`).

API requests are cached on disk so the per-second polling does not hammer
Focusmate. Network failures fall back to the on-disk cache so a flaky
connection does not blank a running timer.

Configuration (first match wins):
  1. $FOCUSMATE_API_KEY
  2. ~/.netrc entry for `api.focusmate.com` (key in the `password` field)

Requires a Focusmate Pro account (free plans cannot mint API keys).
"""
from __future__ import annotations

import json
import netrc
import os
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone
from pathlib import Path

API_HOST = "api.focusmate.com"

API_BASE = "https://api.focusmate.com/v1"
CACHE_PATH = Path("/tmp/waybar-focusmate-cache.json")
CACHE_TTL = 300  # seconds; session list rarely changes mid-day
HTTP_TIMEOUT = 5  # seconds


def emit(obj: dict) -> None:
    sys.stdout.write(json.dumps(obj))
    sys.stdout.write("\n")
    sys.stdout.flush()


def read_api_key() -> str | None:
    env = os.environ.get("FOCUSMATE_API_KEY")
    if env:
        return env.strip()

    # netrc: `machine api.focusmate.com login apikey password <key>`.
    # netrc.netrc() raises if the file is world-readable; treat any failure
    # as "not configured here" and fall through to the next source.
    try:
        auth = netrc.netrc().authenticators(API_HOST)
    except (FileNotFoundError, netrc.NetrcParseError, OSError):
        auth = None
    if auth:
        _login, _account, password = auth
        if password:
            return password.strip()

    return None


def fetch_sessions(api_key: str) -> list[dict]:
    # Cover yesterday..tomorrow so TZ/DST edge cases can't drop a session.
    now = datetime.now(timezone.utc)
    start = (now - timedelta(days=1)).replace(hour=0, minute=0, second=0, microsecond=0)
    end = (now + timedelta(days=1)).replace(hour=23, minute=59, second=59, microsecond=0)

    def fmt(dt: datetime) -> str:
        return dt.isoformat().replace("+00:00", "Z")

    url = f"{API_BASE}/sessions?start={fmt(start)}&end={fmt(end)}"
    req = urllib.request.Request(url, headers={"X-API-Key": api_key})
    with urllib.request.urlopen(req, timeout=HTTP_TIMEOUT) as resp:
        payload = json.loads(resp.read())

    # Focusmate returns {"sessions": [...]}. Be defensive in case of changes.
    if isinstance(payload, dict) and isinstance(payload.get("sessions"), list):
        return payload["sessions"]
    if isinstance(payload, list):
        return payload
    return []


def load_cache(allow_stale: bool = False) -> list[dict] | None:
    try:
        st = CACHE_PATH.stat()
    except FileNotFoundError:
        return None
    if not allow_stale and time.time() - st.st_mtime > CACHE_TTL:
        return None
    try:
        data = json.loads(CACHE_PATH.read_text())
    except (OSError, json.JSONDecodeError):
        return None
    return data if isinstance(data, list) else None


def save_cache(sessions: list[dict]) -> None:
    try:
        CACHE_PATH.write_text(json.dumps(sessions))
    except OSError:
        pass


def get_sessions(api_key: str) -> list[dict]:
    fresh = load_cache(allow_stale=False)
    if fresh is not None:
        return fresh
    try:
        sessions = fetch_sessions(api_key)
    except (urllib.error.URLError, urllib.error.HTTPError, TimeoutError, OSError):
        # Prefer stale data over none, so transient outages don't blank the bar.
        stale = load_cache(allow_stale=True)
        return stale if stale is not None else []
    save_cache(sessions)
    return sessions


def parse_iso(s: str) -> datetime:
    if s.endswith("Z"):
        s = s[:-1] + "+00:00"
    return datetime.fromisoformat(s)


def find_active(
    sessions: list[dict], now: datetime
) -> tuple[dict, datetime, datetime] | None:
    for s in sessions:
        start_raw = s.get("startTime") or s.get("sessionTime")
        duration = s.get("duration")
        if not start_raw or not duration:
            continue
        try:
            start = parse_iso(start_raw)
        except ValueError:
            continue
        # Focusmate returns duration in milliseconds (25/50/75 min sessions
        # come back as 1500000 / 3000000 / 4500000).
        end = start + timedelta(milliseconds=int(duration))
        if start <= now < end:
            return s, start, end
    return None


def main() -> int:
    api_key = read_api_key()
    if not api_key:
        emit({})
        return 0

    sessions = get_sessions(api_key)
    now = datetime.now(timezone.utc)
    active = find_active(sessions, now)
    if not active:
        emit({})
        return 0

    _session, _start, end = active
    remaining = int((end - now).total_seconds())
    if remaining <= 0:
        emit({})
        return 0

    mins, secs = divmod(remaining, 60)
    text = f"🍅 {mins:02d}:{secs:02d}"
    tooltip = f"Focusmate · ends {end.astimezone().strftime('%H:%M')}"
    cls = "ending" if remaining < 60 else "active"
    emit({"text": text, "tooltip": tooltip, "class": cls, "alt": cls})
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception:
        # Never let an unexpected error spew tracebacks into the bar.
        emit({})
        sys.exit(0)
