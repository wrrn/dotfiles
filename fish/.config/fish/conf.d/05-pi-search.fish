# Load pi-search API keys from the agenix-decrypted env file at the path
# stored in $PI_SEARCH_KEYS (typically /run/agenix/pi-search-keys on a
# host configured via modules/pi/linux.nix).
#
# The file is a plain `KEY=value` env file. Blank lines and lines starting
# with `#` are skipped. Values are taken as-is — no quote stripping, no
# variable expansion — so write them raw.
#
# Silent no-op when $PI_SEARCH_KEYS is unset or the file isn't readable.

if not set -q PI_SEARCH_KEYS; or not test -r "$PI_SEARCH_KEYS"
    return 0
end

while read -l line
    string match -qr '^\s*$|^\s*#' -- $line; and continue
    set -l parts (string split -m 1 = -- $line)
    test (count $parts) -eq 2; or continue
    set -gx (string trim -- $parts[1]) (string trim -- $parts[2])
end < "$PI_SEARCH_KEYS"
