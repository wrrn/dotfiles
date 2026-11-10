;; Paneru configuration
;; Translated from niri binds.kdl

(local paneru (require :paneru))

(paneru.setup
 {
  ;; ── Global options ───────────────────────────────────────────────────────
   :options {:focus_follows_mouse true
             :mouse_follows_focus true
             :preset_column_widths [0.25 0.33333 0.50 0.66667 0.75 0.99999]
             :animation_speed 120
             ;; Approximates niri's center-focused-column "on-overflow"
             :auto_center false
             ;; :menubar_height 0
             :insert_windows_mid_strip true
             :reap_empty_workspaces true}

   ;; ── Screen edge padding ──────────────────────────────────────────────────
   :padding {:top 0 :bottom 0 :left 8 :right 8}

   ;; ── Swipe & gestures ─────────────────────────────────────────────────────
   :swipe {:continuous true
           ;; 4 fingers to avoid clashing with macOS 3-finger workspace switching.
           ;; Horizontal swipe scrolls the window strip (niri-style).
           ;; Vertical swipe switches virtual workspace rows.
           :gesture {:fingers_count 0
                     :direction "Natural"}
           ;; Hold alt + scroll to slide windows horizontally (like Mod+scroll in niri).
           ;; Hold alt + shift + scroll to switch virtual workspace rows vertically.
           :scroll {:modifier "alt"
                    :vertical_modifier "shift"}}

   ;; ── Decorations ──────────────────────────────────────────────────────────
   ;; Niri: border { on; width 4; active-color "#ffc87f"; inactive-color "#505050" }
   ;; Niri: geometry-corner-radius 12
   ;; Paneru only supports active-window borders; inactive border color has no equivalent.
   :decorations {:active {:border {:enabled true
                                   :color "#ffc87f"
                                   :width 4.0
                                   :radius 12.0}}}

   ;; ── Window rules ─────────────────────────────────────────────────────────
   :windows {:pip {:title "Picture.*(in)?.*[Pp]icture"
                   :floating true}
             :corfu {:app_name "emacs"
                     :title "^EmacsCorfuGUI.*"
                     :floating true}
             ;; Niri: gaps 16 → 8px per side of each window = 16px between adjacent windows
             :all {:title ".*"
                   :horizontal_padding 8
                   :vertical_padding 8}}})

;; ── Keybindings ────────────────────────────────────────────────────────────

;; Focus ──────────────────────────────────────────────────────────────────────
;; Mod+H/J/K/L → focus-column-left / focus-window-or-workspace-down / up / right
(paneru.bind "alt - h" "window focus west")
(paneru.bind "alt - l" "window focus east")
(paneru.bind "alt - j" "window virtual south")
(paneru.bind "alt - k" "window virtual north")

;; Jump to first / last window (paneru-only, no niri equivalent)
(paneru.bind "shift + cmd - h" "window focus first")
(paneru.bind "shift + cmd - l" "window focus last")

;; Move columns ───────────────────────────────────────────────────────────────
;; Mod+Ctrl+H/L → move-column-left / move-column-right
(paneru.bind "alt + ctrl - h" "window swap west")
(paneru.bind "alt + ctrl - l" "window swap east")

;; Move to strip start/end (paneru-only, no niri equivalent)
(paneru.bind "shift + alt - h" "window swap first")
(paneru.bind "shift + alt - l" "window swap last")

;; Stack / unstack (consume-or-expel) ────────────────────────────────────────
;; Mod+Alt+H → consume-or-expel-window-left  (stack into left neighbour)
;; Mod+Alt+L → consume-or-expel-window-right (unstack to right)
(paneru.bind "alt + cmd - h" "window stack")
(paneru.bind "alt + cmd - l" "window unstack")
(paneru.bind "shift + alt - e" "window snap")

;; Resize ─────────────────────────────────────────────────────────────────────
;; Mod+R → switch-preset-column-width (cycle forward)
(paneru.bind "alt - r" "window resize")

;; Full width / center ────────────────────────────────────────────────────────
;; Mod+F → maximize-column
;; Mod+C → center-column
(paneru.bind "alt - f" "window fullwidth")
(paneru.bind "alt - c" "window center")

;; Display movement ───────────────────────────────────────────────────────────
;; Mod+Alt+I → move-window-to-monitor-next (follow)
;; Mod+I     → focus-monitor-next
(paneru.bind "alt + ctrl - i" "window nextdisplay")
(paneru.bind "alt - i" "mouse nextdisplay")

;; Manage / float toggle ──────────────────────────────────────────────────────
;; Mod+G → switch-focus-between-floating-and-tiling
(paneru.bind "alt - g" "window focus unmanaged")
(paneru.bind "alt + ctrl - g" "window manage")

;; Virtual workspaces (Experimental) ─────────────────────────────────────────
;; Maps your niri workspace navigation to Paneru's virtual workspace rows:
;;
;;   Mod+Shift+J/K  → move-workspace-down/up        → switch virtual row
;;   Mod+Alt+J/K    → move-window-down-or-to-workspace-down/up → move window + follow
;;   Mod+Ctrl+J/K   → move-column-to-workspace-down/up → send window, stay put
;;
(paneru.bind "alt + ctrl - j" "window virtualmove south")
(paneru.bind "alt + ctrl - k" "window virtualmove north")

;; Quit ───────────────────────────────────────────────────────────────────────
;; Mod+Shift+E → quit
(paneru.bind "shift + cmd - e" "quit")

;; ────────────────────────────────────────────────────────────────────────────
;; Bindings WITHOUT a Paneru equivalent (handle outside Paneru):
;;
;;   Mod+T          spawn ghostty        → use Ghostty's own global hotkey setting
;;   Mod+Space      spawn fuzzel         → Raycast / Spotlight (Cmd+Space)
;;   Mod+Q          close-window         → Cmd+W or Cmd+Q (native macOS)
;;   Mod+Shift+R    switch-preset-window-height → no Paneru equivalent
;;   Mod+Ctrl+R     reset-window-height  → no Paneru equivalent
;;   Mod+Shift+F    fullscreen-window    → macOS native fullscreen (Ctrl+Cmd+F)
;;   Mod+U          focus-monitor-previous → Paneru only supports focus-monitor-next
;;   Mod+Alt+U      move-window-to-monitor-previous → Paneru only supports next
;;   Mod+Ctrl+U/I   move-column-to-monitor-prev/next → no Paneru equivalent
;;   Mod+Shift+U/I  move-workspace-to-monitor-prev/next → no Paneru equivalent
;;   Mod+W          toggle-column-tabbed → no Paneru equivalent
;;   Mod+Ctrl+Shift+H/J/K/L fine resize → Paneru only does preset widths
;;   Ctrl+Shift+3/4/5  screenshots       → Cmd+Shift+3/4/5 (native macOS)
;;   Mod+Shift+P    power-off-monitors   → display sleep shortcut
;;   Mod+semicolon  voxtype              → set a global shortcut in Voxtype itself
;;   Mod+A          wl-kbptr             → Wayland-specific, no macOS equivalent
;;   Ctrl+Shift+V   cliphist             → use a macOS clipboard manager (Maccy etc.)
;; ────────────────────────────────────────────────────────────────────────────
