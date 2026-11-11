;; Paneru configuration
;; Mirrors niri (binds.kdl, layout.kdl, window rules).
;;
;; niri sets `mod-key "Alt"`, so niri `Mod` is `alt` here. niri's Super-only
;; bindings (scratchpads, launchers, close-window, ...) are intentionally not
;; ported: on macOS Super is cmd and they would shadow cmd+S/T/M/O/F/H/Q etc.
;; The Mod+Super combinations are ported as `alt + ctrl + cmd`.

(local paneru (require :paneru))

;; niri: layout { gaps 16 } -> 8px of padding on each side of every window.
(local window-gap 8)

(paneru.setup
 {
  ;; ── Global options ───────────────────────────────────────────────────────
   :options {:focus_follows_mouse true
             :mouse_follows_focus true
             ;; niri: preset-column-widths
             :preset_column_widths [0.25 0.33333 0.5 0.66667 0.75]
             :animation_speed 120.0
             ;; niri: center-focused-column "on-overflow". Paneru only has
             ;; always/never; never is the closer of the two.
             :auto_center false
             :insert_windows_mid_strip true
             :reap_empty_workspaces true}

   ;; ── Screen edge padding ──────────────────────────────────────────────────
   ;; niri: struts { left 56; right 56 } plus 16px gaps. Paneru adds the
   ;; per-window padding below on top of this, so 64 + 8 = 72 = 56 + 16.
   :padding {:top window-gap :bottom window-gap :left 64 :right 64}

   ;; ── Swipe & gestures ─────────────────────────────────────────────────────
   :swipe {:continuous true
           ;; Trackpad gestures off; macOS keeps its own 3/4-finger swipes.
           :gesture {:fingers_count 0
                     :direction "Natural"}
           ;; alt + scroll slides the strip (niri Mod+scroll).
           ;; alt + shift + scroll switches virtual workspace rows.
           :scroll {:modifier "alt"
                    :vertical_modifier "shift"}}

   ;; ── Decorations ──────────────────────────────────────────────────────────
   ;; niri draws a focus ring (#ffb45060) and a 1px border (#1a151060 active,
   ;; #b4a89c40 inactive). Paneru has a single active-window border, so it
   ;; takes the focus ring. 0x60 / 0xff = 0.376 opacity.
   ;; niri: geometry-corner-radius 6
   :decorations {:active {:border {:enabled true
                                   :color "#ffb450"
                                   :opacity 0.376
                                   :width 4.0
                                   :radius 6.0}}}

   ;; ── Window rules ─────────────────────────────────────────────────────────
   :windows {;; niri: open-floating, open-focused false. Size and position are
             ;; set by the window_spawned handler below.
             :pip {:title "Picture.*(in)?.*[Pp]icture"
                   :floating true
                   :dont_focus true}
             :corfu {:app_name "emacs"
                     :title "^EmacsCorfuGUI.*"
                     :floating true}
             ;; niri: default-column-width { proportion 0.5; }, gaps 16
             :all {:title ".*"
                   :width 0.5
                   :horizontal_padding window-gap
                   :vertical_padding window-gap}}})

;; ── Helpers ──────────────────────────────────────────────────────────────────
;; Lua binding callbacks get the window set and must return either a new
;; window set or nil. Callbacks that queue commands with paneru.run return nil
;; explicitly, because paneru.run itself returns true.

(fn run [command]
  "A binding callback that runs a command string."
  (fn [_ws] (paneru.run command) nil))

(fn focused-column [ws]
  "The focused window, the window ids in its column (top to bottom), and the
focused window's position in that column. Nothing for a floating window."
  (let [id (ws:focused)
        column (and id (ws:column_of id))]
    (when column
      (let [ids (. (ws:columns) column)]
        (var at nil)
        (each [i window (ipairs ids)]
          (when (= window id) (set at i)))
        (values id ids at)))))

(fn display-count [ws]
  "Displays that hold at least one window."
  (let [seen {}]
    (var n 0)
    (each [_ window (ipairs (ws:windows))]
      (let [display (ws:display_of window.id)]
        (when (and display (not (. seen display.id)))
          (tset seen display.id true)
          (set n (+ n 1)))))
    n))

(fn usable-width [display]
  (- display.width paneru.config.padding.left paneru.config.padding.right))

;; ── Ported niri actions ──────────────────────────────────────────────────────

(fn move-window-or-to-workspace [direction]
  "niri move-window-{down,up}-or-to-workspace-{down,up}: move within the stack,
or to the next virtual row once there's no stack neighbour that way."
  (fn [ws]
    (let [(_ ids at) (focused-column ws)
          neighbour (when at
                      (. ids (if (= direction :south) (+ at 1) (- at 1))))]
      (paneru.run (.. (if neighbour "window swap " "window virtualmove ")
                      direction))
      nil)))

(fn consume-or-expel [side]
  "niri consume-or-expel-window-{left,right}: a stacked window leaves its
column on that side; a lone window joins the neighbouring column."
  (fn [ws]
    (let [(id ids) (focused-column ws)]
      (when id
        (if (> (length ids) 1)
            ;; unstack always lands left of what remains of the stack.
            (let [unstacked (ws:unstack id)]
              (if (= side :west)
                  unstacked
                  (let [rest (icollect [_ window (ipairs ids)]
                               (when (not= window id) window))]
                    (unstacked:swap id (. rest 1)))))
            (let [target (if (= side :west) (ws:west id) (ws:east id))]
              (when target (ws:stack id target))))))))

(fn change-width [delta]
  "niri set-window-width \"+10%\" / \"-10%\"."
  (fn [ws]
    (let [id (ws:focused)
          window (and id (ws:window id))
          display (and id (ws:display_of id))]
      (when (and window window.frame display (not window.floating))
        (let [current (/ (+ window.frame.width (* 2 window-gap))
                         (usable-width display))
              ratio (math.max 0.1 (math.min 1.0 (+ current delta)))]
          (ws:width id ratio))))))

(fn previous-display [command]
  "Paneru only cycles displays forward; going back is next, n - 1 times."
  (fn [ws]
    (for [_ 2 (display-count ws)]
      (paneru.run command))
    nil))

(fn switch-focus-floating-tiling [ws]
  "niri switch-focus-between-floating-and-tiling."
  (let [id (ws:focused)
        window (and id (ws:window id))
        floating (and window (or window.floating (not window.managed)))]
    (paneru.run (if floating "window focus managed" "window focus unmanaged"))
    nil))

(local focusmate-title "^Focusmate.*- Focusmate$")

(fn float-focusmate [ws]
  "niri Mod+Ctrl+G (float-focusmate): float the Focusmate window against the
left edge, vertically centred, at its current size."
  (let [window (ws:find (paneru.match {:title focusmate-title}))
        display (and window (ws:display_of window.id))]
    (if (and window window.frame display)
        (let [width (/ window.frame.width display.width)
              height (/ window.frame.height display.height)]
          (ws:float window.id {:x 0 :y (/ (- 1 height) 2)
                               :width width :height height}))
        (do (paneru.flash "No Focusmate window") nil))))

;; ── Keybindings ──────────────────────────────────────────────────────────────

;; Spawn ─────────────────────────────────────────────────────────────────────
;; Mod+T → ghostty. `open -n` starts a new Ghostty instance, as niri would.
(paneru.bind "alt - t"
             (fn [_ws] (paneru.exec "open" ["-na" "Ghostty"]) nil))

;; Focus ─────────────────────────────────────────────────────────────────────
;; Mod+H/L → focus-column-left/right
;; Mod+J/K → focus-window-or-workspace-down/up
(paneru.bind "alt - h" "window focus west")
(paneru.bind "alt - l" "window focus east")
(paneru.bind "alt - j" "window virtualfocus south")
(paneru.bind "alt - k" "window virtualfocus north")

;; Move ──────────────────────────────────────────────────────────────────────
;; Mod+Ctrl+H/L → move-column-left/right
;; Mod+Ctrl+J/K → move-window-down/up-or-to-workspace-down/up
(paneru.bind "alt + ctrl - h" "window swap west")
(paneru.bind "alt + ctrl - l" "window swap east")
(paneru.bind "alt + ctrl - j" (move-window-or-to-workspace :south))
(paneru.bind "alt + ctrl - k" (move-window-or-to-workspace :north))

;; Mod+Shift+Ctrl+H/L → consume-or-expel-window-left/right
(paneru.bind "alt + shift + ctrl - h" (consume-or-expel :west))
(paneru.bind "alt + shift + ctrl - l" (consume-or-expel :east))

;; Mod+Shift+Ctrl+J/K → move-column-to-workspace-down/up
;; Paneru moves windows, not columns: a stacked window moves on its own.
(paneru.bind "alt + shift + ctrl - j" "window virtualmove south")
(paneru.bind "alt + shift + ctrl - k" "window virtualmove north")

;; Monitors ──────────────────────────────────────────────────────────────────
;; Mod+U/I → focus-monitor-previous/next
(paneru.bind "alt - u" (previous-display "mouse nextdisplay"))
(paneru.bind "alt - i" "mouse nextdisplay")

;; Mod+Ctrl+U/I → move-window-to-monitor-previous/next
(paneru.bind "alt + ctrl - u" (previous-display "window nextdisplay"))
(paneru.bind "alt + ctrl - i" "window nextdisplay")

;; Mod+Shift+Ctrl+U/I → move-column-to-monitor-previous/next
;; Same as moving the window: a stacked window moves on its own.
(paneru.bind "alt + shift + ctrl - u" (previous-display "window nextdisplay"))
(paneru.bind "alt + shift + ctrl - i" "window nextdisplay")

;; Size ──────────────────────────────────────────────────────────────────────
;; Mod+R       → switch-preset-column-width
;; Mod+Shift+R → switch-preset-window-height (stacked windows only)
;; Mod+Ctrl+R  → reset-window-height (equalizes the whole stack)
;; Mod+F       → maximize-column
;; Mod+C       → center-column
(paneru.bind "alt - r" "window resize")
(paneru.bind "alt + shift - r" "window vertical grow")
(paneru.bind "alt + ctrl - r" "window equalize")
(paneru.bind "alt - f" "window fullwidth")
(paneru.bind "alt - c" "window center")

;; Mod+Ctrl+Super+H/L → set-window-width -10% / +10%
;; Mod+Ctrl+Super+J/K → set-window-height -10% / +10%
;; Paneru can't set a height directly, so J/K step through the preset stack
;; heights instead (stacked windows only).
(paneru.bind "alt + ctrl + cmd - h" (change-width -0.1))
(paneru.bind "alt + ctrl + cmd - l" (change-width 0.1))
(paneru.bind "alt + ctrl + cmd - j" "window vertical shrink")
(paneru.bind "alt + ctrl + cmd - k" "window vertical grow")

;; Floating ──────────────────────────────────────────────────────────────────
;; Mod+G      → switch-focus-between-floating-and-tiling
;; Mod+Ctrl+G → float-focusmate
;; Mod+V      → toggle-window-floating (niri's default bind; not in binds.kdl)
(paneru.bind "alt - g" switch-focus-floating-tiling)
(paneru.bind "alt + ctrl - g" float-focusmate)
(paneru.bind "alt - v" "window manage")

;; Session ───────────────────────────────────────────────────────────────────
;; Mod+Shift+P → power-off-monitors
;; Mod+Shift+E → quit
(paneru.bind "alt + shift - p"
             (fn [_ws] (paneru.exec "/usr/bin/pmset" ["displaysleepnow"]) nil))
(paneru.bind "alt + shift - e" "quit")

;; Paneru-only extras (no niri equivalent) ───────────────────────────────────
(paneru.bind "shift + cmd - h" "window focus first")
(paneru.bind "shift + cmd - l" "window focus last")
(paneru.bind "shift + alt - h" "window swap first")
(paneru.bind "shift + alt - l" "window swap last")
(paneru.bind "shift + cmd - e" "window snap")

;; ── Window placement ────────────────────────────────────────────────────────

;; niri picture-in-picture rule: 284x160 in the top-left corner.
(paneru.on :window_spawned (paneru.match {:title "Picture.*(in)?.*[Pp]icture"})
           (fn [event ws]
             (let [display (ws:display_of event.window_id)]
               (when display
                 (ws:float event.window_id
                           {:x 0 :y 0
                            :width (/ 284 display.width)
                            :height (/ 160 display.height)})))))

;; niri slack rule: default-column-width { fixed 1055; }
(paneru.on :window_spawned (paneru.match {:bundle "^com\\.tinyspeck\\.slackmacgap$"})
           (fn [event ws]
             (let [display (ws:display_of event.window_id)]
               (when display
                 (ws:width event.window_id
                           (/ (+ 1055 (* 2 window-gap)) (usable-width display)))))))

;; ────────────────────────────────────────────────────────────────────────────
;; niri bindings NOT ported:
;;
;;   Super-only bindings   cmd clashes on macOS (see top of file)
;;   Mod+Shift+J/K         move-workspace-down/up → Paneru can't reorder rows
;;   Mod+Shift+U/I         move-workspace-to-monitor → no Paneru equivalent
;;   Mod+Shift+F           fullscreen-window → macOS fullscreen (ctrl+cmd+F)
;;   Mod+Shift+Slash       show-hotkey-overlay → no Paneru equivalent
;;   Mod+Shift+Ctrl+Super+L  swaylock → macOS lock (ctrl+cmd+Q)
;;   Mod+Ctrl+Shift+A/S    ddcutil input switching → needs a macOS DDC tool
;;   Mod+Ctrl+Shift+Super+3/4/5  dynamic screencast targets → Wayland-only
;;   Ctrl+Shift+3/4/5      screenshots → macOS cmd+shift+3/4/5
;;   Ctrl+Shift+V          cliphist → a macOS clipboard manager
;;   XF86 media/brightness keys → handled by macOS
;;
;; niri layout/rules NOT ported:
;;   inactive border colour, window opacity and blur, empty-workspace-above-
;;   first, block-out-from screencast, Slack's "messaging" named workspace.
;; ────────────────────────────────────────────────────────────────────────────
