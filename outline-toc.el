;;; outline-toc.el --- Sidebar showing a "table of contents" for an outline-mode buffer

;; Copyright (C) 2017 Austin Bingham

;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Keywords: outline
;; Version: 0.0

;; This file is not part of GNU Emacs.

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:

;; This provides a sidebar buffer which shows a "table of contents" for an
;; associated outline-mode buffer. Basically, this shows you the sections of the
;; outline-mode buffer, but not the bodies. This is to help you remember where
;; you are in a large document.

;; Simply use M-x outline-toc-mode to toggle activation of the outline-toc.
;; Use 'M-x customize-group RET outline-toc RET' to adapt outline-toc to your
;; needs.

;; Much of this was originally adapated from David Engster's excellent
;; minimap.el (https://github.com/dengste/minimap).

;;; Code:

(defgroup outline-toc nil
  "A outline-toc sidebar for Emacs."
  :group 'convenience)

(defface outline-toc-font-face
  '((default :family "DejaVu Sans Mono" :height 30))
  "Face used for text in outline-toc buffer, notably the font family and height.
This height should be really small.  You probably want to use a
TrueType font for this.  After changing this, you should
recreate the outline-toc to avoid problems with recentering."
  :group 'outline-toc)

(defface outline-toc-active-region-background
  '((((background dark)) (:background "#700000"))
    (t (:background "#C847D8FEFFFF")))
  "Face for the active region in the outline-toc.
By default, this is only a different background color."
  :group 'outline-toc)

;; (defface outline-toc-semantic-function-face
;;   '((((background dark))
;;      (:box (:line-width 1 :color "white")
;;     :inherit (font-lock-function-name-face outline-toc-font-face)
;;     :height 2.75 :background "#202414"))
;;     (t (:box (:line-width 1 :color "black")
;;       :inherit (font-lock-function-name-face outline-toc-font-face)
;;       :height 2.75 :background "gray90")))
;;   "Face used for functions in the semantic overlay.")

;; (defface outline-toc-semantic-variable-face
;;   '((((background dark))
;;      (:box (:line-width 1 :color "white")
;;     :inherit (font-lock-variable-name-face outline-toc-font-face)
;;      :height 2.75 :background "gray10"))
;;     (t (:box (:line-width 1 :color "black")
;;       :inherit (font-lock-function-name-face outline-toc-font-face)
;;       :height 2.75 :background "gray90")))
;;   "Face used for variables in the semantic overlay.")

;; (defface outline-toc-semantic-type-face
;;   '((((background dark))
;;      (:box (:line-width 1 :color "white")
;;     :inherit (font-lock-type-face outline-toc-font-face)
;;     :height 2.75 :background "gray10"))
;;     (t (:box (:line-width 1 :color "black")
;;       :inherit (font-lock-function-name-face outline-toc-font-face)
;;       :height 2.75 :background "gray90")))
;;   "Face used for types in the semantic overlay.")

(defcustom outline-toc-width-fraction 0.15
  "Fraction of width which should be used for outline-toc sidebar."
  :type 'number
  :group 'outline-toc)

(defcustom outline-toc-minimum-width 30
  "Minimum width of outline-toc in characters (default size).
Use nil to disable."
  :type 'number
  :group 'outline-toc)

(defcustom outline-toc-window-location 'left
  "Location of the outline-toc window.
Can be either the symbol `left' or `right'."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'outline-toc)

(defcustom outline-toc-buffer-name " *OUTLINE-TOC*"
  "Buffer name of outline-toc sidebar."
  :type 'string
  :group 'outline-toc)

(defcustom outline-toc-update-delay 0.1
  "Delay in seconds after which sidebar gets updated.
Setting this to 0 will let the outline-toc react immediately, but
this will slow down scrolling."
  :type 'number
  :set (lambda (sym value)
         (set sym value)
         (when (and (boundp 'outline-toc-timer-object)
                    outline-toc-timer-object)
           (cancel-timer outline-toc-timer-object)
           (setq outline-toc-timer-object
                 (run-with-idle-timer
                  outline-toc-update-delay t 'outline-toc-update))))
  :group 'outline-toc)

(defcustom outline-toc-always-recenter nil
  "Whether outline-toc sidebar should be recentered after every point movement."
  :type 'boolean
  :group 'outline-toc)

(defcustom outline-toc-recenter-type 'relative
  "Specifies the type of recentering the outline-toc should use.
The outline-toc can use different types of recentering, i.e., how the
outline-toc should behave when you scroll in the main window or when
you drag the active region with the mouse.  The following
explanations will probably not help much, so simply try them and
choose the one which suits you best.
`relative' -- The position of the active region in the outline-toc
corresponds with the relative position of this region in the
buffer.  This the default.
`middle' -- The active region will stay fixed in the middle of
the outline-toc.
`free' -- The position will be more or less free.  When dragging
the active region, the outline-toc will scroll when you reach the
bottom or top."
  :type '(choice (const :tag "Relative" relative)
                 (const :tag "Middle" middle)
                 (const :tag "Free" free))
  :group 'outline-toc)

(defcustom outline-toc-hide-scroll-bar t
  "Whether the outline-toc should hide the vertical scrollbar."
  :type 'boolean
  :group 'outline-toc)

(defcustom outline-toc-hide-fringes nil
  "Whether the outline-toc should hide the fringes."
  :type 'boolean
  :group 'outline-toc)

(defcustom outline-toc-dedicated-window t
  "Whether the outline-toc should create a dedicated window."
  :type 'boolean
  :group 'outline-toc)

;; (defcustom outline-toc-display-semantic-overlays t
;;   "Display overlays from CEDET's semantic analyzer.
;; If you use CEDET and the buffer's major-mode is supported, the
;; outline-toc can display overlays generated by the semantic analyzer.
;; By default, it will apply the faces `outline-toc-semantic-<X>-face',
;; with <X> being \"function\", \"variable\" and \"type\".  Also, it
;; will display the name of the tag in the middle of the overlay in
;; the corresponding font-lock face.
;; See also `outline-toc-enlarge-certain-faces', which can be used as
;; fallback."
;;   :type 'boolean
;;   :group 'outline-toc)

;; (defcustom outline-toc-enlarge-certain-faces 'as-fallback
;;   "Whether certain faces should be enlarged in the outline-toc.
;; All faces listed in `outline-toc-normal-height-faces' will be
;; displayed using the default font height, allowing you to still
;; read text using those faces.  By default, this should enlarge all
;; function names in the outline-toc, given you have font locking
;; enabled.  This variable can have the following values:
;; 'as-fallback (the default) -- The feature will only be activated
;;   if information from CEDET's semantic analyzer isn't available
;;   (see: `outline-toc-display-semantic-overlays').
;; 'always -- Always active.
;; nil -- Inactive."
;;   :type '(choice (const :tag "Fallback if CEDET unavailable." 'as-fallback)
;;       (const :tag "Always active." 'always)
;;       (const :tag "Inactive." nil))
;;   :group 'outline-toc)

;; (defcustom outline-toc-normal-height-faces '(font-lock-function-name-face)
;;   "List of faces which should be displayed with normal height.
;; When `outline-toc-enlarge-certain-faces' is non-nil, all faces in
;; this list will be displayed using the default font height.  By
;; default, this list contains `font-lock-function-name-face', so
;; you can still read function names in the outline-toc."
;;   :type '(repeat face)
;;   :group 'outline-toc)

;; (defcustom outline-toc-sync-overlay-properties '(face invisible)
;;   "Specifies which overlay properties should be synced.
;; Unlike text properties, overlays are not applied automatically to
;; the outline-toc and must be explicitly synced.  This variable
;; specifies which overlay properties should be synced by
;; `outline-toc-sync-overlays'.  Most importantly, this variable should
;; include 'invisible', so that hidden text does not appear in the
;; outline-toc buffer."
;;   :type '(repeat symbol)
;;   :group 'outline-toc)

;; TODO: How do we specify "for all outline-mode" docs? Outline-mode is minor, I think...
(defcustom outline-toc-major-modes '(markdown-mode org-mode)
  "Major modes for which a outline-toc should be created.
This can also be a parent mode like 'prog-mode.
If nil, a outline-toc must be explicitly created for each buffer."
  :type '(repeat symbol)
  :group 'outline-toc)

(defcustom outline-toc-recreate-window t
  "Whether the outline-toc window should be automatically re-created.
If this is non-nil, the side window for the outline-toc will be
automatically re-created as soon as you kill it."
  :type 'boolean
  :group 'outline-toc)

(defcustom outline-toc-automatically-delete-window t
  "Whether the outline-toc window should be automatically deleted.
Setting this to non-nil will delete the minibuffer side window
when you enter a buffer which is not derived from
`outline-toc-major-modes' (excluding the minibuffer)."
  :type 'boolean
  :group 'outline-toc)

;; (defcustom outline-toc-tag-only nil
;;   "Whether the outline-toc should only display parsed tags from CEDET."
;;   :type 'boolean
;;   :group 'outline-toc)

(defcustom outline-toc-highlight-line t
  "Whether the outline-toc should highlight the current line."
  :type 'boolean
  :group 'outline-toc)

;;; Internal variables

;; The buffer currently displayed in the outline-toc
(defvar outline-toc-active-buffer nil)

;; Window start/end from the base buffer
(defvar outline-toc-start nil)
(defvar outline-toc-end nil)

;; General overlay for the outline-toc
(defvar outline-toc-base-overlay nil)

;; Overlay for the active region
(defvar outline-toc-active-overlay nil)

;; Timer
(defvar outline-toc-timer-object nil)

;; Lines the outline-toc can display
(defvar outline-toc-numlines nil)
(defvar outline-toc-pointmin-overlay nil)

;; Line overlay
(defvar outline-toc-line-overlay nil)


;;; Helpers

(defun outline-toc-active-current-buffer-p ()
  "Whether the current buffer is displayed in the outline-toc."
  (and (eq (current-buffer) outline-toc-active-buffer)
       (get-buffer outline-toc-buffer-name)
       (with-current-buffer outline-toc-buffer-name
         (eq outline-toc-active-buffer (buffer-base-buffer)))))

(defsubst outline-toc-get-window ()
  "Get current outline-toc window."
  (when (get-buffer outline-toc-buffer-name)
    (get-buffer-window outline-toc-buffer-name)))

(defsubst outline-toc-kill-buffer ()
  "Kill the outline-toc buffer."
  (when (get-buffer outline-toc-buffer-name)
    (kill-buffer outline-toc-buffer-name)))

(defun outline-toc-create-window ()
  (let ((width (round (* (window-width)
                         outline-toc-width-fraction))))
    (when (< width outline-toc-minimum-width)
      (setq width outline-toc-minimum-width))
    (if (eq outline-toc-window-location 'left)
        (split-window-horizontally width)
      (split-window-horizontally
       (* -1 width))
      (other-window 1))
    ;; Set up the outline-toc window:
    ;; You should not be able to enter the outline-toc window.
    (set-window-parameter nil 'no-other-window t)
    ;; Hide things.
    (when outline-toc-hide-scroll-bar
      (setq vertical-scroll-bar nil))
    (when outline-toc-hide-fringes
      (set-window-fringes nil 0 0))
    ;; Switch to buffer.
    (switch-to-buffer
     (get-buffer-create outline-toc-buffer-name) t t)
    ;; Do not fold lines in the outline-toc.
    (setq truncate-lines t)
    ;; Make it dedicated.
    (when outline-toc-dedicated-window
      (set-window-dedicated-p nil t))
    (prog1
        (selected-window)
      (other-window 1))))

(defun outline-toc-setup-hooks (&optional remove)
  "Hook outline-toc into other modes.
If REMOVE is non-nil, remove outline-toc from other modes."
  ;; (if remove
  ;;     (progn
  ;;       (remove-hook 'outline-view-change-hook 'outline-toc-sync-overlays)
  ;;       (remove-hook 'hs-hide-hook 'outline-toc-sync-overlays)
  ;;       (remove-hook 'hs-show-hook 'outline-toc-sync-overlays))
  ;;   ;; outline-(minor-)mode
  ;;   (add-hook 'outline-view-change-hook 'outline-toc-sync-overlays)
  ;;   ;; hideshow
  ;;   (add-hook 'hs-hide-hook 'outline-toc-sync-overlays)
  ;;   (add-hook 'hs-show-hook 'outline-toc-sync-overlays))
  )

;;; Outline-Toc creation / killing

;;;###autoload
(define-minor-mode outline-toc-mode
  "Toggle outline-toc mode."
  :global t
  :group 'outline-toc
  :lighter "OToc"
  (if outline-toc-mode
      (progn
        (when (and outline-toc-major-modes
                   (apply 'derived-mode-p outline-toc-major-modes))
          (unless (outline-toc-get-window)
            (outline-toc-create-window))
          ;; Create outline-toc.
          (outline-toc-new-outline-toc))
        ;; Create timer.
        (setq outline-toc-timer-object
              (run-with-idle-timer outline-toc-update-delay t 'outline-toc-update))
        ;; Hook into other modes.
        (outline-toc-setup-hooks))
    ;; Turn it off
    (outline-toc-kill)
    (outline-toc-setup-hooks t)))

(defun outline-toc-create ()
  "Create a outline-toc sidebar."
  (interactive)
  (outline-toc-mode 1))

(defun outline-toc-new-outline-toc ()
  "Create new outline-toc BUFNAME for current buffer and window.
Re-use already existing outline-toc window if possible."
  (interactive)
  (let ((currentbuffer (current-buffer))
        (win (outline-toc-get-window))
        (indbuf (make-indirect-buffer (current-buffer)
                                      (concat outline-toc-buffer-name "_temp")))
        (edges (window-pixel-edges)))

    ;; Remember the active buffer currently displayed in the outline-toc.
    (setq outline-toc-active-buffer (current-buffer))

    ;; Hook into CEDET if necessary.
    ;; (when (and outline-toc-display-semantic-overlays
    ;;            (boundp 'semantic-after-toplevel-cache-change-hook))
    ;;   (add-hook 'semantic-after-partial-cache-change-hook
    ;;             'outline-toc-apply-semantic-overlays nil t)
    ;;   (add-hook 'semantic-after-toplevel-cache-change-hook
    ;;             'outline-toc-apply-semantic-overlays nil t))

    (with-selected-window win
      ;; Now set up the outline-toc:
      (when (window-dedicated-p)
        (set-window-dedicated-p nil nil))
      (switch-to-buffer indbuf t t)
      (outline-toc-kill-buffer)
      (rename-buffer outline-toc-buffer-name)

      ;; Do not fold lines in the outline-toc.
      ;; (setq truncate-lines t)

      (when outline-toc-dedicated-window
        (set-window-dedicated-p nil t))

      ;; Set up the base overlay
      (setq outline-toc-base-overlay (make-overlay (point-min) (point-max) nil t t))
      (overlay-put outline-toc-base-overlay 'face 'outline-toc-font-face)
      (overlay-put outline-toc-base-overlay 'priority 1)
      (when outline-toc-tag-only
        (overlay-put outline-toc-base-overlay 'face
                     `(:inherit outline-toc-font-face
                                :foreground ,(face-background 'default))))

      ;; Set up the pointmin overlay
      (setq outline-toc-pointmin-overlay (make-overlay (point-min) (1+ (point-min))))

      ;; Set up the active overlay
      (setq outline-toc-start (window-start)
            outline-toc-end (window-end)
            outline-toc-active-overlay (make-overlay outline-toc-start outline-toc-end)
            line-spacing 0)
      (overlay-put outline-toc-active-overlay 'face
                   'outline-toc-active-region-background)
      (when outline-toc-tag-only
        (overlay-put outline-toc-active-overlay 'face
                     `(:inherit 'outline-toc-active-region-background
                                :foreground ,(face-background 'outline-toc-active-region-background))))
      (overlay-put outline-toc-active-overlay 'priority 5)

      (outline-toc-sb-mode 1)

      ;; (when (and (boundp 'linum-mode)
      ;;            linum-mode)
      ;;   (linum-mode 0))

      (setq buffer-read-only t)

      ;; Calculate the actual number of lines displayable with the outline-toc face.
      (setq outline-toc-numlines
            (floor
             (/
              (- (nth 3 edges) (nth 1 edges))
              (car (progn (redisplay t) (window-line-height)))))))

    (outline-toc-sync-overlays)))

(defun outline-toc-kill ()
  "Kill outline-toc."
  (interactive)
  (when (outline-toc-get-window)
    (delete-window (outline-toc-get-window)))
  (cancel-timer outline-toc-timer-object))

;;; Outline-Toc update

(defun outline-toc-update (&optional force)
  "Update outline-toc sidebar if necessary.
This is meant to be called from the idle-timer or the post command hook.
When FORCE, enforce update of the active region."
  (interactive)
  ;; If we are in the minibuffer, do nothing.
  (unless (active-minibuffer-window)
    (if (outline-toc-active-current-buffer-p)
        ;; We are still in the same buffer, so just update the outline-toc.
        (let ((win (outline-toc-get-window))
              (start (window-start))
              (end (window-end))
              (pt (point)))
          (when (and (null win)
                     outline-toc-recreate-window)
            ;; The outline-toc window is no longer visible, so create it again...
            (setq win (outline-toc-create-window))
            ;; ...and switch to existing outline-toc buffer.
            (with-selected-window win
              (when (window-dedicated-p)
                (set-window-dedicated-p nil nil))
              (switch-to-buffer outline-toc-buffer-name t t)
              (when outline-toc-dedicated-window
                (set-window-dedicated-p nil t))))
          (with-selected-window win
            ;; Make sure the base overlay spans the whole buffer.
            (unless (and (= (overlay-start outline-toc-base-overlay) (point-min))
                         (= (overlay-end outline-toc-base-overlay) (point-max)))
              (move-overlay outline-toc-base-overlay (point-min) (point-max)))
            (unless (and (not force)
                         (= outline-toc-start start)
                         (= outline-toc-end end))
              ;; Update the overlay.
              (move-overlay outline-toc-active-overlay start end)
              (setq outline-toc-start start
                    outline-toc-end end)
              (outline-toc-recenter (line-number-at-pos (/ (+ end start) 2))
                                    (/ (- (line-number-at-pos end)
                                          (line-number-at-pos start))
                                       2)))
            (goto-char pt)
            (beginning-of-line)
            (unless outline-toc-line-overlay
              (setq outline-toc-line-overlay (make-overlay (point) (1+ (point)) nil t))
              (overlay-put outline-toc-line-overlay 'face '(:background "yellow" :foreground "yellow"))
              (overlay-put outline-toc-line-overlay 'priority 6))
            (move-overlay outline-toc-line-overlay (point) (line-beginning-position 2))
            (when outline-toc-always-recenter
              (recenter (round (/ (window-height) 2)))))
          ;; Redisplay
          (sit-for 0))
      ;; The buffer was switched, check if the outline-toc should switch, too.
      (if (and outline-toc-major-modes
               (apply 'derived-mode-p outline-toc-major-modes))
          (progn
            ;; Create window if necessary...
            (unless (outline-toc-get-window)
              (outline-toc-create-window))
            ;; ...and re-create outline-toc with new buffer...
            (outline-toc-new-outline-toc)
            ;; Redisplay
            (sit-for 0)
            ;; ...and call update again.
            (outline-toc-update t))
        ;; Otherwise, delete window if the user so wishes.
        (when (and (outline-toc-get-window)
                   outline-toc-automatically-delete-window)
          ;; We wait a tiny bit before deleting the window, since we
          ;; might only be temporarily in another buffer.
          (run-with-timer 0.3 nil
                          (lambda ()
                            (when (and (null (outline-toc-active-current-buffer-p))
                                       (outline-toc-get-window))
                              (delete-window (outline-toc-get-window))))))))))

;;; Overlay movement

(defun outline-toc-move-overlay-mouse (start-event)
  "Move overlay by tracking mouse movement."
  (interactive "e")
  (mouse-set-point start-event)
  (when (get-buffer-window (buffer-base-buffer (current-buffer)))
    (let* ((echo-keystrokes 0)
           (end-posn (event-end start-event))
           (start-point (posn-point end-posn))
           (make-cursor-line-fully-visible nil)
           (cursor-type nil)
           (outline-toc-automatically-delete-window nil)
           (pcselmode (when (boundp 'pc-selection-mode)
                        pc-selection-mode))
           pt ev)
      (when (and pcselmode (fboundp 'pc-selection-mode))
        (pc-selection-mode -1))
      (move-overlay outline-toc-active-overlay start-point outline-toc-end)
      (track-mouse
        (outline-toc-set-overlay start-point)
        (while (and
                (consp (setq ev (read-event)))
                (eq (car ev) 'mouse-movement))
          (setq pt (posn-point (event-start ev)))
          (when (numberp pt)
            (goto-char pt)
            (beginning-of-line)
            (outline-toc-set-overlay (point)))))
      (select-window (get-buffer-window (buffer-base-buffer)))
      (outline-toc-update)
      (when (and pcselmode (fboundp 'pc-selection-mode))
        (pc-selection-mode 1)))))

(defun outline-toc-set-overlay (pt)
  "Set overlay position, with PT being the middle."
  (goto-char pt)
  (let* ((ovstartline (line-number-at-pos outline-toc-start))
         (ovendline (line-number-at-pos outline-toc-end))
         (ovheight (round (/ (- ovendline ovstartline) 2)))
         (line (line-number-at-pos))
         (winstart (window-start))
         (winend (window-end))
         newstart newend)
    (setq pt (point-at-bol))
    (setq newstart (outline-toc-line-to-pos (- line ovheight)))
    ;; Perform recentering
    (outline-toc-recenter line ovheight)
    ;; Set new position in main buffer and redisplay
    (with-selected-window (get-buffer-window (buffer-base-buffer))
      (goto-char pt)
      (set-window-start nil newstart)
      (redisplay t)
      (setq newend (window-end)))
    (when (eq outline-toc-recenter-type 'free)
      (while (> newend winend)
        (scroll-up 5)
        (redisplay t)
        (setq winend (window-end))))
    (move-overlay outline-toc-active-overlay newstart newend)))

(defun outline-toc-line-to-pos (line)
  "Return point position of line number LINE."
  (save-excursion
    (goto-char 1)
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))
    (point)))

(defun outline-toc-recenter (middle height)
  "Recenter the outline-toc according to `outline-toc-recenter-type'.
MIDDLE is the line number in the middle of the active region.
HEIGHT is the number of lines from MIDDLE to begin/end of the
active region."
  (cond
   ;; Relative recentering
   ((eq outline-toc-recenter-type 'relative)
    (let* ((maxlines (line-number-at-pos (point-max)))
           percentage relpos newline start numlines)
      (setq numlines (count-lines (window-start) (window-end)))
      (setq percentage (/ (float middle) (float maxlines)))
      (setq newline (ceiling (* percentage numlines)))
      (setq start (outline-toc-line-to-pos
                   (- middle height
                      (floor (* percentage
                                (- numlines height height))))))
      (or (> start (point-min))
          (setq start (point-min)))
      ;; If (point-max) already visible, don't go further
      (if (and (> start (window-start))
               (with-selected-window (get-buffer-window (buffer-base-buffer))
                 (= (point-max) (window-end))))
          (save-excursion
            (goto-char (point-max))
            (recenter -1))
        (unless (and (> start (window-start))
                     (= (point-max) (window-end)))
          (set-window-start nil start)))))
   ;; Middle recentering
   ((eq outline-toc-recenter-type 'middle)
    (let ((start (- middle height
                    (floor (* 0.5
                              (- outline-toc-numlines height height))))))
      (if (< start 1)
          (progn
            ;; Hack: Emacs cannot scroll down any further, so we fake
            ;; it using an overlay.  Otherwise, the active region
            ;; would move to the top.
            (overlay-put outline-toc-pointmin-overlay
                         'display (concat
                                   (make-string (abs start) 10)
                                   (buffer-substring (point-min) (1+ (point-min)))))
            (overlay-put outline-toc-pointmin-overlay
                         'face `(:background ,(face-background 'default)))
            (overlay-put outline-toc-pointmin-overlay
                         'priority 10)
            (setq start 1))
        (overlay-put outline-toc-pointmin-overlay 'display "")
        (overlay-put outline-toc-pointmin-overlay 'face nil))
      (set-window-start nil (outline-toc-line-to-pos start))))
   ;; Free recentering
   ((eq outline-toc-recenter-type 'free)
    (let ((newstart (outline-toc-line-to-pos (- middle height)))
          (winstart (window-start)))
      (while (< newstart winstart)
        (scroll-down 5)
        (redisplay t)
        (setq winstart (window-start)))))))

;;; Outline-Toc minor mode

(defvar outline-toc-sb-mode-map (make-sparse-keymap)
  "Keymap used by `outline-toc-sb-mode'.")

(define-key outline-toc-sb-mode-map [down-mouse-1] 'outline-toc-move-overlay-mouse)
(define-key outline-toc-sb-mode-map [down-mouse-2] 'outline-toc-move-overlay-mouse)
(define-key outline-toc-sb-mode-map [down-mouse-3] 'outline-toc-move-overlay-mouse)

(define-minor-mode outline-toc-sb-mode
  "Minor mode for outline-toc sidebar."
  nil "outline-toc" outline-toc-sb-mode-map)

;;; Sync outline-toc with modes which create/delete overlays.

(defun outline-toc-sync-overlays ()
  "Synchronize overlays between base and outline-toc buffer.
Apply semantic overlays or face enlargement if necessary."
  (interactive)
  ;; Get overlays and Semantic status from base buffer.
  (when (and outline-toc-mode
             (outline-toc-active-current-buffer-p))
    (with-current-buffer outline-toc-active-buffer
      (let ((baseov (overlays-in (point-min) (point-max)))
            (semantic (and (boundp 'semantic-version)
                           (semantic-active-p)))
            ov props p)
        ;; Apply overlays to outline-toc.
        (with-current-buffer outline-toc-buffer-name
          ;; Delete overlays (but keep our own).
          (let ((ovs (overlays-in (point-min) (point-max))))
            (dolist (ov ovs)
              (unless (member ov (list outline-toc-pointmin-overlay
                                       outline-toc-base-overlay
                                       outline-toc-active-overlay))
                (delete-overlay ov))))
          (while baseov
            (when (and (eq (overlay-buffer (car baseov)) outline-toc-active-buffer)
                       (setq props (outline-toc-get-sync-properties (car baseov))))
              (setq ov (make-overlay (overlay-start (car baseov))
                                     (overlay-end (car baseov))))
              (while (setq p (car props))
                (overlay-put ov (car p) (cadr p))
                (setq props (cdr props))))
            (setq baseov (cdr baseov)))
          (move-overlay outline-toc-pointmin-overlay (point-min) (1+ (point-min)))
          ;; Re-apply font overlay
          (move-overlay outline-toc-base-overlay (point-min) (point-max)))
        ;; Face enlargement
        (when (and font-lock-mode
                   (or (eq outline-toc-enlarge-certain-faces 'always)
                       (and (eq outline-toc-enlarge-certain-faces 'as-fallback)
                            (or (not outline-toc-display-semantic-overlays)
                                (not semantic)))))
          (when (eq font-lock-support-mode 'jit-lock-mode)
            (jit-lock-fontify-now))
          (outline-toc-enlarge-faces))
        ;; Semantic overlays
        (when (and semantic
                   outline-toc-display-semantic-overlays)
          (outline-toc-apply-semantic-overlays t))))))

(defun outline-toc-get-sync-properties (ov)
  "Get properties from overlay OV which should be synced.
You can specify those properties with
`outline-toc-sync-overlay-properties'."
  (let ((syncprops outline-toc-sync-overlay-properties))
    (when outline-toc-tag-only
      (setq syncprops (delq 'face syncprops)))
    (delq nil
          (mapcar
           (lambda (p)
             (let ((val (overlay-get ov p)))
               (if val
                   (list p val)
                 nil)))
           syncprops))))

(defun outline-toc-enlarge-faces ()
  "Apply default font to all faces in `outline-toc-normal-height-faces'."
  (let ((pos (next-single-property-change (point-min) 'face))
        next ov face)
    (while pos
      (setq face (get-text-property pos 'face))
      (when (and face
                 (member face outline-toc-normal-height-faces))
        (with-current-buffer outline-toc-buffer-name
          (setq ov
                (make-overlay pos
                              (setq pos (next-single-property-change pos 'face))))
          (overlay-put ov 'face `(:family ,(face-font 'default)))
          (overlay-put ov 'priority 5)))
      (setq pos (next-single-property-change pos 'face)))))

(defun outline-toc-apply-semantic-overlays (tags)
  "Apply semantic overlays to the outline-toc.
TAGS is the list of tags.  If it is t, fetch tags from buffer."
  (when (and tags
             outline-toc-mode)
    (with-current-buffer outline-toc-active-buffer
      (let (tag class ov ovnew)
        (when (eq tags t)
          (setq tags (semantic-fetch-tags)))
        (while tags
          (setq tag (car tags))
          (setq class (semantic-tag-class tag))
          (setq ov (semantic-tag-overlay tag))
          (when (and (overlayp ov)
                     (or (eq class 'function)
                         (eq class 'type)
                         (eq class 'variable)))
            (with-current-buffer outline-toc-buffer-name
              (let* ((start (overlay-start ov))
                     (end (overlay-end ov))
                     (name (semantic-tag-name tag))
                     (lstart (line-number-at-pos start))
                     (lend (line-number-at-pos end)))
                ;; First, remove old Semantic overlays.
                (remove-overlays start end 'outline-toc-semantic t)
                (if outline-toc-tag-only
                    ;; Now put the new ones.
                    (overlay-put
                     (setq ovnew (make-overlay start end))
                     'face `(:background ,(face-background
                                           (intern (format "outline-toc-semantic-%s-face"
                                                           (symbol-name class))))
                                         :foreground
                                         ,(face-background
                                           (intern (format "outline-toc-semantic-%s-face"
                                                           (symbol-name class))))
                                         ))
                  ;; Now put the new ones.
                  (overlay-put
                   (setq ovnew (make-overlay start end))
                   'face `(:background ,(face-background
                                         (intern (format "outline-toc-semantic-%s-face"
                                                         (symbol-name class)))))))
                (overlay-put ovnew 'priority 4)
                (when (and (eq class 'function)
                           (> (- lend lstart) 5))
                  (overlay-put ovnew 'priority 1)
                  (overlay-put ovnew 'outline-toc-semantic t)
                  (overlay-put (setq ovnew (make-overlay start (progn (goto-char start) (point-at-eol))))
                               'display (make-string 200 ?\u203E))
                  (overlay-put ovnew 'outline-toc-semantic t)
                  (overlay-put ovnew 'face `(:foreground ,(face-foreground 'default) :overline nil))
                  (overlay-put ovnew 'priority 8)
                  (overlay-put (setq ovnew (make-overlay (progn (goto-char end) (point-at-bol)) end))
                               'display (make-string 200 ?_))
                  (overlay-put ovnew 'face `(:foreground ,(face-foreground 'default)))
                  (overlay-put ovnew 'outline-toc-semantic t)
                  (overlay-put ovnew 'priority 8))
                (setq start
                      (outline-toc-line-to-pos (/ (+ lstart lend) 2)))
                (goto-char start)
                (while (looking-at "^$")
                  (forward-line -1))
                (setq start (point))
                (setq end (progn (goto-char start) (point-at-eol)))
                (setq ovnew (make-overlay start end))
                (overlay-put ovnew 'face (format "outline-toc-semantic-%s-face"
                                                 (symbol-name class)))
                (overlay-put ovnew 'display (concat "  " name "  "))
                (overlay-put ovnew 'priority 7)
                (overlay-put ovnew 'outline-toc-semantic t)


                )))
          (setq tags (cdr tags)))))))

(provide 'outline-toc)

;;; outline-toc.el ends here
