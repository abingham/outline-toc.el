;;; outline-toc.el --- Sidebar showing a "table of contents".

;; Copyright (C) 2017 Austin Bingham

;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Keywords: convenience outlines
;; URL: https://github.com/abingham/outline-toc.el
;; Version: 0.1

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
  "A outline-toc sidebar."
  :group 'convenience)

(defface outline-toc-font-face nil
  "Face used for text in outline-toc buffer, notably the font family and height.
This height should be really small.  You probably want to use a
TrueType font for this.  After changing this, you should
recreate the outline-toc to avoid problems with recentering."
  :group 'outline-toc)

(defface outline-toc-current-section
  '((t (:inherit highlight)))
  "Face for the current line in the TOC."
  :group 'outline-toc)

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
         (when (and (boundp 'outline-toc--timer-object)
                    outline-toc--timer-object)
           (cancel-timer outline-toc--timer-object)
           (setq outline-toc--timer-object
                 (run-with-idle-timer
                  outline-toc-update-delay t 'outline-toc--update))))
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

;; TODO: How do we specify "for all outline-mode" docs? Outline-mode is minor, I think...
(defcustom outline-toc-major-modes '(markdown-mode org-mode outline-mode rst-mode)
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

(defcustom outline-toc-highlight-line t
  "Whether the outline-toc should highlight the current line."
  :type 'boolean
  :group 'outline-toc)

;;; Internal variables

(defvar outline-toc--active-buffer nil
  "The buffer currently displayed in the outline-toc.")

;; Window start/end from the base buffer
(defvar outline-toc--start nil)
(defvar outline-toc--end nil)

;; General overlay for the outline-toc
(defvar outline-toc--base-overlay nil)

;; Timer
(defvar outline-toc--timer-object nil)

;; Lines the outline-toc can display
(defvar outline-toc--numlines nil)

;; Line overlay
(defvar outline-toc--line-overlay nil)


;;; Helpers

(defun outline-toc-active-current-buffer-p ()
  "Whether the current buffer is displayed in the outline-toc."
  (and (eq (current-buffer) outline-toc--active-buffer)
       (get-buffer outline-toc-buffer-name)
       (with-current-buffer outline-toc-buffer-name
         (eq outline-toc--active-buffer (buffer-base-buffer)))))

(defsubst outline-toc--get-window ()
  "Get current outline-toc window."
  (when (get-buffer outline-toc-buffer-name)
    (get-buffer-window outline-toc-buffer-name)))

(defsubst outline-toc-kill-buffer ()
  "Kill the outline-toc buffer."
  (when (get-buffer outline-toc-buffer-name)
    (kill-buffer outline-toc-buffer-name)))

(defun outline-toc-create-window ()
  "Create TOC sidebare window."
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

;;; Outline-Toc creation / killing

;;;###autoload
(define-minor-mode outline-toc-mode
  "Toggle outline-toc mode."
  :global t
  :group 'outline-toc
  :lighter " OToc"
  (if outline-toc-mode
      (progn
        (when (and outline-toc-major-modes
                   (apply 'derived-mode-p outline-toc-major-modes))
          (unless (outline-toc--get-window)
            (outline-toc-create-window))
          ;; Create outline-toc.
          (outline-toc-new-outline-toc))
        ;; Create timer.
        (setq outline-toc--timer-object
              (run-with-idle-timer outline-toc-update-delay t 'outline-toc--update)))
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
        (maj-mode major-mode)
        (win (outline-toc--get-window))
        (indbuf (make-indirect-buffer (current-buffer)
                                      (concat outline-toc-buffer-name "_temp")))
        (edges (window-pixel-edges)))

    ;; Remember the active buffer currently displayed in the outline-toc.
    (setq outline-toc--active-buffer (current-buffer))

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
      (setq outline-toc--base-overlay (make-overlay (point-min) (point-max) nil t t))
      (overlay-put outline-toc--base-overlay 'face 'outline-toc-font-face)
      (overlay-put outline-toc--base-overlay 'priority 1)

      ;; (outline-toc-sb-mode 1)

      ;; (when (and (boundp 'linum-mode)
      ;;            linum-mode)
      ;;   (linum-mode 0))

      (funcall maj-mode)
      (outline-hide-body)
      (setq buffer-read-only t)

      ;; Calculate the actual number of lines displayable with the outline-toc face.
      (setq outline-toc--numlines
            (floor
             (/
              (- (nth 3 edges) (nth 1 edges))
              (car (progn (redisplay t) (window-line-height)))))))

    ;; (outline-toc-sync-overlays)
    ))

(defun outline-toc-kill ()
  "Kill outline-toc."
  (interactive)
  (when (outline-toc--get-window)
    (delete-window (outline-toc--get-window)))
  (cancel-timer outline-toc--timer-object))

;;; Outline-Toc update

(defun outline-toc--update (&optional force)
  "Update outline-toc sidebar if necessary.
This is meant to be called from the idle-timer or the post command hook.
When FORCE, enforce update of the active region."
  (interactive)
  ;; If we are in the minibuffer, do nothing.
  (unless (active-minibuffer-window)
    (when (outline-toc-active-current-buffer-p)
        ;; Recreate toc window if necessary
        (when (null (outline-toc--get-window))
          (outline-toc-create-window))

        ;; Update our position in the TOC window
        (let ((win (outline-toc--get-window))
              (pt (point)))
          (with-selected-window win
            (outline-show-all)
            (goto-char pt)
            (outline-previous-heading)
            (outline-hide-body)
            (recenter)

            (unless outline-toc--line-overlay
              (setq outline-toc--line-overlay (make-overlay (point) (1+ (point)) nil t))
              (overlay-put outline-toc--line-overlay 'face 'outline-toc-current-section)
              (overlay-put outline-toc--line-overlay 'priority 6))
            (move-overlay outline-toc--line-overlay (point) (line-beginning-position 2)))))))

(defun outline-toc--line-to-pos (line)
  "Return point position of line number LINE."
  (save-excursion
    (goto-char 1)
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))
    (point)))

;;; Outline-Toc minor mode

(defvar outline-toc-sb-mode-map (make-sparse-keymap)
  "Keymap used by function `outline-toc-sb-mode'.")

(define-minor-mode outline-toc-sb-mode
  "Minor mode for outline-toc sidebar."
  nil "outline-toc" outline-toc-sb-mode-map)

(provide 'outline-toc)

;;; outline-toc.el ends here
