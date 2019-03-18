;;; mission-control.el --- Mission-control like buffer switching  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuan Fu.

;; Author: Yuan Fu <casouri@gmail.com>
;; Keywords: convenience
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; Mission-control provides you with a way to switch between buffers like
;; switching windows in macOS mission control.

;; It also provides a C-TAB feature to switch between buffers like the command tab
;; in macOS.

;; To use mission-control: simply invoke `mcon-switch'.

;; To use mission-control-c-tab: configure `mcon-c-tab-key-list'
;; and invoke `mcon-c-tab-setup-binding' to bind keys.
;; Then use that keybinding to switch between buffers.

;;; Code:

(defgroup mission-control nil
  "Mission-control-like switch buffer."
  :group 'convenience)


(defcustom mcon-black-list-regexp '("\*.+\*" "magit")
  "Buffers that match regexps inside this list are note included in mission control."
  :type 'list
  :group 'mission-control)

(defcustom mcon-max-buffer 10
  "Maximum number of buffers to show."
  :type 'number
  :group 'mission-control)

(defcustom mcon-number-face '(:height 300)
  "Face of numbers on the modeline on each window."
  :type 'list
  :group 'mission-control)

(defcustom mcon-prompt-face '(:height 200)
  "Face of numbers on the modeline on each window."
  :type 'list
  :group 'mission-control)

(defcustom mcon-thumbnail-font (font-spec :size 10)
  "Font of each preview window.

It should be a ‘font-spec’ object.

For example, (font-spec :size 10)"
  :type 'font-spec
  :group 'mission-control)

(defcustom mcon-frame-name ""
  "Title format for mission-control preview frame.

If you use chunkwm, set it to \"mcon\" and set

chunkc tiling::rule --owner Emacs --except \"mcon.*\" --state tile"
  :type 'string
  :group 'mission-control)

(defun mcon-calculate-shape (length)
  "Calculate a 2D shape base on LENGTH."
  (let* ((row (truncate (sqrt length)))
         (colomn (ceiling (/ (float length) row))))
    `(,row ,colomn)
    ))

(defun mcon--cleanup-gui ()
  "Cleanup GUI elements in new frame."
  (when window-system
    (tool-bar-mode -1)
    (scroll-bar-mode -1))
  (unless (eq window-system 'ns)
    (menu-bar-mode -1)))

(defun mcon--construct-buffer-list (black-list-regexp)
  "Get a list of buffers that doesn't match in BLACK-LIST-REGEXP.

Return a list of buffers."
  (let ((black-list-regexp (string-join black-list-regexp "\\|"))
        (buffer-list ()))
    (dolist (index (number-sequence 1 mcon-c-tab-max-buffer))
      ;; don't include special buffers
      (let ((buffer (nth (- index 1) (buffer-list))))
        (unless (string-match black-list-regexp (buffer-name buffer))
          (push buffer buffer-list))))
    buffer-list))

(defun mcon--make-window (row colomn width height)
  "Create ROW x COLOMN windows in current frame.

Each window is WIDTH x HEIGHT."
  
  (let ((row-window-list ())
        (all-window-list ()))
   ;; add first window to list
   ;; other windows are added in loop
   (push (selected-window) row-window-list)
   ;; make each row
   (dolist (count (number-sequence 1 (- row 1)))
     (select-window (split-window-below height))
     (push (selected-window) row-window-list))

   (let ((row-window-list (reverse row-window-list)))
     ;; make each colomn
     (dolist (row-window row-window-list)
       (select-window row-window)
       (push (selected-window) all-window-list)
       (dolist (count (number-sequence 1 (- colomn 1)))
         (select-window (split-window-right width))
         (push (selected-window) all-window-list)))
     (reverse all-window-list))))

(defun mcon--format-temp-buffer (row colomn window-list buffer-list thumbnail-font number-face &optional extra-form)
  "Format each buffer in preview windows.

ROW & COLOMN are same with window shape.

EXTRA-FORM is a list of extra forms to be evaluated in each buffer.
Argument WINDOW-LIST a list of preview windows.
Argument BUFFER-LIST a list of buffers in Emacs buffer list.
Argument THUMBNAIL-FONT font of each preview window.
Argument NUMBER-FACE face of number and buffer name in modeline of each preview window."
  ;; now all windows are created
  ;; loop throught them again and
  ;; setup temp buffers in them
  (let ((temp-buffer-list ()))
    (dolist (count (number-sequence 1 (* colomn row)))
      (let ((window (nth (- count 1) window-list)))
        (select-window window)
        ;; leftover windows are left blank
        (let* ((buffer (generate-new-buffer (format "tmp-%d" count)))
               (from-buffer (nth (- count 1) buffer-list))
               (mode (when from-buffer (buffer-local-value 'major-mode from-buffer)))
               (name (if from-buffer (buffer-name from-buffer) "")))
          ;; now do all the formating in each buffer
          
          ;; copy some text from original buffer to temp buffer
          (switch-to-buffer buffer)
          (push buffer temp-buffer-list)
          (unless (> count (length buffer-list))
            (insert-buffer-substring from-buffer)
            (goto-char 1))

          ;; format
          ;; syntax highlight
          (when mode (funcall mode))
          (setq-local mode-line-format (propertize (format "%d %s" count name) 'face number-face))
          (set-frame-font thumbnail-font t nil)
          (setq inhibit-message t)
          (when (featurep 'linum)(linum-mode -1))
          (when (featurep 'nlinum)(nlinum-mode -1))
          (dolist (form extra-form)
            (eval form))
          )))
    temp-buffer-list))

(defun mcon-switch ()
  "Open mission control and select a buffer."
  (interactive)
  (let* ((row-window-list    ())
         (colomn-window-list ())
         (all-window-list    ())
         (buffer-list        (mcon--construct-buffer-list mcon-black-list-regexp))
         (frame-height       (frame-parameter nil 'height))
         (frame-width        (frame-parameter nil 'width)))
    
    (make-frame `((height . ,frame-height) (width . ,frame-width) (name . ,mcon-frame-name)))
    (mcon--cleanup-gui)
    
    (let* ((shape (mcon-calculate-shape (length buffer-list)))
           (row (car shape))
           (colomn (nth 1 shape))
           (width (/ frame-width colomn))
           (height (/ frame-height row))
           (all-window-list (mcon--make-window row colomn width height))
           (temp-buffer-list (mcon--format-temp-buffer
                              row
                              colomn
                              all-window-list
                              buffer-list
                              mcon-thumbnail-font
                              mcon-number-face
                              '((mcon-c-tab--setup-next-binding))))
           (number-char-list '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
           (selected-window (string-to-number
                             (char-to-string
                              (read-char-choice
                               (propertize "Select window: "
                                           'face mcon-prompt-face)
                               number-char-list)))))
      
      (mapc #'kill-buffer temp-buffer-list)
      (delete-frame)
      (switch-to-buffer (nth (- selected-window 1) buffer-list)))))

(defgroup mcon-c-tab nil
  "Mission-control-like switch buffer."
  :group 'convenience)

(defcustom mcon-c-tab-timeout 1
  "Idle time before switch to selected buffer."
  :type 'number
  :group 'mcon-c-tab)

(defcustom mcon-c-tab-max-buffer 5
  "Maximum number of buffers that mcon-c-tab will show."
  :type 'number
  :group 'mcon-c-tab)

(defcustom mcon-c-tab-height-ratio 0.3
  "Ratio of height of preview frame to original frame."
  :type 'number
  :group 'mcon-c-tab)

(defcustom mcon-c-tab-number-face '((:height 200))
  "Face of numbers on the modeline on each window."
  :group 'mcon-c-tab)

(defcustom mcon-c-tab-thumbnail-font (font-spec :size 10)
  "Font of each preview window.

It should be a ‘font-spec’ object.

For example, (font-spec :size 10)"
  :type 'font-spec
  :group 'mcon-c-tab)

(defcustom mcon-c-tab-initial-selection-offset 0
  "0 means select original buffer when preview pane pops up.

1 means select next buffer,etc.")

(defcustom mcon-c-tab-key-list '("<C-tab>" "<C-backtab>")
  "Key sequence to invoke mcon-c-tab functions.

e.g. \"<mcon-c-tab>\""
  :type 'list
  :group 'mcon-c-tab)

(defvar mcon-c-tab--window-list ()
  "A list of window to be selected by `mcon-c-tab-next'.")

(defvar mcon-c-tab--buffer-list ()
  "A list of buffers to be selected by `mcon-c-tab-next'.")

(defvar mcon-c-tab--selected-window 1
  "Current selected window.
Counts form 1 instead of 0.")

(defvar mcon-c-tab--buffer-count 0
  "Number of buffers to select from.")

(defvar mcon-c-tab-mode-map (make-sparse-keymap)
  "Key map for mcon-c-tab.")

(defvar mcon-c-tab--inhibit-message-old-value nil
  "Original value of `inhibit-message'.")

(defvar mcon-c-tab-face-to-override '(default
                                  font-lock-comment-face
                                  font-lock-warning-face
                                  hl-line)
  "Faces to be highlighted when mcon-c-tab highlight a buffer.")

(defvar mcon-c-tab--face-remap-list ()
  "A list of remaps returned by `face-remap-add-relative'.

`mcon-c-tab--unhignlight' use them to unhignlight.")

(defun mcon-c-tab-graphic ()
  "Switch between buffers like MCON-C-TAB in mac and windows."
  (interactive)
  (setq mcon-c-tab--inhibit-message-old-value inhibit-message)
  (setq mcon-c-tab--selected-window 1)
  (let* ((frame-height (truncate (* (frame-parameter nil 'height) mcon-c-tab-height-ratio)))
         (frame-width  (frame-parameter nil 'width))
         (frame-top    (if window-system
                           (+ (truncate (* (frame-pixel-height) (- 1 mcon-c-tab-height-ratio) 0.5)) (frame-parameter nil 'top))
                         0))
         (buffer-list  (mcon--construct-buffer-list mcon-black-list-regexp)))
    
    (make-frame `((height . ,frame-height) (width . ,frame-width) (top . ,frame-top) (name . ,mcon-frame-name)))
    (mcon--cleanup-gui)
    
    ;; prepare window and buffers
    (let* ((buffer-list  (reverse buffer-list))
           (buffer-count (length buffer-list))
           (width        (/ frame-width buffer-count))
           (window-list  (mcon--make-window 1 buffer-count width frame-height)))
      
      (setq mcon-c-tab--buffer-list buffer-list)
      (setq mcon-c-tab--buffer-count buffer-count)
      (setq mcon-c-tab--window-list window-list)
      
      ;; now all the windows are created
      ;; loop throught them again and
      ;; setup temp buffers in them
      (let ((temp-buffer-list (mcon--format-temp-buffer
                               1
                               buffer-count
                               window-list
                               buffer-list
                               mcon-c-tab-thumbnail-font
                               mcon-c-tab-number-face
                               '((mcon-c-tab--setup-next-binding)))))
        
        ;; select first window and highlight
        (setq mcon-c-tab--selected-window mcon-c-tab-initial-selection-offset)
        (mcon-c-tab-next)
        
        ;; quit preview panel and select buffer in timeout
        (run-with-idle-timer mcon-c-tab-timeout nil
                             #'mcon-c-tab--cleanup
                             buffer-list temp-buffer-list)
        ))))

(defun mcon-c-tab-next ()
  "Selected next preview window in mcon-c-tab-mode."
  (interactive)
  (mcon-c-tab--unhignlight)
  (setq mcon-c-tab--selected-window (1+ mcon-c-tab--selected-window))
  (when (> mcon-c-tab--selected-window mcon-c-tab--buffer-count)
    (setq mcon-c-tab--selected-window 1))
  (select-window (nth (1- mcon-c-tab--selected-window) mcon-c-tab--window-list))
  (mcon-c-tab--highlight))

(defun mcon-c-tab--highlight ()
  "Highlight current buffer."
  (dolist (face mcon-c-tab-face-to-override)
    (push (face-remap-add-relative face '(highlight))
          mcon-c-tab--face-remap-list)))

(defun mcon-c-tab--unhignlight ()
  "Set buffer face back."
  (dolist (remap mcon-c-tab--face-remap-list)
    (face-remap-remove-relative remap)))

(defun mcon-c-tab--cleanup (buffer-list temp-buffer-list)
  "Switch to selected buffer and clean up temp buffers, windows and frame.
Argument BUFFER-LIST a list of buffers in Emacs buffer list.
Argument TEMP-BUFFER-LIST temperary buffers correspond to each preview window."

  (mapc #'kill-buffer temp-buffer-list)
  (delete-frame)
  (switch-to-buffer (nth (1- mcon-c-tab--selected-window) buffer-list))
  (mcon-c-tab-setup-binding)
  (setq inhibit-message mcon-c-tab--inhibit-message-old-value)
  (setq mcon-c-tab--buffer-count 0)
  (setq mcon-c-tab--buffer-list ())
  (setq mcon-c-tab--window-list ())
  (setq mcon-c-tab--selected-window 1)
  )

(defun mcon-c-tab--setup-next-binding ()
  "Bind keys in `mcon-c-tab-key-list' to `mcon-c-tab-next'."
  (dolist (key mcon-c-tab-key-list)
    (if (featurep 'bind-key)
        (eval `(bind-key* ,key #'mcon-c-tab-next))
      (global-set-key (kbd key) #'mcon-c-tab-next))))

(defun mcon-c-tab-setup-binding ()
  "Bind keys in `mcon-c-tab-key-list' to `mcon-c-tab-graphic'."
  (interactive)
  (dolist (key mcon-c-tab-key-list)
    (if (featurep 'bind-key)
        (eval `(bind-key* ,key #'mcon-c-tab-graphic))
      (global-set-key (kbd key) #'mcon-c-tab-graphic))))

;; (mcon-c-tab-setup-binding)

(provide 'mission-control)

;;; mission-control.el ends here
