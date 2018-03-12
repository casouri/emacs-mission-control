;;; -*- lexical-binding: t -*-

(defgroup mission-control nil
  "Mission-control-like switch buffer."
  :group 'convenience)


(defcustom mission-control-black-list-regexp '("\*.+\*" "magit")
  "Buffers that match regexps inside this list are note included in mission control."
  :type 'list
  :group 'mission-control)

(defcustom mission-control-max-buffer 10
  "Maximum number of buffers to show."
  :type 'number
  :group 'mission-control)

(defcustom mission-control-number-face '(:height 300)
  "Face of numbers on the modeline on each window."
  :type 'list
  :group 'mission-control)

(defcustom mission-control-prompt-face '(:height 200)
  "Face of numbers on the modeline on each window."
  :type 'list
  :group 'mission-control)

(defcustom mission-control-thumbnail-font (font-spec :size 10)
  "Font of each preview window.

It should be a font-spec object.

For example, (font-spec :size 10)"
  :type 'font-spec
  :group 'mission-control)

(defun mission-control-calculate-shape (length)
  "Calculate a 2D shape base on LENGTH."
  (let* ((row (truncate (sqrt length)))
         (colomn (ceiling (/ (float length) row))))
    `(,row ,colomn)
    ))

(defun mission-control--cleanup-gui ()
  "Cleanup GUI elements in new frame."
  (when window-system
    (tool-bar-mode -1)
    (scroll-bar-mode -1))
  (unless (eq window-system 'ns)
    (menu-bar-mode -1)))

(defun mission-control--construct-buffer-list (black-list-regexp)
  "Get a list of buffers that doesn't match in BLACK-LIST-REGEXP.

Return a list of buffers."
  (let ((black-list-regexp (string-join black-list-regexp "\\|"))
        (buffer-list ()))
    (dolist (index (number-sequence 1 mission-control-c-tab-max-buffer))
      ;; don't include special buffers
      (let ((buffer (nth (- index 1) (buffer-list))))
        (unless (string-match black-list-regexp (buffer-name buffer))
          (push buffer buffer-list))))
    buffer-list))

(defun mission-control--make-window (row colomn width height)
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

(defun mission-control--format-temp-buffer (row colomn window-list buffer-list thumbnail-font number-face &optional extra-form)
  "Format each buffer in preview windows.

ROW & COLOMN are same with window shape.

EXTRA-FORM is a list of extra forms to be evaluated in each buffer."
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

(defun mission-control-switch ()
  "Open mission control and select a buffer."
  (interactive)
  (let* ((row-window-list    ())
         (colomn-window-list ())
         (all-window-list    ())
         (buffer-list        (mission-control--construct-buffer-list mission-control-black-list-regexp))
         (frame-height       (frame-parameter nil 'height))
         (frame-width        (frame-parameter nil 'width)))
    
    (make-frame `((height . ,frame-height) (width . ,frame-width)))
    (mission-control--cleanup-gui)
    
    (let* ((shape (mission-control-calculate-shape (length buffer-list)))
           (row (car shape))
           (colomn (nth 1 shape))
           (width (/ frame-width colomn))
           (height (/ frame-height row))
           (all-window-list (mission-control--make-window row colomn width height))
           (temp-buffer-list (mission-control--format-temp-buffer
                              row
                              colomn
                              all-window-list
                              buffer-list
                              mission-control-thumbnail-font
                              mission-control-number-face
                              '((mission-control-c-tab--setup-next-binding))))
           (number-char-list '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
           (selected-window (string-to-number
                             (char-to-string
                              (read-char-choice
                               (propertize "Select window: "
                                           'face mission-control-prompt-face)
                               number-char-list)))))
      
      (mapc #'kill-buffer temp-buffer-list)
      (delete-frame)
      (switch-to-buffer (nth (- selected-window 1) buffer-list)))))

(defgroup mission-control-c-tab nil
  "Mission-control-like switch buffer."
  :group 'convenience)

(defcustom mission-control-c-tab-timeout 1
  "Idle time before switch to selected buffer."
  :type 'number
  :group 'mission-control-c-tab)

(defcustom mission-control-c-tab-max-buffer 5
  "Maximum number of buffers that mission-control-c-tab will show."
  :type 'number
  :group 'mission-control-c-tab)

(defcustom mission-control-c-tab-height-ratio 0.3
  "Ratio of height of preview frame to original frame."
  :type 'number
  :group 'mission-control-c-tab)

(defcustom mission-control-c-tab-number-face '((:height 200))
  "Face of numbers on the modeline on each window."
  :group 'mission-control-c-tab)

(defcustom mission-control-c-tab-thumbnail-font (font-spec :size 10)
  "Font of each preview window.

It should be a font-spec object.

For example, (font-spec :size 10)"
  :type 'font-spec
  :group 'mission-control-c-tab)

(defcustom mission-control-c-tab-initial-selection-offset 0
  "0 means select original buffer when preview pane pops up.

1 means select next buffer,etc.")

(defcustom mission-control-c-tab-key-list '("<mission-control-c-tab>" "<C-backtab>")
  "Key sequence to invoke mission-control-c-tab functions.

e.g. \"<mission-control-c-tab>\""
  :type 'list
  :group 'mission-control-c-tab)

(defvar mission-control-c-tab--window-list ()
  "A list of window to be selected by `mission-control-c-tab-next'.")

(defvar mission-control-c-tab--buffer-list ()
  "A list of buffers to be selected by `mission-control-c-tab-next'.")

(defvar mission-control-c-tab--selected-window 1
  "Current selected window.
Counts form 1 instead of 0.")

(defvar mission-control-c-tab--buffer-count 0
  "Number of buffers to select from.")

(defvar mission-control-c-tab-mode-map (make-sparse-keymap)
  "Key map for mission-control-c-tab.")

(defvar mission-control-c-tab--inhibit-message-old-value nil
  "Original value of `inhibit-message'.")

(defvar mission-control-c-tab-face-to-override '(default
                                  font-lock-comment-face
                                  font-lock-warning-face
                                  hl-line)
  "Faces to be highlighted when mission-control-c-tab highlight a buffer.")

(defvar mission-control-c-tab--face-remap-list ()
  "A list of remaps returned by `face-remap-add-relative'.

`mission-control-c-tab--unhignlight' use them to unhignlight.")

(defun mission-control-c-tab-graphic ()
  "Switch between buffers like MISSION-CONTROL-C-TAB in mac and windows."
  (interactive)
  (setq mission-control-c-tab--inhibit-message-old-value inhibit-message)
  (setq mission-control-c-tab--selected-window 1)
  (let* ((frame-height (truncate (* (frame-parameter nil 'height) mission-control-c-tab-height-ratio)))
         (frame-width  (frame-parameter nil 'width))
         (frame-top    (if window-system
                           (+ (truncate (* (frame-pixel-height) (- 1 mission-control-c-tab-height-ratio) 0.5)) (frame-parameter nil 'top))
                         0))
         (buffer-list  (mission-control--construct-buffer-list mission-control-black-list-regexp)))
    
    (make-frame `((height . ,frame-height) (width . ,frame-width) (top . ,frame-top)))
    (mission-control--cleanup-gui)
    
    ;; prepare window and buffers
    (let* ((buffer-list  (reverse buffer-list))
           (buffer-count (length buffer-list))
           (width        (/ frame-width buffer-count))
           (window-list  (mission-control--make-window 1 buffer-count width frame-height)))
      
      (setq mission-control-c-tab--buffer-list buffer-list)
      (setq mission-control-c-tab--buffer-count buffer-count)
      (setq mission-control-c-tab--window-list window-list)
      
      ;; now all the windows are created
      ;; loop throught them again and
      ;; setup temp buffers in them
      (let ((temp-buffer-list (mission-control--format-temp-buffer
                               1
                               buffer-count
                               window-list
                               buffer-list
                               mission-control-c-tab-thumbnail-font
                               mission-control-c-tab-number-face
                               '((mission-control-c-tab--setup-next-binding)))))
        
        ;; select first window and highlight
        (setq mission-control-c-tab--selected-window mission-control-c-tab-initial-selection-offset)
        (mission-control-c-tab-next)
        
        ;; quit preview panel and select buffer in timeout
        (run-with-idle-timer mission-control-c-tab-timeout nil
                             #'mission-control-c-tab--cleanup
                             buffer-list temp-buffer-list)
        ))))

(defun mission-control-c-tab-next ()
  "Selected next preview window in mission-control-c-tab-mode."
  (interactive)
  (mission-control-c-tab--unhignlight)
  (setq mission-control-c-tab--selected-window (1+ mission-control-c-tab--selected-window))
  (when (> mission-control-c-tab--selected-window mission-control-c-tab--buffer-count)
    (setq mission-control-c-tab--selected-window 1))
  (select-window (nth (1- mission-control-c-tab--selected-window) mission-control-c-tab--window-list))
  (mission-control-c-tab--highlight))

(defun mission-control-c-tab--highlight ()
  "Highlight current buffer"
  (dolist (face mission-control-c-tab-face-to-override)
    (push (face-remap-add-relative face '(highlight))
          mission-control-c-tab--face-remap-list)))

(defun mission-control-c-tab--unhignlight ()
  "Set buffer face back."
  (dolist (remap mission-control-c-tab--face-remap-list)
    (face-remap-remove-relative remap)))

(defun mission-control-c-tab--cleanup (buffer-list temp-buffer-list)
  "Switch to selected buffer and clean up temp buffers, windows and frame."
  (mapc #'kill-buffer temp-buffer-list)
  (delete-frame)
  (switch-to-buffer (nth (1- mission-control-c-tab--selected-window) buffer-list))
  (mission-control-c-tab-setup-mission-control-c-tab-binding)
  (setq inhibit-message mission-control-c-tab--inhibit-message-old-value)
  )

(defun mission-control-c-tab--setup-next-binding ()
  "Bind keys in `mission-control-c-tab-key-list' to `mission-control-c-tab-next'."
  (dolist (key mission-control-c-tab-key-list)
    (if (featurep 'bind-key)
        (eval `(bind-key* ,key #'mission-control-c-tab-next))
      (global-set-key (kbd key) #'mission-control-c-tab-next))))

(defun mission-control-c-tab-setup-mission-control-c-tab-binding ()
  "Bind keys in `mission-control-c-tab-key-list' to `mission-control-c-tab-graphic'."
  (interactive)
  (dolist (key mission-control-c-tab-key-list)
    (if (featurep 'bind-key)
        (eval `(bind-key* ,key #'mission-control-c-tab-graphic))
      (global-set-key (kbd key) #'mission-control-c-tab-graphic))))

;; (mission-control-c-tab-setup-mission-control-c-tab-binding)

(provide 'mission-control)
