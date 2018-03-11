;;; -*- lexical-binding: t -*-

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

It should be a font-spec object.

For example, (font-spec :size 10)"
  :type 'font-spec
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

(defun mcon-switch ()
  "Open mission control and select a buffer."
  (interactive)
  (let* ((row-window-list ())
         (colomn-window-list ())
         (all-window-list ())
         (buffer-list (mcon--construct-buffer-list mcon-black-list-regexp))
         (frame-height (frame-parameter nil 'height))
         (frame-width (frame-parameter nil 'width))
         )
    
    (make-frame `((height . ,frame-height) (width . ,frame-width)))
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
                              '((c-tab--setup-next-binding))))
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

(defgroup c-tab nil
  "Mission-control-like switch buffer."
  :group 'convenience)

(defcustom c-tab-timeout 1
  "Idle time before switch to selected buffer."
  :type 'number
  :group 'c-tab)

(defcustom c-tab-max-buffer 5
  "Maximum number of buffers that c-tab will show."
  :type 'number
  :group 'c-tab)

(defcustom c-tab-height-ratio 0.3
  "Ratio of height of preview frame to original frame."
  :type 'number
  :group 'c-tab)

(defcustom c-tab-number-face '(:height 200)
  "Face of numbers on the modeline on each window."
  :type 'list
  :group 'c-tab)

(defcustom c-tab-thumbnail-font (font-spec :size 10)
  "Font of each preview window.

It should be a font-spec object.

For example, (font-spec :size 10)"
  :type 'font-spec
  :group 'c-tab)

(defcustom c-tab-key-list '("<C-tab>" "<C-backtab>")
  "Key sequence to invoke c-tab functions.

e.g. \"<C-tab>\""
  :type 'list
  :group 'c-tab)

(defvar c-tab--window-list ()
  "A list of window to be selected by `c-tab-next'.")

(defvar c-tab--buffer-list ()
  "A list of buffers to be selected by `c-tab-next'.")

(defvar c-tab--selected-window 1
  "Current selected window.
Counts form 1 instead of 0.")

(defvar c-tab--buffer-count 0
  "Number of buffers to select from.")

(defvar c-tab-mode-map (make-sparse-keymap)
  "Key map for c-tab.")

(defvar c-tab--inhibit-message-old-value nil
  "Original value of `inhibit-message'.")

(defun mcon--construct-buffer-list (black-list-regexp)
  "Get a list of buffers that doesn't match in BLACK-LIST-REGEXP.

Return a list of buffers."
  (let ((black-list-regexp (string-join black-list-regexp "\\|"))
        (buffer-list ()))
    (dolist (index (number-sequence 1 c-tab-max-buffer))
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

(defun mcon--format-temp-buffer (row colomn window-list buffer-list thumbnail-face number-face &optional extra-form)
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
          (set-frame-font thumbnail-face t nil)
          (setq inhibit-message t)
          (dolist (form extra-form)
            (eval form))
          )))
    temp-buffer-list))

(defun c-tab-graphic ()
  "Switch between buffers like C-TAB in mac and windows."
  (interactive)
  (setq c-tab--inhibit-message-old-value inhibit-message)
  (setq c-tab--selected-window 1)
  (let* ((frame-height (truncate (* (frame-parameter nil 'height) c-tab-height-ratio)))
         (frame-width (frame-parameter nil 'width))
         (frame-top (if window-system
                        (+ (truncate (* (frame-pixel-height) (- 1 c-tab-height-ratio) 0.5)) (frame-parameter nil 'top))
                      0))
         (buffer-list (mcon--construct-buffer-list mcon-black-list-regexp)))
    
    (make-frame `((height . ,frame-height) (width . ,frame-width) (top . ,frame-top)))
    (mcon--cleanup-gui)
    
    ;; prepare window and buffers
    (let* ((buffer-list (reverse buffer-list))
           (buffer-count (length buffer-list))
           (width (/ frame-width buffer-count))
           (window-list (mcon--make-window 1 buffer-count width frame-height)))
      
      (setq c-tab--buffer-list buffer-list)
      (setq c-tab--buffer-count buffer-count)
      (setq c-tab--window-list window-list)
      
      ;; now all the windows are created
      ;; loop throught them again and
      ;; setup temp buffers in them
      (let ((temp-buffer-list (mcon--format-temp-buffer
                               1
                               buffer-count
                               window-list
                               buffer-list
                               c-tab-thumbnail-font
                               c-tab-number-face
                               '((c-tab--setup-next-binding)))))
        
        ;; select first window and highlight
        (setq c-tab--selected-window 0)
        (c-tab-next)
        
        ;; quit preview panel and select buffer in timeout
        (run-with-idle-timer c-tab-timeout nil
                             #'c-tab--cleanup
                             buffer-list temp-buffer-list)
        ))))

(defun c-tab-next ()
  "Selected next preview window in c-tab-mode."
  (interactive)
  (buffer-face-set 'default)
  (face-remap-add-relative 'font-lock-comment-face '(font-lock-comment-face))
  (setq c-tab--selected-window (1+ c-tab--selected-window))
  (when (> c-tab--selected-window c-tab--buffer-count)
    (setq c-tab--selected-window 1))
  (select-window (nth (1- c-tab--selected-window) c-tab--window-list))
  (buffer-face-set 'highlight)
  (face-remap-add-relative 'font-lock-comment-face '(highlight)))

(defun c-tab--cleanup (buffer-list temp-buffer-list)
  "Switch to selected buffer and clean up temp buffers, windows and frame."
  (mapc #'kill-buffer temp-buffer-list)
  (delete-frame)
  (switch-to-buffer (nth (1- c-tab--selected-window) buffer-list))
  (c-tab-setup-c-tab-binding)
  (setq inhibit-message c-tab--inhibit-message-old-value)
  )

(defun c-tab--setup-next-binding ()
  "Bind keys in `c-tab-key-list' to `c-tab-next'."
  (dolist (key c-tab-key-list)
    (if (featurep 'bind-key)
        (eval `(bind-key* ,key #'c-tab-next))
      (global-set-key (kbd key) #'c-tab-next))))

(defun c-tab-setup-c-tab-binding ()
  "Bind keys in `c-tab-key-list' to `c-tab-graphic'."
  (interactive)
  (dolist (key c-tab-key-list)
    (if (featurep 'bind-key)
        (eval `(bind-key* ,key #'c-tab-graphic))
      (global-set-key (kbd key) #'c-tab-graphic))))

(c-tab-setup-c-tab-binding)

(provide 'mission-control)
