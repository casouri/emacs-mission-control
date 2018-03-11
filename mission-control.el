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

(defun mcon-switch ()
  "Open mission control and select a buffer."
  (interactive)
  (let* ((row-window-list ())
         (colomn-window-list ())
         (all-window-list ())
         (buffer-list ())
         (window-count 0)
         (temp-buffer-list ())
         (frame-height (frame-parameter nil 'height))
         (frame-width (frame-parameter nil 'width))
         )
    
    (make-frame
     `((height . ,frame-height) (width . ,frame-width)))

    ;; clean up
    (when window-system
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
    (unless (eq window-system 'ns)
      (menu-bar-mode -1))
    
     ;;
     ;; Construct buffer list
    (let ((black-list-regexp (string-join mcon-black-list-regexp "\\|")))
      (dolist (index (number-sequence 1 mcon-max-buffer))
        ;; don't include special buffers
        (let ((buffer (nth (- index 1) (buffer-list))))
          (unless (string-match black-list-regexp (buffer-name buffer))
            (push buffer buffer-list)))))
    
    (let* ((shape (mcon-calculate-shape (length buffer-list)))
           (row (car shape))
           (colomn (nth 1 shape))
           (width (/ frame-width colomn))
           (height (/ frame-height row))
           )
      ;;
      ;; Create windows
      
      ;; add first window to list
      ;; other windows are added in loop
      (push (selected-window) row-window-list)
      ;; make each row
      (dolist (count (number-sequence 1 (- row 1)))
        (select-window (split-window-below height))

        (push (selected-window) row-window-list)
        )

      (setq row-window-list (reverse row-window-list))
      
      ;; make each colomn
      (dolist (row-window row-window-list)
        (select-window row-window)

        (setq window-count (+ window-count 1))
        (push (selected-window) all-window-list)
        
        (dolist (count (number-sequence 1 (- colomn 1)))
          (select-window (split-window-right width))

          (push (selected-window) colomn-window-list)
          (setq window-count (+ window-count 1))
          (push (selected-window) all-window-list)
          ))

      (setq all-window-list (reverse all-window-list))

      ;; now all windows are created
      ;; loop throught them again and
      ;; setup temp buffers in them
      (dolist (count (number-sequence 1 (* colomn row)))
        (let ((window (nth (- count 1) all-window-list)))
          (select-window window)

          ;; leftover windows are left blank
          (let* ((buffer (generate-new-buffer (format "tmp-%d" count)))
                 (from-buffer (nth (- count 1) buffer-list))
                 (mode (when from-buffer (buffer-local-value 'major-mode from-buffer)))
                 (name (if from-buffer (buffer-name from-buffer) "")))
            
            ;; copy some text from original buffer to temp buffer
            (switch-to-buffer buffer)
            (push buffer temp-buffer-list)
            (unless (> count (length buffer-list))
              (insert-buffer-substring from-buffer)
              (goto-char 1)
              )

            (when mode
              ;; (setq-local major-mode mode)
              (funcall mode))

            (setq-local mode-line-format (propertize (format "%d %s" count name) 'face mcon-number-face))
            (set-frame-font mcon-thumbnail-font t nil)
            )))

      (let* ((number-char-list '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
             (selected-window (string-to-number
                               (char-to-string
                                (read-char-choice
                                 (propertize "Select window: "
                                             'face mcon-prompt-face)
                                 number-char-list)))))
        (mapc #'kill-buffer temp-buffer-list)
        (delete-frame)
        (switch-to-buffer (nth (- selected-window 1) buffer-list))
        )
      )
    )
  )

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

(defcustom c-tab-key-list '("<C-tab>" "<backtab>")
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
         (buffer-list ()))
    

    (make-frame
     `((height . ,frame-height) (width . ,frame-width) (top . ,frame-top)))

    ;; clean up
    (when window-system
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
    (unless (eq window-system 'ns)
      (menu-bar-mode -1))

    ;;
    ;; Construct buffer list
    (let ((black-list-regexp (string-join mcon-black-list-regexp "\\|")))
      (dolist (index (number-sequence 1 c-tab-max-buffer))
        ;; don't include special buffers
        (let ((buffer (nth (- index 1) (buffer-list))))
          (unless (string-match black-list-regexp (buffer-name buffer))
            (push buffer buffer-list)))))
    
    ;; prepare window and buffers
    (let* ((buffer-list (reverse buffer-list))
           (buffer-count (length buffer-list))
           (width (/ frame-width buffer-count))
           (window-list ()))

      (setq c-tab--buffer-list buffer-list)
      (setq c-tab--buffer-count buffer-count)

      ;;
      ;; Create windows
      
      ;; add first window to list
      ;; other windows are added in loop
      (push (selected-window) window-list)
      ;; make each row
      (dolist (count (number-sequence 1 (- buffer-count 1)))
        (select-window (split-window-right width))
        (push (selected-window) window-list))

      ;; now all the windows are created
      ;; loop throught them again and
      ;; setup temp buffers in them
      (let ((window-list (reverse window-list))
            (temp-buffer-list ()))

        (setq c-tab--window-list window-list)

        (dolist (count (number-sequence 1 buffer-count))
          (let ((window (nth (- count 1) window-list)))
            (select-window window)

            (let* ((temp-buffer (generate-new-buffer (format "tmp-%d" count)))
                   (from-buffer (nth (- count 1) buffer-list))
                   (mode (when from-buffer (buffer-local-value 'major-mode from-buffer)))
                   (name (if from-buffer (buffer-name from-buffer) "")))
              
              ;; copy some text from original buffer to temp buffer
              (switch-to-buffer temp-buffer)
              (push temp-buffer temp-buffer-list)
              (insert-buffer-substring from-buffer)
              (goto-char 1)
                

              ;; format
              (when mode (funcall mode))
              (setq-local mode-line-format (propertize (format "%d %s" count name) 'face c-tab-number-face))
              (set-frame-font c-tab-thumbnail-font t nil)
              (setq inhibit-message t)
              (dolist (key c-tab-key-list)
                (if (featurep 'bind-key)
                    (eval `(bind-key* ,key #'c-tab-next))
                  (global-set-key (kbd key) #'c-tab-next))
                ))))
          
        ;; select first window and highlight
        (select-window (car c-tab--window-list))
        (buffer-face-set 'highlight)
        
        (run-with-idle-timer c-tab-timeout nil
                             #'c-tab--cleanup
                             buffer-list temp-buffer-list)
        ))))

(defun c-tab-next ()
  "Selected next preview window in c-tab-mode."
  (interactive)
  (buffer-face-set 'default)
  (setq c-tab--selected-window (1+ c-tab--selected-window))
  (when (> c-tab--selected-window c-tab--buffer-count)
    (setq c-tab--selected-window 1))
  (select-window (nth (1- c-tab--selected-window) c-tab--window-list))
  (buffer-face-set 'highlight))

(defun c-tab--cleanup (buffer-list temp-buffer-list)
  "Switch to selected buffer and clean up temp buffers, windows and frame."
  (mapc #'kill-buffer temp-buffer-list)
  (delete-frame)
  (switch-to-buffer (nth (1- c-tab--selected-window) buffer-list))
  (dolist (key c-tab-key-list)
    (if (featurep 'bind-key)
        (eval `(bind-key* ,key #'c-tab-graphic))
      (global-set-key (kbd key) #'c-tab-graphic)))
  (setq inhibit-message c-tab--inhibit-message-old-value)
  )

(dolist (key c-tab-key-list)
  (if (featurep 'bind-key)
      (eval `(bind-key* ,key #'c-tab-graphic))
    (global-set-key (kbd key) #'c-tab-graphic)))

(provide 'mission-control)
