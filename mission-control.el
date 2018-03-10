(defgroup mission-control nil
  "Mission-control-like switch buffer."
  :group 'convenience)


(defcustom mcon-black-list-regexp '("\*.+\*" "magit")
  "Buffers that match regexps inside this list are note included in mission control."
  :type 'list
  :group 'mission-control)

(defcustom mcon-number-face '(:height 300)
  "Face of numbers on the modeline on each window."
  :type 'list
  :group 'mission-control )

(defcustom mcon-thumbnail-font (font-spec :size 10)
  "Font of each preview window.

It should be a font-spec object.

For example, (font-spec :size 10)"
  :type 'font-spec
  :group 'mission-control)

(defun mcon-calculate-shape (length)
  "Calculate a 2D shape base on LENGTH."
  (let* ((row (truncate (sqrt length)))
         (colomn (/ length row)))
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
      (dolist (buffer (buffer-list))
        ;; don't include special buffers
        (unless (string-match black-list-regexp (buffer-name buffer))
          (push buffer buffer-list))))

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
              (insert-buffer-substring from-buffer))

            (when mode
              ;; (setq-local major-mode mode)
              (funcall mode))
            (setq-local mode-line-format (propertize (format "%d %s" count name) 'face mcon-number-face))
            (set-frame-font mcon-thumbnail-font t nil)
            )))

      (let* ((number-char-list '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
            (selected-window (string-to-number (char-to-string (read-char-choice "Select window: " number-char-list)))))
        (mapc #'kill-buffer temp-buffer-list)
        (delete-frame)
        (switch-to-buffer (nth (- selected-window 1) buffer-list))
        )
      )
    )
  )

(provide 'mission-control)
