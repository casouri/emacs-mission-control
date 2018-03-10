(defvar mcon-window-shape '(2 3)
  "The shape of mission control pane. 

(rows colomns)

default is.")

(defvar mcon-black-list-regexp '("\*.+\*" "magit")
  "Buffers that match regexps inside this list are note included in mission control.")

(defvar mcon-number-face '(:height 300)
  "Face of numbers on the modeline on each window.")

(defvar mcon-thumbnail-font (font-spec :size 10)
  "Font of each preview window.

It should be a font-spec object.

For example, (font-spec :size 10)")

(defun mcon-switch ()
  "Open mission control and select a buffer."
  
  (let* ((row (car mcon-window-shape))
          (colomn (nth 1 mcon-window-shape))
          (row-window-list ())
          (colomn-window-list ())
          (all-window-list ())
          (buffer-list ())
          (window-count 0)
          (temp-buffer-list ())
          (frame-height (frame-parameter nil 'height))
          (frame-width (frame-parameter nil 'width))
          (width (/ frame-width colomn))
          (height (/ frame-height row))
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
               (mode (when from-buffer (buffer-local-value 'major-mode from-buffer))))

           ;; copy some text from original buffer to temp buffer
           (switch-to-buffer buffer)
           (push buffer temp-buffer-list)
           (unless (> count (length buffer-list))
             (insert-buffer-substring from-buffer))

           (when mode
             (setq-local major-mode mode))
           (setq-local mode-line-format (propertize (format "%d" count) 'face mcon-number-face))
           (set-frame-font mcon-thumbnail-font t nil)
           )))

     (let ((selected-window (string-to-number (char-to-string (read-char "Select window: ")))))
       (mapc #'kill-buffer temp-buffer-list)
       (delete-frame)
       (switch-to-buffer (nth (- selected-window 1) buffer-list))
       )
     )
   )
