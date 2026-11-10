;; -*- lexical-binding: t; -*-


(setq text "hello")
(setq last-mistake nil)

(defun read-csv (file col)
  (let ((sep1 "\n")
	(sep2 ","))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((lines (split-string (buffer-string) sep1 t))
	     (header (split-string (car lines) sep2 t))
	     (coln (cl-position col header :test #'equal))
	     )
	(mapcar (lambda (e) (nth coln (split-string e sep2 t))) (cdr lines)))
      )
    )
  )

(setq dist 2)

(defun gen-text()
  (save-excursion
    (setq text "")
    (let ((line (line-number-at-pos))
	  (gap dist))
      (goto-line (+ line gap))
      (if (not (equal (line-number-at-pos) (+ line 2)))
	  (insert (make-string gap ?\n))
	)
      (cl-loop for i from 0 to 10 do
	       (setq text (concat text (nth (random (length word-list)) word-list)))
	       (setq text (concat text " "))
	       )
      (insert text)
      )
    )
  )

(defun char-at-pos(l c)
  (save-excursion
    (goto-line l)
    (beginning-of-line)
    (forward-char c)
    (char-after)
    )
  )

(defun line-length(line)
  (let ((out nil))
    (save-excursion
      (goto-line line)
      (beginning-of-line-or-indentation)
      (setq out (length
		 (thing-at-point 'line t)
		 ))
      )
    out
    )
  )

(defun process()
  (interactive)
  (self-insert-command 1)
  (let ((key (this-command-keys ))
	)
    (if (equal key (kbd "<backspace>"))
	(delete-backward-char 1)
      )
    (if (and
	 (>= (current-column) (length text))
	 (not last-mistake)
	 )
	(progn
	  (erase-buffer)
	  (gen-text)
	  )
      )
    (if (not (equal (char-at-pos (line-number-at-pos) (- (current-column) 1))
		    (char-at-pos (+ (line-number-at-pos) dist) (- (current-column) 1))))
	(setq last-mistake (if last-mistake
			       (min (- (current-column) 1) last-mistake)
			     (- (current-column) 1)
			     )
	      )
      (if (or (not last-mistake) (<= (current-column) last-mistake))
	  (setq last-mistake nil)
	)
      )
    (add-text-properties 1 (line-length 1) '(face default))
    (if last-mistake
	(add-text-properties (+ 1 last-mistake) (line-length 1) '(face font-lock-warning-face))
      )
    )
  )

;; necessary?
(defun define-key-list(a)
  (cl-loop for key in a do
	   (define-key keytel-mode-map key 'process)))

(define-derived-mode keytel-mode fundamental-mode "keytel"
 
  (define-key keytel-mode-map (kbd "<escape>") 'quit-window)
  (define-key keytel-mode-map [t] 'process)
  )


(defun keytel ()
  (interactive)
  (switch-to-buffer "keytel")
  (erase-buffer)
  (keytel-mode)
  (setq word-list (read-csv "~/.local/share/keytel/wordlist.csv" "Word"))
  (gen-text)
  )
