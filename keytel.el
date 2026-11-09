;; -*- lexical-binding: t; -*-


(setq text "hello")


(defun read-csv (file col)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n" t))
	  (header (split-string (car lines) "," t))
	  (coln (cl-position col header))))
    (mapcar (lambda (e) (nth coln (split-string e " " t))) (cdr lines))
    )
  )

(read-csv "~/.local/share/keytel/wordlist.csv" "Word")

(defun gen-text()
  (save-excursion
    (let ((line (line-number-at-pos))
	  (gap 2))
      (goto-line (+ line gap))
      (if (not (equal (line-number-at-pos) (+ line 2)))
	  (insert (make-string gap ?\n))
	  )
      (insert text)
      )
    )
  )


(defun process()
  (interactive)
  (self-insert-command 1)
  (let ((key (this-command-keys))
	)
    (if (equal key (kbd "<backspace>"))
	(delete-backward-char 1)
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
  (gen-text)
  )
