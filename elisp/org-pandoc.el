;; org-pandoc mode provides tools for round-tripping markdown and
;; org-mode through html exports.
;;
;; The problem is that DevonThink is able to render HTML documents
;; with nicely and with internal hyperlinks -- however it isn't able
;; to do the same with "plain text" documents.
;;
;; Using org-pandoc, it's easy to store the rendered HTML
;; version of an org-mode document in DevonThink and then edit the raw
;; org-mode text via a hotkey from inside DevonThink.


(defvar org-pandoc-orig-file nil)
(make-variable-buffer-local 'org-pandoc-orig-file)

(defvar org-pandoc-orig-app nil)
(make-variable-buffer-local 'org-pandoc-orig-app)

(defvar org-pandoc-export-args nil)


(defun current-window ()
  (with-temp-buffer
    (shell-command "osascript -e 'set front_app to (path to frontmost application as Unicode text)'" t)
    (goto-char (point-min))
    (re-search-forward "$")
    (buffer-substring-no-properties (point-min) (point))))


(defun focus-window (app)
  (shell-command (format "osascript -e 'tell application \"%s\"
activate
end tell'" app)))


(defun org-pandoc-detect-type (infile)
  (if (eq 0 (call-process
             "grep" nil nil nil
             "<meta name=\"generator\" content=\"Org-mode\"/>" infile))
      'roundtrip
    'markdown))


(defun org-pandoc-name (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "[#*\s]*")
      (buffer-substring-no-properties (point) (line-end-position)))))


(defun org-pandoc-roundtrip-append (html-buffer source-buffer)
  (with-current-buffer html-buffer
    (goto-char (point-max))
    (insert "<!--")
    (save-excursion (insert
                     (with-current-buffer source-buffer
                       (buffer-substring-no-properties (point-min) (point-max)))))
    (while (search-forward "<!--" nil t)
      (replace-match "~~~BC~~~" nil t))
    (goto-char (point-max))
    (insert "-->")))


(defun org-pandoc-roundtrip-restore (html-buffer source-buffer)
  (with-current-buffer html-buffer
    (goto-char (point-max))
    (when (looking-back "-->[[:space:]\n]*")
      (let* ((e (search-backward "-->" nil nil))
             (b (and (search-backward "<!--" nil nil)
                     (search-forward "<!--")))
             (s (and b e (buffer-substring-no-properties b e))))
        (with-current-buffer source-buffer
          (insert s)
          (goto-char (point-min)))
        source-buffer))))


(defun org-pandoc-roundtrip-write (outfile)
  (let ((source-buffer (current-buffer))
        (html-buffer (generate-new-buffer "*pandoc-tmp*")))
    (org-export-as-html nil nil org-pandoc-export-args
                        html-buffer)
    (org-pandoc-roundtrip-append html-buffer source-buffer)
    (with-current-buffer html-buffer
      (write-file outfile)
      (kill-buffer))))


(defun org-pandoc-roundtrip-read (infile)
  (message "%s" infile)
  (let ((html-buffer (generate-new-buffer "*pandoc-tmp*"))
        (source-buffer (generate-new-buffer "*pandoc-in*")))
    (with-current-buffer html-buffer
      (insert-file-contents-literally infile)
      (org-pandoc-roundtrip-restore html-buffer source-buffer)
      (kill-buffer))
    (with-current-buffer source-buffer
      (org-mode)
      (org-global-cycle 3))
    source-buffer))


(defun org-pandoc-markdown-write (outfile)
  (let ((f (or
            (buffer-file-name)
            (make-temp-file "pandoc"))))
    (write-file f)
    (call-process "pandoc" nil nil nil
                  "-s" "-c" "file:///Users/me/Dropbox/pandoc4.css"
                  "-f" "markdown" "-t" "html" "-o" outfile f)))


(defun org-pandoc-markdown-read (infile)
  (let ((buf (generate-new-buffer "*pandoc*")))
    (call-process "pandoc" nil buf nil
                  "-f" "html" "-t" "markdown" infile)
    (with-current-buffer buf
      (markdown-mode)
      buf)))


(defun org-pandoc-write (outfile)
  (let ((exporter (assq major-mode
                        '((markdown . org-pandoc-markdown-write)
                          (org-mode . org-pandoc-roundtrip-write)))))
    (when exporter
      (funcall (cdr exporter) outfile))))


(defun org-pandoc-save ()
  (interactive)
  (if org-pandoc-orig-file
      (org-pandoc-write org-pandoc-orig-file)

    (let* ((name (org-pandoc-name))
           (outfile (make-temp-file "pandoc"))
           (record (progn
                     (org-pandoc-write outfile)
                     (dvm-create-record
                      name "html" nil outfile))))
      (setq org-pandoc-orig-file (aref record 1))
      (org-pandoc-mode))))


(defun org-pandoc-commit ()
  (interactive)
  (when org-pandoc-orig-app
    (org-pandoc-save)
    (focus-window org-pandoc-orig-app)
    (kill-buffer)
    (delete-frame)))


(defun org-pandoc-open (infile &optional inapp)
  (when (null inapp)
    (setq inapp (current-window)))
  (let* ((type (org-pandoc-detect-type infile))
         (importer (assq type
                         '((markdown . org-pandoc-markdown-read)
                           (roundtrip . org-pandoc-roundtrip-read))))
         (buf (and importer
                   (funcall (cdr importer) infile))))
    (with-current-buffer buf
      (org-pandoc-mode)
      (set (make-local-variable 'org-pandoc-orig-file) infile)
      (set (make-local-variable 'org-pandoc-orig-app) inapp))

    (with-selected-frame (make-frame)
      (switch-to-buffer buf)  
      (select-frame-set-input-focus (selected-frame)))))


(define-minor-mode org-pandoc-mode
  "Toggle org-pandoc-mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " pandoc"
  ;; The minor mode bindings.
  '(([remap save-buffer] . org-pandoc-save)
    ("\C-c\C-c" . org-pandoc-commit))
  :group 'pandoc)


(provide 'org-pandoc)
