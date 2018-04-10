(provide 'mike)

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun first-matching-file (dir re)
  (seq-some
   (lambda (file)
     (when (string-match re file)
       file))
   (directory-files dir nil nil t)))

(defun find-file-upwards (current-dir file-re)
  (let* ((parent-dir (parent-directory (expand-file-name current-dir)))
        (file (first-matching-file parent-dir file-re)))
    (if file
        (concat parent-dir file)
      (when parent-dir
        (find-file-upwards parent-dir file-re)))))

(defun find-containing-directory-upwards (file-re)
  (let ((file-path (find-file-upwards (buffer-file-name) file-re)))
    (when file-path
      (parent-directory file-path))))
