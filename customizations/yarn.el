(require 'mike)

(defun yarn-in-project (f)
  (let ((default-directory (find-containing-directory-upwards "package.json")))
    (if default-directory
      (funcall f default-directory)
      (error "Not inside a JavaScript project!"))))

(defvar yarn-grep-history nil)
(defun yarn-grep (term)
  (interactive
   (list
    (let* ((default-term (car yarn-grep-history))
           (prompt (if (s-blank? default-term)
                     "Search for: "
                     (concat "Search string (default \"" default-term "\"): ")))
           (input (read-from-minibuffer prompt nil nil nil 'yarn-grep-history)))
      (if (s-blank? input) default-term input))))
  (yarn-in-project
   (lambda (d)
     (grep (concat "grep --color --exclude-dir=.git --exclude-dir=target --exclude-dir=experiments --exclude-dir=build --exclude-dir=lib --exclude-dir=scripts --exclude=dir=src/content --exclude-dir=coverage --exclude-dir=node_modules -nriH -e \"" term "\" " d)))))
