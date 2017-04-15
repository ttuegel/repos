(import (scheme base))
(import (scheme process-context))

(import (chibi io))
(import (chibi match))
(import (chibi process))
(import (chibi system))

;;;> Returns \scheme{#t} if the current directory is clean.
(define (git-clean dir)
  (let ((outp (process->string
               (list "git" "-C" dir "status" "--porcelain"))))
    (eq? 0 (string-length outp))))

(define (cleanup message)
  (write-string message)
  (newline)
  (let* ((shell (user-shell (user-information (current-user-id)))))
    (unless (system? shell) (exit #f))))

;;;> Prompt the user to commit any changes in the current directory.
(define (commit-git dir)
  (unless (git-clean dir) (cleanup "Please commit your changes")))

(define (git-pull dir)
  (system? (list "git" "-C" dir "pull")))

(define (git-submodule-update dir)
  (system? (list "git" "-C" dir "submodule" "update" "--init")))

(define (pull-git dir)
  (unless (and (git-pull dir)
               (git-submodule-update dir))
    (cleanup "pull-git failed")))

(define (git-push dir)
  (system? (list "git" "-C" dir "push" "--porcelain")))

(define (push-git dir)
  (unless (git-push dir) (cleanup "push-git failed")))

(define (sync-git dir)
  (commit-git dir)
  (pull-git dir)
  (push-git dir))

(sync-git "/home/ttuegel/.emacs.d/")
