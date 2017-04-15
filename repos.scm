(import (scheme base))
(import (scheme process-context))

(import (chibi filesystem))
(import (chibi io))
(import (chibi match))
(import (chibi process))
(import (chibi string))
(import (chibi system))

;;;> Is the current directory clean? Returns \scheme{#f} if the directory
;;;> does not contain a Git repository.
(define (git-clean)
  (let ((outp (process->string (list "git" "status" "--porcelain"))))
    (eq? 0 (string-length outp))))

;;;> The user's default shell.
(define shell (user-shell (user-information (current-user-id))))

;;;> Prompt the user to clean up the current directory by displaying
;;;> \var{message} and dropping them into a shell. If the shell exits
;;;> abnormally, the current process will also exit abnormally.
(define (cleanup message)
  (write-string message)
  (newline)
  (unless (system? shell) (exit #f)))

;;;> Prompt the user to commit any changes in the current directory.
(define (commit-git)
  (unless (git-clean) (cleanup "# Please commit your changes")))

(define (display-command-for-user command)
  (write-string "; ")
  (write-string (string-join command " "))
  (newline))

(define (git-pull)
  (let ((command (list "git" "pull")))
    (display-command-for-user command)
    (apply system? command)))

(define (git-submodule-update)
  (let ((command (list "git" "submodule" "update" "--init")))
    (display-command-for-user command)
    (apply system? command)))

;;;> Pull in remote changes to the current directory.
(define (pull-git)
  (unless (and (git-pull) (git-submodule-update))
    (cleanup "# Please pull in remote changes")))

(define (git-push)
  (let ((command (list "git" "push" "--porcelain")))
    (display-command-for-user command)
    (apply system? command)))

;;;> Push local changes from the current directory.
(define (push-git)
  (unless (git-push) (cleanup "# Please push local changes")))

;;;> Synchronize the Git repository in \var{dir}.
(define (sync-git dir)
  (with-directory dir
    (lambda ()
      (commit-git)
      (pull-git)
      (push-git))))

(sync-git "/home/ttuegel/.emacs.d/")
