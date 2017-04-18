(import (scheme base))
(import (scheme load))
(import (scheme process-context))
(import (scheme write))

(import (chibi filesystem))
(import (chibi io))
(import (chibi match))
(import (chibi pathname))
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

(define (with-directory-or-skip dir body)
  (if (file-directory? dir)
      (begin
        (display-command-for-user (list "cd" dir))
        (with-directory dir body))
      (begin
        (write-string (string-join "# Skipping missing directory `" dir "'"))
        (newline))))

;;;> Synchronize the Git repository in \var{dir}.
(define (sync-git dir)
  (with-directory-or-skip dir (lambda () (commit-git) (pull-git) (push-git))))

(define (git-clone repo dir)
  (let ((command (list "git" "clone" repo dir)))
    (display-command-for-user command)
    (apply system? command)))

;;;> Clone the Git repository at \var{repo} into \var{dir}.
(define (clone-git repo dir)
  (unless (create-directory* dir)
    (die "# Unable to create directory `" dir "'!"))
  (unless (git-clone repo dir) (die "# Clone failed!"))
  (unless (with-directory dir (git-submodule-update))
    (die "# Failed to fetch submodules!")))

(define-values (action targets)
  (match (command-line)
    ((_ action . targets) (values (string->symbol action) targets))
    ((_) (values 'sync '()))))

(define (die . messages)
  (write-string (string-join messages))
  (newline)
  (exit #f))

(match action
 ('clone
  (unless (> 0 (length targets))
    (die "# Action `clone' requires at least one target")))
 ('sync '())
 (_ (die "# Unknown action `" (symbol->string action) "'")))

(define (git repo dir)
  (match action
    ('clone (clone-git repo dir))
    ('sync (sync-git dir))
    (_ '())))

(define config-file
  (let ((home (user-home (user-information (current-user-id)))))
    (make-path home ".config" "repos.scm")))

(unless (file-is-readable? config-file)
  (die "# Config file is not readable!"))

(load config-file)
