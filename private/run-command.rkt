#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [run-command (-> string? path-string? ... string?)]))


(require racket/port
         racket/string
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


; This doesn't support anything but getting stdout -- but that's OK for now!
(define (run-command cmd-name . args)
  (define cmd-path
    (or (find-executable-path cmd-name)
        ; Racket doesn't know about $PATHEXT:
        (find-executable-path (string-append cmd-name ".exe"))))
  (unless cmd-path
    (raise-arguments-error (name run-command)
                           "couldn't find executable in $PATH"
                           "executable" cmd-name))
  (define-values (proc stdout stdin stderr)
    (apply subprocess #f #f #f cmd-path args))
  (close-output-port stdin)
  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))
  (define stdout-string (port->string stdout))
  (define stderr-string (string-trim (port->string stderr)))
  (close-input-port stdout)
  (close-input-port stderr)
  (unless (zero? exit-code)
    (raise-arguments-error (name run-command)
                           "command exited with a nonzero exit code"
                           "command" (string-join (cons cmd-name args) " ")
                           "exit code" exit-code
                           "stderr" stderr-string))
  (when (non-empty-string? stderr-string)
    (raise-arguments-error (name run-command)
                           "command exited successfully, but wrote to stderr"
                           "command" (string-join (cons cmd-name args) " ")
                           "stderr" stderr-string))
  stdout-string)
