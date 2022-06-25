#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [run-command (-> string? (or/c string? path?) ... string?)]))


(require racket/port
         racket/string
         rebellion/private/guarded-block)


;@----------------------------------------------------------------------------------------------------


; This doesn't support anything but getting stdout -- but that's OK for now!
(define/guard (run-command cmd-name . args)
  (define cmd-path
    (or (find-executable-path cmd-name)
        ; Racket doesn't know about $PATHEXT:
        (find-executable-path (string-append cmd-name ".exe"))))
  (guard (not cmd-path) then
    (error (format "couldn't find ~a executable in $PATH" cmd-name)))
  (define-values (proc stdout stdin stderr)
    (apply subprocess #f #f #f cmd-path args))
  (close-output-port stdin)
  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))
  (define stdout-string (port->string stdout))
  (define stderr-string (string-trim (port->string stderr)))
  (close-input-port stdout)
  (close-input-port stderr)
  (define (format-error . format-args)
    (format "command '~a ~a' ~a"
            cmd-path
            (string-join args " ")
            (apply format format-args)))
  (guard (zero? exit-code) else
    (error (format-error "exited with code ~a"
                         exit-code)))
  (guard (zero? (string-length stderr-string)) else
    (error (format-error "wrote to stderr: ~a"
                         stderr-string)))
  stdout-string)
