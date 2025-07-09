(define-module (zeta-lib term)
  #:export (println
	    usage
	    error-with-msg
	    info-with-msg))

(define (println str)
  (display str)
  (newline))

(define (usage)
  (println "Usage: zeta [install|remove|add|del] [-m|--manifest manifest] [packages]")
  (exit 1))

(define (colorize str color)
  (define terminate "\x1b[0m")
  (define colors
    '((red . "\x1b[1;31m")
      (blue . "\x1b[1;34m")
      (yellow . "\x1b[1;33m")))
  (format #f "~a~a~a"
	  (or (assoc-ref colors color) (error "Color not defined."))
	  str
	  terminate))

(define (error-with-msg msg)
  (display (colorize "ERROR: " 'red))
  (println msg)
  (exit 1))

(define (info-with-msg msg)
  (display (colorize "INFO: " 'blue))
  (println msg))

(define (warning-with-msg msg)
  (display (colorize "WARNING: " 'yellow))
  (println msg))
