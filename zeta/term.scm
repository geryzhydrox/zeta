(define-module (zeta term)
  ;; #:use-module (term ansi-color)
  #:export (println
	    usage
	    error-with-msg
	    info-with-msg
	    ))

(define (println str)
  (display str)
  (newline))

(define (usage)
  (println "Usage: zeta [install|remove|add|del] [manifest] [packages]")
  (exit 1))

(define (error-with-msg msg)
  (display "\x1b[1;31mERROR: \x1b[0m")
  (println msg)
  (exit 1))

(define (info-with-msg msg)
  (display "\x1b[1;34mINFO: \x1b[0m")
  (println msg))

(define (warning-with-msg msg)
  (display "\x1b[1;33mWARNING: \x1b[0m")
  (println msg))


