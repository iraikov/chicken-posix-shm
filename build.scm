
(import (chicken base) (chicken format) (chicken process) (chicken process-context) srfi-1 srfi-13 compile-file)
(define args (command-line-arguments))


(define (shm-try-compile ldflags cflags)
  (and (try-compile 
	(string-append "#include <sys/mman.h>\n"
		       "#include <sys/stat.h>\n"
		       "#include<fcntl.h>\n"
		       "\n" 
		       "int main(int argc, char **argv) { shm_open(\"test\",0,0); return 0; }\n")
	ldflags: ldflags
	cflags: cflags)
       (cons ldflags cflags)))


(define-syntax shm-test 
  (syntax-rules ()
    ((_ (flags ...))
     (condition-case (shm-try-compile flags ...)
		     (t ()    #f)))))

(define c+ld-options (or (shm-test ("-lrt" "-DHAVE_POSIX_SHM")) ""))

(define cmd (intersperse (append args (filter (lambda (x) x)
                                              (list (sprintf "-L \"~A\"" (car c+ld-options))
                                                    (and (> (string-length (cdr c+ld-options)) 0)
                                                         (sprintf "-C \"~A\"" (cdr c+ld-options))))))
                                 " "))
(print (string-concatenate cmd))
(system (string-concatenate cmd))
