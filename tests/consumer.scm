(import scheme (chicken base) (chicken format) (chicken file posix) (chicken string) posix-shm)

(define block-size 8)

(let ((path (sprintf "/shmtest~A" 1)))
  (let ((fd (shm-open path (list open/rdonly open/excl ))))
    (set-file-position! fd 0)
    (let recur ((data '()))
      ;; read blocks of size block-size until # bytes read is zero
      (let ((block.bytes (file-read fd block-size)))
        (let ((block (car block.bytes))
              (bytes (cadr block.bytes)))
          (cond ((zero? bytes) (print (apply conc (reverse data))))
                (else (recur (cons (car block.bytes) data))))
          )
        ))
    (file-close fd)
    (shm-unlink path))
  )


