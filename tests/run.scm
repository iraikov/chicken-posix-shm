(import scheme (chicken format) (chicken random) (chicken file posix) (chicken string) posix-shm)

(define block-size 8)

(let* ((str "Hello, world!")
       (path (sprintf "/shmtest~A" (pseudo-random-integer 100)))
       (fd (shm-open path (list open/rdwr open/creat))))
  (file-truncate fd (string-length str))
  (file-write fd str)
  (file-close fd)
  (let ((fd (shm-open path (list open/rdonly open/excl ))))
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


