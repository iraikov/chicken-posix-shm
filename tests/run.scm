(import scheme)

(import scheme (chicken format) (chicken random) (chicken bytevector)
        (chicken string) (chicken file posix) test posix-shm)

(define block-size 8)

;; Writes str into a freshly created POSIX shared memory segment, reads
;; it back in blocks of block-size bytes, and returns the round-tripped
;; string.
(define (shm-round-trip str)
  (let ((path (sprintf "/shmtest~A" (pseudo-random-integer 100000))))
    (let ((fd (shm-open path (list open/rdwr open/creat))))
      (file-truncate fd (string-length str))
      (file-write fd (string->utf8 str))
      (file-close fd))
    (let ((fd (shm-open path (list open/rdonly))))
      (let recur ((chunks '()))
        (let* ((block.bytes (file-read fd block-size))
               (block (car block.bytes))
               (bytes (cadr block.bytes)))
          (if (zero? bytes)
              (begin
                (file-close fd)
                (shm-unlink path)
                (apply string-append (reverse chunks)))
              (recur (cons (utf8->string block 0 bytes) chunks))))))))

(test-group "posix-shm round-trip"

  (test-assert "posix-shm support detected" posix-shm?)

  (test "short string" "Hello, world!" (shm-round-trip "Hello, world!"))

  (test "empty string" "" (shm-round-trip ""))

  (test "string shorter than block size" "hi" (shm-round-trip "hi"))

  (test "string exact multiple of block size" "12345678"
    (shm-round-trip "12345678"))

  (test "string longer than several blocks"
    "The quick brown fox jumps over the lazy dog."
    (shm-round-trip "The quick brown fox jumps over the lazy dog.")))

(test-exit)
