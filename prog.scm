(setf fib (lambda (n) (if (< n 2) 1
                          (+ (fib (- n 2)) (fib (- n 1))))))
(fib 5)
