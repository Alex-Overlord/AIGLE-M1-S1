(defun fibo (n) (cond ((= n 0) 1) ((= n 1) 1) (t ( + (fibo (- n 1)) (fibo (- n 2))))))

(defun fact (n)(if (= n 0)1(* n (fact (- n 1))) ) )

(defun double (n) (* n 2) )
