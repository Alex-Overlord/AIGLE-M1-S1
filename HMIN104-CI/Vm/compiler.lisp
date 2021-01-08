(let ((liste ()))

(defun inAsm (expr &optional fichier)
  (setf liste ())
  (if (atom expr)
      ()
    (asm expr () t ()))
  (concat '(HALT))
  (if fichier(ecrire (reverse liste) fichier)(reverse liste)))
  

(defun asm (expr env top_level envPre)
(if (atom expr)
      (asm-val expr env envPre)
    (cond

     ((EQL 'QUOTE (car expr))
 
      (if (listp (cadr expr))
  (asm-val (cadr expr) env envPre)
(asm-val (cadr expr) env envPre)))
  


     ((EQ 'defun (car expr))

      (concat (list 'LABEL (cadr expr)))

      (let ((env (asm-param-env (caddr expr) env)))
(asm (cadddr expr) env () envPre)
(concat (list 'RTN))))


     ((operationp(car expr))


      (asm (cadr expr) env top_level envPre)
      (concat (list 'PUSH 'R0))

      (asm (caddr expr) env top_level envPre)
      (concat (list 'PUSH 'R0))

      (concat (list 'POP 'R1))
      (concat (list 'POP 'R0))

      (case (car expr )
(+
 (concat (list 'ADD 'R1 'R0)))
(-
 (concat (list 'SUB 'R1 'R0)))
(*
 (concat (list 'MUL 'R1 'R0)))
(/
 (concat (list 'DIV 'R1 'R0)))))

     ((EQ 'if (car expr))


      (let ((saut1 (compteur++)))
(let ((saut2 (compteur++)))

  (asm-op (cadr expr) saut1 env top_level envPre)

  (asm (cadddr expr) env top_level envPre)

  (concat (list 'JMP (list '@ saut2)))

  (concat (list 'LABEL saut1))

  (asm (caddr expr) env top_level envPre)

  (concat (list 'LABEL saut2)))))

     ((EQ 'prog1 (car expr))
      (let ((first (cadr expr)))
(loop for elmt in (cddr expr) 
      do
      (asm elmt env top_level envPre))
(asm first env top_level envPre)))


     ((EQ 'progn (car expr))
      (loop for elmt in (cdr expr) 
    do
    (asm elmt env top_level envPre)))


     ((EQ 'cond (car expr))
      
      
      (let ((fin (compteur++)))
  
(loop for val in (cdr expr)
      do

      (let ((saut1 (compteur++)))
(let ((saut2 (compteur++)))
  
  (asm-op (car val) saut1 env top_level envPre)
  
  (concat (list 'JMP (list '@ saut2)))
  
  (concat (list 'LABEL saut1))

  (loop for elt in (cdr val) 
do
(asm elt env top_level envPre))
  (concat (list 'JMP (list '@ fin)))

  (concat (list 'LABEL saut2)))))

(concat (list 'LABEL fin))))
    

     ((AND (EQ 'loop (car expr))
   (EQ 'while (cadr expr))
   (EQ 'do (cadddr expr)))

      (let ((boucle (compteur++)))
(let ((fin (compteur++)))


  (concat (list 'LABEL boucle))

  (asm-op (caddr expr) fin env top_level envPre)

  (asm (car (cddddr expr)) env top_level envPre)

  (concat (list 'JMP (list '@ boucle)))

  (concat (list 'LABEL fin)))))      

     ((or (EQ 'car (car expr))
  (EQ 'cdr (car expr)))
      
      (asm (cadr expr) env top_level envPre)
      (concat (list 'PUSH 'R0))
      (concat (list 'POP 'R1))
      (concat (list (car expr) 'R1 'R0)))


     ((AND (EQ 'loop (car expr))
   (EQ 'for (cadr expr))
   (EQ 'in (cadddr expr))
   (EQ 'do (cadr (cddddr expr))))
     
      (let ((etiq (compteur++)))
(let ((var (compteur++)))
  (concat (list 'JMP (list '@ etiq)))
  (concat (list 'LABEL var))
  (concat '(NOP))
  (concat (list 'LABEL etiq))

  (let ((etiq2 (compteur++)))
    (let ((var2 (compteur++)))
      (concat (list 'JMP (list '@ etiq2)))
      (concat (list 'LABEL var2))
      (concat '(NOP))
      (concat (list 'LABEL etiq2))

      (let ((env (asm-env 'liste2 (list '@ var) env)))
(let ((env (asm-env (caddr expr) (list '@ var2) env)))

  (let ((boucle (compteur++)))
    (let ((fin (compteur++)))

      (asm (car (cddddr expr)) env top_level envPre)
      (concat (list 'STORE 'R0 (cadr (assoc 'liste2 env :test #'eql))))
      (concat (list 'LOAD (cadr (assoc 'liste2 env :test #'eql)) 'R2))
      (concat (list 'CAR 'R2 'R2))
      (concat (list 'STORE 'R2 (cadr (assoc (caddr expr) env :test #'eql))))

      (concat (list 'LABEL boucle))
      
      (concat (list 'LOAD (cadr (assoc 'liste2 env :test #'eql)) 'R2))
      (concat (list 'MOVE (list '$ 'nil) 'R0))
      
      (concat (list 'CMP 'R2 'R0))
      (concat (list 'JEQ (list '@ fin)))

      (asm (caddr (cddddr expr)) env top_level envPre)
            
      (concat (list 'LOAD (cadr (assoc 'liste2 env :test #'eql)) 'R2))
      (concat (list 'CDR 'R2 'R2))
      (concat (list 'STORE 'R2 (cadr (assoc 'liste2 env :test #'eql))))
      (concat (list 'CAR 'R2 'R2))
      (concat (list 'STORE 'R2 (cadr (assoc (caddr expr) env :test #'eql))))
      
      (concat (list 'JMP (list '@ boucle)))
      (concat (list 'LABEL fin)))))))))))

     ((AND (EQ 'loop (car expr))
   (EQ 'for (cadr expr))
   (EQ 'from (cadddr expr))
   (EQ 'to (cadr (cddddr expr)))
   (EQ 'do (cadddr (cddddr expr))))

      (let ((boucle (compteur++)))
(let ((fin (compteur++)))


  (asm (car (cddddr expr)) env top_level envPre)

   (concat (list 'MOVE 'RO (list '@ (caddr expr))))

  (if (atom (cadddr (cdddr expr)))

      (progn 
(asm (car (cddddr (cddddr expr))) env top_level envPre)
(concat (list 'PUSH 'R0))

(concat (list 'LABEL boucle))
(concat (list 'MOVE (list '@ (caddr expr)) 'R0))
(concat (list 'POP 'R1)))

    (progn
      (concat (list 'LABEL boucle))
      (asm (car (cddddr (cddddr expr))) env top_level envPre)
      (concat (list 'MOVE 'R0 'R1))
      (concat (list 'MOVE (list '@ (caddr expr)) 'R0))))
  
  (concat (list 'SUB 'R0 'R1))
  (concat (list 'CMP 'R1 '($ 0)))
  (concat (list 'JLE (list '@ fin)))
          
  (asm (cadddr (cddddr expr)) env top_level envPre)
  (concat (list 'INCR (list '@ (caddr expr))))
  
  (concat (list 'JMP (list '@ boucle)))

  (concat (list 'LABEL fin)))))

     ((EQ 'let (car expr))
      (if top_level
  (let ((env (asm-let-top-env (cadr expr) env)))
    (asm-let (cddr expr) env top_level envPre))
(let ((env (asm-let-env (cadr expr) env (- 3 (length env)) top_level envPre)))
  (asm-let (cddr expr) env top_level envPre)))
      (loop for elmt from 0 to (- (length (cadr expr)) 1)
    do
    (concat (list 'DECR 'SP))))

     ((EQ 'setf (car expr))
      
      (asm (caddr expr) env top_level envPre)
      (concat (list 'PUSH 'R0))

      (if (cadr (assoc (cadr expr) env :test #'eql))
  (progn
    (concat (list 'POP 'R0))
    (concat (list 'STORE 'R0 (cadr (assoc (cadr expr) env :test #'eql)))))
(progn
  (asm (car(cddadr expr)) env top_level envPre )
  (concat (list 'PUSH 'R0))

  (asm (cadadr expr) env top_level envPre)
  (concat (list 'PUSH 'R0))
  (concat (list 'POP 'R2))
  (concat (list 'POP 'R1))
  (concat (list 'POP 'R0))
  (cond
   ((EQL (caadr expr) 'get)
    (concat (list 'SETFGET 'R2 'R1 'R0)))
   ((EQL (caadr expr) 'aref)
    (concat (list 'SETFAREF 'R2 'R1 'R0)))
   ((EQL (caadr expr) 'gethash)
    (concat (list 'SETFHASH 'R2 'R1 'R0)))   
   ((EQL (caadr expr) 'car)
    (concat (list 'SETFCAR 'R2 'R1 'R0)))    
   ((EQL (caadr expr) 'cdr)
    (concat (list 'SETFCDR 'R2 'R1 'R0)))))))
 

     ((EQ 'case (car expr))
      (let ((fin (compteur++)))
(loop for elmt in (cddr expr)
      do
      (let ((saut1 (compteur++)))
(let ((saut2 (compteur++)))
  (if (EQL (car elmt) 'otherwise)
      (progn
(asm (cadr elmt) env top_level envPre)
(concat (list 'JMP (list '@ fin))))
    (progn
      (asm (cadr expr) env top_level envPre)
      (concat (list 'PUSH 'R0))

      (asm (car elmt) env top_level envPre)
      (concat (list 'PUSH 'R0))

      (concat (list 'POP 'R1))
      (concat (list 'POP 'R0))

      (concat (list 'CMP 'R0 'R1))
      (concat (list 'JNEQ (list '@ saut2)))

      (asm (cadr elmt) env top_level envPre)
      (concat (list 'JMP (list '@ fin)))

      (concat (list 'LABEL saut2)))))))
(concat (list 'LABEL fin))))
     

     ((EQ 'labels (car expr))
      (let ((env_locale (asm-labels-env (cadr expr) (compteur++) ())) (etiq_fin (compteur++)))
(concat (list 'MOVE 'FP 'R0))
(concat (list 'PUSH 'R0))

(asm (caddr expr) env_locale top_level env)
  
(concat (list 'DECR 'SP))
(concat (list 'JMP (list '@ etiq_fin)))

(loop for locale in (cadr expr)
      do
      (concat (list 'LABEL (cadr (assoc (car locale) env_locale :test #'eql))))
      (concat (list 'MOVE 'FP 'R0))
      (concat (list 'PUSH 'R0))
      (let ((env_locale (asm-param-env (cadr locale) env_locale)))
(asm (caddr locale) env_locale top_level env))
      (concat (list 'DECR 'SP))
      (concat (list 'RTN)))

(concat (list 'LABEL etiq_fin))))

     (t
      (if (atom (car expr))
  (progn
    
    (asm-par (cdr expr) env top_level envPre )
      
    (concat (list 'MOVE (list '$ (length (cdr expr))) 'R0))
    (concat (list 'PUSH 'R0))
    (concat (list 'INCR 'R0))
    (concat (list 'MOVE 'FP 'R1))
    (concat (list 'MOVE 'SP 'FP))
    (concat (list 'MOVE 'SP 'R2))
    (concat (list 'SUB 'R0 'R2))
    (concat (list 'PUSH 'R2))
    (concat (list 'PUSH 'R1))

    (let ((etiq (cadr (assoc (car expr) env :test #'eql))))              
      (if (null etiq)
  (concat (list 'JSR (list '@ (car expr))))
(concat (list 'JSR (list '@ etiq)))))

    (concat (list 'POP 'R1))
    (concat (list 'POP 'R2))
    (concat (list 'MOVE 'R1 'FP))
    (concat (list 'MOVE 'R2 'SP)))
(error "Erreur: ~S ne peut pas etre compiler." (car expr)))))))


(defun concat (code_asm)
   (setf liste (cons code_asm liste))))


(defun asm-let (lexpr env top_level envPre)
  (if (atom lexpr)
      ()
    (progn
      (asm (car lexpr) env top_level envPre)
      (asm-let (cdr lexpr) env top_level envPre))))

(defun asm-par (lexpr env top_level envPre) 
  (if (atom lexpr)
      ()
    (progn
      (asm (car lexpr) env top_level envPre)
      (concat (list 'PUSH 'R0))
      (asm-par (cdr lexpr) env top_level envPre))))

(defun asm-val (var env envPre)
  (if (AND
       (symbolp var)
       (NOT (EQL var 'nil)))
              
      (let ((result (cadr (assoc var env :test #'eql))) (result2 (cadr (assoc var envPre :test #'eql))))
(if (null result)
    (if (null result2)
(concat (list 'MOVE (list '$ var) 'R0))
      (progn
(concat (list 'MOVE 'FP 'R2))
(concat (list 'MOVE (list 3 'FP) 'FP))
(concat (list 'MOVE result2 'R0))
(concat (list 'MOVE 'R2 'FP)) 
))

  (if (atom result)
      (concat (list 'MOVE result 'R0))
    (if (EQ (car result) '@)
(concat (list 'LOAD result 'R0))
      (concat (list 'MOVE result 'R0))))))

    (concat (list 'MOVE (list '$ var) 'R0))))


(defun asm-op (expr etiq env top_level envPre)
  (cond
   ((EQL 't expr)
    (concat (list 'JMP (list '@ etiq))))

   ((atom expr)
    (asm expr env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JEQ (list '@ etiq))))
   
   ((comparaisonp (car expr))
    (asm-op-part expr env top_level envPre)
    (asm-cmp expr etiq))

   ((EQL (car expr) 'AND)
    (asm-and (cdr expr) (compteur++) etiq (compteur++) env top_level envPre))

   ((EQL (car expr) 'OR)
    (asm-or (cdr expr) etiq env top_level envPre))

   ((EQL (car expr) 'NOT)
    (asm-not (cdr expr) etiq env top_level envPre))

   (t
    (asm expr env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JEQ (list '@ etiq))))))

(defun asm-and (expr etiqFaux etiqFin etiq env top_level envPre)
  (cond
   ((atom expr)
    (concat (list 'LABEL etiq))
    (concat (list 'JMP (list '@ etiqFin)))
    (concat (list 'LABEL etiqFaux)))
   ((comparaisonp (caar expr))
    (concat (list 'LABEL etiq))
    (asm-op-part (car expr) env top_level envPre)
    (asm-cmp (car expr) (compteur++))      
    (concat (list 'JMP (list '@ etiqFaux)))
    (asm-and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   ((EQ 'NOT (caar expr))
    (asm-not (cdar expr) (compteur++) env top_level envPre)
    (concat (list 'JMP (list '@ etiqFaux)))
    (asm-and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   (t
    (concat (list 'LABEL etiq))
    (asm (car expr) env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JEQ (list '@ (compteur++))))
    (concat (list 'JMP (list '@ etiqFaux)))
    (asm-and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))))

(defun asm-not-and (expr etiqFaux etiqFin etiq env top_level envPre)
  (cond
   ((atom expr)
    (concat (list 'LABEL etiq))
    (concat (list 'JMP (list '@ etiqFin)))
    (concat (list 'LABEL etiqFaux)))
   ((comparaisonp (caar expr))
    (concat (list 'LABEL etiq))
    (asm-op-part (car expr) env top_level envPre)
    (asm-not-cmp (car expr) (compteur++))      
    (concat (list 'JMP (list '@ etiqFaux)))
    (asm-not-and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   (t
    (concat (list 'LABEL etiq))
    (asm (car expr) env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JNEQ (list '@ (compteur++))))
    (concat (list 'JMP (list '@ etiqFaux)))
    (asm-not-and (cdr expr) etiqFaux etiqFin (compteur) env top_level))))

(defun asm-or (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((comparaisonp (caar expr))
    (asm-op-part (car expr) env top_level envPre)
    (asm-cmp (car expr) etiq) 
    (asm-or (cdr expr) etiq env top_level envPre))
   ((EQ 'NOT (caar expr))
    (asm-not (cdar expr) etiq env top_level envPre)
    (asm-or (cdr expr) etiq env top_level envPre))
   (t
    (asm (car expr) env top_level envPre)
    (concat (list 'CMP (caar expr) 'R0))
    (concat (list 'JEQ (list '@ etiq)))
    (asm-or (cdr expr) etiq  env top_level envPre))))

(defun asm-not-or (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((comparaisonp (caar expr))
    (asm-op-part (car expr) env top_level envPre)
    (asm-not-cmp (car expr) etiq) 
    (asm-not-or (cdr expr) etiq env top_level envPre))
   (t
    (asm (car expr) env top_level envPre)
    (concat (list 'CMP (caar expr) 'R0))
    (concat (list 'JNEQ (list '@ etiq)))
    (asm-not-or (cdr expr) etiq  env top_level envPre))))

(defun asm-not (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((comparaisonp (caar expr))
    (asm-op-part (car expr) env top_level envPre)
    (asm-not-cmp (car expr) etiq) 
    (asm-not (cdr expr) etiq env top_level envPre))
   ((EQ (caar expr) 'AND)
    (asm-not-and (cdar expr) (compteur++) etiq (compteur++) env top_level envPre)
    (asm-not (cdr expr) etiq env top_level envPre))
   ((EQ (caar expr) 'OR)
    (asm-not-or (cdar expr) etiq env top_level envPre)
    (asm-not (cdr expr) etiq env top_level envPre))

   (t
    (asm (car expr) env top_level envPre)
    (concat (list 'CMP (caar expr) 'R0))
    (concat (list 'JNEQ (list '@ etiq)))
    (asm-not (cdr expr) etiq  env top_level envPre))))

(defun asm-op-part (expr env top_level envPre)
  (progn
    (asm (cadr expr) env top_level envPre)
    (concat (list 'PUSH 'R0))
    (asm (caddr expr) env top_level envPre)
    (concat (list 'PUSH 'R0))
    (concat (list 'POP 'R1))
    (concat (list 'POP 'R0))
    (concat (list 'CMP 'R0 'R1))))

(defun asm-cmp (expr etiq)
  (case (car expr)
    (=
     (concat (list 'JEQ (list '@ etiq))))
    (EQ
     (concat (list 'JEQ (list '@ etiq))))
    (EQL
     (concat (list 'JEQ (list '@ etiq))))
    (EQUAL
     (concat (list 'JEQ (list '@ etiq))))
    (<
     (concat (list 'JL (list '@ etiq))))
    (>
     (concat (list 'JG (list '@ etiq))))
    (!=
     (concat (list 'JNEQ (list '@ etiq))))
    (<=
     (concat (list 'JLE (list '@ etiq))))
    (>=
     (concat (list 'JGE (list '@ etiq))))))

(defun asm-not-cmp (expr etiq)
  (case (car expr)
    (=
     (concat (list 'JNEQ (list '@ etiq))))
    (EQ
     (concat (list 'JNEQ (list '@ etiq))))
    (EQL
     (concat (list 'JNEQ (list '@ etiq))))
    (EQUAL
     (concat (list 'JNEQ (list '@ etiq))))
    (<
     (concat (list 'JGE (list '@ etiq))))
    (>
     (concat (list 'JLE (list '@ etiq))))
    (!=
     (concat (list 'JEQ (list '@ etiq))))
    (<=
     (concat (list 'JG (list '@ etiq))))
    (>=
     (concat (list 'JL (list '@ etiq))))))


(defun asm-let-env (lexpr env acc top_level envPre)
  (if (atom lexpr)
      env
    (progn
      (asm (cadar lexpr) env top_level envPre)
      (concat (list 'PUSH 'R0))
      (asm-let-env (cdr lexpr) (asm-env (caar lexpr) (list (+ (length env) acc) 'FP) env) acc top_level envPre ))))

(defun asm-let-top-env (lexpr env)
  (if (atom lexpr)
      env
    (let ((etiq (compteur++)))
      ;(compteur++)
      (let ((var (compteur++)))
;(compteur++)
(concat (list 'JMP (list '@ etiq)))
(concat (list 'LABEL var))
(concat (list (cadar lexpr)))
(concat (list 'LABEL etiq))
(asm-let-top-env (cdr lexpr) (asm-env (caar lexpr) (list '@ var) env))))))


(defun asm-labels-env (lexpr etiq env)
  (if (atom lexpr)
      env
    (asm-labels-env (cdr lexpr) (compteur++) (asm-env (caar lexpr) etiq env))))

(defun asm-param-env (lpara env)
  (if (atom lpara)
      env
    (asm-param-env (cdr lpara) (asm-env (car lpara) (list (- 0 (+ (length lpara) 1)) 'FP) env) )))


(defun asm-env (symbole valeur env)
  (acons symbole (list valeur) env))

 

(defun operationp( op )
  (member op '(+ - * /)))


(defun comparaisonp ( op )
  (member op '(= < > != <= >= EQ EQL EQUAL)))


(defun ecrire (listeASM path)
  (with-open-file (stream path :direction :output)
  (loop for elmt in listeASM
do
(format stream (toString elmt)))
  (format stream "nil")))

(defun toString (a)
  (format nil "~A~%" a))


(defun vm-compiler (file &optional fichier)
  (let ((result ()))
    (labels ((lecture (o)
      (let ((lu (read o nil nil nil)))
(if lu
    (progn 
      (setf result (append (inAsm lu) result))
      (lecture o))
  't))))
      (lecture (open file)))
    (if fichier
(ecrire result fichier)
      result)
      (setf asmCode result))
      
      (write asmCode))
    
(let ((etiq 0)) (defun compteur () etiq) (defun compteur++ () (setf etiq (+ etiq 1))) (defun reset () (setf etiq 0)))

