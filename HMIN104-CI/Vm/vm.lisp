(load "Vm/compiler.lisp")

(defun make-vm ( name sizeMemory )
    (progn
      (setf (get name 'memory) (make-array sizeMemory :initial-element ()))
      (setf (get name 'R0) 0)
      (setf (get name 'R1) 0)
      (setf (get name 'R2) 0)
      (setf (get name 'FP) 0)
      (setf (get name 'SP) 0)
      (setf (get name 'PC) (- sizeMemory 1))
      (setf (get name 'BP) (- (size-memory name) 1))
      (setf (get name 'DPP) 0)
      (setf (get name 'DE) 0)
      (setf (get name 'DPG) 0)
      (setf (get name 'PCETIQ) (make-hash-table :size 0))
      (setf (get name 'REF_AVANT) (make-hash-table :size 0))    
      (setf (get name 'RUN) nil)
      (setf (get name 'DEBUG) nil)
      name))

(defun vm-init(vm)
  (reset_vm vm)
  (vm-load-file vm "ASM_loader"))

(defun get-memory (vm memory_case )
  (aref (get vm 'memory) memory_case))

(defun set-memory (vm  memory_case value )
  (setf (aref (get vm 'memory) memory_case) value))

(defun get-register ( vm register )
  (get vm register))

(defun set-register ( vm register value )
  (setf (get vm register) value))

(defun size-memory (vm)
  (length (get vm 'memory)))

(defun get-hash ( vm idTable )
  (get vm idTable))

(defun get-run ( vm )
  (get vm 'RUN))

(defun set-run ( vm value )
  (setf (get vm 'RUN) value))

(defun get-debug ( vm )
  (get vm 'DEBUG))

(defun set-debug ( vm value )
  (setf (get vm 'DEBUG) value))

(defun vm-exec-instr ( vm expression )
  (case (car expression)
    (STORE     (vm-exec-instr-store     vm   ( cdr  expression )))
    (LOAD      (vm-exec-instr-load      vm   ( cdr  expression )))
    (MOVE      (vm-exec-instr-move      vm   ( cdr  expression ))) 
    (PUSH      (vm-exec-instr-push      vm   ( cadr expression )))
    (POP       (vm-exec-instr-pop       vm   ( cadr expression )))
    (INCR      (vm-exec-instr-incr      vm   ( cadr expression )))
    (DECR      (vm-exec-instr-decr      vm   ( cadr expression )))
    (CAR       (vm-exec-instr-car       vm   ( cdr  expression )))
    (CDR       (vm-exec-instr-cdr       vm   ( cdr  expression )))
    (ADD       (vm-exec-instr-add       vm   ( cdr  expression )))
    (SUB       (vm-exec-instr-sub       vm   ( cdr  expression )))
    (MUL       (vm-exec-instr-mult       vm   ( cdr  expression )))
    (DIV       (vm-exec-instr-div       vm   ( cdr  expression )))
    (CMP       (vm-exec-instr-cmp       vm   ( cdr  expression )))
    (JSR       (vm-exec-instr-jsr       vm   ( cadr expression )))
    (JMP       (vm-exec-instr-jmp       vm   ( cadr expression )))
    (JEQ       (vm-exec-instr-jeq       vm   ( cadr expression )))
    (JNEQ      (vm-exec-instr-jne       vm   ( cadr expression )))
    (JLE       (vm-exec-instr-jle       vm   ( cadr expression )))
    (JGE       (vm-exec-instr-jge       vm   ( cadr expression )))
    (JL        (vm-exec-instr-jl        vm   ( cadr expression )))
    (JG        (vm-exec-instr-jg        vm   ( cadr expression )))
    (SETFGET   (vm-exec-instr-setfget   vm   ( cdr  expression )))
    (SETFHASH  (vm-exec-instr-setfhash  vm   ( cdr  expression )))
    (SETFAREF  (vm-exec-instr-setfaref  vm   ( cdr  expression )))
    (HALT      (vm-exec-instr-halt      vm   ))
    (NOP       (vm-exec-instr-nop       vm   ))
    (RTN       (vm-exec-instr-rtn       vm   ))))


(defun vm-exec-instr-load ( vm lexpr )
  (cond 
   ((integerp (car lexpr))
    (setf (get vm (cadr lexpr)) (get-memory vm (car lexpr))))
   ((registerp (car lexpr))
    (setf (get vm (cadr lexpr)) (get-memory vm (get-register (car lexpr)))))
   (t
    (setf (get vm (cadr lexpr)) 
    (get-memory vm (+ (get-register vm (cadar lexpr)) (caar lexpr))))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-store ( vm lexpr )
  (cond
   ((registerp (cadr lexpr))
    (set-memory vm (get-register vm (cadr lexpr)) (get-register vm (car lexpr))))
   ((integerp (cadr lexpr))
    (set-memory vm (cadr lexpr) (get-register vm (car lexpr))))
   (t
    (set-memory vm (+ (get-register vm (cadadr lexpr)) (caadr lexpr)) (get-register vm (car lexpr)))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-move ( vm lexpr )
  (cond 
   ((registerp (car lexpr))
    (setf (get vm (cadr lexpr)) (get vm (car lexpr))))
  
   ((EQL (caar lexpr) '$)
    (setf (get vm (cadr lexpr)) (cadar lexpr)))
   
   (t
    (setf (get vm (cadr lexpr)) (get-memory vm (+ (get-register vm (cadar lexpr)) (caar lexpr))))))

  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-push ( vm register )
  (if (= (get-register vm 'SP) (get-register vm 'PC))
      (error "[vm-exec-instr-push] Erreur le code se trouve sur la pile")
    (prog1
      (set-memory vm (get-register vm 'SP) (get-register vm register))
      (set-register vm 'SP (+ (get-register vm 'SP) 1))
      (set-register vm 'PC (- (get-register vm 'PC) 1))
      )))

(defun vm-exec-instr-pop ( vm register )
  (if (= (get-register vm 'SP) (get-register vm 'FP))
      (error "[vm-exec-instr-pop] Impossible de dépiler")
    (progn 
      (set-register vm 'SP      (- (get-register vm 'SP) 1))
      (set-register vm register ( get-memory vm (get-register vm 'SP)))
      (set-register vm 'PC      (- (get-register vm 'PC) 1 )))))

(defun vm-exec-instr-incr (vm register)
  (if (not (registerp register))
      (error "[vm-exec-instr-incr] INCR seulement pour les registres")
    (set-register vm register (+ (get-register vm register) 1)))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-decr (vm register)
  (if (not (registerp register))
      (error "[vm-exec-instr-decr] DECR seulement pour les registres")
    (set-register vm register (- (get-register vm register) 1)))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-mult ( vm lexpr )
  (set-register vm (cadr lexpr)
    (* (get-register vm (cadr lexpr)) (get-register vm (car lexpr)))) 
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-add ( vm lexpr )
  (set-register vm (cadr lexpr)
    (+ (get-register vm (cadr lexpr)) (get-register vm (car lexpr))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-div ( vm lexpr )
  (set-register vm (cadr lexpr)
    (/ (get-register vm (cadr lexpr)) (get-register vm (car lexpr))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-sub ( vm lexpr )
  (set-register vm (cadr lexpr)
    (- (get-register vm (cadr lexpr)) (get-register vm (car lexpr))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-cmp ( vm lexpr )

  (if (EQL (length lexpr) 2) 
      (cond 
       ((EQL (get-register vm (car lexpr)) (get-register vm (cadr lexpr)))
  (progn 
    (setf (get vm 'DE) 1)
    (setf (get vm 'DPG) 0)
    (setf (get vm 'DPP) 0))) 
  
       ((AND
   (integerp (get-register vm (car lexpr)))
   (integerp (get-register vm (cadr lexpr))))
   
  (cond
   (( < (get-register vm (car lexpr)) (get-register vm (cadr lexpr)))
    (progn 
      (setf (get vm 'DE) 0)
      (setf (get vm 'DPG) 0)
      (setf (get vm 'DPP) 1)))
   (( > (get-register vm (car lexpr)) (get-register vm (cadr lexpr)))
    (progn 
      (setf (get vm 'DE) 0)
      (setf (get vm 'DPG) 1)
      (setf (get vm 'DPP) 0)))

   (t
    (progn 
      (setf (get vm 'DE) 0)
      (setf (get vm 'DPG) 0)
      (setf (get vm 'DPP) 0)))))
       (t
  (progn 
    (setf (get vm 'DE) 0)
    (setf (get vm 'DPG) 0)
    (setf (get vm 'DPP) 0))))
    
    (if (get-register vm (car lexpr))
  (progn 
    (setf (get vm 'DE) 1)
    (setf (get vm 'DPG) 0)
    (setf (get vm 'DPP) 0))
      (progn 
  (setf (get vm 'DE) 0)
  (setf (get vm 'DPG) 0)
  (setf (get vm 'DPP) 0))))
  
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-jmp ( vm etiquette )
  (set-register vm 'PC etiquette))

(defun vm-exec-instr-jsr ( vm etiquette )
  (set-memory vm (get-register vm 'SP) (- (get-register vm 'PC) 1))
  (set-register vm 'SP (+ (get-register vm 'SP) 1))
  (if (listp etiquette)
      (vm-exec-instr-other vm (cadr etiquette))
    (vm-exec-instr-jmp vm etiquette)))

(defun vm-exec-instr-jne ( vm etiquette )
  (if (EQL (get-register vm 'DE) 0) 
      (set-register vm 'PC etiquette)
    (set-register vm 'PC (- (get-register vm 'PC) 1))))

(defun vm-exec-instr-jeq ( vm etiquette )
  (if (EQL (get-register vm 'DE) 1) 
      (set-register vm 'PC etiquette)
    (set-register vm 'PC (- (get-register vm 'PC) 1))))

(defun vm-exec-instr-jl ( vm etiquette )
  (if (EQL (get-register vm 'DPP) 1) 
      (set-register vm 'PC etiquette)
    (set-register vm 'PC (- (get-register vm 'PC) 1))))

(defun vm-exec-instr-jle ( vm etiquette )
  (if (or
       (EQL (get-register vm 'DE) 1)
       (EQL (get-register vm 'DPP) 1))    
      (set-register vm 'PC etiquette)
    (set-register vm 'PC (- (get-register vm 'PC) 1))))

(defun vm-exec-instr-jg ( vm etiquette )
  (if (EQL (get-register vm 'DPG) 1) 
      (set-register vm 'PC etiquette)
    (set-register vm 'PC (- (get-register vm 'PC) 1))))

(defun vm-exec-instr-jge ( vm etiquette )
  (if (or
       (EQL (get-register vm 'DE) 1)
       (EQL (get-register vm 'DPG) 1)) 
      (set-register vm 'PC etiquette)
    (set-register vm 'PC (- (get-register vm 'PC) 1))))

(defun vm-exec-instr-rtn (vm)
  (if (< (- (get-register vm 'SP) 1) 0)
      (error "[vm-exec-instr-rtn] Erreur")
    (progn
      (set-register vm 'SP (- (get-register vm 'SP) 1))
      (set-register vm 'PC (get-memory vm (get-register vm 'SP))))))

(defun vm-exec-instr-nop (vm)
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-halt (vm)
  (set-run vm nil))

(defun vm-exec-instr-car (vm lregister)
  (set-register vm (cadr lregister) (car (get-register vm (car lregister))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-cdr (vm lregister)
  (set-register vm (cadr lregister) (cdr (get-register vm (car lregister))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-setfget (vm lexpr)
  (set-register vm (caddr lexpr)
      (setf (get (get-register vm (car lexpr)) (get-register vm (cadr lexpr))) (get-register vm (caddr lexpr))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-setfhash (vm lexpr)  
  (set-register vm (caddr lexpr)
      (setf (gethash (get-register vm (car lexpr)) (get-register vm (cadr lexpr))) (get-register vm (caddr lexpr))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-setfaref (vm lexpr)  
  (set-register vm (caddr lexpr)
      (setf (aref (get-register vm (car lexpr)) (get-register vm (cadr lexpr))) (get-register vm (caddr lexpr))))
  (set-register vm 'PC (- (get-register vm 'PC) 1)))

(defun vm-exec-instr-other ( vm lexpr )
  (let ((liste (vm-other vm (get-memory vm (- (get-register vm 'FP) 1)) ())))
    (setf (get vm 'R0) (apply lexpr liste))
    (vm-exec-instr-rtn vm)))

(defun vm-load-label (vm ligne etiq)
  (if (atom (gethash etiq (get-hash vm 'PCETIQ)))
      (progn
  (setf (gethash etiq (get-hash vm 'PCETIQ)) (get-register vm 'BP))
  (if (gethash etiq (get-hash vm 'REF_AVANT))
      (ref_avant_vm vm ligne (gethash etiq (get-hash vm 'REF_AVANT)))))))

(defun vm-load-jump (vm ligne etiq saut)
  (progn
    (if (gethash etiq (get-hash vm 'PCETIQ))
  (set-memory vm (get-register vm 'BP) (list saut (gethash etiq (get-hash vm 'PCETIQ))))

      (if (EQL (gethash etiq (get-hash vm 'REF_AVANT)) NIL)
    (progn 
      (setf (gethash etiq (get-hash vm 'REF_AVANT)) (get-register vm 'BP))
      (set-memory vm (get-register vm 'BP) ligne))

  (progn
    (setf (gethash etiq (get-hash vm 'REF_AVANT)) (list (get-register vm 'BP)
                (gethash etiq (get-hash vm 'REF_AVANT))))
    (set-memory vm (get-register vm 'BP) ligne))))

    (set-register vm 'BP (- (get-register vm 'BP) 1))))
  
(defun vm-load-load (vm ligne etiq load reg)
  (progn
    (if (gethash etiq (get-hash vm 'PCETIQ))
  (set-memory vm (get-register vm 'BP) (list load (gethash etiq (get-hash vm 'PCETIQ)) reg))
     
      (if (EQL (gethash etiq (get-hash vm 'REF_AVANT)) NIL)
    (progn
      (setf (gethash etiq (get-hash vm 'REF_AVANT)) (get-register vm 'BP))
      (set-memory vm (get-register vm 'BP) ligne))
            
  (progn
    (setf (gethash etiq (get-hash vm 'REF_AVANT))
    (list(get-register vm 'BP) (gethash etiq (get-hash vm 'REF_AVANT)) reg))
    (set-memory vm (get-register vm 'BP) ligne))))

    (set-register vm 'BP (- (get-register vm 'BP) 1))))
  
(defun vm-load-store (vm ligne etiq store reg)
  (progn
    (if (gethash etiq (get-hash vm 'PCETIQ))
        (set-memory vm (get-register vm 'BP) (list store reg (gethash etiq (get-hash vm 'PCETIQ))))
      
      (if (EQL (gethash(car (cdaddr ligne)) (get-hash vm 'REF_AVANT)) NIL)
    (progn
      (setf (gethash etiq (get-hash vm 'REF_AVANT)) (get-register vm 'BP))
      (set-memory vm (get-register vm 'BP) ligne))
  
  (progn
    (setf (gethash etiq (get-hash vm 'REF_AVANT))
    (list(get-register vm 'BP) reg (gethash etiq (get-hash vm 'REF_AVANT))))
    (set-memory vm (get-register vm 'BP) ligne))))
    
    (set-register vm 'BP (- (get-register vm 'BP) 1))))

(defun vm-load (vm ligne)
  (if (EQL (car ligne) 'LABEL)
      (vm-load-label vm ligne (cadr ligne))

    (if (AND (jumpp (car ligne))
       (EQL (caadr ligne) '@))
  (vm-load-jump vm ligne (cadadr ligne) (car ligne))


      (if (AND (EQL 'LOAD (car ligne))
         (listp (cadr ligne))
         (EQL (caadr ligne) '@))
    (vm-load-load vm ligne (cadadr ligne) (car ligne) (caddr ligne))


  (if (AND (EQL 'STORE (car ligne))
     (listp (caddr ligne))
     (EQL (caaddr ligne) '@))
      (vm-load-store vm ligne (car (cdaddr ligne)) (car ligne) (cadr ligne))

    (progn
      (if (integerp (car ligne))
    (set-memory vm (get-register vm 'BP) (car ligne))
        (set-memory vm (get-register vm 'BP) ligne))
      (set-register vm 'BP (- (get-register vm 'BP) 1))))))))

(defun vm-run (vm position DEBUG)
  (if (> position 0)
      (set-register vm 'PC position)
    (set-register vm 'PC (- (size-memory vm) 1)))
  (set-debug vm DEBUG)
  (set-run vm t )
  
  (loop while (get-run vm)
  do
  (if (get-debug vm)
      (progn 
        (print "----------------------------")
        (print "Expression :" )
        (princ (get-memory vm (get-register vm 'PC)))))

  (vm-exec-instr vm (get-memory vm (get-register vm 'PC)))
      
  (if (get-debug vm)
      (progn 
        (print "value de R0 :" )
        (princ (get-register vm 'R0))
        (print "value de R1 :" )
        (princ (get-register vm 'R1))
        (print "value de R2 :" )
        (princ (get-register vm 'R2))
        (print "value de SP :" )
        (princ (get-register vm 'SP))
        (print "value de FP :" )
        (princ (get-register vm 'FP))
        (print "value de FE :" )
        (princ (get-register vm 'DE))
        (print "value de FL :" )
        (princ (get-register vm 'DPP))
        (print "value de FG :" )
        (princ (get-register vm 'DPG))
        (print "value de PC :" )
        (princ (get-register vm 'PC))
        (print "value de BP :" )
        (princ (get-register vm 'BP))
        (print-vm vm 0 (get-register vm 'SP))
        (print "----------------------------")
        (read-char)
        )))

  (if (EQL (get-run vm) nil)
      (progn
    (print "Resultat :" )
    (princ (get-register vm 'R0)))))

(defun ref_avant_vm (vm ligne ref)
  (let ((refAvant ref))
    (loop while refAvant
    do
    (cond
     ((listp refAvant)
      (progn
        (cond
         ((EQ (car(get-memory vm (car refAvant))) 'LOAD)
    (set-memory vm (car refAvant) (list (car(get-memory vm (car refAvant)))
                  (gethash (cadr ligne)(get-hash vm 'PCETIQ))
                  (nth 2 (get-memory vm (car refAvant))))))
         ((EQ (car(get-memory vm (car refAvant))) 'STORE)
    (set-memory vm (car refAvant) (list (car(get-memory vm (car refAvant)))
                 (nth 1 (get-memory vm (car refAvant)))
                 (gethash (cadr ligne)(get-hash vm 'PCETIQ)))))
         (t
    (set-memory vm (car refAvant) (list (car(get-memory vm (car refAvant)))
                  (gethash (cadr ligne)(get-hash vm 'PCETIQ))))))
        (if (EQL(length refAvant) 1)
      (setf refAvant (car refAvant))    
    (setf refAvant (cadr refAvant)))))
     (T
      (progn
        (cond
         ((EQ (car(get-memory vm refAvant)) 'LOAD)
    (set-memory vm refAvant (list (car(get-memory vm refAvant))
            (gethash (cadr ligne)(get-hash vm 'PCETIQ))
            (nth 2 (get-memory vm refAvant)))))
         ((EQ (car(get-memory vm refAvant)) 'STORE)
    (set-memory vm refAvant (list (car(get-memory vm refAvant))
            (nth 1 (get-memory vm refAvant))
            (gethash (cadr ligne)(get-hash vm 'PCETIQ)))))
         (t
    (set-memory vm refAvant (list (car(get-memory vm refAvant))
            (gethash (cadr ligne)(get-hash vm 'PCETIQ))))))
        (setf refAvant nil)))))))

(defun vm-other ( vm nbParam liste)
  (if (EQL (length liste) nbParam)
      liste
    (vm-other vm nbParam (cons (get-memory vm (- (get-register vm 'FP) (+ (length liste) 2))) liste))))

(defun print-vm (vm begin end)
  (loop for pos from begin to (- end 1)
  do
  (print pos)
  (princ " : ")
  (prin1 (get-memory vm pos))))

(defun registerp (expression)
  (member expression '(R0 R1 R2 FP SP FE FG FL PC)))

(defun jumpp (expression)
  (member expression '(jmp jsr jeq jneq jg jl jge jle)))

(defun vm-compiler-load-exec(vm file expr &optional DEBUG fichierAsm )
    (vm-compiler-load vm file fichierAsm)
    (vm-exec vm expr DEBUG))

(defun vm-exec(vm expr &optional DEBUG)
(if (not DEBUG)
  (setf DEBUG NIL))
  (let ((pos (get-register vm 'BP)))
    (vm-loader vm expr)
    (vm-run vm pos DEBUG)))

(defun vm-compiler-load (vm file &optional fichierAsm)
    (vm-compiler file fichierAsm)
    (if fichierAsm
    (vm-load-file vm fichierAsm)
    (vm-load-code vm)))

(defun vm-loader (vm expr)
  (loop for ligne in expr
  do
  (vm-load vm ligne)))

(defun vm-load-code (vm)
(setf pos 0)
(setf ligne (nth pos asmCode))
    (loop while (not (eql nil ligne)) do
        (vm-load vm ligne)
        (setf pos (+ 1 pos))
        (setf ligne (nth pos asmCode))))
      
(defun vm-load-file (vm fichier)
  (labels ((lecture (o)
        (let ((lu (read o nil nil nil)))
          (loop while lu
          do
          (vm-load vm lu)
          (setf lu (read o nil nil nil))))))
    (lecture (open fichier))))
