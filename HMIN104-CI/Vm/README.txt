Via un fichier asm :

(load "Vm/vm.lisp")

(make-vm 'vm 10000)

(vm-compiler "Vm/functions.lisp" "Vm/asmFunctions.asm")

(vm-load-file 'vm "Vm/asmFunctions.asm")

(vm-exec 'vm (inAsm '(fibo 5)) )

OU

(load "Vm/vm.lisp")

(make-vm 'vm 10000)

(vm-compiler-load 'vm "Vm/functions.lisp" "Vm/asm2.asm")

(vm-exec 'vm (inAsm '(fibo 5)) )

OU

(load "Vm/vm.lisp")

(make-vm 'vm 10000)

(vm-compiler-load-exec 'vm "Vm/functions.lisp" (inAsm '(fibo 5))"Vm/asm3.asm")



Sans fichier :

(load "Vm/vm.lisp")

(make-vm 'vm 10000)

(vm-compiler "Vm/functions.lisp")

(vm-load-code 'vm)

(vm-exec 'vm (inAsm '(fibo 5)))

OU

(load "Vm/vm.lisp")

(make-vm 'vm 10000)

(vm-compiler-load 'vm "Vm/functions.lisp")

(vm-exec 'vm (inAsm '(fibo 5)) )

OU

(load "Vm/vm.lisp")

(make-vm 'vm 10000)

(vm-compiler-load-exec 'vm "Vm/functions.lisp" (inAsm '(fibo 5)) )