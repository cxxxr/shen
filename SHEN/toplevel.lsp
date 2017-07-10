
"Copyright (c) 2015, Mark Tarver

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of Mark Tarver may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Mark Tarver ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Mark Tarver BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."

(DEFUN shen.shen () (do (shen.credits) (shen.loop)))

(set 'shen.*continue-repl-loop* 'true)

(DEFUN exit (V3989) (set 'shen.*continue-repl-loop* 'false))

(DEFUN shen.loop ()
  (do (shen.initialise_environment)
   (do (shen.prompt)
    (do
     (trap-error (shen.read-evaluate-print)
                 #'(LAMBDA (E) (pr (error-to-string E) (stoutput))))
     (IF (shen-cl.true? shen.*continue-repl-loop*)
         (shen.loop)
         'exit)))))

(DEFUN shen.credits ()
  (do
   (shen.prhush "
Shen, copyright (C) 2010-2015 Mark Tarver
"
    (stoutput))
   (do
    (shen.prhush
     (cn "www.shenlanguage.org, "
         (shen.app *version* "
"
          'shen.a))
     (stoutput))
    (do
     (shen.prhush
      (cn "running under "
          (shen.app *language*
           (cn ", implementation: " (shen.app *implementation* "" 'shen.a))
           'shen.a))
      (stoutput))
     (shen.prhush
      (cn "
port "
          (shen.app *port*
           (cn " ported by "
               (shen.app *porters* "
"
                'shen.a))
           'shen.a))
      (stoutput))))))

(DEFUN shen.initialise_environment ()
  (shen.multiple-set
   (CONS 'shen.*call*
         (CONS 0
               (CONS 'shen.*infs*
                     (CONS 0
                           (CONS 'shen.*process-counter*
                                 (CONS 0
                                       (CONS 'shen.*catch* (CONS 0 NIL))))))))))

(DEFUN shen.multiple-set (V3991)
  (COND ((NULL V3991) NIL)
        ((AND (CONSP V3991) (CONSP (CDR V3991)))
         (do (set (CAR V3991) (CAR (CDR V3991)))
          (shen.multiple-set (CDR (CDR V3991)))))
        (T (shen.f_error 'shen.multiple-set))))

(DEFUN destroy (V3993) (declare V3993 'symbol))

(set 'shen.*history* NIL)

(DEFUN shen.read-evaluate-print ()
  (LET ((Lineread (shen.toplineread)))
    (LET ((History shen.*history*))
      (LET ((NewLineread
             (shen.retrieve-from-history-if-needed Lineread History)))
        (LET ((NewHistory (shen.update_history NewLineread History)))
          (LET ((Parsed (fst NewLineread)))
            (shen.toplevel Parsed)))))))

(DEFUN shen.retrieve-from-history-if-needed (V4005 V4006)
  (COND
   ((AND (shen-cl.true? (tuple? V4005))
         (AND (CONSP (snd V4005))
              (shen-cl.true?
               (element? (CAR (snd V4005))
                (CONS (shen.space) (CONS (shen.newline) NIL))))))
    (shen.retrieve-from-history-if-needed (@p (fst V4005) (CDR (snd V4005)))
     V4006))
   ((AND (shen-cl.true? (tuple? V4005))
         (AND (CONSP (snd V4005))
              (AND (CONSP (CDR (snd V4005)))
                   (AND (NULL (CDR (CDR (snd V4005))))
                        (AND (CONSP V4006)
                             (AND
                              (shen.ABSEQUAL (CAR (snd V4005))
                                             (shen.exclamation))
                              (shen.ABSEQUAL (CAR (CDR (snd V4005)))
                                             (shen.exclamation))))))))
    (LET ((PastPrint (shen.prbytes (snd (CAR V4006)))))
      (CAR V4006)))
   ((AND (shen-cl.true? (tuple? V4005))
         (AND (CONSP (snd V4005))
              (shen.ABSEQUAL (CAR (snd V4005)) (shen.exclamation))))
    (LET ((Key? (shen.make-key (CDR (snd V4005)) V4006)))
      (LET ((Find (head (shen.find-past-inputs Key? V4006))))
        (LET ((PastPrint (shen.prbytes (snd Find))))
          Find))))
   ((AND (shen-cl.true? (tuple? V4005))
         (AND (CONSP (snd V4005))
              (AND (NULL (CDR (snd V4005)))
                   (shen.ABSEQUAL (CAR (snd V4005)) (shen.percent)))))
    (do (shen.print-past-inputs #'(LAMBDA (X) 'true) (REVERSE V4006) 0)
     (abort)))
   ((AND (shen-cl.true? (tuple? V4005))
         (AND (CONSP (snd V4005))
              (shen.ABSEQUAL (CAR (snd V4005)) (shen.percent))))
    (LET ((Key? (shen.make-key (CDR (snd V4005)) V4006)))
      (LET ((Pastprint (shen.print-past-inputs Key? (REVERSE V4006) 0)))
        (abort))))
   (T V4005)))

(DEFUN shen.percent () 37)

(DEFUN shen.exclamation () 33)

(DEFUN shen.prbytes (V4008)
  (do (for-each #'(LAMBDA (Byte) (pr (n->string Byte) (stoutput))) V4008)
   (nl 1)))

(DEFUN shen.update_history (V4011 V4012)
  (set 'shen.*history* (CONS V4011 V4012)))

(DEFUN shen.toplineread ()
  (shen.toplineread_loop (read-char-code (stinput)) NIL))

(DEFUN shen.toplineread_loop (V4016 V4017)
  (COND
   ((AND
     (IF (NUMBERP V4016)
         (= V4016 -1))
     (NULL V4017))
    (exit 0))
   ((shen.ABSEQUAL V4016 (shen.hat)) (simple-error "line read aborted"))
   ((shen-cl.true?
     (element? V4016 (CONS (shen.newline) (CONS (shen.carriage-return) NIL))))
    (LET ((Line
           (compile #'(LAMBDA (X) (shen.<st_input> X)) V4017
            #'(LAMBDA (E) 'shen.nextline))))
      (LET ((It (shen.record-it V4017)))
        (IF (OR (EQ Line 'shen.nextline) (NULL Line))
            (shen.toplineread_loop (read-char-code (stinput))
             (APPEND V4017 (CONS V4016 NIL)))
            (@p Line V4017)))))
   (T
    (shen.toplineread_loop (read-char-code (stinput))
     (IF (IF (NUMBERP V4016)
             (= V4016 -1))
         V4017
         (APPEND V4017 (CONS V4016 NIL)))))))

(DEFUN shen.hat () 94)

(DEFUN shen.newline () 10)

(DEFUN shen.carriage-return () 13)

(DEFUN tc (V4023)
  (COND ((EQ '+ V4023) (set 'shen.*tc* 'true))
        ((EQ '- V4023) (set 'shen.*tc* 'false))
        (T (simple-error "tc expects a + or -"))))

(DEFUN shen.prompt ()
  (IF (shen-cl.true? shen.*tc*)
      (shen.prhush
       (cn "

("
           (shen.app (length shen.*history*) "+) " 'shen.a))
       (stoutput))
      (shen.prhush
       (cn "

("
           (shen.app (length shen.*history*) "-) " 'shen.a))
       (stoutput))))

(DEFUN shen.toplevel (V4025) (shen.toplevel_evaluate V4025 shen.*tc*))

(DEFUN shen.find-past-inputs (V4028 V4029)
  (LET ((F (shen.find V4028 V4029)))
    (IF (NULL F)
        (simple-error "input not found
")
        F)))

(DEFUN shen.make-key (V4032 V4033)
  (LET ((Atom
         (CAR
          (compile #'(LAMBDA (X) (shen.<st_input> X)) V4032
           #'(LAMBDA (E)
               (IF (CONSP E)
                   (simple-error
                    (cn "parse error here: "
                        (shen.app E "
"
                         'shen.s)))
                   (simple-error "parse error
")))))))
    (IF (shen-cl.true? (integer? Atom))
        #'(LAMBDA (X) (shen.equal? X (nth (shen.add Atom 1) (REVERSE V4033))))
        #'(LAMBDA (X) (shen.prefix? V4032 (shen.trim-gubbins (snd X)))))))

(DEFUN shen.trim-gubbins (V4035)
  (COND
   ((AND (CONSP V4035) (shen.ABSEQUAL (CAR V4035) (shen.space)))
    (shen.trim-gubbins (CDR V4035)))
   ((AND (CONSP V4035) (shen.ABSEQUAL (CAR V4035) (shen.newline)))
    (shen.trim-gubbins (CDR V4035)))
   ((AND (CONSP V4035) (shen.ABSEQUAL (CAR V4035) (shen.carriage-return)))
    (shen.trim-gubbins (CDR V4035)))
   ((AND (CONSP V4035) (shen.ABSEQUAL (CAR V4035) (shen.tab)))
    (shen.trim-gubbins (CDR V4035)))
   ((AND (CONSP V4035) (shen.ABSEQUAL (CAR V4035) (shen.left-round)))
    (shen.trim-gubbins (CDR V4035)))
   (T V4035)))

(DEFUN shen.space () 32)

(DEFUN shen.tab () 9)

(DEFUN shen.left-round () 40)

(DEFUN shen.find (V4044 V4045)
  (COND ((NULL V4045) NIL)
        ((AND (CONSP V4045)
              (shen-cl.true? (shen.apply V4044 (LIST (CAR V4045)))))
         (CONS (CAR V4045) (shen.find V4044 (CDR V4045))))
        ((CONSP V4045) (shen.find V4044 (CDR V4045)))
        (T (shen.f_error 'shen.find))))

(DEFUN shen.prefix? (V4059 V4060)
  (COND ((NULL V4059) 'true)
        ((AND (CONSP V4059)
              (AND (CONSP V4060) (shen.ABSEQUAL (CAR V4060) (CAR V4059))))
         (shen.prefix? (CDR V4059) (CDR V4060)))
        (T 'false)))

(DEFUN shen.print-past-inputs (V4072 V4073 V4074)
  (COND ((NULL V4073) '_)
        ((AND (CONSP V4073)
              (NOT (shen-cl.true? (shen.apply V4072 (LIST (CAR V4073))))))
         (shen.print-past-inputs V4072 (CDR V4073) (shen.add V4074 1)))
        ((AND (CONSP V4073) (shen-cl.true? (tuple? (CAR V4073))))
         (do (shen.prhush (shen.app V4074 ". " 'shen.a) (stoutput))
          (do (shen.prbytes (snd (CAR V4073)))
           (shen.print-past-inputs V4072 (CDR V4073) (shen.add V4074 1)))))
        (T (shen.f_error 'shen.print-past-inputs))))

(DEFUN shen.toplevel_evaluate (V4077 V4078)
  (COND
   ((AND (CONSP V4077)
         (AND (CONSP (CDR V4077))
              (AND (EQ '|:| (CAR (CDR V4077)))
                   (AND (CONSP (CDR (CDR V4077)))
                        (AND (NULL (CDR (CDR (CDR V4077))))
                             (EQ 'true V4078))))))
    (shen.typecheck-and-evaluate (CAR V4077) (CAR (CDR (CDR V4077)))))
   ((AND (CONSP V4077) (CONSP (CDR V4077)))
    (do (shen.toplevel_evaluate (CONS (CAR V4077) NIL) V4078)
     (do (nl 1) (shen.toplevel_evaluate (CDR V4077) V4078))))
   ((AND (CONSP V4077) (AND (NULL (CDR V4077)) (EQ 'true V4078)))
    (shen.typecheck-and-evaluate (CAR V4077) (gensym 'A)))
   ((AND (CONSP V4077) (AND (NULL (CDR V4077)) (EQ 'false V4078)))
    (LET ((Eval (shen.eval-without-macros (CAR V4077))))
      (print Eval)))
   (T (shen.f_error 'shen.toplevel_evaluate))))

(DEFUN shen.typecheck-and-evaluate (V4081 V4082)
  (LET ((Typecheck (shen.typecheck V4081 V4082)))
    (IF (EQ Typecheck 'false)
        (simple-error "type error
")
        (LET ((Eval (shen.eval-without-macros V4081)))
          (LET ((Type (shen.pretty-type Typecheck)))
            (shen.prhush
             (shen.app Eval (cn " : " (shen.app Type "" 'shen.r)) 'shen.s)
             (stoutput)))))))

(DEFUN shen.pretty-type (V4084)
  (shen.mult_subst shen.*alphabet* (shen.extract-pvars V4084) V4084))

(DEFUN shen.extract-pvars (V4090)
  (COND ((shen-cl.true? (shen.pvar? V4090)) (CONS V4090 NIL))
        ((CONSP V4090)
         (union (shen.extract-pvars (CAR V4090))
          (shen.extract-pvars (CDR V4090))))
        (T NIL)))

(DEFUN shen.mult_subst (V4098 V4099 V4100)
  (COND ((NULL V4098) V4100) ((NULL V4099) V4100)
        ((AND (CONSP V4098) (CONSP V4099))
         (shen.mult_subst (CDR V4098) (CDR V4099)
          (subst (CAR V4098) (CAR V4099) V4100)))
        (T (shen.f_error 'shen.mult_subst))))

