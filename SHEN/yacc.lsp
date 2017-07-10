
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

(DEFUN shen.yacc (V4329)
  (COND
   ((AND (CONSP V4329) (AND (EQ 'defcc (CAR V4329)) (CONSP (CDR V4329))))
    (shen.yacc->shen (CAR (CDR V4329)) (CDR (CDR V4329))))
   (T (shen.f_error 'shen.yacc))))

(DEFUN shen.yacc->shen (V4332 V4333)
  (LET ((CCRules (shen.split_cc_rules 'true V4333 NIL)))
    (LET ((CCBody (map #'(LAMBDA (X) (shen.cc_body X)) CCRules)))
      (LET ((YaccCases (shen.yacc_cases CCBody)))
        (CONS 'define
              (CONS V4332
                    (CONS 'Stream
                          (CONS '->
                                (CONS (shen.kill-code YaccCases) NIL)))))))))

(DEFUN shen.kill-code (V4335)
  (COND
   ((> (occurrences 'kill V4335) 0)
    (CONS 'trap-error
          (CONS V4335
                (CONS
                 (CONS 'lambda
                       (CONS 'E
                             (CONS (CONS 'shen.analyse-kill (CONS 'E NIL))
                                   NIL)))
                 NIL))))
   (T V4335)))

(DEFUN kill () (simple-error "yacc kill"))

(DEFUN shen.analyse-kill (V4337)
  (LET ((String (error-to-string V4337)))
    (IF (EQUAL String "yacc kill")
        (fail)
        V4337)))

(DEFUN shen.split_cc_rules (V4343 V4344 V4345)
  (COND ((AND (NULL V4344) (NULL V4345)) NIL)
        ((NULL V4344)
         (CONS (shen.split_cc_rule V4343 (REVERSE V4345) NIL) NIL))
        ((AND (CONSP V4344) (EQ '|;| (CAR V4344)))
         (CONS (shen.split_cc_rule V4343 (REVERSE V4345) NIL)
               (shen.split_cc_rules V4343 (CDR V4344) NIL)))
        ((CONSP V4344)
         (shen.split_cc_rules V4343 (CDR V4344) (CONS (CAR V4344) V4345)))
        (T (shen.f_error 'shen.split_cc_rules))))

(DEFUN shen.split_cc_rule (V4353 V4354 V4355)
  (COND
   ((AND (CONSP V4354)
         (AND (EQ '|:=| (CAR V4354))
              (AND (CONSP (CDR V4354)) (NULL (CDR (CDR V4354))))))
    (CONS (REVERSE V4355) (CDR V4354)))
   ((AND (CONSP V4354)
         (AND (EQ '|:=| (CAR V4354))
              (AND (CONSP (CDR V4354))
                   (AND (CONSP (CDR (CDR V4354)))
                        (AND (EQ 'where (CAR (CDR (CDR V4354))))
                             (AND (CONSP (CDR (CDR (CDR V4354))))
                                  (NULL (CDR (CDR (CDR (CDR V4354)))))))))))
    (CONS (REVERSE V4355)
          (CONS
           (CONS 'where
                 (CONS (CAR (CDR (CDR (CDR V4354))))
                       (CONS (CAR (CDR V4354)) NIL)))
           NIL)))
   ((NULL V4354)
    (do (shen.semantic-completion-warning V4353 V4355)
        (shen.split_cc_rule V4353
         (CONS '|:=| (CONS (shen.default_semantics (REVERSE V4355)) NIL))
         V4355)))
   ((CONSP V4354)
    (shen.split_cc_rule V4353 (CDR V4354) (CONS (CAR V4354) V4355)))
   (T (shen.f_error 'shen.split_cc_rule))))

(DEFUN shen.semantic-completion-warning (V4366 V4367)
  (COND
   ((EQ 'true V4366)
    (do (shen.prhush "warning: " (stoutput))
        (do
         (for-each
          #'(LAMBDA (X) (shen.prhush (shen.app X " " 'shen.a) (stoutput)))
          (REVERSE V4367))
         (shen.prhush "has no semantics.
"
          (stoutput)))))
   (T 'shen.skip)))

(DEFUN shen.default_semantics (V4369)
  (COND ((NULL V4369) NIL)
        ((AND (CONSP V4369)
              (AND (NULL (CDR V4369))
                   (shen-cl.true? (shen.grammar_symbol? (CAR V4369)))))
         (CAR V4369))
        ((AND (CONSP V4369) (shen-cl.true? (shen.grammar_symbol? (CAR V4369))))
         (CONS 'append
               (CONS (CAR V4369)
                     (CONS (shen.default_semantics (CDR V4369)) NIL))))
        ((CONSP V4369)
         (CONS 'cons
               (CONS (CAR V4369)
                     (CONS (shen.default_semantics (CDR V4369)) NIL))))
        (T (shen.f_error 'shen.default_semantics))))

(DEFUN shen.grammar_symbol? (V4371)
  (and (symbol? V4371)
       (LET ((Cs (shen.strip-pathname (explode V4371))))
         (and (shen.equal? (CAR Cs) "<")
              (shen.equal? (CAR (REVERSE Cs)) ">")))))

(DEFUN shen.yacc_cases (V4373)
  (COND ((AND (CONSP V4373) (NULL (CDR V4373))) (CAR V4373))
        ((CONSP V4373)
         (LET ((P 'YaccParse))
           (CONS 'let
                 (CONS P
                       (CONS (CAR V4373)
                             (CONS
                              (CONS 'if
                                    (CONS
                                     (CONS '=
                                           (CONS P
                                                 (CONS (CONS 'fail NIL) NIL)))
                                     (CONS (shen.yacc_cases (CDR V4373))
                                           (CONS P NIL))))
                              NIL))))))
        (T (shen.f_error 'shen.yacc_cases))))

(DEFUN shen.cc_body (V4375)
  (COND
   ((AND (CONSP V4375) (AND (CONSP (CDR V4375)) (NULL (CDR (CDR V4375)))))
    (shen.syntax (CAR V4375) 'Stream (CAR (CDR V4375))))
   (T (shen.f_error 'shen.cc_body))))

(DEFUN shen.syntax (V4379 V4380 V4381)
  (COND
   ((AND (NULL V4379)
         (AND (CONSP V4381)
              (AND (EQ 'where (CAR V4381))
                   (AND (CONSP (CDR V4381))
                        (AND (CONSP (CDR (CDR V4381)))
                             (NULL (CDR (CDR (CDR V4381)))))))))
    (CONS 'if
          (CONS (shen.semantics (CAR (CDR V4381)))
                (CONS
                 (CONS 'shen.pair
                       (CONS (CONS 'hd (CONS V4380 NIL))
                             (CONS (shen.semantics (CAR (CDR (CDR V4381))))
                                   NIL)))
                 (CONS (CONS 'fail NIL) NIL)))))
   ((NULL V4379)
    (CONS 'shen.pair
          (CONS (CONS 'hd (CONS V4380 NIL))
                (CONS (shen.semantics V4381) NIL))))
   ((CONSP V4379)
    (IF (shen-cl.true? (shen.grammar_symbol? (CAR V4379)))
        (shen.recursive_descent V4379 V4380 V4381)
        (IF (shen-cl.true? (variable? (CAR V4379)))
            (shen.variable-match V4379 V4380 V4381)
            (IF (shen-cl.true? (shen.jump_stream? (CAR V4379)))
                (shen.jump_stream V4379 V4380 V4381)
                (IF (shen-cl.true? (shen.terminal? (CAR V4379)))
                    (shen.check_stream V4379 V4380 V4381)
                    (IF (CONSP (CAR V4379))
                        (shen.list-stream (shen.decons (CAR V4379)) (CDR V4379)
                         V4380 V4381)
                        (simple-error
                         (shen.app (CAR V4379) " is not legal syntax
"
                          'shen.a))))))))
   (T (shen.f_error 'shen.syntax))))

(DEFUN shen.list-stream (V4386 V4387 V4388 V4389)
  (LET ((Test
         (CONS 'and
               (CONS (CONS 'cons? (CONS (CONS 'hd (CONS V4388 NIL)) NIL))
                     (CONS
                      (CONS 'cons?
                            (CONS
                             (CONS 'hd (CONS (CONS 'hd (CONS V4388 NIL)) NIL))
                             NIL))
                      NIL)))))
    (LET ((Placeholder (gensym 'shen.place)))
      (LET ((RunOn
             (shen.syntax V4387
              (CONS 'shen.pair
                    (CONS (CONS 'tl (CONS (CONS 'hd (CONS V4388 NIL)) NIL))
                          (CONS
                           (CONS 'hd (CONS (CONS 'tl (CONS V4388 NIL)) NIL))
                           NIL)))
              V4389)))
        (LET ((Action
               (shen.insert-runon RunOn Placeholder
                (shen.syntax V4386
                 (CONS 'shen.pair
                       (CONS (CONS 'hd (CONS (CONS 'hd (CONS V4388 NIL)) NIL))
                             (CONS
                              (CONS 'hd (CONS (CONS 'tl (CONS V4388 NIL)) NIL))
                              NIL)))
                 Placeholder))))
          (CONS 'if (CONS Test (CONS Action (CONS (CONS 'fail NIL) NIL)))))))))

(DEFUN shen.decons (V4391)
  (COND
   ((AND (CONSP V4391)
         (AND (EQ 'cons (CAR V4391))
              (AND (CONSP (CDR V4391))
                   (AND (CONSP (CDR (CDR V4391)))
                        (AND (NULL (CAR (CDR (CDR V4391))))
                             (NULL (CDR (CDR (CDR V4391)))))))))
    (CONS (CAR (CDR V4391)) NIL))
   ((AND (CONSP V4391)
         (AND (EQ 'cons (CAR V4391))
              (AND (CONSP (CDR V4391))
                   (AND (CONSP (CDR (CDR V4391)))
                        (NULL (CDR (CDR (CDR V4391))))))))
    (CONS (CAR (CDR V4391)) (shen.decons (CAR (CDR (CDR V4391))))))
   (T V4391)))

(DEFUN shen.insert-runon (V4406 V4407 V4408)
  (COND
   ((AND (CONSP V4408)
         (AND (EQ 'shen.pair (CAR V4408))
              (AND (CONSP (CDR V4408))
                   (AND (CONSP (CDR (CDR V4408)))
                        (AND (NULL (CDR (CDR (CDR V4408))))
                             (shen.ABSEQUAL (CAR (CDR (CDR V4408))) V4407))))))
    V4406)
   ((CONSP V4408) (map #'(LAMBDA (Z) (shen.insert-runon V4406 V4407 Z)) V4408))
   (T V4408)))

(DEFUN shen.strip-pathname (V4414)
  (COND ((NOT (shen-cl.true? (element? "." V4414))) V4414)
        ((CONSP V4414) (shen.strip-pathname (CDR V4414)))
        (T (shen.f_error 'shen.strip-pathname))))

(DEFUN shen.recursive_descent (V4418 V4419 V4420)
  (COND
   ((CONSP V4418)
    (LET ((Test (CONS (CAR V4418) (CONS V4419 NIL))))
      (LET ((Action
             (shen.syntax (CDR V4418) (concat 'Parse_ (CAR V4418)) V4420)))
        (LET ((Else (CONS 'fail NIL)))
          (CONS 'let
                (CONS (concat 'Parse_ (CAR V4418))
                      (CONS Test
                            (CONS
                             (CONS 'if
                                   (CONS
                                    (CONS 'not
                                          (CONS
                                           (CONS '=
                                                 (CONS (CONS 'fail NIL)
                                                       (CONS
                                                        (concat 'Parse_
                                                                (CAR V4418))
                                                        NIL)))
                                           NIL))
                                    (CONS Action (CONS Else NIL))))
                             NIL))))))))
   (T (shen.f_error 'shen.recursive_descent))))

(DEFUN shen.variable-match (V4424 V4425 V4426)
  (COND
   ((CONSP V4424)
    (LET ((Test (CONS 'cons? (CONS (CONS 'hd (CONS V4425 NIL)) NIL))))
      (LET ((Action
             (CONS 'let
                   (CONS (concat 'Parse_ (CAR V4424))
                         (CONS
                          (CONS 'hd (CONS (CONS 'hd (CONS V4425 NIL)) NIL))
                          (CONS
                           (shen.syntax (CDR V4424)
                            (CONS 'shen.pair
                                  (CONS
                                   (CONS 'tl
                                         (CONS (CONS 'hd (CONS V4425 NIL))
                                               NIL))
                                   (CONS (CONS 'shen.hdtl (CONS V4425 NIL))
                                         NIL)))
                            V4426)
                           NIL))))))
        (LET ((Else (CONS 'fail NIL)))
          (CONS 'if (CONS Test (CONS Action (CONS Else NIL))))))))
   (T (shen.f_error 'shen.variable-match))))

(DEFUN shen.terminal? (V4436)
  (COND ((CONSP V4436) 'false) ((shen-cl.true? (variable? V4436)) 'false)
        (T 'true)))

(DEFUN shen.jump_stream? (V4442) (COND ((EQ V4442 '_) 'true) (T 'false)))

(DEFUN shen.check_stream (V4446 V4447 V4448)
  (COND
   ((CONSP V4446)
    (LET ((Test
           (CONS 'and
                 (CONS (CONS 'cons? (CONS (CONS 'hd (CONS V4447 NIL)) NIL))
                       (CONS
                        (CONS '=
                              (CONS (CAR V4446)
                                    (CONS
                                     (CONS 'hd
                                           (CONS (CONS 'hd (CONS V4447 NIL))
                                                 NIL))
                                     NIL)))
                        NIL)))))
      (LET ((Action
             (shen.syntax (CDR V4446)
              (CONS 'shen.pair
                    (CONS (CONS 'tl (CONS (CONS 'hd (CONS V4447 NIL)) NIL))
                          (CONS (CONS 'shen.hdtl (CONS V4447 NIL)) NIL)))
              V4448)))
        (LET ((Else (CONS 'fail NIL)))
          (CONS 'if (CONS Test (CONS Action (CONS Else NIL))))))))
   (T (shen.f_error 'shen.check_stream))))

(DEFUN shen.jump_stream (V4452 V4453 V4454)
  (COND
   ((CONSP V4452)
    (LET ((Test (CONS 'cons? (CONS (CONS 'hd (CONS V4453 NIL)) NIL))))
      (LET ((Action
             (shen.syntax (CDR V4452)
              (CONS 'shen.pair
                    (CONS (CONS 'tl (CONS (CONS 'hd (CONS V4453 NIL)) NIL))
                          (CONS (CONS 'shen.hdtl (CONS V4453 NIL)) NIL)))
              V4454)))
        (LET ((Else (CONS 'fail NIL)))
          (CONS 'if (CONS Test (CONS Action (CONS Else NIL))))))))
   (T (shen.f_error 'shen.jump_stream))))

(DEFUN shen.semantics (V4456)
  (COND ((NULL V4456) NIL)
        ((shen-cl.true? (shen.grammar_symbol? V4456))
         (CONS 'shen.hdtl (CONS (concat 'Parse_ V4456) NIL)))
        ((shen-cl.true? (variable? V4456)) (concat 'Parse_ V4456))
        ((CONSP V4456) (map #'(LAMBDA (Z) (shen.semantics Z)) V4456))
        (T V4456)))

(DEFUN shen.snd-or-fail (V4464)
  (COND
   ((AND (CONSP V4464) (AND (CONSP (CDR V4464)) (NULL (CDR (CDR V4464)))))
    (CAR (CDR V4464)))
   (T (fail))))

(DEFUN fail () 'shen.fail!)

(DEFUN shen.pair (V4467 V4468) (CONS V4467 (CONS V4468 NIL)))

(DEFUN shen.hdtl (V4470) (CAR (CDR V4470)))

(DEFUN <!> (V4478)
  (COND
   ((AND (CONSP V4478) (AND (CONSP (CDR V4478)) (NULL (CDR (CDR V4478)))))
    (CONS NIL (CONS (CAR V4478) NIL)))
   (T (fail))))

(DEFUN <e> (V4484)
  (COND
   ((AND (CONSP V4484) (AND (CONSP (CDR V4484)) (NULL (CDR (CDR V4484)))))
    (CONS (CAR V4484) (CONS NIL NIL)))
   (T (shen.f_error '<e>))))

