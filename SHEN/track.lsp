
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

(DEFUN shen.f_error (V4102)
  (do
   (shen.prhush
    (cn "partial function "
        (shen.app V4102 ";
"
         'shen.a))
    (stoutput))
   (do
    (IF (AND (NOT (shen-cl.true? (shen.tracked? V4102)))
             (shen-cl.true?
              (y-or-n? (cn "track " (shen.app V4102 "? " 'shen.a)))))
        (shen.track-function (ps V4102))
        'shen.ok)
    (simple-error "aborted"))))

(DEFUN shen.tracked? (V4104) (element? V4104 shen.*tracking*))

(DEFUN track (V4106)
  (LET ((Source (ps V4106)))
    (shen.track-function Source)))

(DEFUN shen.track-function (V4108)
  (COND
   ((AND (CONSP V4108)
         (AND (EQ 'defun (CAR V4108))
              (AND (CONSP (CDR V4108))
                   (AND (CONSP (CDR (CDR V4108)))
                        (AND (CONSP (CDR (CDR (CDR V4108))))
                             (NULL (CDR (CDR (CDR (CDR V4108))))))))))
    (LET ((KL
           (CONS 'defun
                 (CONS (CAR (CDR V4108))
                       (CONS (CAR (CDR (CDR V4108)))
                             (CONS
                              (shen.insert-tracking-code (CAR (CDR V4108))
                               (CAR (CDR (CDR V4108)))
                               (CAR (CDR (CDR (CDR V4108)))))
                              NIL))))))
      (LET ((Ob (eval-kl KL)))
        (LET ((Tr (set 'shen.*tracking* (CONS Ob shen.*tracking*))))
          Ob))))
   (T (shen.f_error 'shen.track-function))))

(DEFUN shen.insert-tracking-code (V4112 V4113 V4114)
  (CONS 'do
        (CONS
         (CONS 'set
               (CONS 'shen.*call*
                     (CONS
                      (CONS '+
                            (CONS (CONS 'value (CONS 'shen.*call* NIL))
                                  (CONS 1 NIL)))
                      NIL)))
         (CONS
          (CONS 'do
                (CONS
                 (CONS 'shen.input-track
                       (CONS (CONS 'value (CONS 'shen.*call* NIL))
                             (CONS V4112 (CONS (shen.cons_form V4113) NIL))))
                 (CONS
                  (CONS 'do
                        (CONS (CONS 'shen.terpri-or-read-char NIL)
                              (CONS
                               (CONS 'let
                                     (CONS 'Result
                                           (CONS V4114
                                                 (CONS
                                                  (CONS 'do
                                                        (CONS
                                                         (CONS
                                                          'shen.output-track
                                                          (CONS
                                                           (CONS 'value
                                                                 (CONS
                                                                  'shen.*call*
                                                                  NIL))
                                                           (CONS V4112
                                                                 (CONS 'Result
                                                                       NIL))))
                                                         (CONS
                                                          (CONS 'do
                                                                (CONS
                                                                 (CONS 'set
                                                                       (CONS
                                                                        'shen.*call*
                                                                        (CONS
                                                                         (CONS
                                                                          '-
                                                                          (CONS
                                                                           (CONS
                                                                            'value
                                                                            (CONS
                                                                             'shen.*call*
                                                                             NIL))
                                                                           (CONS
                                                                            1
                                                                            NIL)))
                                                                         NIL)))
                                                                 (CONS
                                                                  (CONS 'do
                                                                        (CONS
                                                                         (CONS
                                                                          'shen.terpri-or-read-char
                                                                          NIL)
                                                                         (CONS
                                                                          'Result
                                                                          NIL)))
                                                                  NIL)))
                                                          NIL)))
                                                  NIL))))
                               NIL)))
                  NIL)))
          NIL))))

(set 'shen.*step* 'false)

(DEFUN step (V4120)
  (COND ((EQ '+ V4120) (set 'shen.*step* 'true))
        ((EQ '- V4120) (set 'shen.*step* 'false))
        (T
         (simple-error "step expects a + or a -.
"))))

(DEFUN spy (V4126)
  (COND ((EQ '+ V4126) (set 'shen.*spy* 'true))
        ((EQ '- V4126) (set 'shen.*spy* 'false))
        (T
         (simple-error "spy expects a + or a -.
"))))

(DEFUN shen.terpri-or-read-char ()
  (IF (shen-cl.true? shen.*step*)
      (shen.check-byte (read-byte *stinput*))
      (nl 1)))

(DEFUN shen.check-byte (V4132)
  (COND ((shen.ABSEQUAL V4132 (shen.hat)) (simple-error "aborted")) (T 'true)))

(DEFUN shen.input-track (V4136 V4137 V4138)
  (do
   (shen.prhush
    (cn "
"
        (shen.app (shen.spaces V4136)
         (cn "<"
             (shen.app V4136
              (cn "> Inputs to "
                  (shen.app V4137
                   (cn " 
"
                       (shen.app (shen.spaces V4136) "" 'shen.a))
                   'shen.a))
              'shen.a))
         'shen.a))
    (stoutput))
   (shen.recursively-print V4138)))

(DEFUN shen.recursively-print (V4140)
  (COND ((NULL V4140) (shen.prhush " ==>" (stoutput)))
        ((CONSP V4140)
         (do (print (CAR V4140))
             (do (shen.prhush ", " (stoutput))
                 (shen.recursively-print (CDR V4140)))))
        (T (shen.f_error 'shen.recursively-print))))

(DEFUN shen.spaces (V4142)
  (COND
   ((IF (NUMBERP V4142)
        (= V4142 0))
    "")
   (T (cn " " (shen.spaces (shen.subtract V4142 1))))))

(DEFUN shen.output-track (V4146 V4147 V4148)
  (shen.prhush
   (cn "
"
       (shen.app (shen.spaces V4146)
        (cn "<"
            (shen.app V4146
             (cn "> Output from "
                 (shen.app V4147
                  (cn " 
"
                      (shen.app (shen.spaces V4146)
                       (cn "==> " (shen.app V4148 "" 'shen.s)) 'shen.a))
                  'shen.a))
             'shen.a))
        'shen.a))
   (stoutput)))

(DEFUN untrack (V4150)
  (LET ((Tracking shen.*tracking*))
    (LET ((Tracking (set 'shen.*tracking* (remove V4150 Tracking))))
      (eval (ps V4150)))))

(DEFUN profile (V4152) (shen.profile-help (ps V4152)))

(DEFUN shen.profile-help (V4158)
  (COND
   ((AND (CONSP V4158)
         (AND (EQ 'defun (CAR V4158))
              (AND (CONSP (CDR V4158))
                   (AND (CONSP (CDR (CDR V4158)))
                        (AND (CONSP (CDR (CDR (CDR V4158))))
                             (NULL (CDR (CDR (CDR (CDR V4158))))))))))
    (LET ((G (gensym 'shen.f)))
      (LET ((Profile
             (CONS 'defun
                   (CONS (CAR (CDR V4158))
                         (CONS (CAR (CDR (CDR V4158)))
                               (CONS
                                (shen.profile-func (CAR (CDR V4158))
                                 (CAR (CDR (CDR V4158)))
                                 (CONS G (CAR (CDR (CDR V4158)))))
                                NIL))))))
        (LET ((Def
               (CONS 'defun
                     (CONS G
                           (CONS (CAR (CDR (CDR V4158)))
                                 (CONS
                                  (subst G (CAR (CDR V4158))
                                         (CAR (CDR (CDR (CDR V4158)))))
                                  NIL))))))
          (LET ((CompileProfile (shen.eval-without-macros Profile)))
            (LET ((CompileG (shen.eval-without-macros Def)))
              (CAR (CDR V4158))))))))
   (T
    (simple-error "Cannot profile.
"))))

(DEFUN unprofile (V4160) (untrack V4160))

(DEFUN shen.profile-func (V4164 V4165 V4166)
  (CONS 'let
        (CONS 'Start
              (CONS (CONS 'get-time (CONS 'run NIL))
                    (CONS
                     (CONS 'let
                           (CONS 'Result
                                 (CONS V4166
                                       (CONS
                                        (CONS 'let
                                              (CONS 'Finish
                                                    (CONS
                                                     (CONS '-
                                                           (CONS
                                                            (CONS 'get-time
                                                                  (CONS 'run
                                                                        NIL))
                                                            (CONS 'Start NIL)))
                                                     (CONS
                                                      (CONS 'let
                                                            (CONS 'Record
                                                                  (CONS
                                                                   (CONS
                                                                    'shen.put-profile
                                                                    (CONS V4164
                                                                          (CONS
                                                                           (CONS
                                                                            '+
                                                                            (CONS
                                                                             (CONS
                                                                              'shen.get-profile
                                                                              (CONS
                                                                               V4164
                                                                               NIL))
                                                                             (CONS
                                                                              'Finish
                                                                              NIL)))
                                                                           NIL)))
                                                                   (CONS
                                                                    'Result
                                                                    NIL))))
                                                      NIL))))
                                        NIL))))
                     NIL)))))

(DEFUN profile-results (V4168)
  (LET ((Results (shen.get-profile V4168)))
    (LET ((Initialise (shen.put-profile V4168 0)))
      (@p V4168 Results))))

(DEFUN shen.get-profile (V4170)
  (get/or V4170 'profile (freeze 0) *property-vector*))

(DEFUN shen.put-profile (V4173 V4174)
  (put V4173 'profile V4174 *property-vector*))

