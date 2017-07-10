
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

(DEFUN thaw (V2821) (shen.apply V2821 (LIST)))

(DEFUN eval (V2823)
  (LET ((Macroexpand (shen.walk #'(LAMBDA (Y) (macroexpand Y)) V2823)))
    (IF (shen-cl.true? (shen.packaged? Macroexpand))
        (map #'(LAMBDA (Z) (shen.eval-without-macros Z))
         (shen.package-contents Macroexpand))
        (shen.eval-without-macros Macroexpand))))

(DEFUN shen.eval-without-macros (V2825)
  (eval-kl (shen.elim-def (shen.proc-input+ V2825))))

(DEFUN shen.proc-input+ (V2827)
  (COND
   ((AND (CONSP V2827)
         (AND (EQ 'input+ (CAR V2827))
              (AND (CONSP (CDR V2827))
                   (AND (CONSP (CDR (CDR V2827)))
                        (NULL (CDR (CDR (CDR V2827))))))))
    (CONS 'input+
          (CONS (shen.rcons_form (CAR (CDR V2827))) (CDR (CDR V2827)))))
   ((AND (CONSP V2827)
         (AND (EQ 'shen.read+ (CAR V2827))
              (AND (CONSP (CDR V2827))
                   (AND (CONSP (CDR (CDR V2827)))
                        (NULL (CDR (CDR (CDR V2827))))))))
    (CONS 'shen.read+
          (CONS (shen.rcons_form (CAR (CDR V2827))) (CDR (CDR V2827)))))
   ((CONSP V2827) (map #'(LAMBDA (Z) (shen.proc-input+ Z)) V2827)) (T V2827)))

(DEFUN shen.elim-def (V2829)
  (COND
   ((AND (CONSP V2829) (AND (EQ 'define (CAR V2829)) (CONSP (CDR V2829))))
    (shen.shen->kl (CAR (CDR V2829)) (CDR (CDR V2829))))
   ((AND (CONSP V2829) (AND (EQ 'defmacro (CAR V2829)) (CONSP (CDR V2829))))
    (LET ((Default (CONS 'X (CONS '-> (CONS 'X NIL)))))
      (LET ((Def
             (shen.elim-def
              (CONS 'define
                    (CONS (CAR (CDR V2829))
                          (APPEND (CDR (CDR V2829)) Default))))))
        (LET ((MacroAdd (shen.add-macro (CAR (CDR V2829)))))
          Def))))
   ((AND (CONSP V2829) (AND (EQ 'defcc (CAR V2829)) (CONSP (CDR V2829))))
    (shen.elim-def (shen.yacc V2829)))
   ((CONSP V2829) (map #'(LAMBDA (Z) (shen.elim-def Z)) V2829)) (T V2829)))

(DEFUN shen.add-macro (V2831)
  (LET ((MacroReg shen.*macroreg*))
    (LET ((NewMacroReg (set 'shen.*macroreg* (adjoin V2831 shen.*macroreg*))))
      (IF (shen.ABSEQUAL MacroReg NewMacroReg)
          'shen.skip
          (set '*macros* (CONS (function V2831) *macros*))))))

(DEFUN shen.packaged? (V2839)
  (COND
   ((AND (CONSP V2839)
         (AND (EQ 'package (CAR V2839))
              (AND (CONSP (CDR V2839)) (CONSP (CDR (CDR V2839))))))
    'true)
   (T 'false)))

(DEFUN external (V2841)
  (get/or V2841 'shen.external-symbols
   (freeze
    (simple-error
     (cn "package "
         (shen.app V2841 " has not been used.
"
          'shen.a))))
   *property-vector*))

(DEFUN internal (V2843)
  (get/or V2843 'shen.internal-symbols
   (freeze
    (simple-error
     (cn "package "
         (shen.app V2843 " has not been used.
"
          'shen.a))))
   *property-vector*))

(DEFUN shen.package-contents (V2847)
  (COND
   ((AND (CONSP V2847)
         (AND (EQ 'package (CAR V2847))
              (AND (CONSP (CDR V2847))
                   (AND (EQ 'null (CAR (CDR V2847)))
                        (CONSP (CDR (CDR V2847)))))))
    (CDR (CDR (CDR V2847))))
   ((AND (CONSP V2847)
         (AND (EQ 'package (CAR V2847))
              (AND (CONSP (CDR V2847)) (CONSP (CDR (CDR V2847))))))
    (shen.packageh (CAR (CDR V2847)) (CAR (CDR (CDR V2847)))
     (CDR (CDR (CDR V2847)))))
   (T (shen.f_error 'shen.package-contents))))

(DEFUN shen.walk (V2850 V2851)
  (COND
   ((CONSP V2851)
    (shen.apply V2850 (LIST (map #'(LAMBDA (Z) (shen.walk V2850 Z)) V2851))))
   (T (shen.apply V2850 (LIST V2851)))))

(DEFUN compile (V2855 V2856 V2857)
  (LET ((O (shen.apply V2855 (LIST (CONS V2856 (CONS NIL NIL))))))
    (IF (OR (EQ (fail) O) (NOT (NULL (CAR O))))
        (shen.apply V2857 (LIST O))
        (shen.hdtl O))))

(DEFUN fail-if (V2860 V2861)
  (IF (shen-cl.true? (shen.apply V2860 (LIST V2861)))
      (fail)
      V2861))

(DEFUN @s (V2864 V2865) (cn V2864 V2865))

(DEFUN tc? () shen.*tc*)

(DEFUN ps (V2867)
  (get/or V2867 'shen.source
   (freeze
    (simple-error
     (shen.app V2867 " not found.
"
      'shen.a)))
   *property-vector*))

(DEFUN stinput () *stinput*)

(DEFUN <-address/or (V2871 V2872 V2873)
  (trap-error (<-address V2871 V2872) #'(LAMBDA (E) (thaw V2873))))

(DEFUN value/or (V2876 V2877)
  (trap-error (value V2876) #'(LAMBDA (E) (thaw V2877))))

(DEFUN vector (V2879)
  (LET ((Vector (absvector (shen.add V2879 1))))
    (LET ((ZeroStamp (address-> Vector 0 V2879)))
      (LET ((Standard
             (IF (IF (NUMBERP V2879)
                     (= V2879 0))
                 ZeroStamp
                 (shen.fillvector ZeroStamp 1 V2879 (fail)))))
        Standard))))

(DEFUN shen.fillvector (V2885 V2886 V2887 V2888)
  (COND ((shen.ABSEQUAL V2887 V2886) (address-> V2885 V2887 V2888))
        (T
         (shen.fillvector (address-> V2885 V2886 V2888) (shen.add 1 V2886)
          V2887 V2888))))

(DEFUN vector? (V2890)
  (and (absvector? V2890)
       (LET ((X (<-address/or V2890 0 (freeze -1))))
         (and (number? X) (shen.greater-than-or-equal-to? X 0)))))

(DEFUN vector-> (V2894 V2895 V2896)
  (IF (IF (NUMBERP V2895)
          (= V2895 0))
      (simple-error "cannot access 0th element of a vector
")
      (address-> V2894 V2895 V2896)))

(DEFUN <-vector (V2899 V2900)
  (IF (IF (NUMBERP V2900)
          (= V2900 0))
      (simple-error "cannot access 0th element of a vector
")
      (LET ((VectorElement (<-address V2899 V2900)))
        (IF (EQ VectorElement (fail))
            (simple-error "vector element not found
")
            VectorElement))))

(DEFUN <-vector/or (V2904 V2905 V2906)
  (IF (IF (NUMBERP V2905)
          (= V2905 0))
      (simple-error "cannot access 0th element of a vector
")
      (LET ((VectorElement (<-address/or V2904 V2905 V2906)))
        (IF (EQ VectorElement (fail))
            (thaw V2906)
            VectorElement))))

(DEFUN shen.posint? (V2908)
  (and (integer? V2908) (shen.greater-than-or-equal-to? V2908 0)))

(DEFUN limit (V2910) (<-address V2910 0))

(DEFUN symbol? (V2912)
  (COND
   ((OR (shen-cl.true? (boolean? V2912)) (OR (NUMBERP V2912) (STRINGP V2912)))
    'false)
   (T
    (trap-error
     (LET ((String (str V2912)))
       (shen.analyse-symbol? String))
     #'(LAMBDA (E) 'false)))))

(DEFUN shen.analyse-symbol? (V2914)
  (COND ((EQUAL "" V2914) 'false)
        ((shen-cl.true? (shen.+string? V2914))
         (and (shen.alpha? (pos V2914 0)) (shen.alphanums? (tlstr V2914))))
        (T (shen.f_error 'shen.analyse-symbol?))))

(DEFUN shen.alpha? (V2916)
  (element? V2916
   (CONS "A"
         (CONS "B"
               (CONS "C"
                     (CONS "D"
                           (CONS "E"
                                 (CONS "F"
                                       (CONS "G"
                                             (CONS "H"
                                                   (CONS "I"
                                                         (CONS "J"
                                                               (CONS "K"
                                                                     (CONS "L"
                                                                           (CONS
                                                                            "M"
                                                                            (CONS
                                                                             "N"
                                                                             (CONS
                                                                              "O"
                                                                              (CONS
                                                                               "P"
                                                                               (CONS
                                                                                "Q"
                                                                                (CONS
                                                                                 "R"
                                                                                 (CONS
                                                                                  "S"
                                                                                  (CONS
                                                                                   "T"
                                                                                   (CONS
                                                                                    "U"
                                                                                    (CONS
                                                                                     "V"
                                                                                     (CONS
                                                                                      "W"
                                                                                      (CONS
                                                                                       "X"
                                                                                       (CONS
                                                                                        "Y"
                                                                                        (CONS
                                                                                         "Z"
                                                                                         (CONS
                                                                                          "a"
                                                                                          (CONS
                                                                                           "b"
                                                                                           (CONS
                                                                                            "c"
                                                                                            (CONS
                                                                                             "d"
                                                                                             (CONS
                                                                                              "e"
                                                                                              (CONS
                                                                                               "f"
                                                                                               (CONS
                                                                                                "g"
                                                                                                (CONS
                                                                                                 "h"
                                                                                                 (CONS
                                                                                                  "i"
                                                                                                  (CONS
                                                                                                   "j"
                                                                                                   (CONS
                                                                                                    "k"
                                                                                                    (CONS
                                                                                                     "l"
                                                                                                     (CONS
                                                                                                      "m"
                                                                                                      (CONS
                                                                                                       "n"
                                                                                                       (CONS
                                                                                                        "o"
                                                                                                        (CONS
                                                                                                         "p"
                                                                                                         (CONS
                                                                                                          "q"
                                                                                                          (CONS
                                                                                                           "r"
                                                                                                           (CONS
                                                                                                            "s"
                                                                                                            (CONS
                                                                                                             "t"
                                                                                                             (CONS
                                                                                                              "u"
                                                                                                              (CONS
                                                                                                               "v"
                                                                                                               (CONS
                                                                                                                "w"
                                                                                                                (CONS
                                                                                                                 "x"
                                                                                                                 (CONS
                                                                                                                  "y"
                                                                                                                  (CONS
                                                                                                                   "z"
                                                                                                                   (CONS
                                                                                                                    "="
                                                                                                                    (CONS
                                                                                                                     "*"
                                                                                                                     (CONS
                                                                                                                      "/"
                                                                                                                      (CONS
                                                                                                                       "+"
                                                                                                                       (CONS
                                                                                                                        "-"
                                                                                                                        (CONS
                                                                                                                         "_"
                                                                                                                         (CONS
                                                                                                                          "?"
                                                                                                                          (CONS
                                                                                                                           "$"
                                                                                                                           (CONS
                                                                                                                            "!"
                                                                                                                            (CONS
                                                                                                                             "@"
                                                                                                                             (CONS
                                                                                                                              "~"
                                                                                                                              (CONS
                                                                                                                               ">"
                                                                                                                               (CONS
                                                                                                                                "<"
                                                                                                                                (CONS
                                                                                                                                 "&"
                                                                                                                                 (CONS
                                                                                                                                  "%"
                                                                                                                                  (CONS
                                                                                                                                   "{"
                                                                                                                                   (CONS
                                                                                                                                    "}"
                                                                                                                                    (CONS
                                                                                                                                     ":"
                                                                                                                                     (CONS
                                                                                                                                      ";"
                                                                                                                                      (CONS
                                                                                                                                       "`"
                                                                                                                                       (CONS
                                                                                                                                        "#"
                                                                                                                                        (CONS
                                                                                                                                         "'"
                                                                                                                                         (CONS
                                                                                                                                          "."
                                                                                                                                          NIL)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(DEFUN shen.alphanums? (V2918)
  (COND ((EQUAL "" V2918) 'true)
        ((shen-cl.true? (shen.+string? V2918))
         (and (shen.alphanum? (pos V2918 0)) (shen.alphanums? (tlstr V2918))))
        (T (shen.f_error 'shen.alphanums?))))

(DEFUN shen.alphanum? (V2920) (or (shen.alpha? V2920) (shen.digit? V2920)))

(DEFUN shen.digit? (V2922)
  (element? V2922
   (CONS "1"
         (CONS "2"
               (CONS "3"
                     (CONS "4"
                           (CONS "5"
                                 (CONS "6"
                                       (CONS "7"
                                             (CONS "8"
                                                   (CONS "9"
                                                         (CONS "0"
                                                               NIL))))))))))))

(DEFUN variable? (V2924)
  (COND
   ((OR (shen-cl.true? (boolean? V2924)) (OR (NUMBERP V2924) (STRINGP V2924)))
    'false)
   (T
    (trap-error
     (LET ((String (str V2924)))
       (shen.analyse-variable? String))
     #'(LAMBDA (E) 'false)))))

(DEFUN shen.analyse-variable? (V2926)
  (COND
   ((shen-cl.true? (shen.+string? V2926))
    (and (shen.uppercase? (pos V2926 0)) (shen.alphanums? (tlstr V2926))))
   (T (shen.f_error 'shen.analyse-variable?))))

(DEFUN shen.uppercase? (V2928)
  (element? V2928
   (CONS "A"
         (CONS "B"
               (CONS "C"
                     (CONS "D"
                           (CONS "E"
                                 (CONS "F"
                                       (CONS "G"
                                             (CONS "H"
                                                   (CONS "I"
                                                         (CONS "J"
                                                               (CONS "K"
                                                                     (CONS "L"
                                                                           (CONS
                                                                            "M"
                                                                            (CONS
                                                                             "N"
                                                                             (CONS
                                                                              "O"
                                                                              (CONS
                                                                               "P"
                                                                               (CONS
                                                                                "Q"
                                                                                (CONS
                                                                                 "R"
                                                                                 (CONS
                                                                                  "S"
                                                                                  (CONS
                                                                                   "T"
                                                                                   (CONS
                                                                                    "U"
                                                                                    (CONS
                                                                                     "V"
                                                                                     (CONS
                                                                                      "W"
                                                                                      (CONS
                                                                                       "X"
                                                                                       (CONS
                                                                                        "Y"
                                                                                        (CONS
                                                                                         "Z"
                                                                                         NIL))))))))))))))))))))))))))))

(DEFUN gensym (V2930)
  (concat V2930 (set 'shen.*gensym* (shen.add 1 shen.*gensym*))))

(DEFUN concat (V2933 V2934) (intern (cn (str V2933) (str V2934))))

(DEFUN @p (V2937 V2938)
  (LET ((Vector (absvector 3)))
    (LET ((Tag (address-> Vector 0 'shen.tuple)))
      (LET ((Fst (address-> Vector 1 V2937)))
        (LET ((Snd (address-> Vector 2 V2938)))
          Vector)))))

(DEFUN fst (V2940) (<-address V2940 1))

(DEFUN snd (V2942) (<-address V2942 2))

(DEFUN tuple? (V2944)
  (and (absvector? V2944)
       (shen.equal? 'shen.tuple
                    (<-address/or V2944 0 (freeze 'shen.not-tuple)))))

(DEFUN append (V2947 V2948)
  (COND ((NULL V2947) V2948)
        ((CONSP V2947) (CONS (CAR V2947) (APPEND (CDR V2947) V2948)))
        (T (shen.f_error 'append))))

(DEFUN @v (V2951 V2952)
  (LET ((Limit (limit V2952)))
    (LET ((NewVector (vector (shen.add Limit 1))))
      (LET ((X+NewVector (vector-> NewVector 1 V2951)))
        (IF (IF (NUMBERP Limit)
                (= Limit 0))
            X+NewVector
            (shen.@v-help V2952 1 Limit X+NewVector))))))

(DEFUN shen.@v-help (V2958 V2959 V2960 V2961)
  (COND
   ((shen.ABSEQUAL V2960 V2959)
    (shen.copyfromvector V2958 V2961 V2960 (shen.add V2960 1)))
   (T
    (shen.@v-help V2958 (shen.add V2959 1) V2960
     (shen.copyfromvector V2958 V2961 V2959 (shen.add V2959 1))))))

(DEFUN shen.copyfromvector (V2966 V2967 V2968 V2969)
  (trap-error (vector-> V2967 V2969 (<-vector V2966 V2968))
              #'(LAMBDA (E) V2967)))

(DEFUN hdv (V2971)
  (<-vector/or V2971 1
   (freeze
    (simple-error
     (cn "hdv needs a non-empty vector as an argument; not "
         (shen.app V2971 "
"
          'shen.s))))))

(DEFUN tlv (V2973)
  (LET ((Limit (limit V2973)))
    (IF (IF (NUMBERP Limit)
            (= Limit 0))
        (simple-error "cannot take the tail of the empty vector
")
        (IF (IF (NUMBERP Limit)
                (= Limit 1))
            (vector 0)
            (LET ((NewVector (vector (shen.subtract Limit 1))))
              (shen.tlv-help V2973 2 Limit
               (vector (shen.subtract Limit 1))))))))

(DEFUN shen.tlv-help (V2979 V2980 V2981 V2982)
  (COND
   ((shen.ABSEQUAL V2981 V2980)
    (shen.copyfromvector V2979 V2982 V2981 (shen.subtract V2981 1)))
   (T
    (shen.tlv-help V2979 (shen.add V2980 1) V2981
     (shen.copyfromvector V2979 V2982 V2980 (shen.subtract V2980 1))))))

(DEFUN assoc (V2994 V2995)
  (COND ((NULL V2995) NIL)
        ((AND (CONSP V2995)
              (AND (CONSP (CAR V2995))
                   (shen.ABSEQUAL (CAR (CAR V2995)) V2994)))
         (CAR V2995))
        ((CONSP V2995) (assoc V2994 (CDR V2995))) (T (shen.f_error 'assoc))))

(DEFUN boolean? (V3001)
  (COND ((EQ 'true V3001) 'true) ((EQ 'false V3001) 'true) (T 'false)))

(DEFUN nl (V3003)
  (COND
   ((IF (NUMBERP V3003)
        (= V3003 0))
    0)
   (T
    (do
     (shen.prhush "
"
      (stoutput))
     (nl (shen.subtract V3003 1))))))

(DEFUN difference (V3008 V3009)
  (COND ((NULL V3008) NIL)
        ((CONSP V3008)
         (IF (shen-cl.true? (element? (CAR V3008) V3009))
             (difference (CDR V3008) V3009)
             (CONS (CAR V3008) (difference (CDR V3008) V3009))))
        (T (shen.f_error 'difference))))

(DEFUN do (V3012 V3013) V3013)

(DEFUN element? (V3025 V3026)
  (COND ((NULL V3026) 'false)
        ((AND (CONSP V3026) (shen.ABSEQUAL (CAR V3026) V3025)) 'true)
        ((CONSP V3026) (element? V3025 (CDR V3026)))
        (T (shen.f_error 'element?))))

(DEFUN empty? (V3032) (COND ((NULL V3032) 'true) (T 'false)))

(DEFUN fix (V3035 V3036)
  (shen.fix-help V3035 V3036 (shen.apply V3035 (LIST V3036))))

(DEFUN shen.fix-help (V3047 V3048 V3049)
  (COND ((shen.ABSEQUAL V3049 V3048) V3049)
        (T (shen.fix-help V3047 V3049 (shen.apply V3047 (LIST V3049))))))

(DEFUN dict (V3051)
  (LET ((D (absvector (shen.add 3 V3051))))
    (LET ((Tag (address-> D 0 'shen.dictionary)))
      (LET ((Capacity (address-> D 1 V3051)))
        (LET ((Count (address-> D 2 0)))
          (LET ((Fill (shen.fillvector D 3 (shen.add 2 V3051) NIL)))
            D))))))

(DEFUN dict? (V3053)
  (and (absvector? V3053)
       (shen.equal? (<-address/or V3053 0 (freeze 'shen.not-dictionary))
                    'shen.dictionary)))

(DEFUN shen.dict-capacity (V3055) (<-address V3055 1))

(DEFUN dict-count (V3057) (<-address V3057 2))

(DEFUN shen.dict-count-> (V3060 V3061) (address-> V3060 2 V3061))

(DEFUN shen.<-dict-bucket (V3064 V3065) (<-address V3064 (shen.add 3 V3065)))

(DEFUN shen.dict-bucket-> (V3069 V3070 V3071)
  (address-> V3069 (shen.add 3 V3070) V3071))

(DEFUN shen.set-key-entry-value (V3078 V3079 V3080)
  (COND ((NULL V3080) (CONS (CONS V3078 V3079) NIL))
        ((AND (CONSP V3080)
              (AND (CONSP (CAR V3080))
                   (shen.ABSEQUAL (CAR (CAR V3080)) V3078)))
         (CONS (CONS (CAR (CAR V3080)) V3079) (CDR V3080)))
        ((CONSP V3080)
         (CONS (CAR V3080) (shen.set-key-entry-value V3078 V3079 (CDR V3080))))
        (T (shen.f_error 'shen.set-key-entry-value))))

(DEFUN shen.remove-key-entry-value (V3086 V3087)
  (COND ((NULL V3087) NIL)
        ((AND (CONSP V3087)
              (AND (CONSP (CAR V3087))
                   (shen.ABSEQUAL (CAR (CAR V3087)) V3086)))
         (CDR V3087))
        ((CONSP V3087)
         (CONS (CAR V3087) (shen.remove-key-entry-value V3086 (CDR V3087))))
        (T (shen.f_error 'shen.remove-key-entry-value))))

(DEFUN shen.dict-update-count (V3091 V3092 V3093)
  (LET ((Diff (shen.subtract (length V3093) (length V3092))))
    (shen.dict-count-> V3091 (shen.add Diff (dict-count V3091)))))

(DEFUN dict-> (V3097 V3098 V3099)
  (LET ((N (hash V3098 (shen.dict-capacity V3097))))
    (LET ((Bucket (shen.<-dict-bucket V3097 N)))
      (LET ((NewBucket (shen.set-key-entry-value V3098 V3099 Bucket)))
        (LET ((Change (shen.dict-bucket-> V3097 N NewBucket)))
          (LET ((Count (shen.dict-update-count V3097 Bucket NewBucket)))
            V3099))))))

(DEFUN <-dict/or (V3103 V3104 V3105)
  (LET ((N (hash V3104 (shen.dict-capacity V3103))))
    (LET ((Bucket (shen.<-dict-bucket V3103 N)))
      (LET ((Result (assoc V3104 Bucket)))
        (IF (NULL Result)
            (thaw V3105)
            (CDR Result))))))

(DEFUN <-dict (V3108 V3109)
  (<-dict/or V3108 V3109
   (freeze
    (simple-error "value not found
"))))

(DEFUN dict-rm (V3112 V3113)
  (LET ((N (hash V3113 (shen.dict-capacity V3112))))
    (LET ((Bucket (shen.<-dict-bucket V3112 N)))
      (LET ((NewBucket (shen.remove-key-entry-value V3113 Bucket)))
        (LET ((Change (shen.dict-bucket-> V3112 N NewBucket)))
          (LET ((Count (shen.dict-update-count V3112 Bucket NewBucket)))
            V3113))))))

(DEFUN dict-fold (V3117 V3118 V3119)
  (LET ((Limit (shen.dict-capacity V3118)))
    (shen.dict-fold-h V3117 V3118 V3119 0 Limit)))

(DEFUN shen.dict-fold-h (V3126 V3127 V3128 V3129 V3130)
  (COND ((shen.ABSEQUAL V3130 V3129) V3128)
        (T
         (LET ((B (shen.<-dict-bucket V3127 V3129)))
           (LET ((Acc (shen.bucket-fold V3126 B V3128)))
             (shen.dict-fold-h V3126 V3127 Acc (shen.add 1 V3129) V3130))))))

(DEFUN shen.bucket-fold (V3134 V3135 V3136)
  (COND ((NULL V3135) V3136)
        ((AND (CONSP V3135) (CONSP (CAR V3135)))
         (shen.apply V3134
                     (LIST (CAR (CAR V3135)) (CDR (CAR V3135))
                           (shen.bucket-fold V3134 (CDR V3135) V3136))))
        (T (shen.f_error 'shen.bucket-fold))))

(DEFUN dict-keys (V3138)
  (dict-fold #'(LAMBDA (K) #'(LAMBDA (_) #'(LAMBDA (Acc) (CONS K Acc)))) V3138
   NIL))

(DEFUN dict-values (V3140)
  (dict-fold #'(LAMBDA (_) #'(LAMBDA (V) #'(LAMBDA (Acc) (CONS V Acc)))) V3140
   NIL))

(DEFUN put (V3145 V3146 V3147 V3148)
  (LET ((Curr (<-dict/or V3148 V3145 (freeze NIL))))
    (LET ((Added (shen.set-key-entry-value V3146 V3147 Curr)))
      (LET ((Update (dict-> V3148 V3145 Added)))
        V3147))))

(DEFUN unput (V3152 V3153 V3154)
  (LET ((Curr (<-dict/or V3154 V3152 (freeze NIL))))
    (LET ((Removed (shen.remove-key-entry-value V3153 Curr)))
      (LET ((Update (dict-> V3154 V3152 Removed)))
        V3152))))

(DEFUN get/or (V3159 V3160 V3161 V3162)
  (LET ((Entry (<-dict/or V3162 V3159 (freeze NIL))))
    (LET ((Result (assoc V3160 Entry)))
      (IF (NULL Result)
          (thaw V3161)
          (CDR Result)))))

(DEFUN get (V3166 V3167 V3168)
  (get/or V3166 V3167
   (freeze
    (simple-error "value not found
"))
   V3168))

(DEFUN hash (V3171 V3172)
  (shen.mod (sum (map #'(LAMBDA (X) (string->n X)) (explode V3171))) V3172))

(DEFUN shen.mod (V3175 V3176)
  (shen.modh V3175 (shen.multiples V3175 (CONS V3176 NIL))))

(DEFUN shen.multiples (V3179 V3180)
  (COND ((AND (CONSP V3180) (> (CAR V3180) V3179)) (CDR V3180))
        ((CONSP V3180)
         (shen.multiples V3179 (CONS (shen.multiply 2 (CAR V3180)) V3180)))
        (T (shen.f_error 'shen.multiples))))

(DEFUN shen.modh (V3185 V3186)
  (COND
   ((IF (NUMBERP V3185)
        (= V3185 0))
    0)
   ((NULL V3186) V3185)
   ((AND (CONSP V3186) (> (CAR V3186) V3185))
    (IF (NULL (CDR V3186))
        V3185
        (shen.modh V3185 (CDR V3186))))
   ((CONSP V3186) (shen.modh (shen.subtract V3185 (CAR V3186)) V3186))
   (T (shen.f_error 'shen.modh))))

(DEFUN sum (V3188)
  (COND ((NULL V3188) 0)
        ((CONSP V3188) (shen.add (CAR V3188) (sum (CDR V3188))))
        (T (shen.f_error 'sum))))

(DEFUN head (V3196)
  (COND ((CONSP V3196) (CAR V3196))
        (T (simple-error "head expects a non-empty list"))))

(DEFUN tail (V3204)
  (COND ((CONSP V3204) (CDR V3204))
        (T (simple-error "tail expects a non-empty list"))))

(DEFUN hdstr (V3206) (pos V3206 0))

(DEFUN intersection (V3211 V3212)
  (COND ((NULL V3211) NIL)
        ((CONSP V3211)
         (IF (shen-cl.true? (element? (CAR V3211) V3212))
             (CONS (CAR V3211) (intersection (CDR V3211) V3212))
             (intersection (CDR V3211) V3212)))
        (T (shen.f_error 'intersection))))

(DEFUN reverse (V3214) (shen.reverse_help V3214 NIL))

(DEFUN shen.reverse_help (V3217 V3218)
  (COND ((NULL V3217) V3218)
        ((CONSP V3217)
         (shen.reverse_help (CDR V3217) (CONS (CAR V3217) V3218)))
        (T (shen.f_error 'shen.reverse_help))))

(DEFUN union (V3221 V3222)
  (COND ((NULL V3221) V3222)
        ((CONSP V3221)
         (IF (shen-cl.true? (element? (CAR V3221) V3222))
             (union (CDR V3221) V3222)
             (CONS (CAR V3221) (union (CDR V3221) V3222))))
        (T (shen.f_error 'union))))

(DEFUN y-or-n? (V3224)
  (LET ((Message (shen.prhush (shen.proc-nl V3224) (stoutput))))
    (LET ((Y-or-N (shen.prhush " (y/n) " (stoutput))))
      (LET ((Input (shen.app (read (stinput)) "" 'shen.s)))
        (IF (EQUAL "y" Input)
            'true
            (IF (EQUAL "n" Input)
                'false
                (do
                 (shen.prhush "please answer y or n
"
                  (stoutput))
                 (y-or-n? V3224))))))))

(DEFUN not (V3226)
  (IF (shen-cl.true? V3226)
      'false
      'true))

(DEFUN subst (V3239 V3240 V3241)
  (COND ((shen.ABSEQUAL V3241 V3240) V3239)
        ((CONSP V3241) (map #'(LAMBDA (W) (subst V3239 V3240 W)) V3241))
        (T V3241)))

(DEFUN explode (V3243) (shen.explode-h (shen.app V3243 "" 'shen.a)))

(DEFUN shen.explode-h (V3245)
  (COND ((EQUAL "" V3245) NIL)
        ((shen-cl.true? (shen.+string? V3245))
         (CONS (pos V3245 0) (shen.explode-h (tlstr V3245))))
        (T (shen.f_error 'shen.explode-h))))

(DEFUN cd (V3247)
  (set '*home-directory*
       (IF (EQUAL V3247 "")
           ""
           (shen.app V3247 "/" 'shen.a))))

(DEFUN for-each (V3250 V3251)
  (COND ((NULL V3251) 'true)
        ((CONSP V3251)
         (LET ((_ (shen.apply V3250 (LIST (CAR V3251)))))
           (for-each V3250 (CDR V3251))))
        (T (shen.f_error 'for-each))))

(DEFUN fold-right (V3255 V3256 V3257)
  (COND ((NULL V3256) V3257)
        ((CONSP V3256)
         (shen.apply V3255
                     (LIST (CAR V3256) (fold-right V3255 (CDR V3256) V3257))))
        (T (shen.f_error 'fold-right))))

(DEFUN fold-left (V3261 V3262 V3263)
  (COND ((NULL V3263) V3262)
        ((CONSP V3263)
         (fold-left V3261 (shen.apply V3261 (LIST V3262 (CAR V3263)))
          (CDR V3263)))
        (T (shen.f_error 'fold-left))))

(DEFUN filter (V3266 V3267) (shen.filter-h V3266 NIL V3267))

(DEFUN shen.filter-h (V3277 V3278 V3279)
  (COND ((NULL V3279) (REVERSE V3278))
        ((AND (CONSP V3279)
              (shen-cl.true? (shen.apply V3277 (LIST (CAR V3279)))))
         (shen.filter-h V3277 (CONS (CAR V3279) V3278) (CDR V3279)))
        ((CONSP V3279) (shen.filter-h V3277 V3278 (CDR V3279)))
        (T (shen.f_error 'shen.filter-h))))

(DEFUN map (V3282 V3283) (shen.map-h V3282 V3283 NIL))

(DEFUN shen.map-h (V3289 V3290 V3291)
  (COND ((NULL V3290) (REVERSE V3291))
        ((CONSP V3290)
         (shen.map-h V3289 (CDR V3290)
          (CONS (shen.apply V3289 (LIST (CAR V3290))) V3291)))
        (T (shen.f_error 'shen.map-h))))

(DEFUN length (V3293) (shen.length-h V3293 0))

(DEFUN shen.length-h (V3296 V3297)
  (COND ((NULL V3296) V3297)
        (T (shen.length-h (CDR V3296) (shen.add V3297 1)))))

(DEFUN occurrences (V3309 V3310)
  (COND ((shen.ABSEQUAL V3310 V3309) 1)
        ((CONSP V3310)
         (shen.add (occurrences V3309 (CAR V3310))
                   (occurrences V3309 (CDR V3310))))
        (T 0)))

(DEFUN nth (V3319 V3320)
  (COND
   ((AND
     (IF (NUMBERP V3319)
         (= V3319 1))
     (CONSP V3320))
    (CAR V3320))
   ((CONSP V3320) (nth (shen.subtract V3319 1) (CDR V3320)))
   (T (shen.f_error 'nth))))

(DEFUN integer? (V3322)
  (and (number? V3322)
       (LET ((Abs (shen.abs V3322)))
         (shen.integer-test? Abs (shen.magless Abs 1)))))

(DEFUN shen.abs (V3324)
  (IF (> V3324 0)
      V3324
      (shen.subtract 0 V3324)))

(DEFUN shen.magless (V3327 V3328)
  (LET ((Nx2 (shen.multiply V3328 2)))
    (IF (> Nx2 V3327)
        V3328
        (shen.magless V3327 Nx2))))

(DEFUN shen.integer-test? (V3334 V3335)
  (COND
   ((IF (NUMBERP V3334)
        (= V3334 0))
    'true)
   ((> 1 V3334) 'false)
   (T
    (LET ((Abs-N (shen.subtract V3334 V3335)))
      (IF (> 0 Abs-N)
          (integer? V3334)
          (shen.integer-test? Abs-N V3335))))))

(DEFUN mapcan (V3340 V3341)
  (COND ((NULL V3341) NIL)
        ((CONSP V3341)
         (APPEND (shen.apply V3340 (LIST (CAR V3341)))
                 (mapcan V3340 (CDR V3341))))
        (T (shen.f_error 'mapcan))))

(DEFUN == (V3353 V3354) (COND ((shen.ABSEQUAL V3354 V3353) 'true) (T 'false)))

(DEFUN abort () (simple-error ""))

(DEFUN bound? (V3356)
  (and (symbol? V3356)
       (LET ((Val (value/or V3356 (freeze 'shen.this-symbol-is-unbound))))
         (IF (EQ Val 'shen.this-symbol-is-unbound)
             'false
             'true))))

(DEFUN shen.string->bytes (V3358)
  (COND ((EQUAL "" V3358) NIL)
        (T
         (CONS (string->n (pos V3358 0)) (shen.string->bytes (tlstr V3358))))))

(DEFUN maxinferences (V3360) (set 'shen.*maxinferences* V3360))

(DEFUN inferences () shen.*infs*)

(DEFUN protect (V3362) V3362)

(DEFUN stoutput () *stoutput*)

(DEFUN sterror () *sterror*)

(DEFUN command-line () *argv*)

(DEFUN string->symbol (V3364)
  (LET ((Symbol (intern V3364)))
    (IF (shen-cl.true? (symbol? Symbol))
        Symbol
        (simple-error
         (cn "cannot intern " (shen.app V3364 " to a symbol" 'shen.s))))))

(DEFUN optimise (V3370)
  (COND ((EQ '+ V3370) (set 'shen.*optimise* 'true))
        ((EQ '- V3370) (set 'shen.*optimise* 'false))
        (T
         (simple-error "optimise expects a + or a -.
"))))

(DEFUN os () *os*)

(DEFUN language () *language*)

(DEFUN version () *version*)

(DEFUN port () *port*)

(DEFUN porters () *porters*)

(DEFUN implementation () *implementation*)

(DEFUN release () *release*)

(DEFUN package? (V3372)
  (trap-error (do (external V3372) 'true) #'(LAMBDA (E) 'false)))

(DEFUN function (V3374) (shen.lookup-func V3374))

(DEFUN shen.lookup-func (V3376)
  (get/or V3376 'shen.lambda-form
   (freeze
    (simple-error
     (shen.app V3376 " has no lambda expansion
"
      'shen.a)))
   *property-vector*))

