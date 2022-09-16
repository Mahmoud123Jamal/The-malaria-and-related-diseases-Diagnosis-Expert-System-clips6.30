;;****************
;;* DEFFUNCTIONS *
;;****************
(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* QUERY RULES to determine disease *
;;;***************
(defrule determine-problem-type ""
   (not (problem-type ?))
   (not (diagonosis ?))
   =>
   (assert (problem-type
(ask-question "choose from this five diseases (1-malaria(ma)/2-thypoid(thy)/3-dengue-fever(d-f) / 4- sleeping-sickness(s-s) / 5- pulmonary-tuberculosis(p-t))? ,please write(key) !  "
                    ma thy d-f s-s p-t  ))))


;;;*************************
;;;* QUERY RULES FOR malaria*
;;;*************************


(defrule determine-fever-paroxysmal-of-malaria ""
   (problem-type ma)
   (not (fever-paroxysmal ?))
   (not (diagonosis ?))
   =>
   (assert (fever-paroxysmal (yes-or-no-p "Has the patient been a fever paroxysmal (yes/no)? "))))
   
(defrule determine-shaking-chilis-of-malaria ""
   (problem-type ma)
   (fever-paroxysmal yes)
   (not (diagonosis ?))
   =>
   (assert (shaking-chilis (yes-or-no-p "Is there shaking-chilis (yes/no)? "))))

(defrule determine-positive-Blood-film-of-malaria ""
   (problem-type ma)
    (fever-paroxysmal yes)
    (shaking-chilis yes)
   (not (diagonosis ?))   
   =>
   (assert (positive-Blood-film (yes-or-no-p "Does Thick Blood film for Parasite , is test result positive (yes/no)? "))))
   ;;; may be typhoid or dengue fever
(defrule determine-not-malaria1 ""
   (problem-type ma)
   (positive-Blood-film no)
   (not (diagonosis ?))
   =>
   (assert (printout t "Diagonosis isn't malaria , re-run program and choose between (typhoid , Denge fever)" crlf)))
   
;;; can't Diagonosis
(defrule determine-not-malaria2 ""
   (problem-type ma)
   (fever-paroxysmal no)
   (not (diagonosis ?))
   =>
   (assert (printout t "Diagonosis isn't malaria  " crlf)))
;;;  Diagonosis fever-paroxysmal
   (defrule determine-not-malaria3 ""
   (problem-type ma)
    (shaking-chilis no)
   (not (diagonosis ?))
   =>
   (assert (printout t "Diagonosis is fever paroxysmal " crlf)))
              
;;;*************************
;;;* QUERY RULES FOR Typhoid fever *
;;;*************************


(defrule determine-typhoid-fever ""
   (problem-type thy)
   (not (symptom1 ?))
   (not (diagonosis ?))
   =>
   (assert (symptom1 (yes-or-no-p "Has the patient been a fever paroxysmal (yes/no)? "))))

(defrule determine-symptom2 ""
   (problem-type thy)
   (symptom1 yes)
   (not (diagonosis ?))
   =>
   (assert (symptom2 (yes-or-no-p "Is there shaking-chilis (yes/no)? "))))
   
(defrule determine-symptom3 ""
   (problem-type thy)
    (symptom1 yes)
    (symptom2 yes) 
   (not (diagonosis ?))   
   =>
   (assert (symptom3 (yes-or-no-p "Does Thick Blood film for Parasite , is test result positive (yes/no)? "))))
   

   (defrule determine-fever-gradual ""
   (problem-type thy)
   (symptom3 no)
   (not (diagonosis ?))
   =>
   (assert (fever-gradual (yes-or-no-p " IS fever gradual And sustained with headache And abdominal pain ?! "))))



(defrule determine-vidal-positive ""
   (problem-type thy)
    (fever-gradual yes)
   
   (not (diagonosis ?))   
   =>
   (assert (vidal-positive(yes-or-no-p " Do Blood / stool / urine cluture ; and Vidal Test : is salmonclla OR vidal positive ?! "))))
   
;;;***********************************************************************************************
;; handling other cases
;;; diagonies is malaria
(defrule determine-may-malaria ""
   (problem-type thy)
   (symptom3 yes)
   (not (diagonosis ?))
   =>
  (assert (printout t "There is suggest that you have Malaria" crlf)))


   ;;; check dengue fever

(defrule determine-check-dengue-fever ""
   (problem-type thy)
    (vidal-positive no)
   
   (not (diagonosis ?))   
   =>
   (assert (printout t "check dengue fever , may be dengue fever " crlf)))
   

;;; can't Diagonosis
(defrule determine-not-thypoid1 ""
   (problem-type thy)
   (symptom1 no)
   (not (diagonosis ?))
   =>
   (assert (printout t "Diagonosis isn't typoid  " crlf)))
;;; can't Diagonosis
   (defrule determine-fever-paroxysmal ""
   (problem-type thy)
    (symptom2 no)
   (not (diagonosis ?))
   =>
   (assert (printout t "Diagonosis is fever paroxysmal" crlf)))


;;; can't Diagonosis (fever gradual...... NO)

(defrule determine-not-thypoid3 ""
   (problem-type thy)
   (fever-gradual no)
   (not (diagonosis ?))
   =>
   (assert (printout t "re-run program , can't  Diagonosis " crlf)))


;;;**********************************************************************

;;;*****************************
;;;* QUERY RULES FOR Dengue fever *
;;;*****************************


(defrule determine-sign1 ""
   (problem-type d-f)
   (not (sign1 ?))
   (not (diagonosis ?))
   =>
   (assert (sign1 (yes-or-no-p "Has the patient been a fever paroxysmal (yes/no)? "))))

(defrule determine-sign2 ""
   (problem-type d-f)
   (sign1 yes)
   (not (diagonosis ?))
   =>
   (assert (sign2 (yes-or-no-p "Is there shaking-chilis (yes/no)? "))))
   
(defrule determine-sign3 ""
   (problem-type d-f)
    (sign1 yes)
    (sign2 yes)
   (not (diagonosis ?))   
   =>
   (assert (sign3 (yes-or-no-p "Does Thick Blood film for Parasite , is test result positive (yes/no)? "))))
   

   (defrule determine-sign4 ""
   (problem-type d-f)
   (sign3 no)
   (not (diagonosis ?))
   =>
   (assert (sign4 (yes-or-no-p " IS fever gradual And sustained with headache And abdominal pain ?! "))))


(defrule determine-sign5 ""
   (problem-type d-f)
    (sign4 yes)
   
   (not (diagonosis ?))   
   =>
   (assert (sign5(yes-or-no-p " Do Blood / stool / urine cluture ; and Vidal Test : is salmonclla OR vidal positive ?! "))))
   ;;; vidal-positive (no)
(defrule determine-sign6 ""
   (problem-type d-f)
    (sign5 no)
   (not (diagonosis ?))   
   =>
   (assert (sign6(yes-or-no-p " IS Headache Abrupt with Retro-Bulbar pain , that worsrns with eye movement?! "))))
   
(defrule determine-sign7 ""
   (problem-type d-f)
    (sign6 yes)
   
   (not (diagonosis ?))   
   =>
   (assert (sign7(yes-or-no-p " Does white blood cell count , is test result leucopenic and thrombocytopenic?! "))))
   


;;*************************************************************************
;;; handling other cases
;;; 1-diagonies is malaria

(defrule determine-may-be-malaria ""
   (problem-type d-f)
   (sign3 yes)
   (not (diagonosis ?))
   =>
  (assert (printout t "There is suggest that you have Malaria" crlf)))
;; 2-if vidal-positive test yes then thypoid
(defrule determine-check-thypoid ""
   (problem-type d-f)
    (sign5 yes)
   
   (not (diagonosis ?))   
   =>
   (assert (printout t "it's thypoid , treat it  " crlf)))


;;; can't Diagonosis
(defrule determine-not-dengue-fever1 ""
   (problem-type d-f)
   (sign1 no)
   (not (diagonosis ?))
   =>
   (assert (printout t " can't Diagonosis  " crlf)))
;;; can't Diagonosis
   (defrule determine-not-dengue-fever2 ""
   (problem-type d-f)
    (sign2 no)
   (not (diagonosis ?))
   =>
   (assert (printout t "  Diagonosis fever paroxysmal " crlf)))

;;; can't Diagonosis (fever gradual...... NO)

(defrule determine-not-dengue-fever3 ""
   (problem-type d-f)
   (sign4 no)
   (not (diagonosis ?))
   =>
   (assert (printout t "re-run program , can't  Diagonosis " crlf)))


;;; can't Diagonosis
   (defrule determine-not-dengue-fever4 ""
   (problem-type d-f)
    (sign6 no)
   (not (diagonosis ?))
   =>
   (assert (printout t " can't Diagonosis  " crlf)))

   ;;; can't Diagonosis
   (defrule determine-not-dengue-fever5 ""
   (problem-type d-f)
    (sign7 no)
   (not (diagonosis ?))
   =>
   (assert (printout t " can't Diagonosis correctlly , may be dengue fever  " crlf)))

;;;*************************************************************************************


;;;*****************************
;;;* QUERY RULES FOR sleeping sikness *
;;;**********************
(defrule determine-sleeping-sikness ""
   (problem-type s-s)
   (not (tsetse-fly ?))
   (not (diagonosis ?))
   =>
   (assert (tsetse-fly (yes-or-no-p "Has the patient been A large sore at the site of the tsetse fly bite (yes/no)? "))))

(defrule determine-Malaise ""
   (problem-type s-s)
   (tsetse-fly yes)
   (not (diagonosis ?))
   =>
   (assert (Malaise (yes-or-no-p "Is there Fever, Muscle and joint aches, Headaches , Malaise, Rash or itchy skin (yes/no)? "))))
   
   (defrule determine-lymph-nodes ""
   (problem-type s-s)
   (Malaise yes)
   (not (diagonosis ?))
   =>
   (assert (lymph-nodes (yes-or-no-p "Is there Enlarged lymph nodes  (yes/no)? "))))
   
   (defrule determine-Weight-loss ""
   (problem-type s-s)
   (lymph-nodes yes)
   (not (diagonosis ?))
   =>
   (assert (Weight-loss (yes-or-no-p "Is there Weight loss  (yes/no)? "))))

   
  (defrule determine-Coma ""
   (problem-type s-s)
   (Weight-loss yes)
   (not (diagonosis ?))
   =>
   (assert (Coma (yes-or-no-p "Is there  Severe sleep disorders , Ataxia ,Psychiatric disorders, Profound sensory disturbances ,Seizures ,Strange tone and movement , Coma (yes/no)? "))))


;;;; handling Answers no :

(defrule determine-not-sleeping-sikness1 ""
   (problem-type s-s)
   (tsetse-fly no)
   (not (diagonosis ?))
   =>
    (assert (printout t "Diagonosis isn't sleeping sickness , because tsetse fly bite is major sign " crlf)))
   
   (defrule determine-not-sleeping-sikness2 ""
   (problem-type s-s)
   (Malaise no)
   (not (diagonosis ?))
   =>
   (assert (printout t "Diagonosis isn't sleeping sickness , what!! you have A large sore at the site of the tsetse fly bite and don't suffer from any of this " crlf)))
   
   (defrule determine-not-sleeping-sikness3 ""
   (problem-type s-s)
   (lymph-nodes no)
   (not (diagonosis ?))
   =>
   (assert (printout t " tsetse fly bite Case Enlarged lymph nodes  , what!!!  " crlf)))

   
  (defrule determine-not-sleeping-sikness4 ""
   (problem-type s-s)
   (Weight-loss no)
   (not (diagonosis ?))
   =>
    (assert (printout t " tsetse fly bite case lossing Weight , what!!!  " crlf)))

;;****************************************************************


;;;*****************************
;;;* QUERY RULES FOR pulmonary-tuberculosis *
;;;**********************
(defrule determine-pulmonary-tuberculosis ""
   (problem-type p-t)
   (not (Cough ?))
   (not (diagonosis ?))
   =>
   (assert (Cough (yes-or-no-p "Has the patient been Cough usually productive (yes/no)? "))))

(defrule determine-Sputum-Haemoptysis ""
   (problem-type p-t)
   (Cough yes)
   (not (diagonosis ?))
   =>
   (assert (Sputum-Haemoptysis (yes-or-no-p "Is there Sputum,usually mucopurulent or purulent  and Haemoptysis (yes/no)? "))))
   
   
   
   (defrule determine-Breathlessness ""
   (problem-type p-t)
   (Sputum-Haemoptysis yes)
   (not (diagonosis ?))
   =>
   (assert (Breathlessness (yes-or-no-p "Is there Breathlessness,gradual increase rather than sudden  (yes/no)? "))))

   
  (defrule determine-Weightloss ""
   (problem-type p-t)
   (Breathlessness yes)
   (not (diagonosis ?))
   =>
   (assert (Weightloss (yes-or-no-p "Is there Weight loss, gradual (yes/no)? "))))

  (defrule determine-Anorexia ""
   (problem-type p-t)
   (Weightloss yes)
   (not (diagonosis ?))
   =>
   (assert (Anorexia (yes-or-no-p "Is there Anorexia or Fever and may be associated with night sweats and Malaise (yes/no)? "))))


  (defrule determine-cachexia ""
   (problem-type p-t)
   (Anorexia yes)
   (not (diagonosis ?))
   =>
   (assert (cachexia (yes-or-no-p "Is there Wasting and terminal cachexia,late, ominous signs (yes/no)? "))))



;;;; handling Answers no :


(defrule determine-not-pulmonary-tuberculosis1 ""
   (problem-type p-t)
   (Cough no)
   (not (diagonosis ?))
   =>
  (assert (printout t " not pulmonary-tuberculosis , patient must have cough" crlf)))
   
   (defrule determine-not-pulmonary-tuberculosis2 ""
   (problem-type p-t)
   (Sputum-Haemoptysis no)
   (not (diagonosis ?))
   =>
(assert (printout t " not pulmonary-tuberculosis , diagonosis is ordinary cough " crlf)))
   

   
  (defrule determine-not-pulmonary-tuberculosis3 ""
   (problem-type p-t)
   (Breathlessness no)
   (not (diagonosis ?))
   =>
   (assert (printout t " the diagonosis is caused by popular cold and problems in the nasal arteries " crlf)))

  (defrule determine-not-pulmonary-tuberculosis4 ""
   (problem-type p-t)
   (Weightloss no)
   (not (diagonosis ?))
   =>
  (assert (printout t " diagonies may be pulmonary-tuberculosis , because lossing weight is gradel  " crlf)))


  (defrule determine-not-pulmonary-tuberculosis5 ""
   (problem-type p-t)
   (Anorexia no)
   (not (diagonosis ?))
   =>
   (assert (printout t " popular cold and problems in the nasal arteries and lung problems and digestive problems" crlf)))
(defrule determine-not-pulmonary-tuberculosis6 ""
   (problem-type p-t)
   (cachexia no)
   (not (diagonosis ?))
   =>
  (assert (printout t " diagonies pulmonary-tuberculosis , but not Advanced stage  " crlf)))


;;****************************************************************

;;;***************************
;;;* decisions RULES FOR malaria *
;;;***************************
(defrule malaria-decisions""
   (problem-type ma)
   (fever-paroxysmal yes)
   (shaking-chilis yes)
   (positive-Blood-film yes)

   (not (diagonosis ?))
   =>
   (assert (diagonosis "Diagonosis is malaria , treat it")))


;;;*******************************
;;;* decisions RULES FOR thypoid    *
;;;*******************************
(defrule thypoid-decisions ""
   (problem-type thy)
   (symptom1 yes)
   (symptom2 yes)
   (symptom3 no)
   (fever-gradual yes)
   (vidal-positive yes)
   (not (diagonosis ?))
   =>
   (assert (diagonosis "Diagonosis is thypoid , treat it")))
   
;;;*******************************
;;;* decisions RULES FOR Dengue fever   *
;;;*******************************
(defrule dengue-fever-decisions ""
   (problem-type d-f)
   (sign1 yes)
   (sign2 yes)
   (sign3 no)
   (sign4 yes)
   (sign5 no)
   (sign6 yes)
   (sign7 yes)
   (not (diagonosis ?))
   =>
   (assert (diagonosis "Diagonosis is dengue fever , treat it")))

;;;***************************
;;;* decisions RULES FOR sleeping-sickness *
;;;***************************
   (defrule sleeping-sickness-decisions""
   (problem-type s-s)
   (tsetse-fly yes)
   (Malaise yes)
   (lymph-nodes yes)
   (Weight-loss yes)
   (Coma no)
   (not (diagonosis ?))
   =>
   (assert (diagonosis "Diagonosis is sleeping sickness (stage1) , treat it")))

   (defrule Advanced-sleeping-sickness-decisions""
   (problem-type s-s)
     (tsetse-fly yes)
   (Malaise yes)
   (lymph-nodes yes)
   (Weight-loss yes)
   (Coma yes)

   (not (diagonosis ?))
   =>
   (assert (diagonosis "Diagonosis is Advanced (stage 2) sleeping sickness  , treat it very fast , patient in danger ")))



   ;;;***************************
;;;* decisions RULES FOR pulmonary-tuberculosis *
;;;***************************

   (defrule pulmonary-tuberculosis-decisions""
   (problem-type p-t)
   (Cough yes)
   (Sputum-Haemoptysis yes)
   (Breathlessness yes)
   (Weightloss yes)
   (Anorexia yes)
   (cachexia yes)
   (not (diagonosis ?))
   =>
   (assert (diagonosis "Diagonosis is pulmonary tuberculosis (Advanced), treat it very fast")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************
(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The malaria / thyphoid / dengue fever  Diagnosis Expert System")
  (printout t crlf crlf))
  ;; end of project