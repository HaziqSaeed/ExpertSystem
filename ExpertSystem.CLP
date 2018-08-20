
;;------------Fuzzy Set Definition---------------
(deftemplate bookRead
 0 10 degrees
 ((Sp (0 1) (3 0))
 (Avg (2 0) (4 1) (6 0))
 (Pro (5 0) (8 1))))

(deftemplate difficultyLevel
 0 10 degrees
 ((Sp (0 1) (4 0))
 (Avg (3.5 0) (5.5 1) (7.5 0))
 (Dif (7 0) (9 1))))

(deftemplate Act
 0 100 degrees
 ((Np (0 1) (20 0))
 (Sp (10 0) (30 1) (50 0))
 (Avg (40 0) (50 1) (60 0))
 (AA  (50 0) (65 1) (80 0))
 (Pro (75 0) (90 1))))

;;----------------Crisp Inputs--------------
(defrule Get_Observations
 ?i <- (initial-fact)
 =>
 (printout t "The end of the result of the application is based on the scale. Check the fuzzy output diagram ")
 (printout t "Enter amount of books you have read over the summer. "crlf)
 (bind ?response (read))
 (assert (crispBooks ?response))
 (printout t "Enter the difficult of the book.(Scale 1 to 10). " crlf)
 (bind ?response (read))
 (assert (crispDifficultyLevel ?response))
 (retract ?i))
;; ----------------Fuzzify The Inputs-----------
(defrule fuzzify
 (crispBooks ?a)
 (crispDifficultyLevel ?d)
 =>
 (assert (bookRead (?a 0) (?a 1) (?a 0)))
 (assert ( difficultyLevel(?d 0) (?d 1) (?d 0)))
)



;; FAM rule definition
(defrule PP
 (bookRead Sp)
 (difficultyLevel Sp)
 (printout t "enter")
 =>
 (assert (Act Np)))

(defrule PZ
 (bookRead Sp)
 (difficultyLevel Avg)
 =>
 (assert (Act Sp)))

(defrule PN
 (bookRead Sp)
 (difficultyLevel Dif)
 =>
 (assert (Act AA)))

(defrule ZP
 (bookRead Avg)
 (difficultyLevel Sp)
 =>
 (assert (Act Avg)))
(defrule ZZ
 (bookRead Avg)
 (difficultyLevel Avg)
 =>
 (assert (Act AA)))
(defrule ZN
 (bookRead Avg)
 (difficultyLevel Dif)
 =>
 (assert (Act AA)))
(defrule NP
 (bookRead Pro)
 (difficultyLevel Sp)
 =>
 (assert (Act Avg)))
(defrule NZ
 (bookRead Pro)
 (difficultyLevel Avg)
 =>
 (assert (Act AA)))
(defrule NN
 (bookRead Pro)
 (difficultyLevel Dif)
 =>
 (assert (Act Pro)))

;; Defuzzify The Outputs
(defrule defuzzify1
 (declare (salience -1))
 ?f <- (Act ?)
 =>
 (bind ?t (moment-defuzzify ?f))
 (printout t "action--> " ?t crlf))

(deffacts readingErrors
(errors 0 100)
(errors 1 98)
(errors 2 96)
(errors 3 94)
(errors 4 92)
(errors 5 90))
(deffacts wrongStates
(state 1 1)(state 2 1)( state 3 1) 
(state 1 2)(state 2 2) (state 3 2)
(state 2 3)(state 3 3))

(deffacts wrongAnswers
(wrongA 1 1)(wrongA 2 1)( wrongA 3 1) 
(wrongA 1 2)(wrongA 2 2) (wrongA 3 2)
(wrongA 1 3)(wrongA 3 3))

;----------------Rules Reading Level 1----------------
(defrule systemS 
?p <-(start)
=>
(printout t "      Davis Joint Unified School District  " crlf
	    "	   	    Student Reading Assessment      " crlf)
(printout t" Is your elementary school part of the Davis Joint Unified School District? Yes/ No ")
(assert (partDistrict (read)))
(retract ?p))

(defrule District
	(partDistrict No)
	=>
	(printout t "This test is not for you. Check in with your teacher."))

(defrule StudentInfo
(partDistrict Yes)
=>
(printout t "Enter the student first name: "crlf)
(assert (name (read)))
(printout t "Enter the instructor name: ")
(assert(instructor(read)))
(printout t "Select your current Grade Level? " crlf
		"(1)Grade 1"crlf
		"(2)Grade 2"crlf
		"(3)Grade 3"crlf)
(assert (gradeLevel (read)))
(assert (startAssessment True))
)


(defrule firstLevel 
(startAssessment True)
=> 
(printout t "In order to pass each reading level you must get 90% or higher on your reading skill assessment."crlf
	"Also you need to pass the comprehension check part to move on to the next reading level." crlf)
(printout t "The following reading assessment must be taken under the instructor supervision."crlf)
(printout t 
"Reading Level 1" crlf
"Text : Look at me " crlf
	"Look at me . I am reading." crlf
	"Look at me. I am painting." crlf
	"Look at me. I am singing." crlf
	"Look at me. I am eating." crlf
	"Look at me. I am drinking." crlf
	"Look at me. I am running." crlf
	"Look at me. I am climbing." crlf )
(printout t "How many errors did the student made on the following reading?"crlf)
(assert (errorCheck(read)))
;(assert (reading1 passed))
)
;-------Error Checking State--------
(defrule Error1-5
(errorCheck ?x)
(errors ?x ?p)
=>
(printout t "----------------------------------" crlf)
(printout t"Reading Scores : " ?p crlf)
(assert (reading1 passed))
)
(defrule ErrorM
(errorCheck ?x)
(test(> ?x 5))
=>
(printout t "----------------------------------" crlf)
(printout t "Student:Failed "crlf)
(printout t "Student must get 90% or higher to pass the reading exercise."crlf)
(assert(reading1 failed))
)

(defrule comprehensionCheck
(or(reading1 passed)(reading1 failed))
=>
(printout t " Answer the following questions regarding the reading." crlf)
(printout t " What did the boy do ? "crlf
	"(1) read  (2) jumped  (3) walked"crlf)
	(assert (q1_1 (read)))
(printout t "Did the boy ever ate?" crlf
	"(1) car (2) grass (3) apple"crlf)
	(assert (q1_2 (read)))
)

(defrule passState1
(q1_1 1)
(q1_2 3)
(reading1 passed)
=>
(printout t"Reading Level 1 : Passed"crlf)
(printout t "----------------------------------" crlf
" "crlf)
(assert(passedFirstLevel true)))

(defrule passState2
;(reading1 passed)
(q1_1 ?inputA)
(q1_2 ?inputB)
(or (state ?inputA ?inputB) (reading1 failed) )
=> 
(printout t "--------------Report--------------------" crlf)
(printout t "Reading Level 1 : Failed" crlf)
(printout t " The student needs a lot of practice on his reading skills."crlf)
(printout t "Book Suggested: Frog and Toad Are Friends" crlf)

(printout t "----------------------------------" crlf)

(assert(passedFirstLevel false))
)

;---------------------------------------------
;Reading 2

(defrule readingLevel2
(passedFirstLevel true)
=>
(printout t " Reading Level 2 "crlf
	"    |Text : The balloons| " crlf)
(printout t "----------------------------------" crlf)
(printout t"The red balloon is for you, said Dad."crlf
	"Here you are."crlf
	"The yellow ballon is for you, said Dad." crlf
	"Here you are."crlf
	"The blue balloon is for you, said Dad."crlf
	"Here you are."crlf
	"The little green balloon is for you , said Dad."crlf)
(printout t "----------------------------------" crlf)
(printout t "How many errors student made on the following reading?" crlf)
(assert(errorCheck2(read))))

(defrule errorR2
(errorCheck2 ?z)
(errors ?z ?y)
=>
(printout t "----------------------------------" crlf)
(printout t "Reading Score : " ?y crlf)
(printout t "----------------------------------" crlf)
(assert (readingA2 passed)))


(defrule errorsG
(errorCheck2 ?x)
(test ( > ?x 5))
=>
(printout t "Student:Failed "crlf)
(printout t "Student must get 90% or higher to pass the reading exercise."crlf)
(assert(reading2 failed))
;(retract errorCheck2)
)

(defrule comprehensionCheck2
(or (readingA2 passed) (reading2 failed))
=>
(printout t "Following questions will test your understanding of the reading?"crlf)
(printout t " How many balloon did dad have?"crlf
	"(1) three (2) four (3) five"crlf)
(assert(q2_1(read)))
(printout t " What color balloon did the baby get?"crlf
	"(1) yellow (2) red (3) green"crlf)
(assert(q2_2(read))))

(defrule passStateR2
(q2_1 2)
(q2_2 3)
(readingA2 passed)
=>
(printout t"Passed Level 2" crlf)
(assert(passedSecondLevel true)))

(defrule passStateC2
(q2_1 ?inputA)
(q2_2 ?inputB)
(or(wrongA ?inputA ?inputB)(reading2 failed))  
=> 
(printout t "--------------Report--------------------" crlf)
(printout t "Reading Level 2 : Failed" crlf) 
(printout t " The student is slight proficient at reading. Student still needs a lot of practice on his reading skills."crlf)
(printout t "Boaok Suggested: The Best Seat in Second Grade" crlf)
(printout t "----------------------------------" crlf)
;(assert(passedSecondLevel false))
)
(defrule readingLevel3
(passedSecondLevel true)
=>
(printout t " Reading Level 3 "crlf
	"    |Text : Little Cat is Hungry| " crlf)
(printout t "----------------------------------" crlf)
(printout t"Here comes Little Cat."crlf
	"Little Cat is hungry. Meow ! Meow!" crlf
	"Little Cat can see a lizard. The lizard is asleep in the sun." crlf
	"Here comes Little Cat. The lizard wakes up." crlf
	"Look at the lizard. "crlf
	"Little Cat can see a bird. The bird can not see Little Cat."clrf
	" Here comes Little Cat. The bird sees Little Cat."crlf
	"The bird is up in a big tree. Little Cat looks up at the bird."crlf
	)
(printout t "----------------------------------" crlf)
(printout t "How many errors student made on the following reading?" crlf)
(assert(errorstudent(read))))

(defrule errorR3
(errorstudent ?z)
(errors ?z ?y)
=>
(printout t "----------------------------------" crlf)
(printout t "Reading Score : " ?y crlf)
(printout t "----------------------------------" crlf)
(assert (readingA3 passed)))

(defrule errorsG3
(errorstudent ?x)
(test ( > ?x 5))
=>
(printout t "Student:Failed "crlf)
(printout t "Student must get 90% or higher to pass the reading exercise."crlf)
(assert(reading3 failed))
)

(defrule comprehensionCheck3
(or (readingA3 passed) (reading3 failed))
=>
(printout t "Following questions will test your understanding of the reading?"crlf)
(printout t " What did Little Cat try to catch?"crlf
	"(1) lizard (2) frog (3) snake"crlf)
(assert(q3_1(read)))
(printout t " What did the bird do when it saw Little Cat?"crlf
	"(1) flew back to farm  (2) flew back to house (3) flew to big tree"crlf)
(assert(q3_3(read))))

(defrule passStateR3
(q3_1 1)
(q3_3 3)
(readingA3 passed)
=>
(printout t"Passed Level 3" crlf)
(assert(passedThirdLevel true)))


(defrule failState
(q3_1 ?userInput)
(q3_3 ?userInput)
(or (state ?userInput ?userInput)(reading3 failed)) 
=> 
(printout t "--------------Report--------------------" crlf)
(printout t "Reading Level 3 : Failed" crlf) 
(printout t " The student is Above Average proficient at reading. Student still needs a lot of practice on his reading skills."crlf)
(printout t "Book Suggested: Superfuge" crlf)

(printout t "----------------------------------" crlf)
(assert(passedThirdLevel false))
)


(deffacts startup
(start))