(deffacts path
(from sacramento to davis)
(from chico to davis)
(from fairfield to galt)
(from galt to sacramento)
(from tahoe to fairfield) 
(from chico to galt))

(defrule routInfo
?p <-(start)
=> 
(printout t "Enter point A (starting location)." crlf)
(assert(pointA(read)))
(printout t "Enter point B (destinationlocation)." crlf)
(assert(pointB (read)))
(retract ?p))

(defrule check1
(pointA ?x)
(from ?x to ?xp)
=>
(printout t "Checkpoint 1: drive from " ?x " to "?xp crlf)
(assert(saveL1 ?xp))

)
(defrule check2
(saveL1 ?currentLocation)
(from ?currentLocation to ?nextLocation)
=>
(printout t "Checkpoint 2: drive from " ?currentLocation " to " ?nextLocation crlf)
(assert (saveL2 ?nextLocation))
)

(defrule check3
(saveL2 ?currentLocationL)
(from ?currentLocationL to ?nextLocationOld)
=>
 (printout t "Checkpoint 3: drive from " ?currentLocationL " to " ?nextLocationOld crlf)
(assert (saveL3 ?nextLocationOld)))



(deffacts startup
(start))