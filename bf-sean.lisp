;;;;;; The TABLE (HISTOGRAM)-BASED BAYES FILTER and THE PARTICLE FILTER
;;;;;; These are two simple filter examples.  The example world model
;;;;;; we have provided is a discrete model, which is a little silly
;;;;;; for the particle filter, and so the total amount of code for
;;;;;; the praticle filter is larger than for the discrete table-based
;;;;;; Bayes filter.  But in a continuous example, where we would have to
;;;;;; discretize the model, the Bayes filter would get much uglier and
;;;;;; the Particle filter would stay very pretty.


;;;;;;; What you need to do:
;;;;;;;
;;;;;;; 1. Implement the Table (Histogram) Bayes and Particle Filters.  You may use different
;;;;;;;    internal functions than the templates that I have provided, but please
;;;;;;;    use the same BAYES-FILTER and PARTICLE-FILTER function forms.
;;;;;;;
;;;;;;; 2. Demonstrate that the filters produce the same results.  What happens
;;;;;;;    when you change the number of particles the particle filter uses?
;;;;;;;
;;;;;;; 3. Implement a fun new problem domain for one or both of the filters.  If you wish to
;;;;;;;    do a problem domain for FORWARD function rather than one for a filter,
;;;;;;;    you merely need to specify a single action!
;;;;;;;
;;;;;;; 4. Explore different situations in your new problem domain.


;;;;;;; OUR EXAMPLE MODEL
;;;;;;;
;;;;;;; This simple model is discrete, to keep things simple -- but in fact
;;;;;;; our particle filter should be able to be modified for a continuous
;;;;;;; world fairly trivially. 
;;;;;;;
;;;;;;; In this model we have five rooms:  0 1 2 3 4
;;;;;;; the rooms are organized in a torus (a loop).
;;;;;;;
;;;;;;; [Note that I'm using keywords for actions and sensations]
;;;;;;;
;;;;;;; The robot can perform two actions:   :forward    and    :backward
;;;;;;; The robot can sense two possible sensations in a room:   :even   and   :odd
;;;;;;;
;;;;;;; If a robot is in room x, and tries to go forward, with 1/2 probability
;;;;;;; he will wind up in room x+1 and with 1/2 probability he will stay in room x
;;;;;;;
;;;;;;; If a robot is in an even room (0, 2, or 4) with 95% probability he will
;;;;;;; sense ":even" and with 5% probability he will sense ":odd".  The opposite
;;;;;;; occurs if he is in an odd room (1 or 3). 
;;;;;;;
;;;;;;; Hint: instead of directly accessing *action-model*, *sensor-model*, and *states*
;;;;;;; in your code later on, you might use the states, action-probability, and
;;;;;;; sensor-probability functions instead.  That way if you change your model
;;;;;;; you just need to resubmit those three functions.

(defparameter *debug* nil)

(defparameter *actions* '(:forward :backward))
(defparameter *states* '(0 1 2 3 4))
(defparameter *sensors* '(:even :odd))

(defun states () *states*) 

;;; A description of our action model


(defparameter *action-model*
        #3A((  ;; forward
                (.5 .5 0 0 0)
                (0 .5 .5 0 0)
                (0 0 .5 .5 0)
                (0 0 0 .5 .5)
                (.5 0 0 0 .5))

                ;; backward
               ((.5 0 0 0 .5)
                (.5 .5 0 0 0)
                (0 .5 .5 0 0)
                (0 0 .5 .5 0)
                (0 0 0 .5 .5)))
    "A table showing P(new | old, action) where action is either 
forward or backward, old is the row value, and new is the column 
value.  This basically models the notion that, in a toroidal 
one-dimensional world, if you move forward (or backward), you
have a 1/2 probability of accidentally staying put."
        ;;; note that the values in each row must sum to 1
)
(defun dprint (some-variable &optional (additional-message '()))
	"Debug Print - useful for allowing error/status messages
to be printed while debug=t."
	(if *debug*
		(progn 
			(if additional-message (print additional-message) nil) 
			(print some-variable))
		some-variable))


(defun action-probability (new-state old-state action)
        "Returns the probability that, given an action and 
old state, the new state will be achieved" ;;returns p(newstate|oldstate, action)
        (aref *action-model* 
                (position action *actions*)
                (position old-state *states*)
                (position new-state *states*)))

;;;; A description of our sensor model

(defparameter *sensor-model*
        #2A(    (0.95 0.05)
                (0.05 0.95)
                (0.95 0.05)
                (0.05 0.95)
                (0.95 0.05))
    "A table showing P(sensor | state), where state is the row 
value, and sensor is the column value.  This basically models 
the notion that in a room of index i (0 to 4), your sensor 3/4 
of the time will give you 0 if i is even and 1 if i is odd.  
I dunno, maybe the rooms are light and dark or something.  :-)"
        ;;; note that the values in each row must sum to 1
)

(defun sensor-probability (sensor state)
        "Returns the probability that, given a state, that the
sensor will be the given value"
        (aref *sensor-model* 
                (position state *states*)
                (position sensor *sensors*)))

;;;;;; Some utility functions for normalizing tables, 
;;;;;; creating particles, and counting
;;;;;; particles at the end.

(defun normalize (lis)
        "Normalizes a table"
        (let ((sum (+ 0.0 (apply #'+ lis)))) ;; force to float
                (mapcar (lambda (elt) (/ elt sum)) lis)))

(defun random-elt (seq)
  "Returns a random element from a sequence"
  (elt seq (random (length seq))))

(defmacro gather (times &rest body)
  "Format:  (gather num-times -body-)
Calls the -body- code num-times times.  Each time the value of the last expression
in -body- is appended to a list.  The full list is then returned."
  (let ((x (gensym)) (y (gensym)))
    `(let (,y) (dotimes (,x ,times) (push (progn ,@body) ,y)) (nreverse ,y))))

(defun counts (distribution)
  "Counts the number of times a state particle appears in the distribution"
  (mapcar (lambda (state) (count state distribution)) *states*))


;;;;; Table-based Bayes Filter.  The function BAYES-FILTER takes a collection of
;;;;; previous beliefs about what state we're in and returns a new collection.
;;;;; Given our simple model above, these beliefs will take the form '(p0 p1 p2 p3 p4),
;;;;; which is just a table of probabilities, one per state.

(defun action-probabilities (new-state action);; returns p(new-state|oldState,action) for all old states
  "Given a new state and an action, returns a list of probabilities, one
per old state, of the probability of transitioning to the new state from
that old state given the action"
;; get sta
	;;action-probability (new-state old-state action)
	(dprint "args are")
	(dprint new-state)
	(dprint action)
	(dprint "/")
	(let ((n -1))
		(gather (length (states)) (incf n) (action-probability n new-state action))
	)
)

(defun sensor-probabilities (sensor)
  "Given a sensor value, returns a list of probabilities, one
per state, of the probability of receiving that sensor value given the state"
;;(defun sensor-probability (sensor state))
	(let ((n -1))
		(gather (length (states)) (incf n) (sensor-probability sensor n)))
;;; IMPLEMENT ME

)

(defun bayes-filter (action sensor previous-beliefs)
        "Given a previous belief table, an action, and a sensor 
result, returns a  new belief table about what our new states 
might possibly be.  Belief tables are simply lists of the form 
'(p1 p2 p3 p4 p5 ...) where p1 is the probability for the first 
state, p2 is the probability for the second state, and so on."

	;;; IMPLEMENT ME
	
	
	(let ((current-beliefs (copy-list previous-beliefs)))
		(setf current-beliefs (mapcar '- previous-beliefs  previous-beliefs)) ;; i want it all zeros but the same size as previous beliefs, easiest way to do it i could think of, not efficient
		(dprint "current beliefs are")
		(dprint current-beliefs)
		(dprint "previous beliefs are")
		(dprint previous-beliefs)
		;;call action probabilities, once for each previous belief. 
		(dotimes (n (length (states)))
			(setf current-beliefs (mapcar '+ (dprint (mapcar '* (gather (length (states)) (nth n previous-beliefs)) (action-probabilities n action)) ) current-beliefs))
		)
		
		(dprint "after summation beliefs")
		(dprint current-beliefs)
		;;make these your current beliefs
		(setf current-beliefs (mapcar '* (sensor-probabilities sensor) current-beliefs))
		(dprint "final result after evidence:")
		(dprint (normalize current-beliefs))
		;;call sensor-probabilities
		
		;; apply sensor probabilities to current beliefs
	)
)


;;;;; The particle filter.  The function PARTICLE-FILTER is similar to BAYES-FILTER
;;;;; except that its collections of beliefs take the form of BAGS of STATES.  The same
;;;;; state may repeatedly appear many times in this bag.  The number of times a state is
;;;;; is in the bag basically approximates the probability that we believe we're likely in
;;;;; that state.


;; I use this function to sample a random index from a distribution
;; of the form '(0.1 0.4 0.3 0.2)  for example.  In this example, the
;; returned value would be one of 0, 1, 2, or 3.  You can do this in O(n) if you
;; want, it's okay to be inefficient here.
(defun sample-from-distribution (distribution)
  "Given a possibly non-normalized distribution, in the form of a list of probabilities,
selects from the distribution randomly and returns the index of the selected element."

	(random-elt distribution)
)



;; I use this function to convert a distribution of the form '((x1 w1) (x2 w2) ...)
;; into one like this: '(x1 x1 x1 x2 x2 x3 x4 x4 x4 x5 ...), basically randomly grabbing
;; x's from the previous distribution, proportionally to their weights, and sticking them
;; in the new distribution.  Note that the weights don't have to sum to 1 -- they're
;; weights, not probabilities.
(defun resample-distribution (samples-and-weights &key (sample #'first) (weight #'second))
  "Given a distribution (a list) of M elements, samples M times from that distribution and
forms a new distribution of elements.  Uses a Low-variance 'Stochastic Universal Sampling'
 (check my book for information on this algorithm) sampler, which is what the book uses; 
but a plain-old roulette wheel sampler or something else could have
been used just as well.  The function -sample- provides the probability for an element in the
list.  The function -weight- provides the resulting element that should be returned if 
selected.  By default these are #'first and #'second, presuming that the provided distribution is
of the form ((sample1 weight1) (sample2 weight2) ...)."


	(let ((index 0)
				(beta 0.0)
				(max_w 0.0)
				(new-beliefs ()))
		(dprint index "index: ")
		(dotimes (n (length samples-and-weights))
			(setf max_w (max max_w (funcall weight (nth n samples-and-weights))))
			)
		(dprint max_w "max_w: ")
		(dotimes (n (length samples-and-weights))
			(setf beta (* (random 1.0) max_w 2))
			(dprint beta "beta: ")
			(setf index (random (length samples-and-weights)))
			(loop do
						(setf index (mod (incf index) (length samples-and-weights)))	
						(setf beta (- beta (funcall weight (nth index samples-and-weights))))
						(dprint beta "beta: ")
						(dprint index "new index: ")
				while (> beta 0)
			)
			(setf new-beliefs (append new-beliefs (list (funcall sample (nth index samples-and-weights)))))
		)
		(dprint new-beliefs "new beliefs: ")
	)
)

;;	(dprint new-beliefs "new beliefs: ")


;; Here I grab a random new state selected from the distribution of old ones.
;; note that I'm grabbing from a table -- that doesn't have to be the case,
;; I'm doing it to be compatable with my bayes filter examples.  In fact, you
;; don't have to have a "distribution" at all -- you can just create a function
;; which picks a random new state given the old state and action.  Often these
;; functions are much easier to write than generating a whole probability distribution.
;; And they allow arbitrarily complex continuous distributions to boot.  But here
;; we're going with the simple backward-compatable code...
(defun select-from-action-probabilities (old-state action)
  "Given an old-state and an action, selects a new-state at random and returns it
given the probability distribution P(new-state | old-state, action)."

	(let ((random-new-state (random 1.0)))
		(dprint random-new-state)
		(dolist (new-state (states))
			(if (< (- random-new-state (dprint (setf action-prob (action-probability new-state old-state action)))) 0)
				(return new-state)
				(setf random-new-state (- random-new-state action-prob))
			)
		)
	)
)

;;; (defun action-probability (new-state old-state action)

;; Now we just do the belief update.  Note that beliefs now are different than they
;; were in the past: they're particles, each one representing a state.  Also note that
;; just as you could have written select-from-action-probabilities above without even
;; *having* a distribution, you can do the same for sensor-probability.  That's a major
;; strength of the particle filter.  It makes for easier representations of your
;; "distributions".
(defun particle-filter (action sensor previous-beliefs)
  "Given a previous belief table, an action, and a sensor 
result, returns a  new belief table about what our new states 
might possibly be.  Belief tables are lists of particles.  Each
particle is simply a state."
	(let ((current-beliefs ())
				(current-weights ()))
			(dprint current-beliefs "current beliefs are")
			(dprint previous-beliefs "previous beliefs are")
		(dolist (old-state previous-beliefs current-beliefs)
			(setf current-beliefs (append current-beliefs (list (dprint (select-from-action-probabilities old-state action)))))
		)
			(dprint current-beliefs "after particle movement")
		(dolist (old-state current-beliefs current-weights)
			(setf current-weights (append current-weights (list (dprint (sensor-probability sensor old-state)))))
		)
			(dprint current-weights "after sample measurement")
		(setf samples-and-weights (mapcar #'list current-beliefs current-weights))
			(dprint samples-and-weights "samples and weights")
		(setf current-beliefs (resample-distribution samples-and-weights))
			(dprint current-beliefs "returned from resampling:")
	)
)



;;;; SOME TEST FUNCTIONS
;;;; (example-1-bayes)  and (example-1-particle) should return approximately
;;;; the same numbers.  Likewise for (example-2-bayes) and (example-2-particle)

;;;; For convenience:
(defvar *b*)

;;;; Example 1 : move forward through the environment,
;;;; then back to where you started.  You don't know where
;;;; you started.  Where are you likely to be?


;;; with the bayes filter
(defun example-1-bayes ()
  (let ((b (normalize '(1 1 1 1 1))))
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :forward :even b)) 
    (setf b (bayes-filter :forward :odd b)) 
    (setf b (bayes-filter :forward :even b)) 
    (setf b (bayes-filter :forward :even b)) 
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :forward :even b))
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :odd b)) 
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :odd b)) 
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :odd b))
    (setf b (bayes-filter :backward :even b))
    (format t "Bayes Filter Results: ~a" b)))


;;; with the particle filter
(defun example-1-particle ()
  (let ((b (gather 10000 (random-elt *states*))))
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :forward :even b)) 
    (setf b (particle-filter :forward :odd b)) 
    (setf b (particle-filter :forward :even b)) 
    (setf b (particle-filter :forward :even b)) 
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :forward :even b))
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :odd b)) 
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :odd b)) 
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :odd b))
    (setf b (particle-filter :backward :even b))
    (format t "Particle Filter Results: ~a" (normalize (counts b)))))





;;; example two: If you knew you were in room 0,
;;; then moved forward twice and backward thrice,
;;; and your sensors said even odd even even odd,
;;; where are you likely to be?

;;; with the bayes filter

(defun example-2-bayes ()
  (let ((b '(1 0 0 0 0)))
    (setf b (bayes-filter :forward :even b))
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :backward :even b))
    (setf b (bayes-filter :backward :even b))
    (setf b (bayes-filter :backward :odd b))
    (format t "Bayes Filter Results: ~a" (normalize b))))



;;; with the particle filter
(defun example-2-particle ()
  (let ((b (gather 10000 0)))
    (setf b (particle-filter :forward :even b))
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :backward :even b))
    (setf b (particle-filter :backward :even b))
    (setf b (particle-filter :backward :odd b))
    (format t "Particle Filter Results: ~a" (normalize (counts b)))))

;;; 
;;; Test Functions
;;; 	
	
(defun bayes-filter-test ()
	(dprint (action-probabilities 0 :forward)) 
	(dprint (action-probabilities 0 :backward)) 
	(dprint "sensor test")
	(dprint (sensor-probabilities :odd))    
	(print "bayes filter small example:")  
	(print (bayes-filter :forward :even '(1 0 0 0 0)))
	(example-2-bayes)
)


(defun particle-filter-test ()
	(print "Particle filter small example:")
	(let ((b (gather 10000 0))) ;;(random-elt *states*))))
    		(setf b (particle-filter :forward :even b))

    	(format t "Particle Filter Results: ~a" (normalize (counts b))))	
	(example-2-particle)
)
(bayes-filter-test)
(particle-filter-test)
