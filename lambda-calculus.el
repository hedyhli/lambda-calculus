;; -*- lexical-binding: t; fill-column: 80; eval: (ruler-mode); eval: (load-theme 'modus-vivendi t) -*-
;;
;; This file is intended to be run interactively in Emacs or a similar editor
;; that supports evaluating s-expressions and viewing the result live.
;;
;; Only uncommented s-exps are meant to be evaluated uninteractively as part of
;; the tutorial.
;;
;; Coming back after having finished the whole thing? eval-buffer is perfectly
;; safe to do!
;;
;; Elisp note
;;; Elisp lambdas can define multiple parameters, but we restrict ourselves to
;;; using only 1 to demonstrate lambda-calculus.
;;;
;;; defun is not used, and the use of lambda bound to a symbol using 'setq gives
;;; us the restriction of using 'funcall for calling them. This doesn't really
;;; matter because as functions are going to be passed around, funcall would
;;; have to be used anyway. Sticking to lambdas all the way makes the experience
;;; consistent and closer to that of lambda-calculus. Convenience functions for
;;; these to make it less tedious (despite being lambda-calculus) will be
;;; defined later on.
;;
;; Conventions for naming helpers
;;; !*  shows results of a type as a human-readable symbol shown in the minibuffer
;;;     (reflection)
;;; :*  expression, implements a core or just an alias
;;; /*  statement, which implements a core or just an alias
;;
;; We'll be restricting ourselves to using only the following elisp symbols
;; (except in cases of helpers definitions):
;;; setq     bind a name
;;; lambda   function expression!  (this and above will be helper-ized later on)
;;; funcall  call a name or function expression!
;;;
;;; As well as constraint that all functions here must return a function. These
;;; functions can be reflected using =* functions into the elisp world.
;;;
;;; Functions that interact with the elisp world are *named elisp functions*
;;; that can be called directly. Lambda-calculus functions must be called
;;; funcall.
;;
;; And now we'll ignore elisp ever existed, and begin with lambda-calculus!
;;
;; What we'll be defining
;;; Symbols
;;;; true, false, num/0, num/1, num/2
;;; Functions
;;;; ift   if then
;;;; or, and, not
;;;; S     successor of a number
;;; reflection
;;;; !bool, !num

;; Just a few helpers to start with.
;;; NOTE Another char is needed after `:` so emacs highlights it has a keyword
;;;      because apparently alias functions are not highlighted as functions.
(defalias ':: 'funcall)
(defalias ':L 'lambda)

;; Basic reminder
;;; Define a function
;;;; (setq func-name (:L (only-one-arg-please) 'return-either-symbol-or-function))
;;;
;;; Call a function
;;;; (:: func-name 'the-one-argument)
;;;
;;; Additional arguments
;;;; (setq multi-args (:L (first) (:L (second) 'do-something)))
;;;; (:: (:: multi-args 'first-arg) 'second-arg)
;;;;
;;;; Notice how there isn't really much of a difference between a partial
;;;; function and a function that actually returns a function!
;;;
;;; That's all you need to know to start with!

;; Define booleans
;;; Use as symbols
(setq true (:L (x) (:L (y) x)))
(setq false (:L (x) (:L (y) y)))

;; Define simple conditional
;;; Use as (:: (:: (:: ift cond) if-true-run-this) else-run-this)
(setq ift (:L (b) (:L (x) (:L (y) (:: (:: b x) y)))))
;; Try it out!
;;;; (:: (:: (:: ift true) 'yes) 'nope)
;;;; (:: (:: (:: ift false) 'yes) 'nope)

;; A few s-exps to explore the evaluation of to get an intuition of how it
;; works.

;;; What's a 'true?
;;;; (closure ((x . 1)) (y) x)
;;; It returns a function that will return its first argument, 1, regardless
;;; what the next argument you pass to the resulting function.
;;;; (:: true 1)
;;; And a 'false?
;;;; (closure (t) (y) y)
;;; It does not care what you pass it, returns an identity function.
;;;; (:: false 1)

;; Now digging into the definition of 'ift.
;;;; (:: (:: b x) y)
;;;
;;; Argument x is passed to the predicate (which in lambda calculus, is, of
;;; course, a function), then evaluate the return value, which is also a
;;; function, with argument y.
;;;
;;; Magic!
;;;
;;; Traceback
;;;; (:: (:: (:: ift true) 'yes) 'nope)
;;;;    (:: (:: true 'yes) 'nope)
;;;;       (:: '(:L (y) 'yes) 'nope)
;;;;         ('(:L ('nope) 'yes))
;;;;         'yes
;;;
;;;; (:: (:: (:: ift false) 'yes) 'nope)
;;;;    (:: (:: false 'yes) 'nope)
;;;;       (:: '(:L (y) y) 'nope)
;;;;         ('(:L ('nope) 'nope) 'yes)
;;;;         'nope
;;;
;;; Beautiful!
;;;

;;; Now that we know what, really, a boolean is...
;;; Achievement unlocked... new helper!
(defun !bool (x) (:: (:: (:: ift x) 'true) 'false))
;;;; (!bool true)

;; BTW What happens when you pass true and false to each other?
;;; How do you make sense of this? 🤔
;;;; (!bool (:: (:: true false) false))
;;;; (!bool (:: (:: true false) true))
;;;; (!bool (:: (:: true true) false))
;;;; (!bool (:: (:: true true) true))
;;;; (!bool (:: (:: false false) false))
;;;; (!bool (:: (:: false false) true))
;;;; (!bool (:: (:: false true) false))
;;;; (!bool (:: (:: false true) true))

;; Try defining boolean operators!
;;; This reads quite literally as, if true then false, else true.
;;; Never knew 'not depends on 'if!
(setq not (:L (x) (:: (:: (:: ift x) false) true)))
;;; Try it out!
;;;; (!bool (:: not true))
;;;; (!bool (:: not false))
;;;; (:: (:: (:: ift (:: not true)) 'yes) 'nope)
;;;; (:: (:: (:: ift (:: not false)) 'yes) 'nope)
;;; Reads as
;;;; if x is true
;;;;   then true
;;;;   else, whatever y is
(setq or (:L (x) (:L (y) (:: (:: (:: ift x) true) y)  )))
;;;; (!bool (:: (:: or false) true))
;;;; (!bool (:: (:: or true) false))
;;;; (!bool (:: (:: or false) false))
;;;; (!bool (:: (:: or true) true))
;;;; (:: (:: (:: ift (:: (:: or true) false)) 'yes) 'nope)
;;;; (:: (:: (:: ift (:: (:: or false) false)) 'yes) 'nope)
;;; Reads as
;;;; if x is true
;;;;   then whatever y is
;;;;   else, false
(setq and (:L (x) (:L (y) (:: (:: (:: ift x) y) false) )))
;;; Example
;;;; (!bool (:: (:: and true) true))
;;;; (!bool (:: (:: and false) true))
;;;; (:: (:: (:: ift (:: (:: and true) true)) 'yes) 'nope)
;;;; (:: (:: (:: ift (:: (:: and true) false)) 'yes) 'nope)
;;; Boolean operators combined!
;;;; (T or F) and T
;;;; (!bool (:: (:: and (:: (:: or true) false)) true))

;; +*+ Achievement unlocked +*+
;; +*+   *3* new helpers!   +*+
;; Here's our last funcall helper that we should be familiar with booleans and
;; multiple-arguments.
(defmacro : (fn &rest args)
  "Call function with multiple arguments.
Use as (: my-function arg1 arg2 arg3)"
  (seq-reduce (lambda (prev arg) (list ':: prev arg)) args fn))

;;; Examples
;;;; (!bool (: not true))
;;;; (macroexpand '(: ift true 'yes 'no))
;;; Convenience for if-then functions
;;;; (: ift true 'yes 'no)
;;;; (: ift (: not true) 'yes 'no)
;;; Convenience for boolean operators
;;;; (!bool (: and false true))
;;;; (: ift (: and true (: or true false)) 'yes 'no)
;;; now these functions sure feel a lot like lisps'

;; A final helper for lambda (until we get... keyword/var-args?!), for we should
;; now be familiar with how multiple arguments are implemented.
(defmacro ::L (params body)
  "A lambda with multiple arguments
Use as (setq func-name (::L (a b c) a))"
  (seq-reduce (lambda (prev p) (list ':L (list p) prev)) (reverse params) body))
;;; See how it works...
;;; (macroexpand '(::L (a b c) a))
(defmacro /define (name params body)
  "Shortcut for defining a function with multiple arguments.
Usage: (/define func-name (x y z) 'my-result)"
  `(setq ,name (::L ,params ,body)))
;;; (macroexpand-all '(/define func-name (a b) a))
;;; We'll be using these from now on!

;; Now let's do numbers!
;;; ✨ Church Numerals ✨
;;;
;;; Using /define declutters the definition from the body and makes it clear
;;; what's happening: Repeatedly call f with the result, using x as the initial
;;; value N times, where N is the actual number! 🤯
(/define num/0 (f x) x)
(/define num/1 (f x) (:: f x))
(/define num/2 (f x) (:: f (:: f x)))
;;; See how it works?
;;; Try defining a !num!
;;; Unlike !bool, which converts a boolean true/false from LC into elisp as a
;;; a symbol as a hack to make it print in the minibuffer without quotes. this
;;; new reflection !num converts it into an elisp number!
(defun !num (n)
  (: n (:L (x) (1+ x)) 0))
;;;; Intuitive, isn't it? A little like the reduce function from functional
;;;; programming. Repeatedly call '1+ with the given result, starting with 0.
;;;;
;;;; Try it -- (!num num/2)
;;; Now here's our successor function.
;;; Using the :: shortcut makes it intuitive.
;;; It calls f with x using the existing number n, then calls it again using the
;;; result!
(/define S (n f x) (: f (: n f x)))

;; Examples in action!
;;; 1 + 1 + 0
;;;; (!num (: S (: S num/0)))
;;; (!num num/2)
;;; 1 + 1 + 1 + 1 + 2
;;;; (!num (: S (: S (: S (: S num/2)))))
;; (!num (::L (f x) (: f (: num/1 f x))))

;; Arithmetic!
;;; See if you can define addition, multiplication, and exponetiation yourself.
;;;
;;; HINT: Whatever you do, do *NOT* use elisp numbers (1, 2, 3, ...), use num/0,
;;;       num/1 instead!
;;;
;;; Addition
;;;; For each m, apply S on the previous result, where the first arg passed to S
;;;; is n.
(/define add (n m) (: m S n))
;;;; (!num (: add num/1 num/2))
;;;
;;; Let's make some more numbers for convenience.
;;; num/3 is based on num/2, by emulating S.
(/define num/3 (f x) (: f (: num/2 f x)))
;;; and we'll just use S for num/4
(setq num/4 (: S num/3))
;;;; (!num (: add (: add num/2 num/3) num/3))
;;;; (= 7 (!num (: add num/3 num/4)))
;;;
;;; Multiplication
;;;; Make a partial function that will run f m times, then pass that function
;;;; to n which will run it n times.
(/define mult (n m f) (: n (: m f)))
;;;; (!num (: mult num/2 num/3))
;;;; Let's try... S(1 + 3) * (2 + 3 + 1) = 30
;;;;; (!num (: mult (: S (: add num/1 num/3)) (: add (: add num/2 num/3) num/1)))
;;
;; Now see if you can define a "reverse !num". Convert an elisp number into a LC
;; number!
;;
;; Here're some testcases:
;; (= (!num num/2) (!num (num/from 2)))
;; (= 11 (!num (: S (num/from 10))))
(defmacro num/from (n)
  (let ((res 'x))
    (while (> n 0)
      (setq res `(: f ,res)
	    n    (1- n)))
    `(::L (f x) ,res)))

;; Here comes exponentiation!
(/define exp (n e) (: e (: mult n) num/1))
;;; (!num (: exp num/1 num/0))
;;; (!num (: exp num/0 num/2))
;;; (!num (: exp num/2 num/3))
;;; 2^3 == 4 * 2
;;;; (= (!num (: exp num/2 num/3)) (!num (: mult num/4 num/2)))

;; Putting numbers and booleans together.
;;; First, let's make a number equality check.
;;; Previously, we're escaping into elisp numbers to check whether two numbers
;;; are the same. Let's change that.
(/define eq-0 (n) (: n (:L (_) false) true))
;;;; Alternatively, using 'true as a Kestrel:
;;;;; (/define eq-0 (n) (: n (: true false) true))
;;; (!bool (: eq-0 num/0))
;;; (!bool (: eq-0 num/1))
(/define eq-1 (n) (: n (:L (b) (: ift b false true)) false))
;;;; Hmm, that sure looks a lot like 'not!
;;;;; (/define eq-1 (n) (: n not false))
;;; (!bool (: eq-1 num/0))
;;; (!bool (: eq-1 num/1))
;;; (!bool (: eq-1 num/2))
