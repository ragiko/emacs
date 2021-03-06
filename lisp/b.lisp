(defparameter *small* 1)
(defparameter *big* 1)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
    (setf *small* (1+ (guess-my-number)))
    (guess-my-number))

(labels ((a (n)
	   (+ n 5))
	 (b (n)
	   (+ (a n) 6)))
  (b 10))

(if '()
    'i-am-true
    'i-am-false)


(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))


(defun foo (&key (a "test"))
  a)

;; 12/2
; マクロでswiftのラベルっぽくかきたい
(defun foo (&key a)
  (let ((b a))
    (format t "a=~a" b)))
(foo :a 10)

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))
(setq add2 (make-adder 2)
      add5 (make-adder 5))

(defparameter *foo* (lambda ()
		      5))


(defun add (a b)
  (let1 x (+ a b)
    (format t "~a" x)
    x))

;; 12/3
(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

(defun my-length2 (lst)
  (labels ((f (lst acc)
	     (split lst
		    (f tail (1+ acc))
		    acc)))
    (f lst 0)))

(defmacro while (test &body body)
  `(do ()
    ((not ,test))
    ,@body))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
	     (tail (cdr ,val)))
	 ,yes)
       ,no))

(defmacro split/2 (val yes no)
  `(let1 x ,val
     (if ,val
	 (let ((head (car ,val))
	       (tail (cdr ,val)))
	   ,yes)
	 ,no)))

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro split/3 (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,val
	   (let ((head (car ,val))
		 (tail (cdr ,val)))
	     ,yes)
	   ,no))))

(defun my-length/3 (lst)
  (labels ((f (lst acc)
	     (split/3 lst
		      (f tail (1+ acc))
		      acc)))
    (f lst 0)))

(princ (reverse
	(format nil "test test test" 1.5)))


(defun pairs (lst)
  (labels ((f (lst acc)
	     (split/3 lst
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
		,@body))
       (self ,@(mapcar #'cdr p)))))

(recurse (n 9)
  (fresh-line)
  (if (zerop n)
      (princ "off")
      (progn (princ n)
	     (self (1- n)))))

(defun my-length/4 (lst)
  (recurse (lst lst acc 0)
    (split/3 lst
	   (self tail (1+ acc))
	   acc)))
