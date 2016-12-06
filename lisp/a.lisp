(defvar *db* nil)
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
(defun add-record (cd) (push cd *db*))

; add cd
(add-record (make-cd "a" "b" "c" t))

(defun dump-db ()
    (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

; 入力を読み取る
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t))
    (y-or-n-p "Ripped [y/n]")))

(defun add-cds()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]:")) (return))))


(defun save-db (filename)
  (with-open-file (out filename
                    :direction :output
                    :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
(defun make-comparison-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title (setf (getf row :title) title))
                (if artist (setf (getf row :artist) artist))
                (if rating (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped-p) ripped-p)))
              row) *db*)))


;;;;;;;;;;;;;;;;;;;;;;;;;
; util
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;
; test freamwork
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
                `(let ((,result t))
                   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
                   ,result)))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(macroexpand-1 '(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*))))

(deftest test-+ ()
         (check
           (= (+ 1 2) 3)
           (= (+ 1 2 3) 6)       
           (= (+ -1 -3) -4)))

(deftest test-* ()
         (check
           (= (* 1 2) 2)
           (= (* 3 2) 6)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
         (test-arithmetic))
(test-math)

(defun fib (n)
  (if (< n 2) 
    n
    (+ (fib (1- n)) (fib (- n 2)))))

(test-+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; male
; id
; status 
(defvar *db* nil)
(defvar *item* '(sex id status))

(defun add-item (item) 
  (push item *db*))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defmacro make-item (fields)
  `(list ,@(loop for f in fields collect `(:f ,f))))

*item*


(defmacro a ()
  (loop for i in `(list "a" "b" "c") collect `(:,i)))
(macroexpand-1 '(a))


(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))



(add-item (make-room :sex "a" :id "c")) 

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title (setf (getf row :title) title))
                (if artist (setf (getf row :artist) artist))
                (if rating (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped-p) ripped-p)))
              row) *db*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; spam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defpackage :com.gigamonkeys.spam
;   (:use :common-lisp :com.gigamonkeys.pathnames))

;;; 徐々にめんどくさくなってきたので
;; (in-package :cl-user)
;; 
;; (defpackage :com.gigamonkys.pathnames
;;   (:use :common-lisp)
;;   (:export
;;     :list-directory
;;     :file-exist-p
;;     :directory-pathname-p
;;     :file-pathname-p
;;     :pathname-as-directory
;;     :pathname-as-file
;;     :walk-directory
;;     :directory-p
;;     :file-p))
;; 
;; (defun component-present-p (value)
;;   (and value (not (eql value :unspecific))))
;; 
;; (defun directory-pathname-p (p)
;;   (and
;;     (not (component-present-p (pathname-name p)))
;;     (not (component-present-p (pathname-type p)))
;;     p))
;; 
;; (defun pathname-as-directory (name)
;;   (let ((pathname (pathname name)))
;;         (when (wild-pathname-p pathname)
;;           (error "Can't reliably convert wild pathnames."))
;;         (if (not (directory-pathname-p name))
;;           (make-pathname
;;             :directory (append (or (pathname-directory pathname) (list :relative))
;;                                (list (file-namestring pathname)))
;;             :name nil
;;             :type nil
;;             :defaults pathname)
;;           pathname)))
;; 
;; (defun directory-wildcard (dirname)
;;   (make-pathname
;;     :name :wild
;;     :type #-clisp :wild #+clisp nil
;;     :defaults (pathname-as-directory dirname)))
;; 
;; (defun list-directory (dirname)
;;   (when (wild-pathname-p dirname)
;;     (error "Can only list concrete directory names."))
;;   (directory (directory-wildcard dirname)))
;; 
;; (defun file-exists-p (pathname)
;;   (probe-file pathname)
;;   (error "file-exists-p not implemented"))

(ql:quickload "cl-fad")
(import '(cl-fad:list-directory cl-fad:walk-directory cl-fad:delete-directory-and-files))

(list-directory "/Users/htaguchi/auction/auction/build")

(walk-directory
  #p"/Users/htaguchi/auction/auction/build"
  (lambda (file)
    (when (string-equal (pathname-type file) "md")
      (fresh-line)
      (princ file))))

; (defun classify (text)
;   (classification (score (extruct-features text))))

(defpackage :com.gigamonkeys.spam
  (:use :common-lisp))

(in-package :com.gigamonkeys.spam)

(defparameter *max-ham-score* .4)
(defparameter *max-spam-score* .6)

(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))

(defclass word-feature ()
  ((word
     :initarg :word
     :accessor word
     :initform (error "Must supply :word")
     :documentation "この特徴を表す単語")
   (spam-count
     :initarg :spam-count
     :accessor spam-count
     :initform 0
     :documentation "この特徴が出現したスパムの数")
   (ham-count
     :initarg :ham-count
     :accessor ham-count
     :initform 0 
     :documentation "この特徴が出現したハムの数")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
     (setf *feature-database* (make-hash-table :test #'equal)))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(ql:quickload "cl-ppcre")
(defun extract-words (text)
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
    :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

; object inspector 
(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

;; (defun train (text type)
;;   (ecase type
;;     (ham (incf (ham-count feature)))
;;     (spam (incf (spam-count feature)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; http client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :drakma)
(drakma:http-request "http://lisp.org/")



