;; to test with eql
(defvar *elp-secret-value* nil)
(setq *elp-secret-value* "secret")
(defvar *elp-undefined* nil)
(setq *elp-undefined* "undefined")


(defvar *elp-object-prototype*
  (make-hash-table))

(defun elp-call (__object __prop &rest __args)
  ;; uhh.. i dunno how to cache lookup.
  ;; if property value is a function, we apply it.
  ;; if property value is any other value type, return it
  (let ((self __object))
    ;; we use flet and let to set the dynamic
    ;; bindings for self for both variable and
    ;; function.
    (lexical-let ((val (elp-prop __object __prop)))
      (if (functionp val)
          (apply val __args)
          val))))

(defmacro self (&rest args)
  `(elp-call-form (self ,@args)))

(defmacro elp-call-form (form)
  (let* ((object (car form))
         (es (reverse (cdr form)))
         args
         method
         chain)
    (while (not (null es))
      (catch 'next
        (while t
          (if (keywordp (car es))
              (throw 'next nil)
              (if (null es)
                  (error "expects a keyword")
                  (push (pop es) args)))))
      (push (cons (pop es) args) chain)
      (setq args nil))
    ;;(pr 'call-form form)
    (labels ((recur (calls object)
               (if (null calls)
                   object
                   (let* ((call (car calls))
                          (method (intern (substring (symbol-name (car call)) 1)))
                          (args (cdr call)))
                     (recur (cdr calls)
                            `(elp-call ,object ',method
                                       ,@args)))
                   )))
      (recur chain object)
      )))



(defvar Object nil)
(setq Object (vector 'Object *elp-object-prototype* (make-hash-table)))

(puthash 'constructor 'Object *elp-object-prototype*)
(puthash 'get 'elp-get *elp-object-prototype*)
(puthash 'set 'elp-set *elp-object-prototype*)

(defun Object ()
  (vector 'Object Object (make-hash-table)))

(defun elp-set (prop val)
  (case prop
    ;; quick access for builtin properties
    (get (error "get is constant"))
    (set (error "set is constant"))
    (constructor (error "set is fixed"))
    (prototype
     (if (eql self val)
         (error "can't set prototype to self")
         (aset self 1 val)))
    (:properties
     (if (hash-table-p val)
         (aset self 2 val)
         (error "must set property to a hashtable")))
    (t (puthash prop val (aref self 2)))))

(defun elp-get (prop)
  (elp-prop self prop))

(defun elp-prop (__object __prop)
  ;; first lookup in object properties
  (case __prop
    ;; quick access for builtin properties
    (get 'elp-get)
    (set 'elp-set)
    (constructor (aref __object 0))
    (prototype (aref __object 1))
    (properties (aref __object 2))
    (t (lexical-let ((val (gethash __prop (aref __object 2) *elp-undefined*)))
         (if (eql *elp-undefined* val)
             (if (eql __object Object)
                 ;; lookup the prototype of root object. If not found, return undefined
                 (gethash __prop (aref __object 1) *elp-undefined*)
                 ;; recurse into prototype
                 (elp-prop (aref __object 1) __prop))
             val)))))

(defmacro obj (bindings-or-var &rest body)
  (cond ((listp bindings-or-var)
         (let ((bindings bindings-or-var)
               (body body))
           (labels ((recur (bindings body)
                      (if (null bindings)
                          body
                          (recur (cddr bindings)
                                 (let ((name (second bindings))
                                       (val (first bindings))
                                       (args (gensym)))
                                   `(let ((,name ,val))
                                      (macrolet ((,name (&rest ,args)
                                                   `(elp-call-form (,',name ,@,args))))
                                        ,body))
                                   )
                                 ))))
             (recur (reverse bindings) `(progn ,@body)))))
        ((symbolp bindings-or-var)
         (let ((name bindings-or-var)
               (args (gensym)))
           (pr 'obj name)
           `(macrolet ((,name (&rest ,args)
                           (pr ,args)
                           `(elp-call-form (,',name ,@,args))))
                ,@body)
           ))))

(defmacro defobj (name init-args &rest body)
  `(progn
     (defvar ,name nil)
     (setq ,name (vector ',name Object (make-hash-table)))
     (defun ,name ,init-args
       (obj (self (vector ',name ,name (make-hash-table)))
            ,@body
            self))))

;; bootstrap completed

(defun elp-extend (o)
  (obj o
    (maphash (lambda (k v) (self :set k v))
             (o :get 'properties))
       self))

(obj Object
  (Object :set 'extend 'elp-extend))

(defobj Enum ())

(obj Enum
     (Enum :set 'map 'elp-enum-map)
     (Enum :set 'select 'elp-enum-select))

(defun elp-enum-map (fn)
  (lexical-let ((fn fn)
                (acc nil))
    (self :each
          (lambda (e)
            (setq acc (cons (funcall fn e) acc))))
    (reverse acc)))

(defun elp-enum-select (fn)
  (lexical-let ((fn fn)
                (acc nil))
    (self :each
          (lambda (e)
            (if (funcall fn e)
                (setq acc (cons e acc))
                (next))))
    (reverse acc)))

(defun elp-enum-map (fn)
  ;; lame. gotta use lexical binding, else nested
  ;; lambdas bounded to the same symbol would
  ;; flatten to self recursion.
  (lexical-let ((fn fn)
                (acc nil))
    (self :each
          (lambda (e)
            (setq acc (cons (funcall fn e) acc))))
    (reverse acc)))

(defobj Array (o)
  (self :set 'content
        (cond ((arrayp o) o)
              ((listp o) (vconcat o)))))

(obj Array
  (Array :set 'size
         (lambda () (length (self :content))))
  (Array :set 'length
         (lambda () (self :size)))
  (Array :set 'at
         (lambda (i) (aref (self :content) i)))
  (Array :set 'each 'elp-array-each)
  (Array :extend Enum))

(defun elp-array-each (fn)
  (lexical-let ((fn fn))
    (flet ((next () (throw 'next nil))
           (break () (throw 'break nil)))
      (catch 'break
        ;; (loop for i from 0 to (1- (self :length))
;;            do (catch 'next (funcall fn (self :at i))))
        (let ((content (self :content)))
          (loop for i from 0 to (1- (length content))
             do (catch 'next (funcall fn (aref content i)))))
        self))))

