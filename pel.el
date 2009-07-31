;; to test with eql
(defvar *pel-secret-value* nil)
(setq *pel-secret-value* "secret")
(defvar *pel-undefined* nil)
(setq *pel-undefined* "undefined")


(defvar *pel-object-prototype*
  (make-hash-table))

(defun pel-call (__object __prop &rest __args)
  ;; uhh.. i dunno how to cache lookup.
  ;; if property value is a function, we apply it.
  ;; if property value is any other value type, return it
  (let ((self __object))
    ;; we use flet and let to set the dynamic
    ;; bindings for self for both variable and
    ;; function.
    (lexical-let ((val (pel-prop __object __prop)))
      (if (functionp val)
          (apply val __args)
          val))))

(defmacro self (&rest args)
  `(pel-call-form (self ,@args)))

(defmacro pel-call-form (form)
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
                            `(pel-call ,object ',method
                                       ,@args)))
                   )))
      (recur chain object)
      )))



(defvar Object nil)
(setq Object (vector 'Object *pel-object-prototype* (make-hash-table)))

(puthash 'constructor 'Object *pel-object-prototype*)
(puthash 'get 'pel-get *pel-object-prototype*)
(puthash 'set 'pel-set *pel-object-prototype*)

(defun Object ()
  (vector 'Object Object (make-hash-table)))

(defun pel-set (prop val)
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

(defun pel-get (prop)
  (pel-prop self prop))

(defun pel-prop (__object __prop)
  ;; first lookup in object properties
  (case __prop
    ;; quick access for builtin properties
    (get 'pel-get)
    (set 'pel-set)
    (constructor (aref __object 0))
    (prototype (aref __object 1))
    (properties (aref __object 2))
    (t (lexical-let ((val (gethash __prop (aref __object 2) *pel-undefined*)))
         (if (eql *pel-undefined* val)
             (if (eql __object Object)
                 ;; lookup the prototype of root object. If not found, return undefined
                 (gethash __prop (aref __object 1) *pel-undefined*)
                 ;; recurse into prototype
                 (pel-prop (aref __object 1) __prop))
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
                                                   `(pel-call-form (,',name ,@,args))))
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
                           `(pel-call-form (,',name ,@,args))))
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

(defun pel-extend (o)
  (obj o
    (maphash (lambda (k v) (self :set k v))
             (o :get 'properties))
    self))

;; bootstrap completed

(obj Object
  (Object :set 'extend 'pel-extend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enum

(defobj Enum ())

(obj Enum
     (Enum :set 'map 'pel-enum-map)
     (Enum :set 'select 'pel-enum-select)
     (Enum :set 'all? 'pel-enum-all)
     (Enum :set 'any? 'pel-enum-any))

(defun pel-enum-map (fn)
  (lexical-let ((fn fn)
                (acc nil))
    (self :each
          (lambda (e)
            (push (funcall fn e) acc)))
    (self :constructor (nreverse acc))))

(defun pel-enum-all (fn)
  (lexical-let ((fn fn)
                (r t))
    (self :each
          (lambda (e)
            (unless (funcall fn e) (setq r nil)
                    (break))))
    r))

(defun pel-enum-any (fn)
  (lexical-let ((fn fn)
                (r nil))
    (self :each
          (lambda (e)
            (if (funcall fn e) (setq r t)
                (break))))
    r))

(defun pel-enum-select (fn)
  (lexical-let ((fn fn))
    (self :map
          (lambda (e) 
            (if (funcall fn e)
                e
                (next))))))

(defun pel-enum-map (fn)
  ;; lame. gotta use lexical binding, else nested
  ;; lambdas bounded to the same symbol would
  ;; flatten to self recursion.
  (lexical-let ((fn fn)
                (acc nil))
    (self :each
          (lambda (e) (push (funcall fn e) acc)))
    (List (nreverse acc))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seq Types
;;
;; Rather than going along with emacs, I chose to
;; call Vector "Array", and Array "Vector".
(defobj Seq ())
;; Array, String, List
(obj Seq
  (Seq :extend Enum)
  (Seq :set 'size
         (lambda () (length (self :el))))
  (Seq :set 'length
         (lambda () (self :size)))
  (Seq :set 'at
         (lambda (i) (elt (self :el) i)))
  (Seq :set 'at!
       (lambda (i v)
         (setf (elt (self :el) i) v)
         self))
  (Seq :set 'copy
         (lambda () (self :constructor (copy-sequence (self :el)))))
  (Seq :set 'to_a
       (lambda () (Array (self :el))))
  (Seq :set 'to_s
       (lambda () (String (self :el))))
  (Seq :set 'to_l
       (lambda () (List (self :el))))
  (Seq :set 'to_sym
       (lambda () (intern (self :to_s :el)))))

;;

;; Array, String, char-table, bool-vector
(defobj Vector ())
(obj Vector
  (Vector :extend Seq)
  (Vector :set 'each 'pel-vector-each)
  )

(defun pel-vector-each (fn)
  (lexical-let ((fn fn))
    (flet ((next () (throw 'next nil))
           (break () (throw 'break nil)))
      (catch 'break
        ;; (loop for i from 0 to (1- (self :length))
;;            do (catch 'next (funcall fn (self :at i))))
        (let ((el (self :el)))
          (loop for i from 0 to (1- (length el))
             do (catch 'next (funcall fn (aref el i)))))
        self))))

(defobj Array (o)
  (self :set 'el
        (cond ((vectorp o) o)
              ((sequencep o) (vconcat o))
              (t (error "expects a sequence")))))
(obj Array
  (Array :extend Vector))

(defobj String (o)
  (self :set 'el
        (cond ((stringp o) o)
              ((sequencep o) (concat o)))))

(obj (o String)
  (o :extend Vector)
  (o :set 'gsub
     (lambda (regexp rep)
       (String (replace-regexp-in-string regexp rep (self :el))))
     )
  (o :set 'match
     (lambda (regexp fn)
       (lexical-let* ((str (self :el))
                      (fn fn))
         (when (string-match regexp str)
           (let (;; before match
                 ($b (substring str 0 (match-beginning 0)))
                 ;; match itself
                 ($m (match-string 0 str))
                 ;; after match
                 ($a (substring str (match-end 0) (length str))))
             (flet (($ (n) (match-string n str))
                    ;; for consistency
                    ($b () $b)
                    ($m () $m)
                    ($a () $a)
                    )
               (funcall fn)))))))
  (o :set 'capitalize
     (lambda () (String (capitalize (self :el))))))

(defobj List (o)
  (self :set 'el
        (cond ((listp o) o)
              ((sequencep o) (append o nil)))))

(obj List
  (List :extend Seq)
  (List :set 'each 'pel-list-each))

(defun pel-list-each (fn)
  (lexical-let ((fn fn))
    (flet ((next () (throw 'next nil))
           (break () (throw 'break nil)))
      (catch 'break
        (let ((el (self :el)))
          (loop for e in el
             do (catch 'next (funcall fn e))))
        self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit Types

;;(Buffer :temp)
;;(Buffer "name")
;;(Buffer) == (Buffer :current)
(defobj Buffer (&optional buf-or-name)
  (self :set 'el (if buf-or-name (get-buffer buf-or-name)
                     (current-buffer))))

(macrolet ((wcb (&rest body)
             `(with-current-buffer (self :el)
                ,@body)))
  (obj (b Buffer)
    (b :set 'to_s
       (lambda (&optional from to no-properties)
         (String (self :substr from to no-properties))))
    (b :set 'substr
       (lambda (&optional from to no-properties)
         (wcb (let ((from (or from (point-min)))
                    (to (or to (point-max))))
                (if no-properties
                    (buffer-substring-no-properties from to)
                    (buffer-substring from to))))))
    (b :set 'size
       (lambda ()
         (buffer-size (self :el))))
    (b :set 'length
       (lambda () (self :size)))
    (b :set 'n
       (lambda () (wcb (point))))
    (b :set 'eob?
       (lambda () (wcb (eobp))))
    (b :set 'each
       (lambda (fn &optional no-props)
         (lexical-let ((fn fn)
                       (done nil))
           (wcb (self :gg)
                (while (not done)
                  (let ((a (self :^ :n))
                        (b (self :$ :n)))
                    (if (eobp) (setq done t)
                        (self :> 1 'line)
                        (funcall fn (self :to_s a b no-props)))))))))
    ;; insertion
    (b :set 'insert
       (lambda (str) (with-current-buffer (self :el)
                       (insert str))))
    ;;insert-before-markers
    ;;insert-buffer-substring
    ;;insert-buffer-substring-no-properties
    ;;
    ;; motion methods
    (b :set 'go
       (lambda (pos)
         (wcb (pel-buffer-goto pos))
         self))
    (b :set 'gg
       (lambda ()
         (wcb (beginning-of-buffer))
         self))
    (b :set 'GG
       (lambda ()
         (wcb (end-of-buffer))
         self))
    (b :set '^
       (lambda ()
         (wcb (beginning-of-line))
         self))
    (b :set '$
       (lambda ()
         (wcb (end-of-line))
         self))
    (b :set '>
       (lambda (&optional n kind)
         (wcb (pel-buffer-move 1 n kind))
         self))
    (b :set '<
       (lambda (&optional n kind)
         (wcb (pel-buffer-move -1 n kind))
         self))

    (b :set 're>
       ;; don't want to return self, because we want to know the search result.
       (lambda (re &optional fn)
         (pel-buffer-re> re fn)))
    (b :set 're<
       (lambda (re &optional n fn)
         (pel-buffer-re< re fn)))
    
    ;; context setting methods
    (b :set 'narrow
       (lambda (&optional from to)
         (wcb (let ((from (or from (point-min)))
                    (to (or to (point-max))))
                (narrow-to-region from to)))))
    (b :set 'widen
       (lambda ()
         (wcb (let ((from (or from (point-min)))
                    (to (or to (point-max))))
                (widen)))))
    (b :set 'restrict
       (lambda (from to fn)
         (lexical-let ((fn fn))
           (save-restriction
             (self :narrow from to)
             (wcb (funcall fn)))
           )))
    (b :set 'keep
       (lambda (fn)
         (lexical-let ((fn fn))
           (save-excursion
             (wcb (funcall fn))))))
    (b :set 'do
       (lambda (fn)
         (lexical-let ((fn fn))
           (wcb (funcall fn)))))))


(defun pel-buffer-goto (n &optional kind)
  (let ((kind (or kind 'char)))
    (cond ((eql kind 'char)
           (goto-char n))
          ((eql kind 'line)
           (goto-line n))
          )))

(defun pel-buffer-move (dir &optional n kind)
  (let ((n (* (or n 1) dir))
        (kind (or kind 'char)))
    (case kind
      (char (forward-char n))
      (word (forward-word n))
      (line (forward-line n)))))

(defun pel-buffer-re> (re &optional fn)
  (pel-buffer-re-move 're-search-forward re fn))

(defun pel-buffer-re< (re &optional fn)
  (pel-buffer-re-move 're-search-backward re fn))

(defun pel-buffer-re-move (re-search re &optional fn)
  (lexical-let ((fn fn)
                (r (funcall re-search re nil t)))
    (when r
      (flet (($ (n) (match-string n))
             (sub (replacement &optional subexp fixedcase literal)
               (replace-match replacement
                              fixedcase literal nil subexp)
               ;; apparently replace-match doesn't
               ;; return anything when acting on
               ;; buffer. More useful to return non-nil value.
               t))
        (if fn (funcall fn)
            r)))))




;current-word
