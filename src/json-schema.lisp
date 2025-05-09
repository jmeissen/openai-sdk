(in-package #:cl-user)

(uiop:define-package openai-sdk/json-schema
  (:use #:cl)
  (:shadow #:step)
  (:nicknames #:oai/js)
  (:export #:generate-json-schema
           #:structured-output-model
           #:def-list-type
           #:string-oai-list
           #:integer-oai-list
           #:float-oai-list
           #:boolean-oai-list))

(in-package #:openai-sdk/json-schema)

(defclass structured-output-model () ())

(defmacro def-list-type (class)
  (let ((class-string (format nil "~a" class)))
    (flet ((funsym (el str) (intern (concatenate 'string el str))))
      (let ((class-sym (funsym class-string "-LIST-P"))
            (type-sym (funsym class-string "-OAI-LIST")))
        `(progn
           (defun ,class-sym (el)
             (and (consp el)
                  (typep el ',class)))
           (deftype ,type-sym ()
             '(satisfies ,class-sym))
           '(,class-sym ,type-sym))))))

(def-list-type string)
(def-list-type integer)
(def-list-type float)
(def-list-type boolean)

(defparameter *default-json-schema-types*
  '((string . "string")
    (integer . "int")
    (float . "float")
    (boolean . "bool")
    (null . "null")))

(defmethod com.inuoe.jzon:coerced-fields ((element structured-output-model))
  (macrolet ((%type-hash-table (type) `(alexandria:plist-hash-table (list "type" ,type) :test #'equal))
             (%coerced-fields-slots (element)
               `(let ((class (class-of ,element)))
                  (c2mop:ensure-finalized class)
                  (alexandria:alist-hash-table
                   (mapcar
                    (lambda (s)
                      (let ((slot-name (c2mop:slot-definition-name s))
                            (slot-type (c2mop:slot-definition-type s)))
                        (cons
                         (symbol-munger:lisp->underscores slot-name) ; snake_case slot-name
                         (cond         ; determine slot-value content (slot-type)
                           ((assoc slot-type *default-types*) (%type-hash-table (cdr (assoc slot-type *default-types*))))
                           ((and (consp slot-type) (eq 'member (car slot-type))) (let ((items (mapcar 'symbol-munger:lisp->underscores
                                                                                                      (cdr (introspect-environment:typexpand
                                                                                                            slot-type)))))
                                                                                   (%type-hash-table (if (= 1 (length items))
                                                                                                         (car items)
                                                                                                         items))))
                           ((find-class slot-type nil) (make-instance (find-class slot-type)))
                           ((and (eql 'satisfies (car (introspect-environment:typexpand slot-type)))
                                 (search "-oai-list" (format nil "~(~A~)" slot-type) :test #'equal :from-end t))
                            (alexandria:plist-hash-table (let* ((type-str (format nil "~A" slot-type))
                                                                (type-class (intern (subseq type-str 0 (- (length type-str)
                                                                                                          (length "-oai-list")))))
                                                                (default-type-p (assoc type-class *default-types*)))
                                                           (list :type "array"
                                                                 :items (or (and default-type-p (%type-hash-table (cdr default-type-p)))
                                                                            (make-instance (find-class type-class)))))))))))
                    (c2mop:class-slots class))))))
    `(("type" "object")
      ("properties" ,(%coerced-fields-slots element))
      ("required" ,(mapcar (lambda (slot)
                             (symbol-munger:lisp->underscores (c2mop:slot-definition-name slot)))
                           (c2mop:class-slots (c2mop:ensure-finalized (class-of element)))))
      ("additionalProperties" nil boolean))))
