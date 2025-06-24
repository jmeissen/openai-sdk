(in-package #:cl-user)

(defpackage openai-sdk/tool-schema-generator
  (:use #:cl)
  (:nicknames :oai/tsg)
  (:export #:type-string
           #:type-boolean
           #:type-number
           #:type-integer
           #:type-object
           #:type-array
           #:type-enum
           #:type-any-of))

(in-package #:openai-sdk/tool-schema)

(setf (fdefinition '∴) #'alexandria:plist-hash-table)

(defun type-string-format-p (el)
  (member el '("date-time" "time" "date" "duration" "email" "hostname" "ipv4" "ipv6" "uuid") :test #'equal))

(deftype str-format ()
  '(satisfies type-string-format-p))

(defun type-string (&rest args &key pattern format enum description)
  (declare (ignore pattern description))
  (when format (check-type format str-format))
  (when enum (check-type enum simple-vector))
  (∴ (concatenate 'list args '(:type "string"))))

(defun type-boolean (&rest args &key description &allow-other-keys)
  (declare (ignore description))
  (∴ (concatenate 'list args '(:type "boolean"))))

(defmacro %type-number (number-type)
  "Defines `type-<NUMBER-TYPE>' function."
  `(defun ,(intern (str:concat "TYPE-" (symbol-name number-type))) (&rest args &key multiple-of maximum exclusive-maximum minimum exclusive-minimum description)
     (declare (ignore multiple-of maximum exclusive-maximum minimum exclusive-minimum description))
     (∴ (concatenate 'list
                     (loop for el in (alexandria:plist-alist args)
                           nconc (list (symbol-munger:lisp->camel-case (car el))
                                       (cdr el)))
                     '(:type ,(format nil "~(~a~)" number-type))))))

(%type-number integer)
(%type-number number)

(defun type-object (required &rest args &key properties description (additional-properties nil) &allow-other-keys)
  "PROPERTIES must be a plist.

Example:
(type-object nil #(\"unit\")
             :properties `(:unit ,(type-enum #(\"string\" \"null\")
                                             :enum #(\"F\" \"C\")
                                             :description \"The unit to return the temperature\")))
becomes (after serialization to string)
{
  \"type\": \"object\",
  \"properties\": {
    \"unit\": {
      \"enum\": [\"F\",\"C\"],
      \"description\": \"The unit to return the temperature\",
      \"type\": [\"string\",\"null\"]
    }
  },
  \"additionalProperties\": false,
  \"required\": [\"unit\"]
}"
  (declare (ignore description))
  (check-type required simple-vector)
  (∴ (concatenate 'list `(:type "object"
                          :properties ,(∴ properties)
                          additional-properties
                          :required ,required)
                  (progn (remf args :properties)
                         args))))

(defun type-object (required &rest args &key properties description additional-properties &allow-other-keys)
  "PROPERTIES must be a plist.

Example:
(type-object nil #(\"unit\")
             :properties `(:unit ,(type-enum #(\"string\" \"null\")
                                             :enum #(\"F\" \"C\")
                                             :description \"The unit to return the temperature\")))
becomes (after serialization to string)
{
  \"type\": \"object\",
  \"properties\": {
    \"unit\": {
      \"enum\": [\"F\",\"C\"],
      \"description\": \"The unit to return the temperature\",
      \"type\": [\"string\",\"null\"]
    }
  },
  \"additionalProperties\": false,
  \"required\": [\"unit\"]
}"
  (declare (ignore properties description additional-properties))
  (check-type required simple-vector)
  (∴ (concatenate 'list '(:type "object")
                  (loop for (key . val) in (alexandria:plist-alist args)
                        nconc (list (symbol-munger:lisp->camel-case key)
                                    (if (eq key :properties)
                                        (∴ val)
                                        val)))
                  `(:required ,required))))

(defun type-array (type &rest args &key min-items max-items description &allow-other-keys)
  (declare (ignore min-items max-items description))
  (∴ (concatenate 'list (loop for el in (alexandria:plist-alist args)
                              nconc (list (symbol-munger:lisp->camel-case (car el))
                                          (cdr el)))
                  `(:type "array"
                    :items ,type))))

(defun type-enum (types &rest args &key enum description &allow-other-keys)
  "Retrieve a correctly jzon serializable enum-type object.

Example:
(type-enum #(\"string\" \"null\")
           :enum #(\"F\" \"C\")
           :description \"The unit to return the temperature\")
becomes (after serialization to string):
{
    \"type\": [\"string\", \"null\"],
    \"description\": \"The unit to return the temperature in\",
    \"enum\": [\"F\", \"C\"]
}
"
  (declare (ignore description))
  (when enum (check-type enum simple-vector))
  (check-type types simple-vector)
  (∴ (concatenate 'list args `(:type ,types))))

(defun type-any-of (types &rest args &key description &allow-other-keys)
  (declare (ignore description))
  (check-type types simple-vector)
  (∴ (concatenate 'list args `("anyOf" ,types))))


;; Supported properties
;; In addition to specifying the type of a property, you can specify a selection of additional constraints:

;; Supported string properties:

;; pattern — A regular expression that the string must match.
;; format — Predefined formats for strings. Currently supported:
;; date-time
;; time
;; date
;; duration
;; email
;; hostname
;; ipv4
;; ipv6
;; uuid

;; Supported number properties:

;; multipleOf — The number must be a multiple of this value.
;; maximum — The number must be less than or equal to this value.
;; exclusiveMaximum — The number must be less than this value.
;; minimum — The number must be greater than or equal to this value.
;; exclusiveMinimum — The number must be greater than this value.

;; Supported array properties:

;; minItems — The array must have at least this many items.
;; maxItems — The array must have at most this many items.
