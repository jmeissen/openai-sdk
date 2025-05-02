(in-package #:cl-user)

(defpackage openai-sdk/request
  (:use #:cl
        #:openai-sdk/generics
        #:openai-sdk/client)
  (:shadow #:format)
  (:shadowing-import-from #:openai-sdk/generics #:function #:type)
  (:import-from #:openai-sdk/response #:make-chat-completion)
  (:import-from #:openai-sdk/util #:objectify)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:create-chat-completion))

(in-package #:openai-sdk/request)

(defclass openai-request () ())

(defmethod jzon:coerced-fields ((element openai-request))
  (macrolet ((%coerced-fields-slots (element)
               `(let ((class (class-of ,element)))
                  (c2mop:ensure-finalized class)
                  (mapcar (lambda (s)
                            (let ((slot-name (c2mop:slot-definition-name s)))
                              ;; underscore string conversion for object keys
                              (list (symbol-munger:lisp->underscores slot-name)
                                    (slot-value ,element slot-name)
                                    (c2mop:slot-definition-type s))))
                          (remove-if-not (lambda (s) (slot-boundp ,element (c2mop:slot-definition-name s)))
                                         (c2mop:class-slots class))))))
    (%coerced-fields-slots element)))

(defmacro req (method path openai messages content-plist)
  `(multiple-value-bind (body code response-headers uri stream)
       (let ((retry-request (dex:retry-request (openai-sdk/client:max-retries ,openai) :interval 2))
             (content (jzon:stringify
                       (alexandria:plist-hash-table
                        (openai-sdk/util:underscore-plist-keys
                         (openai-sdk/util:merge-plist
                          (list :model (openai-sdk/client:default-model ,openai)
                                :messages ,messages)
                          ,content-plist))))))
         (handler-bind ((dex:http-request-internal-server-error retry-request)
                        (dex:http-request-not-implemented retry-request)
                        (dex:http-request-service-unavailable retry-request)
                        (dex:http-request-bad-gateway retry-request))
           (dex:request (concatenate 'string (openai-sdk/client:base-url ,openai) ,path)
                        :method ,method
                        :bearer-auth (openai-sdk/client:api-key ,openai)
                        :headers (remove-if
                                  #'null
                                  (list (car (openai-sdk/client:default-headers ,openai))
                                        (when (openai-sdk/client:organization-id ,openai)
                                          (cons "OpenAI-Organization"
                                                (openai-sdk/client:organization-id ,openai)))
                                        (when (openai-sdk/client:project-id ,openai)
                                          (cons "OpenAI-Project"
                                                (openai-sdk/client:project-id ,openai)))))
                        :connect-timeout (openai-sdk/client:connect-timeout ,openai)
                        :read-timeout (openai-sdk/client:read-timeout ,openai)
                        :content content)))
     (values (jzon:parse body)
             code
             response-headers
             uri
             stream)))

(defclass complex-message-content (openai-request)
  ((type :initform "text"
         :reader type)
   (text :initarg :text
         :accessor text
         :type string)))

(defclass refusal-message-content (complex-message-content)
  ((type :initform "refusal")
   (refusal :initarg :refusal
            :accessor refusal)))

(defun complex-message-content-type-p (v)
  (or (stringp v)
      (and (consp v)
           (every (lambda (el) (typep el 'complex-message-content))
                  v))))

(deftype complex-message-content-type ()
  `(satisfies complex-message-content-type-p))

(defun string-list-p (v)
  (and (consp v)
       (every #'stringp v)))

(deftype string-list ()
  `(satisfies string-list-p))

(defun make-complex-message-content (content)
  (etypecase content
    (string content)
    (string-list (map 'list (lambda (el)
                              (make-instance 'complex-message-content :text el))
                      content))))

(defclass base-message (openai-request)
  ((content
    :accessor content
    :initarg :content)
   (role
    :accessor role
    :initarg :role)))

(defclass regular-message (base-message)
  ((name
    :accessor name
    :initarg :name)))

(defclass complex-content-message (regular-message)
  ((content :type complex-message-content-type)))

(defclass message (regular-message) ())

(defclass developer-message (complex-content-message)
  ((role :initform "developer")))

(defclass system-message (complex-content-message)
  ((role :initform "system")))

(defclass user-message (complex-content-message)
  ((role :initform "user")))

(defclass function-message (regular-message)
  ((role :initform "function")
   (name :documentation "the name of the function to call.")
   (content :initform 'null
            :documentation "the contents of the function message.")))

(defclass assistant-message-audio (openai-request)
  ((id :type string :accessor id :initarg :id)))

(defclass assistant-message-function-call (openai-request)
  ((arguments :type string
              :initarg :arguments
              :accessor arguments
              :documentation "the arguments to call the function with, as generated by the model in json
format. note that the model does not always generate valid json, and may hallucinate
parameters not defined by your function schema. validate the arguments in your code
before calling your function.")
   (name :type string
         :initarg :name
         :accessor name)))

(defun make-assistant-message-function-call (arguments name)
  (make-instance 'assistant-message-function-call :arguments arguments
                                                  :name name))

(defclass assistant-message-tool-call (openai-request)
  ((function :type assistant-message-function-call
             :accessor function
             :initarg :function)
   (id :type string
       :accessor id
       :initarg :id
       :documentation "the id of the tool call.")
   (type :reader type
         :initform "function"
         :type string)))

(defun make-assistant-message-tool-call (function id)
  (make-instance 'assistant-message-tool-call :function function
                                              :id id))

(defun assistant-message-tool-call-type (el)
  (and (consp el)
       (every (lambda (v) (typep v 'assistant-message-tool-call)) el)))

(defclass assistant-message (complex-content-message)
  ((role :initform "assistant")
   (audio :initarg :audio
          :accessor audio
          :type assistant-message-audio)
   (function-call :initarg :function-call
                  :accessor function-call
                  :type assistant-message-function-call)
   (refusal :type string
            :initarg :refusal
            :accessor refusal
            :documentation "the refusal message by the assistant.")
   (tool-calls :type assistant-message-tool-call-type
               :accessor tool-calls
               :initarg :tool-calls)))

(defclass tool-message (complex-content-message)
  ((role :initform "tool")
   (tool-call-id :accessor tool-call-id
                 :initarg :tool-call-id
                 :type string)))

(defun typed-string-list-p (el)
  (and (consp el)
       (keywordp (car el))
       (every #'stringp (cdr el))))

(deftype typed-string-list ()
  `(satisfies typed-string-list-p))

(defun make-assistant-message-content (content)
  "CONTENT must be a `complex-message-content-type', `string-list' or `typed-string-list'"
  (etypecase content
    (complex-message-content-type content)
    (string-list (map 'list (lambda (el)
                              (make-instance 'complex-message-content :text el))
                      content))
    (typed-string-list (let ((class (cond ((eq :text (car content)) '(:text . complex-message-content))
                                          ((eq :refusal (car content)) '(:refusal . refusal-message-content)))))
                         (map 'list (lambda (el) (make-instance (cdr class) (car class) el)) (cdr content))))))

(defun make-assistant-message (&rest args &key audio content function-call name refusal
                                            tool-calls)
  (declare (ignore audio function-call name refusal tool-calls))
  (when content
    (setf (getf args :content) (make-assistant-message-content content)))
  (apply #'make-instance 'assistant-message args))

(defun make-function-message (name &rest args &key content)
  (declare (ignore content))
  (apply #'make-instance 'function-message :name name args))

(defun make-developer-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'developer-message :content (make-complex-message-content content)
         args))

(defun make-system-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'system-message :content (make-complex-message-content content)
         args))

(defun make-user-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'user-message :content (make-complex-message-content content)
         args))

(defun make-tool-message (content tool-call-id)
  (make-instance 'tool-message
                 :content (make-complex-message-content content)
                 :tool-call-id tool-call-id))

(defun list-of-messages-p (list)
  (and (consp list)
       (every (lambda (el) (typep el 'base-message)) list)))

(deftype messages ()
  `(satisfies list-of-messages-p))

(defclass chat-completion-audio (openai-request)
  ((format :accessor format
           :initarg :format)
   (voice :accessor voice
          :initarg :voice)))

(defun make-chat-completion-audio (format voice)
  "Required when audio output is requested with modalities: [\"audio\"]"
  (make-instance 'chat-completion-audio :format format :voice voice))

(defclass function (openai-request)
  ((name
    :accessor name
    :initarg :name
    :documentation "The name of the function to be called. Must be a-z, A-Z, 0-9, or
 contain underscores and dashes, with a maximum length of 64.")
   (description
    :accessor description
    :initarg :description
    :documentation "A description of what the function does, used by the model to
 choose when and how to call the function.")
   (parameters
    :accessor parameters
    :initarg :parameters
    :documentation "The parameters the functions accepts, described as a JSON Schema
 object. See the guide for examples, and the JSON Schema reference for documentation
 about the format.")))

(defun string-<=-n-p (s n)
  (and (stringp s)
       (<= (length s) n)))

(defun string-<=-64-p (s)
  (string-<=-n-p s 64))

(deftype string-<=-64 ()
  `(satisfies string-<=-64-p))

(defun string-<=-512-p (s)
  (string-<=-n-p s 512))

(deftype string-<=-512 ()
  `(satisfies string-<=-512-p))

(defparameter *slug-scanner* "[0-9A-Za-z-_]+")

(defun string-slug-p (s)
  (and (stringp s)
       (= (length s)
          (nth-value 1 (ppcre:scan *slug-scanner* s)))))

(deftype string-slug ()
  `(satisfies string-slug-p))

(deftype string-<=-64-slug ()
  `(and (satisfies string-<=-64-p)
        (satisfies string-slug-p)))

(defun make-function (name &rest args &key description parameters)
  (declare (ignore description parameters))
  (apply #'make-instance 'function :name name args))

(defun make-metadata (alist)
  "https://platform.openai.com/docs/api-reference/chat/create#chat-create-metadata"
  (unless (every (lambda (el) (typep (car el) 'string-<=-64)) alist)
    (error "KEY in metadata ALIST longer than 64"))
  (unless (every (lambda (map) (typep (cdr map) 'string-<=-512)) alist)
    (error "VALUE in metadata ALIST longer than 512"))
  (alexandria:alist-hash-table alist :test #'equal))

(defclass json-schema (openai-request)
  ((name
    :initarg :name
    :accessor name)
   (description
    :initarg :description
    :accessor description)
   (schema
    :initarg :schema
    :accessor schema)
   (strict
    :initarg :strict
    :accessor strict)))

(defun make-json-schema (name &rest args &key description schema strict)
  (declare (ignore description schema strict))
  (unless (typep name 'string-<=-64-slug)
    (error "NAME must satisfy `string-<=-64-slug'"))
  (apply #'make-instance :name name args))

(defclass response-format (openai-request)
  ((type
    :initarg :type
    :accessor type)
   (json-schema
    :initarg :json-schema
    :accessor json-schema)))

(defun make-response-format (type &key json-schema)
  (if json-schema
      (make-instance 'response-format :type type :json-schema (apply #'make-json-schema json-schema))
      (make-instance 'response-format :type type)))

(defgeneric create-chat-completion (openai messages
                                    &rest content-plist
                                    &key model audio
                                      frequency-penalty function-call
                                      functions logit-bias
                                      logprobs max-completion-tokens
                                      max-tokens metadata
                                      modalities n
                                      parallel-tool-calls prediction
                                      presence-penalty reasoning-effort
                                      response-format seed
                                      service-tier stop
                                      store
                                      ;; stream stream-options
                                      temperature
                                      tool-choice tools
                                      top-logprobs top-p
                                      user web-search-options)
  (:documentation "https://platform.openai.com/docs/api-reference/chat/create"))

(defmethod create-chat-completion ((openai openai) messages
                                   &rest content-plist
                                   &key model audio
                                     frequency-penalty function-call
                                     functions logit-bias
                                     logprobs max-completion-tokens
                                     max-tokens metadata
                                     modalities n
                                     parallel-tool-calls prediction
                                     presence-penalty reasoning-effort
                                     response-format seed
                                     service-tier stop
                                     store
                                     ;; stream stream-options
                                     temperature
                                     tool-choice tools
                                     top-logprobs top-p
                                     user web-search-options)
  (declare (ignore model audio frequency-penalty function-call functions logit-bias logprobs
                   max-completion-tokens max-tokens metadata modalities n parallel-tool-calls
                   prediction presence-penalty reasoning-effort response-format seed service-tier
                   stop store temperature tool-choice tools top-logprobs
                   top-p user web-search-options))
  (multiple-value-bind (body status response-headers uri stream)
      (req
       :post "chat/completions"
       openai
       (etypecase messages
         (messages messages)
         (string-list (map 'list #'make-user-message messages))
         (string (list (make-user-message messages))))
       content-plist)
    (values (objectify 'make-chat-completion body)
            status
            response-headers
            uri
            stream)))
