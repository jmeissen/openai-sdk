(in-package #:cl-user)

(uiop:define-package openai-sdk/response
  (:use #:cl
        #:openai-sdk/core
        #:openai-sdk/util)
  (:shadowing-import-from #:openai-sdk/core #:format #:type #:function)
  (:export #:accepted-prediction-tokens
           #:annotations
           #:arguments
           #:audio
           #:audio-tokens
           #:bytes
           #:cached-tokens
           #:choices
           #:completion-tokens
           #:completion-tokens-details
           #:content
           #:created
           #:data
           #:end-index
           #:expires-at
           #:finish-reason
           #:function
           #:function-call
           #:id
           #:index
           #:logprob
           #:logprobs
           #:make-annotation
           #:make-audio
           #:make-base-logprob
           #:make-chat-completion
           #:make-choice
           #:make-completion-tokens-details
           #:make-function-call
           #:make-logprobs
           #:make-message
           #:make-prompt-tokens-details
           #:make-special-logprob
           #:make-tool-call
           #:make-url-citation
           #:make-usage
           #:model
           #:name
           #:object
           #:prompt-tokens
           #:prompt-tokens-details
           #:reasoning-tokens
           #:refusal
           #:rejected-prediction-tokens
           #:response-message
           #:role
           #:service-tier
           #:start-index
           #:system-fingerprint
           #:title
           #:token
           #:tool-calls
           #:top-logprobs
           #:total-tokens
           #:transcript
           #:type
           #:url
           #:url-citation
           #:usage))

(in-package #:openai-sdk/response)

(defclass completion-tokens-details ()
  ((accepted-prediction-tokens :accessor accepted-prediction-tokens
                               :initarg :accepted-prediction-tokens
                               :type integer)
   (audio-tokens :accessor audio-tokens
                 :initarg :audio-tokens
                 :type integer)
   (reasoning-tokens :accessor reasoning-tokens
                     :initarg :reasoning-tokens
                     :type integer)
   (rejected-prediction-tokens :accessor rejected-prediction-tokens
                               :initarg :rejected-prediction-tokens
                               :type integer)))

(defun make-completion-tokens-details (&rest args &key accepted-prediction-tokens audio-tokens reasoning-tokens rejected-prediction-tokens)
  (declare (ignore accepted-prediction-tokens audio-tokens reasoning-tokens rejected-prediction-tokens))
  (apply #'make-instance 'completion-tokens-details args))

(defclass prompt-tokens-details ()
  ((audio-tokens :accessor audio-tokens
                 :initarg :audio-tokens
                 :type integer)
   (cached-tokens :accessor cached-tokens
                  :initarg :cached-tokens
                  :type integer)))

(defun make-prompt-tokens-details (&rest args &key audio-tokens cached-tokens)
  (declare (ignore audio-tokens cached-tokens))
  (apply #'make-instance 'prompt-tokens-details args))

(defclass usage ()
  ((completion-tokens-details :accessor completion-tokens-details
                              :initarg :completion-tokens-details
                              :type completion-tokens-details)
   (prompt-tokens-details :accessor prompt-tokens-details
                          :initarg :prompt-tokens-details
                          :type prompt-tokens-details)
   (total-tokens :accessor total-tokens
                 :initarg :total-tokens
                 :type integer)
   (completion-tokens :accessor completion-tokens
                      :initarg :completion-tokens
                      :type integer)
   (prompt-tokens :accessor prompt-tokens
                  :initarg :prompt-tokens
                  :type integer)))

(defun make-usage (&key completion-tokens-details prompt-tokens-details total-tokens completion-tokens prompt-tokens)
  (make-instance 'usage :completion-tokens-details (objectify 'make-completion-tokens-details completion-tokens-details)
                        :prompt-tokens-details (objectify 'make-prompt-tokens-details prompt-tokens-details)
                        :total-tokens total-tokens
                        :completion-tokens completion-tokens
                        :prompt-tokens prompt-tokens))

(defclass base-logprob ()
  ((bytes
    :accessor bytes
    :initarg :bytes)
   (logprob
    :accessor logprob
    :initarg :logprob
    :type number)
   (token
    :accessor token
    :initarg :token
    :type string)))

(defun make-base-logprob (&rest args &key bytes logprob token)
  (declare (ignore bytes logprob token))
  (apply #'make-instance 'base-logprob args))

(defclass special-logprob (base-logprob)
  ((top-logprobs :initarg :top-logprobs
                 :accessor top-logprobs
                 :type simple-vector)))

(defun make-special-logprob (&key bytes logprob token top-logprobs)
  (make-instance
   'special-logprob
   :bytes bytes
   :logprob logprob
   :token token
   :top-logprobs (map 'simple-vector (lambda (tlp) (objectify 'make-base-logprob tlp)) top-logprobs)))

(defclass logprobs ()
  ((content :accessor content
            :initarg :content
            :type special-logprob)
   (refusal :accessor refusal
            :initarg :refusal
            :type special-logprob)))

(defun make-logprobs (&key content refusal)
  (make-instance 'logprobs :content (objectify 'make-special-logprob content)
                           :refusal (objectify 'make-special-logprob refusal)))

(defclass url-citation ()
  ((end-index
    :accessor end-index
    :initarg :end-index
    :type integer)
   (start-index
    :accessor start-index
    :initarg :start-index
    :type integer)
   (title
    :accessor title
    :initarg :title
    :type string)
   (url
    :accessor url
    :initarg :url
    :type string)))

(defun make-url-citation (&rest args &key end-index start-index title url)
  (declare (ignore end-index start-index title url))
  (apply #'make-instance 'url-citation args))

(defclass annotation ()
  ((type
    :accessor type
    :initarg :type
    :type string)
   (url-citation
    :accessor url-citation
    :initarg :url-citation
    :type url-citation)))

(defun make-annotation (&key type url-citation)
  (make-instance 'annotation :type type :url-citation (objectify 'make-url-citation url-citation)))

(defclass audio ()
  ((id
    :accessor id
    :initarg :id
    :type string)
   (data
    :accessor data
    :initarg :data
    :type string)
   (expires-at
    :accessor expires-at
    :initarg :expires-at
    :type local-time:timestamp)
   (transcript
    :accessor transcript
    :initarg :transcript
    :type string)))

(defun make-audio (&key id data expires-at transcript)
  (make-instance 'audio :id id :data data :expires-at (local-time:unix-to-timestamp expires-at) :transcript transcript))

(defclass function-call ()
  ((arguments
    :accessor arguments
    :initarg :arguments
    :type string)
   (name
    :accessor name
    :initarg :name
    :type string)))

(defun make-function-call (&rest args &key arguments name)
  (declare (ignore arguments name))
  (apply #'make-instance 'function-call args))

(defclass tool-call ()
  ((id
    :accessor id
    :initarg :id)
   (function
    :accessor function
    :initarg :function
    :type function-call)
   (type
    :accessor type
    :initarg :type
    :type string)))

(defun make-tool-call (&key id function type)
  (make-instance 'tool-call :id id :function (objectify 'make-function-call function) :type type))

(defclass response-message ()
  ((content
    :accessor content
    :initarg :content
    :type string)
   (refusal
    :accessor refusal
    :initarg :refusal
    :type string)
   (role
    :accessor role
    :initarg :role
    :type string)
   (annotations
    :accessor annotations
    :initarg :annotations
    :type simple-vector)
   (audio
    :accessor audio
    :initarg :audio
    :type audio)
   (function-call
    :accessor function-call
    :initarg :function-call
    :type function-call)
   (tool-calls
    :accessor tool-calls
    :initarg :tool-calls
    :type simple-vector)))

(defun make-message (&key content refusal role annotations audio function-call tool-calls)
  (make-instance
   'response-message
   :content content
   :refusal refusal
   :role role
   :annotations (map 'simple-vector (lambda (a) (objectify 'make-annotation a)) annotations)
   :audio (objectify 'make-audio audio)
   :function-call (objectify 'make-function-call function-call)
   :tool-calls (map 'simple-vector (lambda (tc) (objectify 'make-tool-call tc)) tool-calls)))

(defclass choice ()
  ((finish-reason
    :accessor finish-reason
    :initarg :finish-reason
    :type string)
   (index
    :accessor index
    :initarg :index
    :type integer)
   (logprobs
    :accessor logprobs
    :initarg :logprobs
    :type logprobs)
   (message
    :accessor message
    :initarg :message
    :type response-message)))

(defun make-choice (&key finish-reason index logprobs message)
  (make-instance 'choice :finish-reason finish-reason
                         :index index
                         :logprobs (objectify 'make-logprobs logprobs)
                         :message (objectify 'make-message message)))

(defclass completion ()
  ((id :accessor id
       :initarg :id
       :type string)
   (choices :accessor choices
            :initarg :choices
            :type simple-vector)
   (created :accessor created
            :initarg :created
            :type local-time:timestamp)
   (model :accessor model
          :initarg :model
          :type string)
   (object :accessor object
           :initarg :object
           :type string)
   (service-tier :accessor service-tier
                 :initarg :service-tier
                 :type string)
   (usage :accessor usage
          :initarg :usage
          :type usage)
   (system-fingerprint :accessor system-fingerprint
                       :initarg :system-fingerprint
                       :type string)))

(defun make-chat-completion (&key id choices created model object service-tier usage system-fingerprint)
  (make-instance 'completion :id id
                             :choices (map 'simple-vector (lambda (choice)
                                                            (objectify 'make-choice choice))
                                           choices)
                             :created (local-time:unix-to-timestamp created)
                             :model model
                             :object object
                             :service-tier service-tier
                             :usage (objectify 'make-usage usage)
                             :system-fingerprint system-fingerprint))
