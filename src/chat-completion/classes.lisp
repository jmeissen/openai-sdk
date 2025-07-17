(in-package #:cl-user)

(uiop:define-package openai-sdk/chat-completion/classes
  (:use #:cl #:openai-sdk/interface)
  (:shadowing-import-from #:openai-sdk/interface #:type #:format #:function #:stream)
  (:import-from #:openai-sdk/util #:objectify #:format-b64)
  (:export

   ;; Classes
   #:annotation
   #:approximate
   #:assistant-message
   #:assistant-message-audio
   #:audio
   #:audio-content-part
   #:base-logprob
   #:chat-completion
   #:chat-completion-audio
   #:chat-completion-function-call
   #:choice
   #:chunk-choice
   #:chunked-tool-call
   #:completion
   #:completion-message
   #:completion-tokens-details
   #:content-part
   #:delta
   #:developer-message
   #:file
   #:file-content-part
   #:function
   #:function-call
   #:function-message
   #:image-content-part
   #:image-url
   #:input-audio
   #:json-schema
   #:logprobs
   #:message
   #:named-message
   #:prediction
   #:prediction-content-part
   #:prompt-tokens-details
   #:response-format
   #:special-logprob
   #:stream-options
   #:system-message
   #:text-content-part
   #:tool
   #:tool-call
   #:tool-choice-part
   #:tool-message
   #:url-citation
   #:usage
   #:user-location
   #:user-message
   #:web-search-options

   ;; Constructors
   #:make-annotation
   #:make-approximate
   #:make-assistant-message
   #:make-assistant-message-audio
   #:make-audio
   #:make-base-logprob
   #:make-chat-completion
   #:make-chat-completion-audio
   #:make-chat-completion-function-call
   #:make-choice
   #:make-chunk-choice
   #:make-chunked-completion
   #:make-chunked-tool-call
   #:make-completion
   #:make-completion-message
   #:make-completion-tokens-details
   #:make-delta
   #:make-developer-message
   #:make-file
   #:make-file-data-content-part
   #:make-file-id-content-part
   #:make-function
   #:make-function-call
   #:make-function-message
   #:make-json-schema
   #:make-logprobs
   #:make-message-from-keyword
   #:make-prediction
   #:make-prediction-content-part
   #:make-prompt-tokens-details
   #:make-response-format
   #:make-special-logprob
   #:make-stream-options
   #:make-system-message
   #:make-text-content-part
   #:make-tool
   #:make-tool-call
   #:make-tool-choice
   #:make-tool-choice-part
   #:make-tool-message
   #:make-url-citation
   #:make-usage
   #:make-user-location
   #:make-user-message
   #:make-web-search-options))

(in-package #:openai-sdk/chat-completion/classes)

(defmethod com.inuoe.jzon:coerced-fields ((element openai-json-serializable))
  (macrolet ((%coerced-fields-slots (element)
               `(let ((class (class-of ,element)))
                  (c2mop:ensure-finalized class)
                  (mapcar
                   (lambda (s) (let ((slot-name (c2mop:slot-definition-name s)))
                                 ;; Serialize slot-names as snake_case:
                                 `(,(symbol-munger:lisp->underscores slot-name)
                                   ,(slot-value ,element slot-name)
                                   ,(c2mop:slot-definition-type s))))
                   (remove-if-not (lambda (s) (slot-boundp ,element
                                                           (c2mop:slot-definition-name s)))
                                  (c2mop:class-slots class))))))
    (%coerced-fields-slots element)))

;; Chat Completion classes

(defclass message (openai-json-serializable)
  ((role :reader role)
   (content :initarg :content
            :accessor content)))

(defclass named-message (message)
  ((name :initarg :name
         :accessor name)))

(defclass content-part (openai-json-serializable)
  ((type :type string
         :reader type)))

(defclass text-content-part (content-part)
  ((type :initform "text")
   (text :type string
         :accessor text
         :initarg :text)))

(defun make-text-content-part (text)
  (make-instance 'text-content-part :text text))

(defclass image-url (openai-json-serializable)
  ((url :initarg :url
        :accessor url
        :type string
        :documentation "Either a URL of the image or the base64 encoded image data.")
   (detail :initarg :detail
           :accessor detail
           :type string
           :documentation "Specifies the detail level of the image. See: https://platform.openai.com/docs/guides/vision#low-or-high-fidelity-image-understanding")))

(defclass image-content-part (content-part)
  ((type :initform "image_url")
   (image-url :initarg :image-url
              :accessor image-url
              :type image-url)))

(defclass input-audio (openai-json-serializable)
  ((data :type string
         :accessor data
         :initarg :data
         :documentation "Base64 encoded audio data.")
   (format :type string
           :accessor format
           :initarg :format
           :documentation "The format of the encoded audio data. Currently supports \"wav\" and \"mp3\".The format of the encoded audio data. Currently supports \"wav\" and \"mp3\".")))

(defclass audio-content-part (content-part)
  ((type :initform "input_audio")
   (input-audio :initarg :input-audio
                :accessor input-audio
                :type input-audio)))

(defclass file (openai-json-serializable)
  ((file-data :initarg :file-data
              :accessor file-data
              :type string
              :documentation "The base64 encoded file data, used when passing the
 file to the model as a string.")
   (file-id :initarg :file-id
            :accessor file-id
            :type string
            :documentation "The ID of an uploaded file to use as input.")
   (filename :initarg :filename
             :accessor filename
             :type string
             :documentation "The name of the file, used when passing the file to the
 model as a string.")))

(defun make-file (&rest args &key file-id file-data filename)
  "When building message content, either use both FILE-DATA and FILENAME, or FILE-ID.

FILE-DATA is the base64 encoded file data.
FILENAME is the string filename for the FILE-DATA.

FILE-ID is the string reference to an already uploaded file."
  (declare (ignore file-id file-data filename))
  (apply #'make-instance 'file args))

(defclass file-content-part (content-part)
  ((type :initform "file")
   (file :initarg :file
         :accessor file
         :type file)))

(defun make-file-data-content-part (file-data filename mime-type)
  (make-instance 'file-content-part :file (make-file :file-data (format-b64 file-data mime-type) :filename filename)))

(defun make-file-id-content-part (file-id)
  (make-instance 'file-content-part :file (make-file :file-id file-id)))

(defun %user-message-content-p (msg)
  (or (stringp msg)
      (and (consp msg)
           (every (lambda (el) (or (typep el 'text-content-part)
                                   (typep el 'image-content-part)
                                   (typep el 'audio-content-part)
                                   (typep el 'file-content-part)))
                  msg))))

(deftype user-message-content ()
  `(satisfies %user-message-content-p))

(defclass user-message (named-message)
  ((role :initform "user")
   (content :type user-message-content)))

(defun make-user-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'user-message :content content args))

(defun %developer-message-content-p (msg)
  (or (stringp msg)
      (and (consp msg)
           (every (lambda (el) (or (typep el 'text-content-part)))
                  msg))))

(deftype developer-message-content ()
  `(satisfies %developer-message-content-p))

(defclass developer-message (named-message)
  ((role :initform "developer")
   (content :type developer-message-content)))

(defun make-developer-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'developer-message :content content args))

(deftype system-message-content ()
  `(satisfies %developer-message-content-p))

(defclass system-message (developer-message)
  ((role :initform "system")
   (content :type system-message-content)))

(defun make-system-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'system-message :content content args))

(deftype assistant-message-content ()
  `(satisfies %developer-message-content-p))

(defclass assistant-message-audio (openai-json-serializable)
  ((id :initarg :id
       :accessor id
       :type string
       :documentation "Unique identifier for a previous audio response from the
 model.")))

(defun make-assistant-message-audio (id)
  (make-instance 'assistant-message-audio :id id))

(defclass function-call (openai-json-serializable)
  ((arguments :type string
              :accessor arguments
              :initarg :arguments
              :documentation "The arguments to call the function with, as generated by
 the model in JSON format. Note that the model does not always generate valid JSON,
 and may hallucinate parameters not defined by your function schema. Validate the
 arguments in your code before calling your function.")
   (name :type string
         :accessor name
         :initarg :name
         :initform (error "name required")
         :documentation "The name of the function to call."))
  (:documentation "Deprecated and replaced by tool_calls. The name and arguments of a function that
should be called, as generated by the model."))

(defun make-function-call (&rest args &key name arguments)
  (declare (ignore name arguments))
  (apply #'make-instance 'function-call args))

(defclass tool-call (openai-json-serializable)
  ((type :initform "function"
         :initarg :type
         :reader type
         :documentation "The type of the tool. Currently, only 'function' is supported.")
   (function :initarg :function
             :accessor function
             :type function-call
             :initform (error "required"))
   (id :initarg :id
       :accessor id
       :type string
       :initform (error "required")
       :documentation "The ID of the tool call.")))

(defun make-tool-call (&rest args &key function id type)
  (declare (ignore function id type))
  (apply #'make-instance 'tool-call args))

(defun make-completion-tool-call (&key function id type)
  (make-instance 'tool-call :function (objectify 'make-function-call function)
                            :id id
                            :type type))

(defun %tool-calls-p (tc)
  (and (consp tc)
       (every (lambda (tt) (typep tt 'tool-call))
              tc)))

(deftype tool-calls ()
  `(satisfies %tool-calls-p))

(defclass assistant-message (developer-message)
  ((content :type assistant-message-content)
   (refusal :initarg :refusal
            :accessor refusal
            :type string
            :documentation "The refusal message by the assistant.")
   (role :initform "assistant")
   (audio :initarg :audio
          :accessor audio
          :type assistant-message-audio
          :documentation "Data about a previous audio response from the model.")
   (function-call :initarg :function-call
                  :accessor function-call
                  :type function-call
                  :documentation "Deprecated and replaced by tool_calls. The name and
 arguments of a function that should be called, as generated by the model.")
   (tool-calls :initarg :tool-calls
               :accessor tool-calls
               :type tool-calls
               :documentation "The tool calls generated by the model, such as
 function calls.")))

(defun make-assistant-message (&rest args &key audio content function-call name refusal tool-calls)
  (declare (ignore audio content function-call name refusal tool-calls))
  (apply #'make-instance 'assistant-message args))

(deftype tool-message-content ()
  `(satisfies %developer-message-content-p))

(defclass tool-message (message)
  ((role :initform "tool")
   (content :type tool-message-content
            :initform (error "required"))
   (tool-call-id :type string
                 :accessor tool-call-id
                 :initarg :tool-call-id
                 :initform (error "required"))))

(defun make-tool-message (content tool-call-id)
  (make-instance 'tool-message :content content :tool-call-id tool-call-id))

(defclass function-message (named-message)
  ((role :initform "function")
   (name :initform (error "required")
         :documentation "The name of the function to call.")
   (content :initform (error "required")
            :type string
            :documentation "The contents of the function message.")))

(defun make-function-message (name &optional (content 'null))
  (make-instance 'function-message :name name :content content))

(defclass chat-completion-audio (openai-json-serializable)
  ((format
    :accessor format
    :initarg :format
    :type string
    :documentation "Specifies the output audio format. Must be one of wav, mp3, flac,
 opus, or pcm16.")
   (voice
    :accessor voice
    :initarg :voice
    :type string
    :documentation "The voice the model uses to respond. Supported voices are alloy, ash, ballad, coral,
echo, fable, nova, onyx, sage, and shimmer."))
  (:documentation "Parameters for audio output. Required when audio output is requested with modalities:
[\"audio\"]. Learn more."))

(defun make-chat-completion-audio (&optional (format "mp3") (voice "echo"))
  (make-instance 'chat-completion-audio :format format :voice voice))

(defclass chat-completion-function-call (openai-json-serializable)
  ((name :initarg :name
         :accessor name
         :initform (error "required")
         :documentation "The name of the function to call.")))

(defun make-chat-completion-function-call (name)
  (make-instance 'chat-completion-function-call :name name))

(defun %chat-completion-function-call-p (el)
  (or (stringp el)
      (typep el 'chat-completion-function-call)))

(deftype chat-completion-function-call-type ()
  `(satisfies %chat-completion-function-call-p))

(defclass function (openai-json-serializable)
  ((name
    :accessor name
    :initarg :name
    :type string
    :initform (error "required")
    :documentation "The name of the function to be called. Must be a-z, A-Z, 0-9,
 or contain underscores and dashes, with a maximum length of 64.")
   (description
    :accessor description
    :initarg :description
    :type string
    :documentation "A description of what the function does, used by the
 model to choose when and how to call the function.")
   (parameters
    :accessor parameters
    :initarg :parameters
    :type hash-table
    :documentation "The parameters the functions accepts, described as a
 JSON Schema object. Omitting parameters defines a function with an empty
 parameter list.

See `openai-sdk/tool-call:make-tool' for an example how to generate the appropriate
hash-table relatively conveniently.")
   (strict
    :accessor strict
    :initarg :strict
    :type boolean
    :documentation "Strict schema adherence when generating the function call.

If `strict' is `t'
1. The model will follow the exact schema defined in the `parameters'-field
2. Only a subset of JSON Schema is supported
3. All object properties (in `parameters') must be present in the \"required\"-field
4. \"additionalProperties\" must be set to `nil' in \"object\"-schema-type

Note 1: With `strict' `t', optional fields can be denoted by appending to enum type \"null\"
Note 2: Do not set for legacy functions.")))

(defun make-function (&rest args &key name description parameters strict)
  (declare (ignore name description strict))
  (check-type parameters hash-table "A jzon parseable hash-table")
  (apply #'make-instance 'function args))

(defun %functions-p (els)
  (and (consp els)
       (every (lambda (el) (typep el 'function))
              els)))

(deftype functions ()
  `(satisfies %functions-p))

(defclass approximate (openai-json-serializable)
  ((city
    :accessor city
    :initarg :city
    :type string
    :documentation "Free text input for the city of the user, e.g. San Francisco.")
   (country
    :accessor country
    :initarg :country
    :type string
    :documentation "The two-letter ISO country code of the user, e.g. US.")
   (region
    :accessor region
    :initarg :region
    :type string
    :documentation "Free text input for the region of the user, e.g. California.")
   (timezone
    :accessor timezone
    :initarg :timezone
    :type string
    :documentation "The IANA timezone of the user, e.g. America/Los_Angeles."))
  (:documentation "Approximate location parameters for chat completion web search."))

(defun make-approximate (&rest args &key city country region timezone)
  (declare (ignore city country region timezone))
  (apply #'make-instance 'approximate args))

(defclass user-location (openai-json-serializable)
  ((type :initform "approximate"
         :reader type)
   (approximate
    :accessor approximate
    :initarg :approximate
    :type approximate
    :documentation "Approximate location parameters for the search.")))

(defun make-user-location (approximate)
  (make-instance 'user-location :approximate approximate))

(defclass web-search-options (openai-json-serializable)
  ((search-context-size
    :initarg :search-context-size
    :accessor search-context-size
    :type string
    :documentation "High level guidance for the amount of context window space to use
 for the search. One of low, medium, or high. medium is the default.")
   (user-location
    :accessor user-location
    :initarg :user-location
    :type user-location
    :documentation "Approximate location parameters for the search."))
  (:documentation "This tool searches the web for relevant results to use in a
 response. Learn more about the web search tool:
 https://platform.openai.com/docs/guides/tools-web-search?api-mode=chat"))

(defun make-web-search-options (&rest args &key search-context-size user-location)
  (declare (ignore search-context-size user-location))
  (apply #'make-instance 'web-search-options args))

(defun %list-of-strings-p (el)
  (and (consp el)
       (every #'stringp el)))

(deftype list-of-string ()
  `(satisfies %list-of-strings-p))

(defclass prediction-content-part (content-part)
  ((type
    :accessor type
    :initarg :type)
   (text
    :accessor text
    :initarg :text
    :type string
    :documentation "The text content.")))

(defun make-prediction-content-part (text type)
  (make-instance 'prediction-content-part :text text :type type))

(defun prediction-content-p (el)
  (or (stringp el)
      (and (consp el)
           (every (lambda (l) (typep l 'prediction-content-part)) el))))

(deftype prediction-content ()
  `(satisfies prediction-content-p))

(defclass prediction (openai-json-serializable)
  ((type :initform "content" :reader type)
   (content
    :accessor content
    :initarg :content
    :type prediction-content
    :documentation "The content that should be matched when generating a model
 response. If generated tokens would match this content, the entire model response
 can be returned much more quickly."))
  (:documentation "Configuration for a Predicted Output, which can greatly improve
 response times when large parts of the model response are known ahead of time. This
 is most common when you are regenerating a file with only minor changes to most of
 the content."))

(defun make-prediction (content)
  (make-instance #'prediction :content content))

(defun %stop-p (content)
  (or (stringp content)
      (and (consp content)
           (every #'stringp content)
           (<= 4 (length content)))))

(deftype stop ()
  `(satisfies %stop-p))

(defun response-format-type-p (el)
  (member el '("text" "json_schema" "json_object") :test #'equal))

(deftype response-format-type ()
  `(satisfies response-format-type-p))

(defclass json-schema (openai-json-serializable)
  ((name
    :accessor name
    :initarg :name
    :initform (error "required")
    :documentation "The name of the response format. Must be a-z, A-Z, 0-9, or
 contain underscores and dashes, with a maximum length of 64.")
   (description
    :accessor description
    :initarg :description
    :documentation "A description of what the response format is for, used by the
 model to determine how to respond in the format.")
   (schema
    :accessor schema
    :initarg :schema
    :documentation "The schema for the response format, described as a JSON Schema
 object. Learn how to build JSON schemas here.")
   (strict
    :accessor strict
    :initarg :strict
    :type boolean
    :documentation "Whether to enable strict schema adherence when generating the
 output. If set to true, the model will always follow the exact schema defined in the
 schema field. Only a subset of JSON Schema is supported when strict is true. To
 learn more, read the Structured Outputs guide.")))

(defun make-json-schema (name &rest args &key description schema strict)
  (declare (ignore description schema strict))
  (apply #'make-instance 'json-schema :name name args))

(defclass response-format (openai-json-serializable)
  ((type
    :accessor type
    :initarg :type
    :type response-format-type
    :documentation "The type of response format being defined. Can be either 'text',
 'json_schema', or 'json_object'.")
   (json-schema
    :accessor json-schema
    :initarg :json-schema
    :type json-schema
    :documentation "Structured Outputs configuration options, including a JSON
 Schema. Required when type is 'json_schema'.")))

(defun make-response-format (type &rest args &key json-schema)
  (declare (ignore json-schema))
  (apply #'make-instance 'response-format :type type args))

(defclass stream-options (openai-json-serializable)
  ((include-usage
    :accessor include-usage
    :initarg :include-usage
    :type boolean
    :documentation "If set, an additional chunk will be streamed before the data:
 [DONE] message. The usage field on this chunk shows the token usage statistics for
 the entire request, and the choices field will always be an empty array. All other
 chunks will also include a usage field, but with a null value. NOTE: If the stream
 is interrupted, you may not receive the final usage chunk which contains the total
 token usage for the request.")))

(defclass tool-choice-part (content-part)
  ((type :initform "function")
   (function
    :accessor function
    :initarg :function
    :type chat-completion-function-call)))

(defun make-tool-choice-part (tool-name)
  (make-instance 'tool-choice-part
                 :function (make-chat-completion-function-call tool-name)))

(defun %tool-choice-p (el)
  (or (and (stringp el)
           (or (string= el "none")
               (string= el "auto")
               (string= el "required")))
      (typep el 'tool-choice-part)))

(deftype tool-choice ()
  `(satisfies %tool-choice-p))

(defun make-tool-choice (choice-or-tool-name)
  (if (typep choice-or-tool-name 'tool-choice)
      choice-or-tool-name
      (make-tool-choice-part choice-or-tool-name)))

(defun make-stream-options (&optional include-usage)
  (make-instance 'stream-options :include-usage include-usage))

(defclass tool (content-part)
  ((type :initform "function")
   (function
    :accessor function
    :initarg :function
    :type function)))

(defun make-tool (function)
  (make-instance 'tool :function (if (typep function 'function)
                                     function
                                     (apply #'make-instance 'function function))))

(defun %tools-p (tools)
  (and (consp tools)
       (every (lambda (el) (typep el 'tool)) tools)))

(deftype tools ()
  `(satisfies %tools-p))

(defclass chat-completion (openai-json-serializable)
  ((messages :initarg :messages
             :accessor messages
             :documentation "A list of messages comprising the conversation so far.
 Depending on the model you use, different message types (modalities) are supported,
 like text, images, and audio.")
   (model
    :initarg :model
    :accessor model
    :documentation "Model ID used to generate the response, like gpt-4o or o3.")
   (audio
    :initarg :audio
    :accessor audio
    :type chat-completion-audio
    :documentation "Parameters for audio output. Required when audio output is
 requested with modalities: [\"audio\"].")
   (frequency-penalty
    :accessor frequency-penalty
    :initarg :frequency-penalty
    :type integer
    :documentation "Number between -2.0 and 2.0. Positive values penalize new tokens
 based on their existing frequency in the text so far, decreasing the model's
 likelihood to repeat the same line verbatim.")
   (function-call
    :accessor function-call
    :initarg :function-call
    :type chat-completion-function-call-type
    :documentation "Deprecated in favor of tool_choice. Controls which (if any)
 function is called by the model. none means the model will not call a function and
 instead generates a message. auto means the model can pick between generating a
 message or calling a function. Specifying a particular function via {\"name\":
 \"my_function\"} forces the model to call that function. none is the default when no
 functions are present. auto is the default if functions are present.")
   (functions
    :accessor functions
    :initarg :functions
    :type functions
    :documentation "Deprecated in favor of tools. A list of functions the model may
 generate JSON inputs for.")
   (logit-bias
    :accessor logit-bias
    :initarg :logit-bias
    :type hash-table
    :documentation "Modify the likelihood of specified tokens appearing in the
 completion. Accepts a JSON object that maps tokens (specified by their token ID in
 the tokenizer) to an associated bias value from -100 to 100. Mathematically, the
 bias is added to the logits generated by the model prior to sampling. The exact
 effect will vary per model, but values between -1 and 1 should decrease or increase
 likelihood of selection; values like -100 or 100 should result in a ban or exclusive
 selection of the relevant token.")
   (logprobs
    :accessor logprobs
    :initarg :logprobs
    :type boolean
    :documentation "Whether to return log probabilities of the output tokens or not.
 If true, returns the log probabilities of each output token returned in the content
 of message.")
   (max-completion-tokens
    :accessor max-completion-tokens
    :initarg :max-completion-tokens
    :type integer
    :documentation "An upper bound for the number of tokens that can be generated for
 a completion, including visible output tokens and reasoning tokens.")
   (max-tokens
    :accessor max-tokens
    :initarg :max-tokens
    :type integer
    :documentation "The maximum number of tokens that can be generated in the chat
 completion. This value can be used to control costs for text generated via API. This
 value is now deprecated in favor of max_completion_tokens, and is not compatible
 with o-series models.")
   (metadata
    :accessor metadata
    :initarg :metadata
    :type hash-table
    :documentation "Set of 16 key-value pairs that can be attached to an object. This
 can be useful for storing additional information about the object in a structured
 format, and querying for objects via API or the dashboard. Keys are strings with a
 maximum length of 64 characters. Values are strings with a maximum length of 512
 characters.")
   (modalities
    :accessor modalities
    :initarg :modalities
    :type list-of-strings
    :documentation "Output types that you would like the model to generate. Most
 models are capable of generating text, which is the default: [\"text\"] The
 gpt-4o-audio-preview model can also be used to generate audio. To request that this
 model generate both text and audio responses, you can use: [\"text\", \"audio\"]")
   (n
    :accessor n
    :initarg :n
    :type integer
    :documentation "How many chat completion choices to generate for each input
 message. Note that you will be charged based on the number of generated tokens
 across all of the choices. Keep n as 1 to minimize costs.")
   (parallel-tool-calls
    :accessor parallel-tool-calls
    :initarg :parallel-tool-calls
    :documentation "Whether to enable parallel function calling during tool use. (Default:
 true)")
   (prediction
    :accessor prediction
    :initarg :prediction
    :type prediction
    :documentation "Configuration for a Predicted Output, which can greatly improve
 response times when large parts of the model response are known ahead of time. This
 is most common when you are regenerating a file with only minor changes to most of
 the content.")
   (presence-penalty
    :accessor presence-penalty
    :initarg :presence-penalty
    :documentation "Number between -2.0 and 2.0. Positive values penalize new tokens
 based on whether they appear in the text so far, increasing the model's likelihood
 to talk about new topics.")
   (reasoning-effort
    :accessor reasoning-effort
    :initarg :reasoning-effort
    :documentation "Constrains effort on reasoning for reasoning models. Currently
 supported values are low, medium, and high. Reducing reasoning effort can result in
 faster responses and fewer tokens used on reasoning in a response. (Default: medium) (o-series
 models only)")
   (response-format
    :accessor response-format
    :initarg :response-format
    :type response-format
    :documentation "An object specifying the format that the model must output.
 Setting to { \"type\": \"json_schema\", \"json_schema\": {...} } enables Structured
 Outputs which ensures the model will match your supplied JSON schema. Learn more in
 the Structured Outputs guide. Setting to { \"type\": \"json_object\" } enables the
 older JSON mode, which ensures the message the model generates is valid JSON. Using
 json_schema is preferred for models that support it.")
   (seed
    :accessor seed
    :initarg :seed
    :type integer
    :documentation "This feature is in Beta. If specified, our system will make a
 best effort to sample deterministically, such that repeated requests with the same
 seed and parameters should return the same result. Determinism is not guaranteed,
 and you should refer to the system_fingerprint response parameter to monitor changes
 in the backend.")
   (service-list
    :accessor service-list
    :initarg :service-list
    :type string
    :documentation "Specifies the latency tier to use for processing the request.
 This parameter is relevant for customers subscribed to the scale tier service: If
 set to 'auto', and the Project is Scale tier enabled, the system will utilize scale
 tier credits until they are exhausted. If set to 'auto', and the Project is not
 Scale tier enabled, the request will be processed using the default service tier
 with a lower uptime SLA and no latency guarentee. If set to 'default', the request
 will be processed using the default service tier with a lower uptime SLA and no
 latency guarentee. If set to 'flex', the request will be processed with the Flex
 Processing service tier. Learn more. When not set, the default behavior is 'auto'.
 When this parameter is set, the response body will include the service_tier
 utilized.")
   (stop
    :accessor stop
    :initarg :stop
    :type stop
    :documentation "Not supported with latest reasoning models o3 and o4-mini. Up to
 4 sequences where the API will stop generating further tokens. The returned text
 will not contain the stop sequence.")
   (store
    :accessor store
    :initarg :store
    :type boolean
    :documentation "Whether or not to store the output of this chat completion
 request for use in our model distillation or evals products.")
   (stream
    :accessor stream
    :initarg :stream
    :type (or boolean 'null)
    :documentation "If set to true, the model response data will be streamed to the
 client as it is generated using server-sent events. See the Streaming section for
 more information, along with the streaming responses guide for more information on
 how to handle the streaming events.
 https://platform.openai.com/docs/api-reference/chat-streaming")
   (stream-options
    :accessor stream-options
    :initarg :stream-options
    :type stream-options
    :documentation "Options for streaming response. Only set this when you set stream: true.")
   (temperature
    :accessor temperature
    :initarg :temperature
    :type number
    :documentation "What sampling temperature to use, between 0 and 2. Higher values
 like 0.8 will make the output more random, while lower values like 0.2 will make it
 more focused and deterministic. We generally recommend altering this or top_p but
 not both.")
   (tool-choice
    :accessor tool-choice
    :initarg :tool-choice
    :type tool-choice
    :documentation "Controls which (if any) tool is called by the model.

Can be either a string or a `openai-sdk/chat-completion:tool-choice-part'.

- \"unbound\" or NIL means the model will not call any tool and instead generates a
- message.  \"auto\" means the model can pick between generating a message or calling
- one or more tools.  \"required\" means the model must call one or more tools.

Specifying a particular tool (via `openai-sdk/chat-completion:make-tool-choice')
forces the model to call that tool.

Note 1: \"none\" is the default when no tools are present.
Note 2: \"auto\" is the default if tools are present.")
   (tools
    :accessor tools
    :initarg :tools
    :type tools
    :documentation "A list of tools the model may call.

Tools can be defined with `openai-sdk/tool-call:make-tool', which is more or less a
copy of gptel's tool-calling feature.

The list of tools may contain strings of function names,
`openai-sdk/tool-call:tool'-objects, and `openai-sdk/chat-completion:tool'-objects.
Before request serialization, the strings and `openai-sdk/tool-call:tool'-objects
will be converted to `openai-sdk/chat-completion:tool'-objects.

Note: A max of 128 functions are supported (by OpenAI).")
   (top-logprobs
    :accessor top-logprobs
    :initarg :top-logprobs
    :type integer
    :documentation "An integer between 0 and 20 specifying the number of most likely
 tokens to return at each token position, each with an associated log probability.
 logprobs must be set to true if this parameter is used.")
   (top-p
    :accessor top-p
    :initarg :top-p
    :type number
    :documentation "An alternative to sampling with temperature, called nucleus
 sampling, where the model considers the results of the tokens with top_p probability
 mass. So 0.1 means only the tokens comprising the top 10% probability mass are
 considered. OpenAI recommends altering this or temperature but not both.")
   (user
    :accessor user
    :initarg :user
    :type string
    :documentation "A unique identifier representing your end-user, which can help
 OpenAI to monitor and detect abuse. Learn more:
 https://platform.openai.com/docs/guides/safety-best-practices#end-user-ids")
   (web-search-options
    :accessor web-search-options
    :initarg :web-search-options
    :type web-search-options
    :documentation "This tool searches the web for relevant results to use in a
 response.")))

(defun make-chat-completion (&rest args &key messages audio frequency-penalty function-call
                                          functions logit-bias logprobs
                                          max-completion-tokens max-tokens
                                          metadata modalities model n parallel-tool-calls
                                          prediction presence-penalty reasoning-effort
                                          response-format seed service-list
                                          stop store stream stream-options
                                          temperature tool-choice tools
                                          top-logprobs top-p user web-search-options)
  "Every parameter must be taken care of by you. See the `oai/cc:chat-completions'-class
for documentation per slot. Each parameter that has a class as type exposes a
oai/cc:make-<class>.

When the content is streamed, upon finishing, the normal completion-object is
returned. However, the `:stream'-keyword is treated as a special property when passed
to `oai:send'. The streamed chunks may be manipulated and redirected in four
ways. First, when its value is `t', then the chunk-delta is written to
`*standard-output*'. Second, if its value is of type `stream', then the chunk-delta
is outputted to that stream. Third, if its value is of type `function', then the
chunk-object of type `oai/cc:completion' is passed as a parameter to that
function. Fourth, its value coheres with the argument list of
`openai-sdk/chat-completion/request::%parse-chunks-from-stream'.

So, for example, in the fourth case:
(oai:make-chat-completion [...]
                          :stream (list :output *my-output*
                                        :func #'my-func
                                        :close-stream-p nil)
                          [...])

This will:
- output the chunk-delta content to *my-output* in reading order
- call #'my-func on each chunk in reading order
- not close the stream (which is returned as the last return-value of the
  `openai-sdk/core:request').

There are two limitations with outputting the chunk-delta content. The first is that,
when passing `:n' > 1, only the first chunk-delta is outputted to the stream. Second,
function-calls and tool-calls are not outputted."
  (declare (ignore messages audio frequency-penalty function-call functions logit-bias logprobs
                   max-completion-tokens max-tokens metadata modalities model n parallel-tool-calls
                   prediction presence-penalty reasoning-effort response-format seed service-list
                   stop store stream stream-options temperature tool-choice tools top-logprobs
                   top-p user web-search-options))
  (apply #'make-instance 'chat-completion args))

;;
;; COMPLETION-specific objects
;;

(defclass completion-tokens-details (openai-json-serializable)
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

(defclass prompt-tokens-details (openai-json-serializable)
  ((audio-tokens :accessor audio-tokens
                 :initarg :audio-tokens
                 :type integer)
   (cached-tokens :accessor cached-tokens
                  :initarg :cached-tokens
                  :type integer)))

(defun make-prompt-tokens-details (&rest args &key audio-tokens cached-tokens)
  (declare (ignore audio-tokens cached-tokens))
  (apply #'make-instance 'prompt-tokens-details args))

(defclass usage (openai-json-serializable)
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

(defclass base-logprob (openai-json-serializable)
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

(defclass logprobs (openai-json-serializable)
  ((content :accessor content
            :initarg :content
            :type special-logprob)
   (refusal :accessor refusal
            :initarg :refusal
            :type special-logprob)))

(defun make-logprobs (&key content refusal)
  (make-instance 'logprobs :content (objectify 'make-special-logprob content)
                           :refusal (objectify 'make-special-logprob refusal)))

(defclass url-citation (openai-json-serializable)
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

(defclass annotation (openai-json-serializable)
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

(defclass audio (openai-json-serializable)
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

;; TODO: morph into assistant-message when reusing in request
(defclass completion-message (openai-json-serializable)
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

(defun make-completion-message (&key content refusal role annotations audio function-call tool-calls)
  (make-instance
   'completion-message
   :content content
   :refusal refusal
   :role role
   :annotations (map 'simple-vector (lambda (a) (objectify 'make-annotation a)) annotations)
   :audio (objectify 'make-audio audio)
   :function-call (objectify 'make-function-call function-call)
   :tool-calls (map 'simple-vector (lambda (tc) (objectify 'make-completion-tool-call tc)) tool-calls)))

(defclass choice (openai-json-serializable)
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
    :type completion-message)))

(defun make-choice (&key finish-reason index logprobs message)
  (make-instance 'choice :finish-reason finish-reason
                         :index index
                         :logprobs (objectify 'make-logprobs logprobs)
                         :message (objectify 'make-completion-message message)))

(defclass completion (openai-json-serializable)
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

(defun make-completion (&key id choices created model object service-tier usage system-fingerprint)
  (make-instance 'completion :id id
                             :choices (map 'simple-vector (lambda (choice)
                                                            (objectify 'make-choice choice))
                                           choices)
                             :created (or (and created (local-time:unix-to-timestamp created))
                                          (local-time:now))
                             :model model
                             :object object
                             :service-tier service-tier
                             :usage (objectify 'make-usage usage)
                             :system-fingerprint system-fingerprint))

(defclass chunked-tool-call (tool-call)
  ((index :initarg :index
          :accessor index
          :type integer)))

(defun make-chunked-tool-call (&key index function id type)
  (make-instance 'chunked-tool-call :index index
                                    :function (objectify 'make-function-call function)
                                    :id id
                                    :type type))

(defclass delta (openai-json-serializable)
  ((role :initarg :role
         :accessor role
         :type string)
   (content :initarg :content
            :accessor content
            :type string)
   (refusal :initarg :refusal
            :accessor refusal
            :type string)
   (function-call :initarg :function-call
                  :accessor function-call
                  :type function-call)
   (tool-calls :initarg :tool-calls
               :accessor tool-calls
               :type simple-vector)))

(defun make-delta (&key role content refusal function-call tool-calls)
  (make-instance 'delta :role role
                        :content content
                        :refusal refusal
                        :function-call (objectify 'make-function-call function-call)
                        :tool-calls (map 'simple-vector (lambda (tc)
                                                          (objectify 'make-chunked-tool-call tc))
                                         tool-calls)))

(defclass chunk-choice (openai-json-serializable)
  ((index :initarg :index
          :accessor index
          :type integer)
   (delta :initarg :delta
          :accessor delta
          :type delta)
   (logprobs :initarg :logprobs
             :accessor logprobs
             :type logprobs)
   (finish-reason :initarg :finish-reason
                  :accessor finish-reason
                  :type string)))

(defun make-chunk-choice (&key index delta logprobs finish-reason)
  (make-instance 'chunk-choice :index index
                               :delta (objectify 'make-delta delta)
                               :logprobs (objectify 'make-logprobs logprobs)
                               :finish-reason finish-reason))

(defun make-chunked-completion (&key id choices created model object service-tier usage system-fingerprint)
  (make-instance 'completion :id id
                             :choices (map 'simple-vector (lambda (choice)
                                                            (objectify 'make-chunk-choice choice))
                                           choices)
                             :created (local-time:unix-to-timestamp created)
                             :model model
                             :object object
                             :service-tier service-tier
                             :usage (objectify 'make-usage usage)
                             :system-fingerprint system-fingerprint))
