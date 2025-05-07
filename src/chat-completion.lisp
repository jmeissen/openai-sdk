(in-package #:cl-user)

(defpackage openai-sdk/chat-completion
  (:use #:cl
        #:openai-sdk/core)
  (:shadowing-import-from #:openai-sdk/core #:function #:type #:format)
  (:import-from #:openai-sdk/util #:format-b64)
  (:export #:approximate
           #:arguments
           #:assistant-message
           #:assistant-message-audio
           #:assistant-message-content
           #:audio
           #:audio-content-part
           #:chat-completion
           #:chat-completion-audio
           #:chat-completion-function-call
           #:chat-completion-function-call-p
           #:chat-completion-function-call-type
           #:city
           #:content
           #:content-part
           #:country
           #:data
           #:description
           #:detail
           #:developer-message
           #:developer-message-content
           #:developer-message-content-p
           #:file
           #:file-content-part
           #:file-data
           #:file-id
           #:filename
           #:format
           #:frequency-penalty
           #:function
           #:function-call
           #:function-message
           #:functions
           #:functions-p
           #:id
           #:image-content-part
           #:image-url
           #:include-usage
           #:input-audio
           #:json-schema
           #:list-of-strings
           #:list-of-strings-p
           #:logit-bias
           #:logprobs
           #:make-approximate
           #:make-assistant-message
           #:make-assistant-message-audio
           #:make-chat-completion
           #:make-chat-completion-audio
           #:make-chat-completion-function-call
           #:make-developer-message
           #:make-file-data-content-part
           #:make-file-id-content-part
           #:make-function
           #:make-function-call
           #:make-function-message
           #:make-json-schema
           #:make-message-from-keyword
           #:make-prediction
           #:make-prediction-content-part
           #:make-response-format
           #:make-stream-options
           #:make-system-message
           #:make-text-content-part
           #:make-tool
           #:make-tool-call
           #:make-tool-choice
           #:make-tool-message
           #:make-user-location
           #:make-user-message
           #:make-web-search-options
           #:max-completion-tokens
           #:max-tokens
           #:message
           #:messages
           #:metadata
           #:modalities
           #:model
           #:n
           #:name
           #:named-message
           #:parallel-tool-calls
           #:parameters
           #:parse-message
           #:prediction
           #:prediction-content
           #:prediction-content-p
           #:prediction-content-part
           #:presence-penalty
           #:reasoning-effort
           #:refusal
           #:region
           #:response-format
           #:response-format-type
           #:response-format-type-p
           #:role
           #:schema
           #:search-context-size
           #:seed
           #:service-list
           #:stop
           #:stop-p
           #:store
           #:chat-completion-stream
           #:stream-options
           #:strict
           #:system-message
           #:system-message-content
           #:temperature
           #:text
           #:text-content-part
           #:timezone
           #:tool
           #:tool-call
           #:tool-call-id
           #:tool-calls
           #:tool-calls-p
           #:tool-choice
           #:tool-choice-p
           #:tool-choice-part
           #:tool-message
           #:tool-message-content
           #:tools
           #:tools-p
           #:top-logprobs
           #:top-p
           #:type
           #:url
           #:user
           #:user-location
           #:user-message
           #:user-message-content
           #:user-message-content-p
           #:voice
           #:web-search-options))

(in-package #:openai-sdk/chat-completion)

(defclass message (openai-request)
  ((role :reader role)
   (content :initarg :content
            :accessor content)))

(defclass named-message (message)
  ((name :initarg :name
         :accessor name)))

(defclass content-part (openai-request)
  ((type :type string
         :reader type)))

(defclass text-content-part (content-part)
  ((type :initform "text")
   (text :type string
         :accessor text
         :initarg :text)))

(defun make-text-content-part (text)
  (make-instance 'text-content-part :text text))

(defclass image-url (openai-request)
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

(defclass input-audio (openai-request)
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

(defclass file (openai-request)
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

(defun user-message-content-p (msg)
  (or (stringp msg)
      (and (consp msg)
           (every (lambda (el) (or (typep el 'text-content-part)
                                   (typep el 'image-content-part)
                                   (typep el 'audio-content-part)
                                   (typep el 'file-content-part)))
                  msg))))

(deftype user-message-content ()
  `(satisfies user-message-content-p))

(defclass user-message (named-message)
  ((role :initform "user")
   (content :type user-message-content)))

(defun make-user-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'user-message :content content args))

(defun developer-message-content-p (msg)
  (or (stringp msg)
      (and (consp msg)
           (every (lambda (el) (or (typep el 'text-content-part)))
                  msg))))

(deftype developer-message-content ()
  `(satisfies developer-message-content-p))

(defclass developer-message (named-message)
  ((role :initform "developer")
   (content :type developer-message-content)))

(defun make-developer-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'developer-message :content content args))

(deftype system-message-content ()
  `(satisfies developer-message-content-p))

(defclass system-message (developer-message)
  ((role :initform "system")
   (content :type system-message-content)))

(defun make-system-message (content &rest args &key name)
  (declare (ignore name))
  (apply #'make-instance 'system-message :content content args))

(deftype assistant-message-content ()
  `(satisfies developer-message-content-p))

(defclass assistant-message-audio (openai-request)
  ((id :initarg :id
       :accessor id
       :type string
       :documentation "Unique identifier for a previous audio response from the
 model.")))

(defun make-assistant-message-audio (id)
  (make-instance 'assistant-message-audio :id id))

(defclass function-call (openai-request)
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

(defun make-function-call (name &rest args &key arguments)
  (declare (ignore arguments))
  (apply #'make-instance 'function-call :name name args))

(defclass tool-call (openai-request)
  ((type :initform "function"
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

(defun make-tool-call (function id type)
  (make-instance 'tool-call :function function :id id :type type))

(defun tool-calls-p (tc)
  (and (consp tc)
       (every (lambda (tt) (typep tt 'tool-call))
              tc)))

(deftype tool-calls ()
  `(satisfies tool-calls-p))

(defclass assistant-message (developer-message)
  ((role :initform "assistant")
   (content :type assistant-message-content)
   (audio :initarg :audio
          :accessor audio
          :type assistant-message-audio
          :documentation "Data about a previous audio response from the model.")
   (function-call :initarg :function-call
                  :accessor function-call
                  :type function-call
                  :documentation "Deprecated and replaced by tool_calls. The name and
 arguments of a function that should be called, as generated by the model.")
   (refusal :initarg :refusal
            :accessor refusal
            :type string
            :documentation "The refusal message by the assistant.")
   (tool-calls :initarg :tool-calls
               :accessor tool-calls
               :type tool-calls
               :documentation "The tool calls generated by the model, such as
 function calls.")))

(defun make-assistant-message (&rest args &key audio content function-call name refusal tool-calls)
  (declare (ignore audio content function-call name refusal tool-calls))
  (apply #'make-instance 'assistant-message args))

(deftype tool-message-content ()
  `(satisfies developer-message-content-p))

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

(defun build-user-message-content-parts (user-message-content-list)
  (loop for msg in user-message-content-list
        collect (let ((mime (mimes:mime msg)))
                  (cond ((stringp msg)
                         (let* ((uri (quri:make-uri :defaults msg))
                                (scheme (quri:uri-scheme uri)))
                           (cond ((and (not (position #\  msg :test #'equal))
                                       scheme
                                       (mimes:mime-equal "image/*" mime))
                                  (make-instance 'image-content-part :image-url msg))
                                 (t (make-instance 'text-content-part :text msg)))))
                        ((pathnamep msg)
                         (if (mimes:mime-equal "image/*" mime)
                             (make-instance 'image-content-part :image-url (format-b64 (b64encode msg) mime))
                             (if (mimes:mime-equal "audio/*" mime)
                                 (if (not (or (mimes:mime-equal "audio/mp3" mime)
                                              (mimes:mime-equal "audio/wav" mime)))
                                     (error "Unsupported audio mime type")
                                     (make-instance 'audio-content-part
                                                    :input-audio (make-instance 'input-audio :data (format-b64 (b64encode msg) mime)
                                                                                             :format (subseq mime (+ 1 (position #\/ mime :test #'equal))))))
                                 (make-instance 'file-content-part :file (make-instance 'file :filename (file-namestring msg)
                                                                                              :file-data (format-b64 (b64encode msg) mime))))))
                        (t (error "User message type not implemented"))))))

(defun make-message-from-keyword (&key system developer user)
  (cond
    (system (make-system-message (if (typep system 'system-message-content)
                                     system
                                     (error "Incorrect system message content"))))
    (developer (make-developer-message (if (typep developer 'developer-message-content)
                                           developer
                                           (error "Incorrect developer message content"))))
    (user (make-user-message (if (typep user 'user-message-content)
                                 user
                                 (if (consp user)
                                     (build-user-message-content-parts user)
                                     (error "Incorrect user message content")))))))

(defclass chat-completion-audio (openai-request)
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

(defclass chat-completion-function-call (openai-request)
  ((name :initarg :name
         :accessor name
         :initform (error "required")
         :documentation "The name of the function to call.")))

(defun make-chat-completion-function-call (name)
  (make-instance 'chat-completion-function-call :name name))

(defun chat-completion-function-call-p (el)
  (or (stringp el)
      (typep el 'chat-completion-function-call)))

(deftype chat-completion-function-call-type ()
  `(satisfies chat-completion-function-call-p))

(defclass function (openai-request)
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
    :documentation "The parameters the functions accepts, described as a
 JSON Schema object. See the guide for examples, and the JSON Schema reference for
 documentation about the format. Omitting parameters defines a function with an empty
 parameter list.")
   (strict
    :accessor strict
    :initarg :strict
    :type boolean
    :documentation "Whether to enable strict schema adherence when generating the
 function call. If set to true, the model will follow the exact schema defined in the
 parameters field. Only a subset of JSON Schema is supported when strict is true.
 Learn more about Structured Outputs in the function calling guide. Note: Do not set
 for legacy functions.")))

(defun make-function (name &rest args &key description parameters strict) ; FIXME: paramaters json schema object serialization
  (declare (ignore description parameters strict))
  (apply #'make-instance 'function :name name args))

(defun functions-p (els)
  (and (consp els)
       (every (lambda (el) (typep el 'function)))))

(deftype functions ()
  `(satisfies functions-p))

(defclass approximate (openai-request)
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

(defclass user-location (openai-request)
  ((type :initform "approximate"
         :reader type)
   (approximate
    :accessor approximate
    :initarg :approximate
    :type approximate
    :documentation "Approximate location parameters for the search.")))

(defun make-user-location (approximate)
  (make-instance 'user-location :approximate approximate))

(defclass web-search-options (openai-request)
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
  (apply #'make-instance 'web-search-options args))

(defun list-of-strings-p (el)
  (and (consp el)
       (every #'stringp el)))

(deftype list-of-string ()
  `(satisfies list-of-strings-p))

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

(defclass prediction (openai-request)
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

(defun stop-p (content)
  (or (stringp content)
      (and (consp content)
           (every #'stringp content)
           (<= 4 (length content)))))

(deftype stop ()
  `(satisfies stop-p))

(defun response-format-type-p (el)
  (member el '("text" "json_schema" "json_object") :test #'equal))

(deftype response-format-type ()
  `(satisfies response-format-type-p))

(defclass json-schema (openai-request)
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

(defclass response-format (openai-request)
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

(defclass stream-options (openai-request)
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

(defun tool-choice-p (el)
  (or (and (stringp el)
           (or (string= el "none")
               (string= el "auto")
               (string= el "required")))
      (typep el 'tool-choice-part)))

(deftype tool-choice ()
  `(satisfies tool-choice-p))

(defun make-tool-choice (call)
  (if (typep call 'tool-choice)
      call
      (make-instance 'tool-choice-part
                     :function (make-chat-completion-function-call call))))

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

(defun tools-p (tools)
  (and (consp tools)
       (every (lambda (el) (typep el 'tool)) tools)))

(deftype tools ()
  `(satisfies tools-p))

(defclass chat-completion (openai-request)
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
    :accessor chat-completion-stream
    :initarg :stream
    :documentation "If set to true, the model response data will be streamed to the
 client as it is generated using server-sent events. See the Streaming section for
 more information, along with the streaming responses guide for more information on
 how to handle the streaming events.
 https://platform.openai.com/docs/api-reference/chat/streaming")
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
    :documentation "Controls which (if any) tool is called by the model. none means
 the model will not call any tool and instead generates a message. auto means the
 model can pick between generating a message or calling one or more tools. required
 means the model must call one or more tools. Specifying a particular tool via
 {\"type\": \"function\", \"function\": {\"name\": \"my_function\"}} forces the model
 to call that tool. none is the default when no tools are present. auto is the
 default if tools are present.")
   (tools
    :accessor tools
    :initarg :tools
    :type tools
    :documentation "A list of tools the model may call. Currently, only functions are
 supported as a tool. Use this to provide a list of functions the model may generate
 JSON inputs for. A max of 128 functions are supported.")
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

(defun parse-message (msg)
  (cond ((and (consp msg) (keywordp (car msg)) (or (stringp (cadr msg)) (consp (cadr msg))))
         (apply #'make-message-from-keyword msg))
        ((stringp msg)
         (make-user-message msg))
        ((typep msg 'message)
         msg)))

(defun make-chat-completion (messages &rest args &key audio frequency-penalty function-call
                                                   functions logit-bias logprobs
                                                   max-completion-tokens max-tokens
                                                   metadata modalities model n parallel-tool-calls
                                                   prediction presence-penalty reasoning-effort
                                                   response-format seed service-list
                                                   stop store stream stream-options
                                                   temperature tool-choice tools
                                                   top-logprobs top-p user web-search-options)
  (declare (ignore audio frequency-penalty function-call functions logit-bias logprobs
                   max-completion-tokens max-tokens metadata modalities model n parallel-tool-calls
                   prediction presence-penalty reasoning-effort response-format seed service-list
                   stop store stream stream-options temperature tool-choice tools top-logprobs
                   top-p user web-search-options))
  (apply #'make-instance 'chat-completion :messages (mapcar #'parse-message (etypecase messages
                                                                              (cons messages)
                                                                              (string (list messages))))
         args))
