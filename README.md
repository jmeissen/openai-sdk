# openai-sdk
Common Lisp [OpenAI SDK](https://platform.openai.com/docs/api-reference/introduction)
implementation.

Disclaimer: this is still a **work in progress**. AKA the public API may change unannounced.
This project does not follow any logical versioning below v1.0.0.

*Please send feedback if you have any.*

Only the chat-completion API has been fully implemented.

See the git logs for more attempts at convenience that I have abandoned.

# Goal

Implement the complete API as 1 to 1 as possible.

# Use (Chat Completions only)

## Init
```lisp
(oai:make-client "<api-key>")
```

## Send

Currently only supports sending a `chat-completion`-object as first parameter. The generic:
```lisp
(oai:send {request-object} [{client}])
```
See `oai/cc:make-chat-completion` for further reference. In principle, it's the API
docs in CLOS, both in request and response.

## Simple messaging
chat completions are referenced with `oai/cc:`. Methods are available also with `oai:`
```lisp
(oai:make-client "<api-key>")

(oai:content
 (oai:message
  (aref
   (oai:choices
    (oai:send
     (oai/cc:make-chat-completion
      :messages (list (oai/cc:make-user-message "Hello, world")))))
   0)))
```

## Function calling

```lisp
(defvar *function-call*
  (oai:send
   (oai/cc:make-chat-completion
    :messages (list (oai/cc:make-user-message "What is the weather like in Paris today?"))
    :functions (list
                (oai/cc:make-function
                 :name "get-weather"
                 :description "Get current temperature for a given location"
                 :parameters (let ((parameters (make-hash-table :test #'equal))
                                   (properties (make-hash-table :test #'equal))
                                   (location (make-hash-table :test #'equal)))
                               (setf (gethash "type" parameters) "object")
                               (setf (gethash "type" location) "string")
                               (setf (gethash "description" location) "City and country e.g. Bogotá, Colombia")
                               (setf (gethash "location" properties) location)
                               (setf (gethash "properties" parameters) properties)
                               (setf (gethash "required" parameters) '("location"))
                               (setf (gethash "additionalProperties" parameters) nil)
                               parameters)))
    :function-call (oai/cc:make-function-call :name "get-weather"))))


(defvar *function-call-response-arguments*             ;= "{\"location\":\"Paris, France\"}"
  (oai:arguments
   (oai:function-call
    (oai:message
     (aref (oai:choices *function-call*) 0)))))
```

## Tool calling

```lisp
(defvar *tool-call-function* (com.inuoe.jzon:parse "{
  \"type\": \"object\",
  \"properties\": {
    \"location\": {
      \"type\": \"string\",
      \"description\": \"City and country e.g. Bogotá, Colombia\"
    }
  },
  \"required\": [
    \"location\"
  ],
  \"additionalProperties\": false
}"))

(defvar *tool-call-response*
  (oai:send
   (oai/cc:make-chat-completion
    :messages (list (oai/cc:make-user-message "What is the weather like in Paris today?"))
    :tools (list (oai/cc:make-tool
                  (oai/cc:make-function
                   :name "get-weather"
                   :description "Get current temperature for a given location"
                   :parameters *tool-call-function*
                   :strict t)))
    :tool-choice (oai/cc:make-tool-choice "get-weather"))))

(defvar *tool-call-response-arguments*  ;= "{\"location\":\"Paris, France\"}"
  (oai:arguments
   (oai:function
    (aref (oai:tool-calls
           (oai:message
            (aref (oai:choices *tool-call-response*) 0)))
          0))))
```

## Structured outputs

```lisp
(defvar *structured-output-json-schema* (com.inuoe.jzon:parse "{
  \"type\": \"object\",
  \"properties\": {
    \"steps\": {
      \"type\": \"array\",
      \"items\": {
        \"type\": \"object\",
        \"properties\": {
          \"explanation\": {
            \"type\": \"string\"
          },
          \"output\": {
            \"type\": \"string\"
          }
        },
        \"required\": [
          \"explanation\",
          \"output\"
        ],
        \"additionalProperties\": false
      }
    },
    \"final_answer\": {
      \"type\": \"string\"
    }
  },
  \"required\": [
    \"steps\",
    \"final_answer\"
  ],
  \"additionalProperties\": false
}"))

(defvar *structured-output*
  (oai:send
   (oai/cc:make-chat-completion
    :messages (list (oai/cc:make-system-message "You are a helpful math tutor. Guide the user through the solution step by step.")
                    (oai/cc:make-user-message "how can I solve 8x + 7 = -23"))
    :response-format (oai/cc:make-response-format
                      "json_schema"
                      :json-schema (oai/cc:make-json-schema
                                    "math-reasoning"
                                    :schema *structured-output-json-schema*
                                    :strict t)))))

(defvar *structured-output-response-content*
  (oai:content
   (oai:message
    (aref
     (oai:choices *structured-output*) 0))))
```

# OpenAI client definition

```lisp
(defclass client ()
  ((base-url :initarg :base-url
             :accessor base-url
             :initform *default-base-url*)
   (api-key :initarg :api-key
            :accessor api-key)
   (organization-id :initarg :organization-id
                    :accessor organization-id
                    :initform nil)
   (project-id :initarg :project-id
               :accessor project-id
               :initform nil)
   (read-timeout :initarg :read-timeout
                 :accessor read-timeout
                 :initform 600)
   (connect-timeout :initarg :connect-timeout
                    :accessor connect-timeout
                    :initform 5.0)
   (max-retries :initarg :max-retries
                :accessor max-retries
                :initform 3)
   (default-headers :initarg :default-headers
                    :accessor default-headers
                    :initform *default-headers*)
   (default-model :initarg :default-model
                  :accessor default-model
                  :initform *default-model*)))
```

# All message types
```lisp
(oai/cc:make-function-message (name &rest args &key content))
(oai/cc:make-developer-message (content &rest args &key name))
(oai/cc:make-system-message (content &rest args &key name))
(oai/cc:make-user-message (content &rest args &key name))
(oai/cc:make-tool-message (content tool-call-id))
(oai/cc:make-assistant-message (&rest args &key audio content function-call name refusal
                                            tool-calls))
```

## Platforms API

### Models

#### List
```lisp
CL-USER> (oai-models:list)
#("gpt-4-0613" "gpt-4" "gpt-3.5-turbo" "o3-pro-2025-06-10" "codex-mini-latest"
  "o3-pro" "gpt-4o-realtime-preview-2025-06-03"
  "gpt-4o-audio-preview-2025-06-03" "davinci-002" "babbage-002"
  "gpt-3.5-turbo-instruct" "gpt-3.5-turbo-instruct-0914" "dall-e-3" "dall-e-2"
  "gpt-4-1106-preview" ...)
```

#### Search
```lisp
CL-USER> (oai-models:search "o3")
#("o3" "o3-2025-04-16" "o3-mini" "o3-mini-2025-01-31" "o3-pro"
  "o3-pro-2025-06-10")
```

#### [untested] Delete a fine-tuned model
```lisp
CL-USER> (oai-models:delete "ft:gpt-4o-mini:acemeco:suffix:abc123")
#<DELETED-MODEL {7009974813}>
```

# Install
## qlot
```sh
qlot add jmeissen/openai-sdk
```

## local-projects
```sh
git clone https://github.com/jmeissen/openai-sdk ~/common-lisp/
# or
git clone https://github.com/jmeissen/openai-sdk ~/.quicklisp/local-projects/
```

```lisp
(ql:register-local-projects)
(ql:quickload :openai-sdk)
```

# TODO

Note: I had big plans on convenience, but I decided that this does not fit in this
API wrapper. Check the git logs for previous attempts on improving comfort (date of
writing: 2025-07-11).

## Implement
- [x] Chat Completions API
  - [ ] Streaming
- [ ] Responses API
- [ ] Platform APIs
  - [ ] Audio
  - [ ] Images
  - [ ] Embeddings
  - [ ] Evals
  - [ ] Fine-tuning
  - [ ] Files
  - [ ] Uploads
  - [x] Models
  - [ ] Moderations
- [ ] Vector stores API
- [ ] Assistants API
- [ ] Administration API
- [ ] [Rate limits](https://platform.openai.com/docs/guides/rate-limits)
## Proper exception handling
## Tests
