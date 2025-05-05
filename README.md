# openai-sdk
Common Lisp [OpenAI SDK](https://platform.openai.com/docs/api-reference/introduction)
implementation.

Disclaimer: this is a **work in progress**! Stuff is definitely missing, even the
`create-chat-completion` is largely untested (think function calling, etc.).
This project follows semantic versioning.

# Use
```lisp
(defvar *openai* (oai:make-openai "<api-key>"))
(oai:content (oai:message (aref (oai:choices
                                  (oai:create-chat-completion *openai* "Hello, world!"))
                                0)))
```

## OpenAI client
The client exposes the regular stuff, so it should be usable with other LLM providers
adhering to the OpenAI SDK.

```lisp
(defclass openai ()
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

## Create Chat Completion generic
```lisp
(create-chat-completion (openai chat-completion))
```

Currently supports the following `chat-completion` specializations:
- `list` (containing other plists, of which the keywords act like references to
  `string`, `pathname` or encoded `string` URIs including their schemas;
- `string`
- `chat-completion`

### List

```lisp
CL-USER> (oai:create-chat-completion *openai* `((:system "Find the required parameters according to the user specification sourcing from the provided document.")
                                                (:user ("Here's the document and an image, do what you must!"
                                                        #p"test-file.pdf"
                                                        "https://something.com/whatever.jpg"))))
```

Will transform the chat-completion into the following:
```lisp
(#<OPENAI-SDK/CHAT-COMPLETION:SYSTEM-MESSAGE>
 #<OPENAI-SDK/CHAT-COMPLETION:USER-MESSAGE>)
```

After that, the software will generate the `chat-completion`-object, and finally call
the API and get the response as displayed at the top of the page. User messages
containing files (audio/video/otherwise) will automatically be base64-encoded if
their path is given and consequently transformed into the correct user message
content part objects, assuming that the extension is also given to correctly
recognize the mime-type.

However, you are still free to generate messages threads yourself.

## Different message types
```lisp
(oai:make-function-message (name &rest args &key content))
(oai:make-developer-message (content &rest args &key name))
(oai:make-system-message (content &rest args &key name))
(oai:make-user-message (content &rest args &key name))
(oai:make-tool-message (content tool-call-id))
(oai:make-assistant-message (&rest args &key audio content function-call name refusal
                                            tool-calls))
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

# Future goal

I want request creation as intuitive and painless as possible, so
multiple input types for request creation will be supported.

For example, when using the Chat Completion API
```lisp
;; single user message
(oai:create-chat-completion *openai* "hello world")

;; another single user message
(oai:create-chat-completion *openai* (list "hello world"))

;; Multi message type messages
(oai:create-chat-completion *openai*
    (list (make-system-message "some msg")
          (make-developer-message "some other msg")
          (make-user-message "Hello world!"))
```

Something like this should also be the case for any of the structured outputs, such
as function calling. The idea is that regular data types such as strings and plists
should be supported, or in the case of classes, builtin.

## Avoid reinventing the nominal wheel
OpenAI API names will be nominally mapped to be of the lispy kind where
possible. This means, first, that the name for creating a chat completion is called
`create-chat-completion`.

Second, nested object function helpers should express parameter
cardinality. For example, `make-user-message` requires `CONTENT` while allows `NAME`:

```lisp
(defun make-user-message (content &rest args &key name) ...)
```
While `make-tool-message` requires both `CONTENT` and `TOOL-CALL-ID`:
```lisp
(defun make-tool-message (content tool-call-id) ...)
```

Third, all OpenAI symbols are parsed from snake_case to kebab-case, while
`com.inuoe.jzon` also has been wrapped to `stringify` all classes to snake_case with
`symbol-munger`.

## Providing intuitively expressive utility functions

I use structured outputs often. It is my intention therefore, to also include
convenience wrappers, in addition to a easy as possible wrapper of the OpenAI API.

# TODO

## Decent exception handling
## Writing tests

## Convenience

- [x] Message threads
- [ ] Token usage stats

## Implement
- [x] Chat Completions API
  - [ ] Structured output/function calling/tool calling framework
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
  - [ ] Models
  - [ ] Moderations
- [ ] Vector stores API
- [ ] Assistants API
- [ ] Administration API
- [ ] (maybe) Completions API
