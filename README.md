# openai-sdk
Common Lisp [OpenAI SDK](https://platform.openai.com/docs/api-reference/introduction)
implementation.

Disclaimer: this is a **work in progress**!
This project will follow semantic versioning from version 1.0.0.

# Use
```lisp
(oai:make-openai "<api-key>") ; Will be set in OAI:*OPENAI*
(oai:content (oai:message (aref (oai:choices
                                  (oai:create-chat-completion "Hello, world!"))
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

## Generic create chat completion
```lisp
(create-chat-completion (chat-completion &optional openai))
```

Currently supports the following `chat-completion` specializations:
- `list` (containing other plists, of which the keywords act like references to
  `string`, `pathname` or encoded `string` URIs including their schemas;
- `string`
- `chat-completion`

### List

```lisp
CL-USER> (oai:create-chat-completion `((:system "Find the required parameters according to the user specification sourcing from the provided document.")
                                       (:user ("Here's the document and an image, do what you must!"
                                               #p"test-file.pdf"
                                               "https://something.com/whatever.jpg"))))
```

Will first call `openai-sdk/chat-completion:make-message-from-keyword` to transform the chat-completion into the following:
```lisp
(#<OPENAI-SDK/CHAT-COMPLETION:SYSTEM-MESSAGE>
 #<OPENAI-SDK/CHAT-COMPLETION:USER-MESSAGE>)
```

After that, `oai:create-chat-completion` will generate the
`openai-sdk/chat-completion:chat-completion`-object, and finally call the API and get
the response-object `openai-sdk/response:chat-completion` as displayed at the top of
the page. User messages containing files (audio/video/otherwise) will - if their path
and extension is given - be correctly put into their objects, respectively, and have
their contents base64 encoded.

However, you are still free to generate messages threads yourself. See below for an example.

# Examples

## Simple messages
```lisp
;; single user message
(oai:create-chat-completion "hello world")

;; another single user message
(oai:create-chat-completion (list "hello world"))

;; Multi message type messages
(oai:create-chat-completion
    (list (make-system-message "some msg")
          (make-developer-message "some other msg")
          (make-user-message "Hello world!"))
```
### All message types
```lisp
(oai:make-function-message (name &rest args &key content))
(oai:make-developer-message (content &rest args &key name))
(oai:make-system-message (content &rest args &key name))
(oai:make-user-message (content &rest args &key name))
(oai:make-tool-message (content tool-call-id))
(oai:make-assistant-message (&rest args &key audio content function-call name refusal
                                            tool-calls))
```

## Legacy function calling

Note: see the last example for CLOS -> JSON Schema -> CLOS.

```lisp
(oai:make-openai "<api-key>")

(defvar *function-call*
  (oai:create-chat-completion
   (oai:make-chat-completion
    "What is the weather like in Paris today?"
    :functions (list
                (oai:make-function
                 "get-weather"
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
    :function-call (oai:make-function-call "get-weather"))))


(defvar *function-call-response-arguments*             ;= "{\"location\":\"Paris, France\"}"
  (oai:arguments
   (oai:function-call
    (oai:message
     (aref (oai:choices *response*) 0)))))
```

## Tool calling
```lisp
(defvar *tool-call-response*
  (oai:create-chat-completion
   (oai:make-chat-completion
    "What is the weather like in Paris today?"
    :tools (list (oai:make-tool
                  (oai:make-function
                   "get-weather"
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
                                 parameters)
                   :strict t)))
    :tool-choice (oai:make-tool-choice "get-weather"))))

(defvar *tool-call-response-arguments*  ;= "{\"location\":\"Paris, France\"}"
  (oai:arguments
   (oai:function
    (aref (oai:tool-calls
           (oai:message
            (aref (oai:choices *tool-call-response*) 0)))
          0))))
```

## Structured outputs

Defining classes and types. See below for more info on JSON schemas:
```lisp
(defclass math-step ()
  ((explanation :type string)
   (output :type string)))

(oai/json:def-schema-list-type math-step)

(defclass math-reasoning ()
  ((steps :type math-step-oai-list)
   (final-answer :type string)))
```

Constructing object based on response:
```lisp
CL-USER> (oai:structured-output 'math-reasoning
                                "You are a helpful math tutor. Guide the user through the solution step by step."
                                '("how can I solve 8x + 7 = -23"))
#<MATH-REASONING {7009974813}>
```

In JSON, looks like the following:
```json
{
  "steps": [
    {
      "explanation": "We start with the equation 8x + 7 = -23. Our goal is to isolate the variable x. We first need to eliminate the constant term on the left-hand side, which is 7.",
      "output": "8x + 7 = -23"
    },
    {
      "explanation": "To eliminate the 7, subtract 7 from both sides of the equation. This will remove the 7 from the left side.",
      "output": "8x + 7 - 7 = -23 - 7"
    },
    {
      "explanation": "Simplifying both sides, we are left with 8x on the left, and -30 on the right.",
      "output": "8x = -30"
    },
    {
      "explanation": "Now, divide both sides of the equation by 8 to solve for x. This will isolate the x.",
      "output": "8x / 8 = -30 / 8"
    },
    {
      "explanation": "Simplifying the right side, we perform the division -30 divided by 8. It results in a fraction or a decimal. Here, as a fraction it is:",
      "output": "x = -30/8"
    },
    {
      "explanation": "Simplify the fraction -30/8 by finding the greatest common divisor of 30 and 8, which is 2, and divide both numerator and denominator by 2.",
      "output": "x = -15/4"
    },
    {
      "explanation": "Alternatively, convert -15/4 to a decimal by dividing 15 by 4 to get an approximate decimal value:",
      "output": "x = -3.75"
    }
  ],
  "final_answer": "x = -15/4 or x = -3.75"
}
```

## JSON Schemas
As you surely know, JSON schemas are applied in OpenAI's function-/tool calling and
structured outputs. However, writing JSON schemas is a pain in the ass. So, instead
of writing out the json schemas, it is my goal to provide a convenient way to go from
`class` through OpenAI to the wanted `object` instantiated.

Comfort is limited, however. Most importantly because of the way class slot types
function. Specifically, slot `:type`s are only evaluated compile-time to denote
return values of readers and to throw warnings when the wrong type is used in the
`:initform`, and what to do on runtime slot allocation with incorrect data types is
undefined by the spec. In other words, runtime datatype analysis to correctly convert
to JSON schema is a pain in the ass. To limit yakshaving, at the time of writing, I
impose four limitations on objects in order to be able to be used. This is subject
to change, as I'm sure I can just overwrite the jzon writer to do what I want.

### Limitations
1. Slot types MUST either be:
   1. standard types understood by JSON Schema (`number` is not permitted)
   2. class symbols
   3. list type as defined by the `oai/json:def-schema-list-type`-macro making
      available a `<class>-oai-list`-type
2. The `response-format`-object must have the `:strict`-option `t`, because I haven't
   adapted the schema creation yet to support the non-strict variant. See [Strict Mode documentation](https://platform.openai.com/docs/guides/function-calling#strict-mode) for more info.
3. Having `error` in an `initform` is not supported at the moment: the classes are
   instantiated before `slot-values` are assigned.

I'll extend the parser later to support a `:initarg` flag, so that
`:initform (error "something")` is supported for the classes to parse objects by
class symbol. Anyway, the following is the least bad implementation I could think of
at the time of writing. If you have ideas on how to make the internals require less
of the user, I'm all ears.

### CLOS Structured Output schema creation/parsing example

Let's take the example from the structured outputs example from above.

```lisp
(defclass math-step ()
  ((explanation :type string)
   (output :type string)))

(oai/json:def-schema-list-type math-step)        ; <- See limitation 1

(defclass math-reasoning ()
  ((steps :type math-step-oai-list)          ; `def-list-type'-macro defines the type <CLASS>-OAI-ALIST, see limitation 1
   (final-answer :type string)))

(defvar *structured-output-with-clos*
  (oai:create-chat-completion
   (oai:make-chat-completion
    `((:system "You are a helpful math tutor. Guide the user through the solution step by step.")
      (:user "how can I solve 8x + 7 = -23"))
    :response-format (oai:make-response-format
                      "json_schema"
                      :json-schema (oai:make-json-schema "math-reasoning"
                                                         :schema (oai/json:schema 'math-reasoning)
                                                         :strict t ; <- See limitation 2
                                                         )))))

(defvar *structured-output-response-with-clos*
  (oai/json:parse 'math-reasoning
                  (com.inuoe.jzon:parse
                   (oai:content
                    (oai:message
                     (aref (oai:choices *structured-output-with-clos*)
                           0))))))
```

As you can see, you have to manually map it back at the moment. When I've implemented
more, I'll write convenience functions for quick one-twos.

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

I want request creation as intuitive and painless as possible, so multiple input
types for request creation will be supported.

For example, when using the Chat Completion API


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
- [ ] [Rate limits](https://platform.openai.com/docs/guides/rate-limits)
- [ ] (maybe) Completions API
