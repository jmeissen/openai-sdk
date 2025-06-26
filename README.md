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

While deprecated, its still usable. See the following section for how the JSON Schema
comes about, and the section after that for tool-calling and the convenience wrapper.

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
                 :parameters (oai:type-object #(location)
                                              :properties `(:location ,(oai:type-string :description "City and country e.g. Bogotá, Colombia"))
                                              :additional-properties nil)))
    :function-call (oai:make-function-call "get-weather"))))


(defvar *function-call-response-arguments*             ;= "{\"location\":\"Paris, France\"}"
  (oai:arguments
   (oai:function-call
    (oai:message
     (aref (oai:choices *response*) 0)))))
```

## JSON Schema generation for tool calling

I have included a convenience wrapper for writing JSON Schemas to include as
`:parameters`. However, this part is not yet feature-complete (I have not included
references). So if you want to do esoteric stuff in the JSON schema tool-call
parameters, then you must conform to `com.inuoe.jzon` type-mappings yourself when
defining your tool (defined
[here](https://github.com/Zulu-Inuoe/jzon?tab=readme-ov-file#type-mappings). Additionally,
then, see the documentation on supported OpenAI JSON Schema keywords
[here](https://platform.openai.com/docs/guides/structured-outputs?api-mode=chat#supported-schemas). For
normal cases, the convenience wrapper will do.

Note that in my opinion, forcing `com.inuoe.jzon` adherence is *not* a bug because I
think writing anything non-s-exp in Lisp is a pain in the ass and having
documentation on what values are valid is convenient, besides having the complete
request in Common Lisp code ready for introspection. If you disagree, then feel free
to open an issue.

Full Examples:

```lisp
CL-USER> (com.inuoe.jzon:stringify
          (oai:type-string))
"{\"type\":\"string\"}"

CL-USER> (com.inuoe.jzon:stringify
          (oai:type-boolean))
"{\"type\":\"boolean\"}"

CL-USER> (com.inuoe.jzon:stringify
          (oai:type-number))
"{\"type\":\"number\"}"

CL-USER> (com.inuoe.jzon:stringify
          (oai:type-integer))
"{\"type\":\"integer\"}"

CL-USER> (com.inuoe.jzon:stringify
          (oai:type-object #()))
"{\"type\":\"object\",\"required\":[]}"

CL-USER> (com.inuoe.jzon:stringify
          (oai:type-array (oai:type-string)))
"{\"type\":\"array\",\"items\":{\"type\":\"string\"}}"

CL-USER> (com.inuoe.jzon:stringify
          (oai:type-enum #(string null)))
"{\"type\":[\"string\",\"null\"]}"

CL-USER> (com.inuoe.jzon:stringify
          (oai:type-any-of (coerce (list (oai:type-string)
                                         (oai:type-number))
                                   'simple-vector)))
"{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"number\"}]}"

```

Of course, they can be nested:

```lisp
(format t (com.inuoe.jzon:stringify (oai:type-object #(test some-number)
                   :properties `(:test ,(oai:type-string)
                                 :some-number ,(oai:type-integer)
                                 :array-of-objects ,(oai:type-array
                                                     (oai:type-any-of
                                                      (coerce
                                                       (list
                                                        (oai:type-object #()
                                                                         :properties `(:some-optional-string
                                                                                       ,(oai:type-string
                                                                                         :description "this is only optional"
                                                                                         :enum #("this" "or" "the other"))))
                                                        (oai:type-object #(mandatory-string)
                                                                         :properties `(:mandatory-string
                                                                                       ,(oai:type-enum
                                                                                         #(string null)
                                                                                         :description "also optional through its multiple types"
                                                                                         :enum #("this" "or" "the other")))))
                                                       'simple-vector))))
                   :description "this is a test with a number, string, and array of objects") :pretty t))
```
Which corresponds to
```json
{
  "type": "object",
  "properties": {
    "test": {"type": "string"},
    "some-number": {"type": "integer"},
    "array-of-objects": {
      "type": "array",
      "items": {
        "anyOf": [
          {
            "type": "object",
            "properties": {
              "some-optional-string": {
                "description": "this is only optional",
                "enum": ["this", "or", "the other"],
                "type": "string"
              }
            },
            "required": []
          },
          {
            "type": "object",
            "properties": {
              "mandatory-string": {
                "description": "also optional through its multiple types",
                "enum": ["this", "or", "the other"],
                "type": ["string", "null"]
              }
            },
            "required": ["mandatory-string"]
          }
        ]
      }
    }
  },
  "description": "this is a test with a number, string, and array of objects",
  "required": ["test", "some-number"]
}
```

For more info, see the package `openai-sdk:tool-schema-generator`.

## Tool calls
Schema generation is handy for tool calls and structured outputs. And you can apply
the above JSON Schema wrapper for your own tool call builds.

Example:
```lisp
(defvar *tool-call-response*
  (oai:create-chat-completion
   (oai:make-chat-completion
    "What is the weather like in Paris today?"
    :tools (list (oai:make-tool
                  (oai:make-function
                   "get-weather"
                   :description "Get current temperature for a given location"
                   :parameters (oai:type-object #(location)
                                                :properties `(:location ,(oai:type-string :description "City and country e.g. Bogotá, Colombia"))
                                                :additional-properties nil)
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

As the above example shows, you are completely free to handle tool calls with chat
completions how you like. However, I have additionally *more or less* copied the
[Gptel](https://github.com/karthink/gptel) API to conveniently manage tool use. Most
important exported functions are:

- `oai:make-tool`
- `oai:tool-call`
- `oai:get-tool`

First, two propositions and one consequence, however. The first is that `:parameter`
order is not maintained because JSON-objects in `com.inuoe.jzon` are
`hash-table`s. Additionally, and secondly, *afaik(?)* the
`function-lambda-expression` is lost in the default compilation optimization. The
consequence is that the function you want to automagically call **must** be wrapped
in a `lambda` and the argument names **must** equate to the `:parameter`-keywords in
the generated schema in order for my tool-call convenience wrapper to
work.

Alternatively, (afaik) two solutions exist. First, generate an argument plist that
gets applied to the compiled function. This may be a viable way to assign any
function without wrapping lambdas. Second is to allow only one argument, the list of
results. Both, I think, are restrictive, and the lambda is atm easiest. Please open
an issue if you disagree.

Now, an example of tool-calling the convenient way:

```lisp
CL-USER> (oai:make-tool :function (lambda (city-name)
                           (format nil "Par example, it's probably something like 18℃ in the 'ol town of ~a" city-name))
               :name "get-weather"
               :description "Get current temperature for the town name in celsius"
               :args (oai/tsg:type-object #("city-name") ; both symbols, keywords, and strings are fine to use.
                                          :properties `(city-name ,(oai:type-string)) ; same here
                                          :additional-properties nil)
               :strict t)
#<OPENAI-SDK/TOOL-CALL:TOOL {7009375A13}>
```

Like gptel, without specifying a `:category`, it gets dumped in the category "misc"
of the variable `openai-sdk/tool-call::*registry*`.

Verifying its existence:
```lisp
CL-USER> openai-sdk/tool-call::*registry*
(("misc" ("get-weather" . #<OPENAI-SDK/TOOL-CALL:TOOL {700BC00283}>)))
```

Easier, however, the tool is retrieved with `oai:get-tool`. If distinct categories
exist with equal function names in it, then the tool-category can be prepended to the
tool name for specified search.

```lisp
CL-USER> (oai:get-tool "misc")
(("get-weather" . #<TOOL {700BC00283}>))
CL-USER> (oai:get-tool "get-weather")
#<TOOL {700BC00283}>
CL-USER> (oai:get-tool "misc" "get-weather")
#<TOOL {700BC00283}>
```

Calling the function of the tool from the registry is easy now:

```lisp
CL-USER> (oai:tool-call "get-weather"
                        "You retrieve the city from a string."
                        "What is the weather like in Alyssa P Hacker's city of Cambridge?")
"Par example, it's probably something like 18℃ in the 'ol town of Cambridge"
"call_l8JcbKlb98dk7cCZ3CfcnGP4"
#<OPENAI-SDK/RESPONSE::COMPLETION {700A6DC2A3}>
```

The three return values come in handy when applying the response in a thread, as the
completion response can be reused in new calls.

## Structured outputs and JSON Schemas

Comfort is limited here, however. The main cause is the way class slot types
function. Specifically, slot `:type`s are only evaluated compile-time to denote
return values of readers and to throw warnings when the wrong type is used in the
`:initform`, and what to do on runtime slot allocation with incorrect data types is
undefined by the spec. In other words, runtime datatype analysis to correctly convert
to JSON schema is a pain in the ass. To limit yakshaving, at the time of writing, I
impose three limitations on `defclass` as schema. This is subject to change when I
learn what limitations are problematic in practice.

### Limitations
1. Slot types MUST either be:
   1. schemas understood by [OpenAI's JSON Schema](https://platform.openai.com/docs/guides/structured-outputs#supported-schemas)
   2. class symbols
   3. list type as defined by the `oai/json:def-schema-list-type`-macro making
      available a `<class>-oai-list`-type
2. The `response-format`-object must have the `:strict`-option `t`, because I haven't
   adapted the schema creation yet to support the non-strict variant. See [Strict Mode documentation](https://platform.openai.com/docs/guides/function-calling#strict-mode) for more info.
3. Having `error` in an `initform` is not supported at the moment: the classes are
   instantiated before `slot-values` are assigned.

One solution is to extend the parser to support a `:initarg` flag, so that
`:initform (error "something")` is supported for the classes to parse objects by
class symbol, but I have had no reason yet to prioritize this. Anyway, the following
is the least bad implementation I could think of at the time of writing. If you have
ideas on how to make the internals require less of the user, please open an issue.

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

As you can see, you can manually map it back if you want. However, I've implemented a
convenience for quick one-twos.

### The convenience wrapper

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
CL-USER> (format t (com.inuoe.jzon:stringify * :pretty t))
{
  "steps": [
    {
      "explanation": "We start by isolating the term with x, which is 8x. To do this, subtract 7 from both sides of the equation.",
      "output": "8x + 7 - 7 = -23 - 7"
    },
    {
      "explanation": "After subtracting 7 from both sides, the equation simplifies. The +7 and -7 on the left side cancel out, leaving us with 8x.",
      "output": "8x = -30"
    },
    {
      "explanation": "Now, to solve for x, divide both sides by 8 to isolate x.",
      "output": "x = -30 / 8"
    },
    {
      "explanation": "Simplify the fraction -30/8 by dividing both the numerator and the denominator by the greatest common divisor, which is 2.",
      "output": "x = -15 / 4"
    }
  ],
  "final-answer": "x = -\\frac{15}{4}"
}
NIL
```

## Platforms API

### Models

#### List
```lisp
CL-USER> (oai:list-models)
#("gpt-4-0613" "gpt-4" "gpt-3.5-turbo" "o3-pro-2025-06-10" "codex-mini-latest"
  "o3-pro" "gpt-4o-realtime-preview-2025-06-03"
  "gpt-4o-audio-preview-2025-06-03" "davinci-002" "babbage-002"
  "gpt-3.5-turbo-instruct" "gpt-3.5-turbo-instruct-0914" "dall-e-3" "dall-e-2"
  "gpt-4-1106-preview" ...)
```

#### Search
```lisp
CL-USER> (oai:search-model "o3")
#("o3" "o3-2025-04-16" "o3-mini" "o3-mini-2025-01-31" "o3-pro"
  "o3-pro-2025-06-10")
```

#### Delete a fine-tuned model
```lisp
CL-USER> (oai:delete-model "ft:gpt-4o-mini:acemeco:suffix:abc123")
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
  - [x] Structured output/function calling/tool calling framework
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
