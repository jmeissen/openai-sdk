;; Heavily inspired by https://github.com/karthink/gptel

(in-package #:cl-user)

(defpackage openai-sdk/tool-call
  (:use #:cl)
  (:nicknames :oai/tool)
  (:import-from #:openai-sdk/request #:create-chat-completion)
  (:import-from #:openai-sdk/chat-completion
                #:make-chat-completion
                #:system-message
                #:make-system-message)
  (:import-from #:openai-sdk/response
                #:choices
                #:message
                #:tool-calls)
  (:import-from #:openai-sdk/client #:*openai*)
  (:export #:make-tool
           #:get-tool
           #:parse-tools
           #:tool
           #:tool-call))

(in-package #:openai-sdk/tool-call)

(defvar *registry* nil
  "Alist of tools arranged by category.

A \"tool\" is a function spec (definition and description). Each tool is assigned a
category when it is created, with a category of \"misc\" if none is specified.

This is a two-level alist mapping categories and tool names to the tool itself. It is
used as a global register of available tools.

Example:

   CATEGORY         TOOL NAME          TOOL
 ((\"filesystem\" . ((\"read_file\"      . cat-tool)
                   (\"list_directory\" . ls-tool)))
  (\"emacs\"      . ((\"read_buffer\"    . buffer-substring-tool)
                   (\"send_message\"   . message-tool))))

This variable is for internal use only, to define a tool use
`make-tool'.")

(defclass tool ()
  ((function :documentation "Function that runs the tool"
             :type function
             :initarg :function
             :accessor tool-function)
   (name :documentation "Tool name"
         :type string
         :initarg :name
         :accessor tool-name)
   (description :documentation "What the tool does, intended for the LLM"
                :type string
                :initarg :description
                :accessor tool-description
                :initform "")
   (args :documentation "List of plists specifying function arguments"
         :type list
         :accessor tool-args
         :initarg :args)
   (strict :documentation "Whether to enable strict schema adherence. If set to true,
the model will follow the exact schema defined in the parameters field. Only a subset
of JSON Schema is supported when strict is true."
           :type boolean
           :initarg :strict
           :accessor tool-strict)
   (category :documentation "Use to group tools by purpose"
             :initarg :category
             :type string
             :accessor tool-category))
  (:documentation "Struct to specify tools for LLMs to run.

A tool is a function specification sent to the LLM along with
a (plain language) task. If the LLM decides to use the tool to
accomplish the task, the tool will run.

You can add tools via `make-tool'."))

(defun %make-tool (&rest spec &key function name description args category strict)
  (declare (ignore function name description args category strict))
  (apply #'make-instance 'tool spec))

(defun map-nested-elt (alist keys &key (test #'string=))
  (let ((key-list (if (listp keys) keys (list keys))))
    (reduce (lambda (current key)
              (when current
                (cdr (assoc key current :test test))))
            key-list
            :initial-value alist)))

(defun get-tool (&rest path)
  "Get tool from registry at PATH.

PATH must be
- string: \"category-name\"; or \"function-name\"),
- multiple strings: (get-tool \"category-name\" \"function-name\")
"
  (if (= 1 (length path))
      (uiop:if-let ((category (assoc (car path) *registry* :test #'equal)))
        (cdr category)
        (loop for (_ . tools) in *registry*
              when (cdr (assoc (car path) tools :test #'equal))
                return it))
      (map-nested-elt *registry* path)))

(defun add-tool-to-registry (category command tool)
  (let ((category-entry (assoc category *registry* :test #'equal)))
    (if category-entry
        (uiop:if-let (command-entry (assoc command (cdr category-entry) :test #'equal))
          (setf (cdr command-entry) tool)
          (push (cons command tool) (cdr category-entry)))
        (push (cons category (list (cons command tool))) *registry*))))

(defun make-tool (&rest slots &key function name description args strict category)
  "Make a tool for LLM use.

The following keyword arguments are available, of which the first
four are required.

NAME: The name of the tool, can be a symbol, keyword, or string. Also used in
get-tool.

FUNCTION: The function itself (lambda or symbol) that runs the tool.

DESCRIPTION: A verbose description of what the tool does, how to
call it and what it returns.

ARGS: `com.inuoe.jzon:stringify'-able JSON Schema
- Can be conveniently generated with funcs from `openai-sdk/tool-schema'-package.

STRICT: Set to `t' when requiring strict adherence to the schema.

Optional:


CATEGORY: A string indicating a category for the tool.  This is
used only for grouping in gptel's UI.  Defaults to \"misc\".

Example:

(oai:make-tool :function (lambda (city) (declare (ignore city)) '(fake-temp 18 degrees celsius))
               :name \"get-weather\"
               :description \"Get current temperature for provided coordinates in celsius\"
               :args (oai/tsg:type-object #(\"city\")
                                          :properties (list \"city\" (oai:type-string))
                                          :additional-properties nil)
               :strict t)

"
  (declare (ignore function name description args strict category))
  (let* ((tool (apply #'%make-tool slots))
         (category (if (slot-boundp tool 'category)
                       (tool-category tool)
                       (setf (tool-category tool) "misc"))))
    (add-tool-to-registry category (tool-name tool) tool)
    tool))

(defmethod openai-sdk/core:%morph ((tool tool))
  (openai-sdk/chat-completion:make-tool
   (apply #'openai-sdk/chat-completion:make-function (tool-name tool)
          (nconc
           (when (slot-boundp tool 'description)
             `(:description ,(tool-description tool)))
           (when (slot-boundp tool 'strict)
             `(:strict ,(tool-strict tool)))
           (when (slot-boundp tool 'args)
             `(:parameters ,(tool-args tool)))))))

(defun registered-tools (&optional (registry *registry*))
  (mapcan (lambda (item)
            (mapcar #'cdr (cdr item)))
          registry))

(defun parse-tools ()
  "Parse TOOLS and return a list of prompts.

TOOLS is a list of `tool' structs."
  (mapcar #'openai-sdk/core:%morph (registered-tools)))

(defmethod openai-sdk/request:create-chat-completion :around
    ((chat-completion openai-sdk/chat-completion:chat-completion) &optional (openai *openai*))
  (declare (ignore openai))
  ;; When tools are set with the `get-tool` command
  (alexandria:when-let (tools (and (slot-boundp chat-completion 'openai-sdk/chat-completion:tools)
                                   (openai-sdk/chat-completion:tools chat-completion)))
    (unless (every (lambda (tool)
                     (typep tool 'openai-sdk/chat-completion:tool))
                   tools)
      (loop while tools do
        (etypecase
            (car tools)
          (string (rplaca tools (openai-sdk/core:%morph (get-tool (car tools)))))
          (tool (rplaca tools (openai-sdk/core:%morph (car tools))))
          (openai-sdk/chat-completion:tool nil))
        (setq tools (cdr tools)))))
  (call-next-method))

(defmethod %extract-argument-names ((tool tool))
  (mapcar (lambda (el) (format nil "~(~a~)" el))
          (cadr (function-lambda-expression (tool-function tool)))))

(defun %response-argument-values-list (argument-names response-arguments)
  (loop for el in argument-names
        collect (cdr (assoc el response-arguments :test #'equal))))

(defmethod tool-call ((tool-path list) (system-message string) &optional messages)
  (tool-call (apply #'get-tool tool-path) system-message messages))

(defmethod tool-call ((tool-path string) (system-message string) &optional messages)
  (tool-call (get-tool tool-path) system-message messages))

(defmethod tool-call ((tool tool) (system-message string) &optional messages)
  (let* (chat-completion-response
         tool-call-response
         (response-arguments (alexandria:hash-table-alist
                              (com.inuoe.jzon:parse
                               (openai-sdk/response:arguments
                                (openai-sdk/response:function
                                 (setf tool-call-response
                                       (aref (tool-calls
                                              (message
                                               (aref (choices (setf chat-completion-response
                                                                    (create-chat-completion
                                                                     (make-chat-completion
                                                                      (cons (make-system-message system-message)
                                                                            (etypecase messages
                                                                              (string (list messages))
                                                                              (openai-sdk/chat-completion:user-message (list messages))
                                                                              (list messages)))
                                                                      :tools (list tool)
                                                                      :tool-choice (openai-sdk/chat-completion:make-tool-choice
                                                                                    (tool-name tool))))))
                                                     0)))
                                             0))))))))
    (values (apply (tool-function tool)
                   (%response-argument-values-list (%extract-argument-names tool)
                                                   response-arguments))
            (openai-sdk/core:id tool-call-response)
            chat-completion-response)))
