(in-package #:cl-user)

(uiop:define-package openai-sdk/platform-apis/models/all
  (:use #:cl #:openai-sdk/core)
  (:nicknames #:openai-sdk/platform-apis/models
              #:oai-models
              #:oai/pa/models)
  (:shadow #:list #:search #:delete)
  (:use-reexport #:openai-sdk/platform-apis/models/classes)
  (:import-from #:openai-sdk/client :*client*)
  (:export #:*models* #:list #:search #:delete))

(in-package #:openai-sdk/platform-apis/models)

(defvar *models* nil)

(defun %get (path &optional body response-class (client *client*))
  (request :get (oai:base-url client) path
           (oai:api-key client)
           (remove-if #'null `(,(car (oai:default-headers client))
                               ,(when (oai:organization-id client)
                                  (cons "OpenAI-Organization"
                                        (oai:organization-id client)))
                               ,(when (oai:project-id client)
                                  (cons "OpenAI-Project"
                                        (oai:project-id client)))))
           (oai:max-retries client)
           (oai:connect-timeout client)
           (oai:read-timeout client)
           (when body
             (com.inuoe.jzon:stringify body))
           (when response-class
             response-class)))

(defun list (&optional refresh (view-less t) (client *client*))
  "List all models from OpenAI

REFRESH: whether to force a request to the BASE-URL of the OPENAI-object

VIEW-LESS: Only get an array of model string names, else get the full model-object list.

CLIENT: The object to use."
  (let ((models-list (or (and (not refresh) *models*)
                         (setf *models* (%get "models" nil 'make-models-list client)))))
    (if view-less
        (map 'simple-vector #'oai:id (oai:data models-list))
        (oai:data models-list))))

(defun search (substring)
  (sort (remove-if-not (lambda (id) (str:containsp substring id)) (list)) #'string<))

(defun delete (path &optional body response-class (client *client*))
  (request :delete (oai:base-url client) path
           (oai:api-key client)
           (remove-if #'null `(,(car (oai:default-headers client))
                               ,(when (oai:organization-id client)
                                  (cons "OpenAI-Organization"
                                        (oai:organization-id client)))
                               ,(when (oai:project-id client)
                                  (cons "OpenAI-Project"
                                        (oai:project-id client)))))
           (oai:max-retries client)
           (oai:connect-timeout client)
           (oai:read-timeout client)
           (when body
             (com.inuoe.jzon:stringify body))
           (when response-class
             response-class)))


(defgeneric delete-model (model &optional openai))

(defmethod delete-model ((model model) &optional (client *client*))
  (delete (oai:id model) client))

(defmethod delete-model ((model-id string) &optional (client *client*))
  (delete (format nil "models/~a" model-id) nil 'make-model client))
