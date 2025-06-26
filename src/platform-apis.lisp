(in-package #:cl-user)

(defpackage openai-sdk/platform-apis
  (:use #:cl #:openai-sdk/client)
  (:shadow :get :delete)
  (:import-from #:openai-sdk/request
                #:request)
  (:export #:list-models
           #:search-model
           #:delete-model))

(in-package #:openai-sdk/platform-apis)

(defvar *models* nil)

(defclass model ()
  ((id :type string :initarg :id :reader id)
   (object :type string :initarg :object)
   (created :type local-time:timestamp :initarg :created)
   (owned-by :type string :initarg :owned-by)))

(defun make-model (&key id object created owned-by)
  (make-instance 'model :id id
                        :object object
                        :created (local-time:unix-to-timestamp
                                  created)
                        :owned-by owned-by))

(defclass models-list ()
  ((object :type string :initarg :object)
   (data :type simple-vector :initarg :data :reader data)))

(defun make-models-list (&key object data)
  (make-instance 'models-list :object object
                              :data (map 'simple-vector (lambda (ht) (openai-sdk/util:objectify 'make-model ht))
                                         data)))

(defun get (path &optional body response-class (openai *openai*))
  (request :get (base-url openai) path
           (api-key openai)
           (remove-if #'null (list (car (default-headers openai))
                                   (when (organization-id openai)
                                     (cons "OpenAI-Organization"
                                           (organization-id openai)))
                                   (when (project-id openai)
                                     (cons "OpenAI-Project"
                                           (project-id openai)))))
           (max-retries openai)
           (connect-timeout openai)
           (read-timeout openai)
           (when body
             (com.inuoe.jzon:stringify body))
           (when response-class
             response-class)))

(defun list-models (&optional refresh (view-less t) (openai *openai*))
  "List all models from OpenAI

REFRESH: whether to force a request to the BASE-URL of the OPENAI-object

VIEW-LESS: Only get an array of model string names, else get the full model-object list.

OPENAI: The object to use."
  (let ((models-list (or (and (not refresh) *models*)
                         (setf *models* (get "models" nil 'make-models-list openai)))))
    (if view-less
        (map 'simple-vector #'id (data models-list))
        (data models-list))))

(defun search-model (substring)
  (sort (remove-if-not (lambda (id) (str:containsp substring id)) (list-models)) #'string<))

(defun delete (path &optional body response-class (openai *openai*))
  (request :delete (base-url openai) path
           (api-key openai)
           (remove-if #'null (list (car (default-headers openai))
                                   (when (organization-id openai)
                                     (cons "OpenAI-Organization"
                                           (organization-id openai)))
                                   (when (project-id openai)
                                     (cons "OpenAI-Project"
                                           (project-id openai)))))
           (max-retries openai)
           (connect-timeout openai)
           (read-timeout openai)
           (when body
             (com.inuoe.jzon:stringify body))
           (when response-class
             response-class)))

(defclass deleted-model ()
  ((id :type string :initarg :id :reader id)
   (object :type string :initarg :object)
   (deleted :type boolean :initarg :deleted)))

(defun make-deleted-model (&rest args &key id object deleted)
  (declare (ignore id object deleted))
  (apply #'make-instance args))

(defgeneric delete-model (model &optional openai))

(defmethod delete-model ((model model) &optional (openai *openai*))
  (delete-model (id model) openai))

(defmethod delete-model ((model-id string) &optional (openai *openai*))
  (delete (format nil "models/~a" model-id) nil 'make-deleted-model openai))
