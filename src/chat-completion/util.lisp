(in-package #:cl-user)

(uiop:define-package openai-sdk/chat-completion/util
  (:use #:cl)
  (:import-from #:openai-sdk/chat-completion/classes
                #:completion
                #:make-completion)
  (:import-from #:openai-sdk/interface
                #:load-completion
                #:save-completion))

(in-package #:openai-sdk/chat-completion/util)

(defmethod load-completion (path)
  (oai:objectify 'make-completion
                 (with-open-file (s path)
                   (com.inuoe.jzon:parse s))))

;; TODO: serialize local-time timestamps to unix timestamps
(defmethod save-completion ((completion completion) path)
  (with-open-file (s path :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (com.inuoe.jzon:stringify completion :stream s :pretty t))
  path)
