(in-package #:cl-user)

(uiop:define-package openai-sdk/chat-completion/request
  (:use #:cl #:openai-sdk/core #:openai-sdk/interface)
  (:shadowing-import-from #:openai-sdk/interface #:stream #:type #:function #:format)
  (:import-from #:openai-sdk/client #:*client*)
  (:import-from #:openai-sdk/chat-completion/classes
                #:chat-completion
                #:completion-message
                #:make-completion
                #:completion
                #:make-chunked-completion
                #:choice)
  (:import-from #:openai-sdk/chat-completion/conditions #:more-than-one)
  (:import-from #:openai-sdk/util #:objectify))

(in-package #:openai-sdk/chat-completion/request)

(defun %merge-chunks (chunks)
  (let (max-choices)
    (dolist (chunk chunks)
      (dotimes (i (length (choices chunk)))
        (push (index (aref (choices chunk) i)) max-choices)))
    (setf max-choices (remove-duplicates max-choices))
    (let (;; TODO: implement
          ;; (tool-calls (make-array))
          ;; (function-call (oai/cc:make-function-call))
          (messages (make-array (length max-choices)
                                :initial-contents
                                (loop for i from 0 below (length max-choices)
                                      collect (make-instance 'completion-message
                                                             :role nil
                                                             :content nil))))
          (converted-choices (make-array (length max-choices) :initial-contents
                                         (loop for i from 0 below (length max-choices)
                                               collect (make-instance 'choice :finish-reason nil
                                                                              :index i
                                                                              :logprobs nil
                                                                              :message nil))))
          ids
          roles
          models
          objects
          service-tiers
          system-fingerprints
          ;; TODO: usages
          )
      ;; set new choice index
      (dotimes (i (length messages))
        (setf (index (aref converted-choices i)) i))
      ;; Fill new choice with chunk-choice deltas
      (loop for completion in chunks
            do (loop for choice across (choices completion)
                     do (let ((current-message (aref messages (index choice)))
                              (current-cchoice (aref converted-choices (index choice)))
                              ;; choice-delta values
                              (role (role (delta choice)))
                              (content (content (delta choice)))
                              (refusal (refusal (delta choice)))
                              ;; (function-call (function-call (delta choice)))
                              ;; (tool-calls (tool-calls (delta choice)))
                              (logprobs (logprobs choice))
                              (finish-reason (finish-reason choice)))
                          (unless (str:emptyp content)
                            (push content (content current-message)))
                          (when (and (stringp refusal) (not (str:emptyp refusal)))
                            (setf (refusal current-message) refusal))
                          (when (and (stringp role) (not (str:emptyp role)))
                            (push role roles))
                          (unless (or (null logprobs) (eq 'null logprobs) (= 0 (length logprobs)))
                            (setf (logprobs current-cchoice) logprobs))
                          (unless (or (null finish-reason) (eq 'null finish-reason) (str:emptyp finish-reason))
                            (setf (finish-reason current-cchoice) finish-reason))))
               (push (id completion) ids)
               (push (model completion) models)
               (push (object completion) objects)
               (push (service-tier completion) service-tiers)
               (push (system-fingerprint completion) system-fingerprints))
      (dotimes (i (length messages))
        ;; Fix content list to string
        (setf (content (aref messages i))
              (apply #'str:concat (content (aref messages i))))
        ;; Put message under choice
        (setf (message (aref converted-choices i))
              (aref messages i)))
      (macrolet ((select-unique-string (item-name)
                   `(let* ((filtered (and (remove-duplicates (remove-if (lambda (el) (or (null el)
                                                                                         (eq 'null el)))
                                                                        ,(intern (cl:format nil "~AS" item-name)))
                                                             :test #'equal))))
                      (restart-case (if (< 1 (length filtered))
                                        (error 'more-than-one :what filtered)
                                        (car filtered))
                        (select-first-item ()
                          :report ,(cl:format nil "Select first ~A" item-name)
                          (car filtered))
                        (set-item-name ()
                          :report ,(cl:format nil "Set new ~a" item-name)
                          :interactive (lambda ()
                                         (cl:format *query-io* ,(cl:format nil "Please enter a new ~a" item-name))
                                         (force-output *query-io*)
                                         (cl:format nil "~(~A~)" (read *query-io*))))))))
        (let ((id (select-unique-string id))
              (role (select-unique-string role))
              (model (select-unique-string model))
              (object (select-unique-string object))
              (service-tier (select-unique-string service-tier))
              (system-fingerprint (select-unique-string system-fingerprint)))
          ;; Set role
          (dotimes (i (length messages))
            (setf (role (aref messages i)) role))
          ;; Return completion
          (make-instance 'completion
                         :id id
                         :choices converted-choices
                         :created (created (car chunks))
                         :model model
                         :object (str:substring 0 (position #\. object :from-end t) object)
                         :service-tier service-tier
                         :system-fingerprint system-fingerprint
                         :usage nil     ;TODO: usage
                         ))))))

(defun %read-line-until-newline (stream)
  (let (chars)
    (loop for ch = (read-char stream nil nil)
          while (and ch (not (char= ch #\Newline)))
          do (push ch chars))
    (coerce (nreverse chars) 'string)))

(defun %parse-chunks-from-stream (stream &key output func (close-stream-p t))
  "Takes STREAM as OpenAI body stream and returns the list of parsed chunk objects of
type `oai/cc:completion' in reversed order of parsing.

CLOSE-STREAM-P determines whether to close the stream at the end of parsing (default: t).

OUTPUT determines where to output the message delta to.
FUNC takes one argument, a single parsed chunk object of type `oai/cc:completion'."
  (unwind-protect
       (let ((pattern "data: ")
             (pattern-length (length "data: "))
             (end-pattern "[DONE]")
             results)
         (loop with match-pos = 0
               for ch = (read-char stream nil nil)
               while ch
               do
                  (when (char= ch #\Newline)
                    (setf match-pos 0))
                  (if (char= ch (aref pattern match-pos))
                      (incf match-pos)
                      (setf match-pos (if (char= ch (aref pattern 0)) 1 0)))
                  (when (= match-pos pattern-length)
                    (let ((stream-pattern (%read-line-until-newline stream)))
                      (when (string= stream-pattern end-pattern)
                        (return-from %parse-chunks-from-stream results))
                      (let ((obj (objectify 'make-chunked-completion
                                            (com.inuoe.jzon:parse
                                             stream-pattern))))
                        (push obj results)
                        ;; Call user-defined function on chunk:
                        (when func
                          (funcall func obj))
                        ;; Output chunk-delta to user-defined stream:
                        (let ((p (position 0 (choices obj) :key #'index)))
                          (when p
                            (cl:format output "~A"
                                       (or (content (delta (aref (choices obj) p))) ""))))))
                    (setf match-pos 0))))
    (when close-stream-p
      (close stream))))

(defmethod send ((chat-completion chat-completion) &optional (client *client*))
  (unless (slot-boundp chat-completion 'model)
    (setf (model chat-completion) (default-model client)))
  (let* ((stream-p (and (slot-boundp chat-completion 'stream)
                        (not (or (null (stream chat-completion))
                                 (eq 'null (stream chat-completion))))))
         (stream-handler (when stream-p (stream chat-completion)))
         (body-stream-wrapper (if stream-p

                                  ;; Stream body handler
                                  (lambda (body-stream)
                                    (%merge-chunks
                                     (apply #'%parse-chunks-from-stream body-stream
                                            (etypecase stream-handler
                                              (cl:function (list :func stream-handler))
                                              (cl:stream (list :output stream-handler))
                                              (cons stream-handler)
                                              ((eql t) (list :output *standard-output*))))))

                                  ;; Default body handler
                                  (lambda (body-stream)
                                    (objectify 'make-completion
                                               (com.inuoe.jzon:parse body-stream))))))
    (when stream-p
      (setf (stream chat-completion) t))

    (multiple-value-prog1
        (request :post (base-url client) "chat/completions"
                 (api-key client)
                 (remove-if #'null (list (car (default-headers client))
                                         (when (organization-id client)
                                           (cons "OpenAI-Organization"
                                                 (organization-id client)))
                                         (when (project-id client)
                                           (cons "OpenAI-Project"
                                                 (project-id client)))))
                 (max-retries client)
                 (connect-timeout client)
                 (read-timeout client)
                 (com.inuoe.jzon:stringify chat-completion)
                 body-stream-wrapper)
      (when stream-p
        (setf (stream chat-completion) stream-handler)))))
