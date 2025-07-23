(in-package #:cl-user)

(uiop:define-package openai-sdk/core
  (:use #:cl #:openai-sdk/conditions)
  (:nicknames #:oai/core)
  (:export #:request))

(in-package #:openai-sdk/core)

(defun request (method base-url path bearer-auth headers max-retries connect-timeout read-timeout content &optional body-stream-wrapper)
  (multiple-value-bind (body code response-headers uri stream)
      (let ((retry-request (dex:retry-request max-retries :interval 2)))
        (handler-bind ((dex:http-request-internal-server-error retry-request)
                       (dex:http-request-not-implemented retry-request)
                       (dex:http-request-service-unavailable retry-request)
                       (dex:http-request-bad-gateway retry-request)
                       (dex:http-request-bad-request
                         (lambda (condition)
                           (apply #'error 'openai-sdk-bad-request
                                  (loop with ht = (gethash "error" (com.inuoe.jzon:parse (dex:response-body condition)))
                                        for key being the hash-keys of ht
                                        for value = (gethash key ht)
                                        nconc (list (symbol-munger:underscores->keyword key)
                                                    value))))))
          (dex:request (concatenate 'string base-url path)
                       :method method
                       :bearer-auth bearer-auth
                       :headers headers
                       :connect-timeout connect-timeout
                       :read-timeout read-timeout
                       :content content
                       :want-stream t)))
    (unwind-protect
         (values (if body-stream-wrapper (funcall body-stream-wrapper body) body)
                 code response-headers uri stream)
      (close body))))
