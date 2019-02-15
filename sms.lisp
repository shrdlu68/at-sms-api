(defpackage :at-api
  (:use :cl)
  (:nicknames "at"))

(in-package :at-api)

(ql:quickload :drakma)
(ql:quickload :xmls)

(defparameter *username* "")
(defparameter *api-key* "")

(defun set-default-creds (username api-key)
  (setf *username* username
	*api-key* api-key))

(defun format-url-encode (stream arg &optional colon at-sign)
  (declare (ignorable colon at-sign))
  (format stream "~A" (drakma:url-encode (format nil "~A" arg) drakma:*drakma-default-external-format*)))

(defun send-sms (&key (api-key *api-key*) (username *username*)
		   (accept "application/xml") dest msg from enqueue bulksmsmode
		   keyword linkid retry-duration)
  "<dest> can either be a single string or a list of strings"
  (multiple-value-bind (body status-code headers)
      (drakma:http-request "https://api.africastalking.com/version1/messaging"
			   :method :POST
			   :content (format nil
					    "username=~/at-api::format-url-encode/~
	                                    &to=~:[~/at-api::format-url-encode/~;~{~/at-api::format-url-encode/~^,~}~]~
        	                            &message=~/at-api::format-url-encode/~
                	                    ~@[&from=~/at-api::format-url-encode/~]~
                        	            ~@[&enqueue=~/at-api::format-url-encode/~]~
					    ~@[&bulkSMSMode=~/at-api::format-url-encode/~]~
					    ~@[&keyword=~/at-api::format-url-encode/~]~
					    ~@[&linkid=~/at-api::format-url-encode/~]~
				    	    ~@[&retryDurationInHours=~/at-api::format-url-encode/~]" 
					    username (listp dest) dest msg from enqueue
					    bulksmsmode keyword linkid retry-duration)
			   :accept accept
			   :additional-headers (pairlis (list "Apikey")
							(list api-key)))
    (cond ((and (<= 200 status-code 300)
		(search "text/xml" (cdr (assoc :content-type headers))))
	   (values status-code (xmls:parse-to-list body)))
	  (t
	   (values status-code body)))))

(export '(set-default-creds send-sms) :at-api)
