;;; Absolutely brain dead tests. Does not prove anything aside from
;;; code not throwing exceptions

(defun test-wait-for-reader ()
  (let ((c (make-instance 'channel)))
    (bt:make-thread (lambda () (send c :hello-from-channel)) :name "send thread")
    (sleep 2)
    (recv c)))

(defun test-wait-for-writer ()
  (let ((c (make-instance 'channel)))
    (bt:make-thread (lambda () (recv c))
                    :name "recv thread")
    (sleep 2)
    (send c :hello-from-channel)))

(defun test-reply-with-recv ()
  (let ((c (make-instance 'channel)))
    (bt:make-thread (lambda ()
                      (let ((r (recv c)))
                        (send c r)))
                    :name "recv thread")
    (send c :hello-from-channel)
    (recv c)))
