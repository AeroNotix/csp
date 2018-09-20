;;; Absolutely brain dead tests. Does not prove anything aside from
;;; code not throwing exceptions

(in-package :csp)

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

(defun multiple-receivers-multiple-senders ()
  (let ((c (make-instance 'channel)))
    (bt:make-thread (lambda ()
                        (send c :hello-thread1))
                    :name "send thread1")
    (bt:make-thread (lambda ()
                      (send c :hello-thread2))
                    :name "send thread2")
    (list (recv c) (recv c))))


;; (test-wait-for-reader)
;; (test-wait-for-writer)
;; (test-reply-with-recv)
;; (multiple-receivers-multiple-senders)
