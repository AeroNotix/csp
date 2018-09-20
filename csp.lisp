(defpackage :csp
  (:use :cl :stmx)
  (:export #:send
           #:recv
           #:select))
(in-package :csp)


(transactional
 (defclass channel ()
   ((lock
     :initform (bt:make-lock)
     :accessor qlock)
    (channel-readers
     :initform nil
     :accessor channel-readers)
    (channel-writers
     :initform nil
     :accessor channel-writers)
    (channel-reader-selectors
     :initform nil
     :accessor channel-reader-selectors)
    (queue
     :initform nil
     :accessor queue))))


(defclass condition-variable-wrapper ()
  ((condvar
    :initform (bt:make-condition-variable)
    :accessor condvar)))

(defun make-condvar ()
  (make-instance 'condition-variable-wrapper))

(defmethod condvar-wait ((cv condition-variable-wrapper) lock)
  (bt:condition-wait (condvar cv) lock))

(defmethod condvar-notify ((cv condition-variable-wrapper))
  (bt:condition-notify (condvar cv)))

(defclass semaphore-wrapper ()
  ((semaphore
    :initform (bt:make-semaphore)
    :accessor semaphore)))

(defun make-semaphore ()
  (make-instance 'semaphore-wrapper))

(defgeneric notify-waiter (waiter))

(defmethod notify-waiter ((w condition-variable-wrapper))
  (condvar-notify w))

(defmethod notify-waiter ((w semaphore-wrapper))
  (bt:signal-semaphore (semaphore w)))

(defmacro random-removef (place)
  `(let ((elt (alexandria:random-elt ,place)))
     (alexandria:removef ,place elt)
     elt))

(macrolet ((define-channel-state-macro (name place)
             `(defmacro ,name (channel condition-variable &body body)
                `(unwind-protect
                      (progn (push ,condition-variable (,',place ,channel)) ,@body)
                   (remove ,condition-variable (,',place ,channel))))))
  (define-channel-state-macro with-send-offer channel-writers)
  (define-channel-state-macro with-recv-offer channel-readers))

(defmethod send ((c channel) value &key (block-p t))
  (let ((when-receiver-available-notify (make-condvar)))
    (with-slots (lock channel-readers queue) c
      (bt:with-recursive-lock-held (lock)
        (with-send-offer c when-receiver-available-notify
          (loop while (null channel-readers)
             if block-p
             do (condvar-wait when-receiver-available-notify lock)
             else
             do (return-from send nil))
          (let ((reader (random-removef channel-readers)))
            ;; genericize this
            (push value queue)
            (condvar-notify reader)
            (condvar-wait when-receiver-available-notify lock)
            (condvar-notify reader)
            t))))))

(defmethod recv ((c channel) &key (block-p t))
  (let ((when-sender-available-notify (make-condvar)))
    (with-slots (lock channel-writers queue) c
      (bt:with-recursive-lock-held (lock)
        (with-recv-offer c when-sender-available-notify
          (loop while (null channel-writers)
             if block-p
             do (condvar-wait when-sender-available-notify lock)
             else
             do (return-from recv nil))
          (let ((writer (random-removef channel-writers)))
            (condvar-notify writer)
            (condvar-wait when-sender-available-notify lock)
            (let ((return-value (pop queue)))
              (condvar-notify writer)
              return-value)))))))

(macrolet ((define-channel-insertion-defun (name place)
             `(defun ,name (&rest channels)
                (let ((sem (make-semaphore)))
                  (atomic
                   (loop for channel in channels
                      do
                        (push sem (,place channel))))))))
  (define-channel-insertion-defun add-as-channel-writer channel-writers)
  (define-channel-insertion-defun add-as-channel-reader channel-readers))

;; (defun channels-from-clauses (clauses)
;;   (mapcar #'second clauses))

;; (defmacro select (clauses)
;;   (let ((channels (channels-from-clauses clauses)))
;;     `(progn ,@channels)))
