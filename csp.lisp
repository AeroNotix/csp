(defpackage :csp
  (:use :cl :bt))
(in-package :csp)


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
   (queue
    :initform nil
    :accessor queue)))


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
  (let ((when-receiver-available-notify (make-condition-variable)))
    (with-slots (lock channel-readers wake-channel-writer queue) c
      (with-recursive-lock-held (lock)
        (with-send-offer c when-receiver-available-notify
          (loop while (null channel-readers)
             if block-p
             do (bt:condition-wait when-receiver-available-notify lock)
             else
             do (return-from send nil))
          (let ((reader (random-removef channel-readers)))
            ;; genericize this
            (push value queue)
            (condition-notify reader)
            (condition-wait when-receiver-available-notify lock)))))))

(defmethod recv ((c channel) &key (block-p t))
  (let ((when-sender-available-notify (make-condition-variable)))
    (with-slots (lock channel-writers wake-channel-reader queue) c
      (with-recursive-lock-held (lock)
        (with-recv-offer c when-sender-available-notify
          (loop while (null channel-writers)
             if block-p
             do (bt:condition-wait when-sender-available-notify lock)
             else do (return-from recv nil))
          (let ((writer (random-removef channel-writers))
                ;; genericize this
                (return-value (pop queue)))
            (condition-notify writer)
            return-value))))))
