(in-package :pushbackstreams)

;
; Kolla även http://www.sbcl.org/manual/Output-prefixing-character-stream.html#Output-prefixing-character-stream
;

(defclass pushbackstream ()
  ((pushback-buffer 
    :initform (make-array 10 :fill-pointer 0)
    :accessor pushback-buffer)
   (wrapped-stream
    :initarg :stream
    :initform (error "Must supply a stream to wrap.")
    :accessor wrapped-stream)))

(defclass chunk-vector ()
  ((chunk-size
    :initarg :chunk-size
    :initform 8196
    :accessor chunk-size)
   (chunks
    :initform  (make-array 0 :fill-pointer 0 :adjustable t)
    :accessor chunks)))

(defmethod initialize-instance :after ((vec chunk-vector) &key)
  (with-slots (chunk-size chunks) vec
    (vector-push-extend (make-array chunk-size :fill-pointer 0) chunks)))

(defun pb-stream-position (pb-stream)
  (- (file-position (wrapped-stream pb-stream))
     (fill-pointer (pushback-buffer pb-stream))))

(defun chunk-vector-push (el vec)
  (with-slots (chunk-size chunks) vec
    (let ((current-chunk (elt chunks (- (fill-pointer chunks) 1))))
      (if (= (fill-pointer current-chunk) (array-dimension current-chunk 0))
	  (progn
	    (setf current-chunk (make-array chunk-size :fill-pointer 0))
	    (vector-push-extend current-chunk chunks)))
      (vector-push-extend el current-chunk))))

(defun chunk-vector-pop (vec)
  (with-slots (chunks) vec
    (let* ((current-chunk (elt chunks (- (fill-pointer chunks) 1)))
	   (el (vector-pop current-chunk)))
      (if (and
	   (= (fill-pointer current-chunk) 0)
	   (> (fill-pointer chunks) 1))
	  (vector-pop chunks))
      el)))

(defun size-of-chunks (vec)
  (with-slots (chunks) vec
    (loop for c across chunks summing (length c))))

(defun pb-read-byte (str)
  (with-slots (pushback-buffer wrapped-stream) str
    (cond 
      ((> (fill-pointer pushback-buffer) 0) (vector-pop pushback-buffer))
      (t (read-byte wrapped-stream)))))

(defun pb-unread-byte (b str)
  (with-slots (pushback-buffer) str
    ;(format t "pb-unread-byte: ~2,'0X~%" b)
    (vector-push b pushback-buffer)))

(defun pb-read-sequence (seq str)
  (with-slots (wrapped-stream) str
    (read-sequence seq wrapped-stream)))

(defun pb-peek-char (str)
  (with-slots (pushback-buffer wrapped-stream) str
    (cond 
      ((> (fill-pointer pushback-buffer) 0) 
       (aref pushback-buffer (- (fill-pointer pushback-buffer) -1)))
      (t (peek-char wrapped-stream)))))

(defun pb-read-bytes-until (str terminator &key (chunk-size 8192))
  (let ((terminator-value
	 (loop with val = 0
	      for b in terminator
	      do (setf val (logior (ash val 8) b))
	      finally (return val)))
	(terminator-mask (- (ash 1 (* (length terminator) 8)) 1))
	(mark 0)
	(chunks (make-instance 'chunk-vector :chunk-size chunk-size)))
    (loop 
       while (/= mark terminator-value)
       do (let ((b (pb-read-byte str)))
	    (setf mark (logand terminator-mask (logior (ash mark 8) b)))
	    (chunk-vector-push b chunks))
       finally
	   (loop repeat (length terminator)
	      do (pb-unread-byte (chunk-vector-pop chunks) str)))
    chunks))

(defun pb-write-chunks (vec str)
  (with-slots (chunks) vec
    (loop for chunk across chunks do
	 (loop for b across chunk do
	      (write-byte b str)))))
