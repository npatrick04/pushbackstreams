;;;; package.lisp

(in-package :cl-user)

(defpackage :pushbackstreams
  (:use :common-lisp)
  (:export :pushbackstream
	   :pb-read-byte
	   :pb-read-bytes-until
	   :pb-read-sequence
	   :pb-unread-byte
	   :pb-write-chunks
	   :size-of-chunks
	   :chunk-vector
	   :chunk-vector-push
	   :chunk-vector-pop
	   :wrapped-stream
	   :pb-stream-position))

