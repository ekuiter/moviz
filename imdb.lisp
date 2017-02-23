(in-package :imdb)

(defun back-char (stream)
  "Undo a read-char operation."
  (let ((pos (file-position stream)))
    (when (> pos 0)
      (file-position stream (- pos 1))
      (peek-char nil stream))))

(defun back-line (stream)
  "Undo a read-line operation (jumps to begin of line)."
  (dotimes (i 2)
    (do ((ch (back-char stream) (back-char stream)))
	((char= ch #\Newline))))
  (read-char stream nil))

(defmacro with-open-list (file-name &body body)
  "Creates a stream for a list file."
  `(with-open-file (stream (file-name ,file-name) :external-format :iso-8859-1) ,@body))

(defmacro define-lazy-slot (class slot documentation &body body)
  "Defines an accessor that only computes its value only once."
  `(defmethod ,slot ((instance ,class))
     ,documentation
     (with-slots (file-name ,slot) instance
       (or ,slot (setf ,slot (with-open-list instance ,@body))))))

(defmacro do-lines (stream var-init-steps end-test &body body)
  "Iterates over a file line by line."
  `(do ((line (read-line ,stream nil) (read-line ,stream nil)) ,@var-init-steps)
       (,end-test)
     ,@body))

(defclass imdb-list ()
  ((file-name :initarg :file-name
	      :initform (error "Must supply file name")
	      :reader file-name)
   (file-length :initform nil)
   (data-start :initform nil)
   (data-end :initform nil)
   (notice-shown :initform nil
		 :allocation :class)))

(defmethod initialize-instance :after ((list imdb-list) &key)
  "Initializes a list file."
  (unless (probe-file (file-name list))
    (error "list file not found. check (ccl:current-directory)")))

(defmethod show-notice ((list imdb-list))
  (unless (slot-value list 'notice-shown)
    (format t "Information courtesy of IMDb (http://www.imdb.com). Used with permission.~%")
    (setf (slot-value list 'notice-shown) t)))

(define-lazy-slot imdb-list file-length
    "Returns the file length."
  (cl:file-length stream))

(defgeneric data-start (list)
  (:documentation "Returns the offset of the first record."))

(defgeneric data-end (list)
  (:documentation "Returns the offset after the last record."))

(defmethod data-length ((list imdb-list))
  "Returns the data length."
  (- (data-end list) (data-start list)))

(defmethod end-of-data-p ((list imdb-list) pos)
  "Returns whether the given offset is after the last record."
  (> pos (data-end list)))