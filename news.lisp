(in-package :pacnews)

(defclass news ()
  ((title :initarg :title
          :reader news-title
          :type string)
   (content :initarg :content
            :reader news-content
            :type string)
   (date :initarg :date
         :reader news-date
         :type string)
   (readp :initarg :readp
          :reader news-readp
          :type bool))
  (:documentation
   "NEWS represents a single news item."))

(defgeneric serialize (news &optional stream)
  (:documentation
   "Serialize is for serializing data objects into the given stream.
Note that `STREAM' must be an valid opened stream."))

(defgeneric deserialize (stream)
  (:documentation
   "deserialize Read value back to object from given stream.
Note that `stream' must be an valid opened stream."))

(defmethod serialize ((news news) &optional (stream *standard-output*))
  (print (list (news-date news) (news-title news) (news-content news)) stream))


(defmethod deserialize (stream)
  (let ((list (read stream)))
    (make-instance 'news :date (first list)
                   :title (second list)
                   :content (last-elt list))))

