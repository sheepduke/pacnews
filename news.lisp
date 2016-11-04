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
          :accessor news-readp
          :type bool))
  (:documentation
   "NEWS represents a single news item."))


(defun load-all-news (path)
  "Read all the news from given PATH, which represents a valid path to
existing file.
A list of news is returned as the result; if there is no news inside
the given PATH, NIL is returned."
  (with-open-file (stream path
                          :direction :input
                          :if-does-not-exist :create)
    (let ((news-list (make-array 100 :adjustable t :fill-pointer 0)))
      (loop for arguments = (read stream nil nil)
         for news = (apply 'make-instance 'news arguments)
         while arguments
         do (vector-push-extend news news-list))
      news-list)))


(defun dump-all-news (path news-list)
  "Dump all the news specified by NEWS-LIST into corresponding PATH.
Existing file of given PATH will be overwritten."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    (loop for news across news-list
       do (print news stream))))


(defun print-news-list (news-list &key (stream *standard-output*) (type :unread))
  "Print given list of news NEWS-LIST with index etc."
  (loop for news across news-list
     for index from 1
     when (or (equal type :all)
              (and (equal type :unread)
                   (not (news-readp news))))
     do (format stream "~&~6A~A~&"
                (format nil "[~A]" index)
                (print-news-title news :stream nil))))


(defun print-news-title (news &key (stream *standard-output*))
  "Print the news in a more friendly form."
  (format stream "~A  ~A  ~A"
          (if (news-readp news) " " "N")
          (news-date news)
          (news-title news)))


(defun print-news-content (news &key (stream *standard-output*))
  "Print the detailed content of given NEWS."
  (format stream "
Title:    ~A
Date:     ~A
Content:  ~A
"
          (news-title news)
          (news-date news)
          (news-content news)))


(defmethod print-object ((news news) stream)
  "Print the object NEWS in a more readable format to given STREAM,
which must be an opened output stream."
  (format stream "(:date \"~A\" :title \"~A\" :content \"~A\" :readp ~A)"
          (news-date news)
          (news-title news)
          (news-content news)
          (news-readp news)))

(defun read-news (news-list &key (index nil) (stream *standard-output*))
  "Display the news one by one, and mark them as read.
If INDEX is given and not NIL, process corresponding news;
otherwise, process all the unread news."
  ;; TODO index :unread :all [number]
  (labels ((process-single-news (news)
             (print-news-content news :stream stream)
             (setf (news-readp news) t)))
    (cond ((and index (numberp index))
           (process-single-news (elt news-list index)))
          (t (loop for news across news-list
                do (process-single-news news)
                  (format stream "~&~%"))))))

;; (main '("pacman" "list" "--all"))

(let ((news-list (load-all-news (concatenate 'string *pacnews-dir* *pacnews-news-file*))))
  (print news-list)
  (read-news news-list)
  (print news-list))
