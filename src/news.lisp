(in-package :pacnews)

(defstruct news title content date readp)

(defun load-all-news (path)
  "Read all the news from given PATH, which represents a valid path to
existing file.
A list of news is returned as the result; if there is no news inside
the given PATH, NIL is returned."
  (with-open-file (stream path
                          :direction :input
                          :if-does-not-exist :create)
    (let ((news-list (make-array 100 :adjustable t :fill-pointer 0)))
      (loop for news = (read stream nil nil)
         while news
         do (vector-push-extend news news-list))
      (sort news-list
            (lambda (a b)
              (string< (news-date a) (news-date b)))))))

(defmethod print-object ((news news) stream)
  (format stream "#S(PACNEWS:NEWS :TITLE ~s :CONTENT ~s :DATE ~s :READP ~a)"
          (news-title news)
          (news-content news)
          (news-date news)
          (news-readp news)))

(defun dump-all-news (news-list path)
  "Dump all the news specified by NEWS-LIST into corresponding PATH.
Existing file of given PATH will be overwritten."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
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

(defun process-news (news-list &key (index :unread)
                                 (printp t)
                                 (mark :read)
                                 (stream *standard-output*))
  "Display the news one by one, and mark them as read or unread.
If INDEX is given and not NIL, process corresponding news;
otherwise, process all the unread news.
MARK decides whether the news will be marked as read or unread."
  (unless (or (equal index :unread)
              (equal index :all)
              (numberp index))
    (error "INDEX must be one of :ALL, :UNREAD, or a number."))
  (unless (or (equal mark :unread)
              (equal mark :read))
    (error "MARK must be one of :READ or :UNREAD."))
  (labels ((process-single-news (news)
             (when printp 
               (print-news-content news :stream stream)
               (format stream "~&~%"))
             (setf (news-readp news)
                   (case mark
                     (:read t)
                     (:unread nil)))))
    (cond ((numberp index)
           (if (and (>= index 0)
                    (< index (length news-list)))
               (process-single-news (elt news-list index))
               (error "INDEX '~d' is out of bound." index)))
          (t
           (loop for news across news-list
              when (or (equal index :all)
                       (and (equal index :unread)
                            (not (news-readp news))))
              do (process-single-news news))))))
