(in-package :pacnews)

(defstruct news-url
  "The date and URL of news."
  date url)

(defun sync-news (news-list)
  (let ((latest (get-latest-date news-list)))
    (loop for page from 1
       for news-page = (fetch-news-page page)
       while news-page
       do (loop for news-url in news-page
             do (if (string< latest (news-url-date news-url))
                    (vector-push-extend (fetch-news (news-url-url news-url))
                                        news-list)
                    (return-from sync-news nil))))))

(defun get-latest-date (news-list)
  "Return the latest date from given NEWS-LIST.
NEWS-LIST is a sorted sequence of NEWS objects.
If NEWS-LIST is nil, return \"0000-00-00\", which means the smallest date."
  (if (emptyp news-list)
      "0000-00-00"
      (news-date (last-elt news-list))))

(defun fetch-news-page (&optional (page 1))
  "Fetch news list of given PAGE from archlinux news website."
  (assert (numberp page))
  (let ((url (format nil "~a/news?page=~d" *pacnews-host* page)))
    (format t "~&Fetching ~a~&" url)
    (multiple-value-bind (response state)
        (http-request url
                      :method :get
                      :close nil)
      (if (= state 200)
          (extract-news-urls response)
          nil))))

(defun fetch-news (url)
  "Fetch news from given URL and return a NEWS object."
  (assert (stringp url))
  (let ((url (concatenate 'string *pacnews-host* url)))
    (format t "~&Fetching ~a~&" url)
    (extract-news-contents
     (http-request url
                   :close nil))))

(defun extract-news-contents (html)
  "Return NEWS object."
      (let (title content date)
      ;; Extract message date.
      (setf date
            (scan-to-strings "[0-9-]+"
             (scan-to-strings "<meta itemprop=\"datePublished\".+/>" html)))
      ;; Extract message title.
      (setf title (scan-to-strings "<h2 itemprop=\"headline\">.+</h2>" html)
            title (regex-replace-all "<[^>]+>" title "")
            title (regex-replace-all "</h2>" title ""))
      ;; Extract message content.
      (let* ((start (nth-value 1 (scan "<div class=\"article-content[^>]+>" html)))
             (end (scan "</div>" html :start start)))
        (setf content (subseq html start end))
        (setf content (regex-replace-all "<(p|pre)>" content (string #\newline))
              content (regex-replace-all "<a href=\"" content "[")
              content (regex-replace-all "\">" content "] ")
              content (regex-replace-all "<[a-z^>]+>" content "")
              content (regex-replace-all "</[^>]+>" content "")))
      (make-news :title title :content content
                 :date date :readp nil)))

(defun extract-news-urls (html)
  "Collect URLs of news. Return a list of news-url."
  (let ((start 0))
    (loop with end
       do (multiple-value-setq (start end)
            (scan "<tr class=\"(even|odd).*" html :start start))
       while start
       collect (make-news-url :date (scan-to-strings "([0-9]|-)+" html :start end)
                              :url (scan-to-strings "/news/([a-z0-9]|-)+/"
                                                    html :start end))
       do (setf start end))))
