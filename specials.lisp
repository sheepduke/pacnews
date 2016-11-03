(in-package :pacnews)

(defparameter *entry-url* "https://www.archlinux.org/news"
  "The entry point of the archlinux news.")

(defvar *news-dom* nil
  "DOM element of news.")

(defvar *news-dir* (concatenate 'string (uiop:getenv "HOME") "/.pacnews/")
  "Directory for storing local news.")

(defvar *news-list* nil
  "List of news.")
