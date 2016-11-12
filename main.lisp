(in-package :pacnews)

(define-condition argument-error (error)
  ((argument :initarg :argument :reader argument)))


(defun ensure-environment ()
  (ensure-directories-exist *pacnews-dir*)
  (if (not (uiop:directory-exists-p *pacnews-dir*))
      (error "Cannot create news directory '~A'" *pacnews-dir*)))

(defun execute (argv)
  (pop argv)
  (ensure-environment)
  (let* ((news-file (concatenate 'string
                                 *pacnews-dir*
                                 *pacnews-news-file*))
         (news-list (load-all-news news-file)))
    (loop while argv
       for arg = (pop argv)
       do (switch (arg :test 'equal)
            ("list" (let ((type :unread))
                      (loop while argv
                         for arg = (pop argv)
                         do (switch (arg :test 'equal)
                              ("--all" (setf type :all))
                              (t (error 'argument-error :argument arg))))
                      (print-news-list news-list :type type)))
            ("sync" (sync-news news-list)
                    (dump-all-news news-list news-file))
            ("read" (let ((index :unread))
                      (when argv
                        (let ((arg (pop argv)))
                          (switch (arg :test 'string=)
                            ("--all" (setf index :all))
                            (t (handler-case (setf index (1- (parse-integer arg)))
                                 (error () (error 'argument-error :argument arg)))))))
                      (process-news news-list :index index :mark :read)
                      (dump-all-news news-list news-file)))
            ("unread" (let ((index nil))
                      (when argv
                        (let ((arg (pop argv)))
                          (switch (arg :test 'string=)
                            ("--all" (setf index :all))
                            (t (handler-case (setf index (1- (parse-integer arg)))
                                 (error () (error 'argument-error :argument arg)))))))
                      (unless index
                        (error "Option --all or number N must be given."))
                      (process-news news-list :index index :printp nil :mark :unread)
                      (dump-all-news news-list news-file)))
            ("help" (print-help-message))
            ("--help" (print-help-message))
            ("-h" (print-help-message))
            ("--version" (print-version-info))
            ("-v" (print-version-info))
            (t (error 'argument-error :argument arg))))))
       
(defun main (argv)
  "Main function for the program."
  (handler-case (execute argv)
    (argument-error (err)
      (format t "Invalid option '~A'. 
Type '~A --help' for help.~%"
              (argument err)
              *pacnews-program*))
    (error (err)
      (format t "~A~&" err))))

(defun print-help-message ()
  "Print help message."
  (format t "pacnews - archlinux news reader

Description
  pacnews provides similar functionality as Gentoo 'eselect news'.

Usage
  list [--all]              list unread or all the news if --all is given
  sync                      synchronize with archlinux server
  read [--all|N]            read news and mark it as 'read'
                                default to unread news
                                if --all is given, process all the news
                                if number N is given, process news with index
  unread [--all|N]          mark news as unread, with index N
                                if --all is given, mark every news as 'read'

  help, -h, --help          print this help message

  version, -v, --version    print version information
"))

(defun print-version-info ()
  (format t "pacnews version 1.0.0"))
