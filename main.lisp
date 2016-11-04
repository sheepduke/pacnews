(in-package :pacnews)

(defun list-local-news (&key (type :unread))
  type)

(defun sync-news ()
  )

(define-condition argument-error (error)
  ((argument :initarg :argument :reader argument)))


(defun ensure-environment ()
  (ensure-directories-exist *pacnews-dir*)
  (if (not (uiop:directory-exists-p *pacnews-dir*))
      (error "Cannot create news directory '~A'" *pacnews-dir*)))

(defun execute (argv)
  (pop argv)
  (ensure-environment)
  (let ((news-list (load-all-news (concatenate 'string
                                               *pacnews-dir*
                                               *pacnews-news-file*))))
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
            ("sync" 'TODO)
            ("read" (let ((index nil))
                      (when argv
                        (let ((arg (pop argv)))
                          (handler-case (setf index (parse-integer arg))
                            (error () (error 'argument-error :argument arg)))))
                      (read-news news-list :index index)))
            ("unread" 'TODO)
            (t (error 'argument-error :argument arg))))))
       
(defun main (argv)
  "Main function for the program."
  (handler-case (execute argv)
    (argument-error (err)
      (format t "Invalid option '~A'. 
Type '~A --help' for help.~%"
              (argument err)
              *pacnews-program*))
    ;; (error (err)
    ;;   (format t "~A~&" err))
    ))
