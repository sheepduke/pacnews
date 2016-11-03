(in-package :pacnews)

(defun list-local-news (&key (type :unread))
  type)

(defun sync-news ()
  )

(define-condition argument-error (error)
  ((argument :initarg :argument :reader argument)))


(defun ensure-environment ()
  (ensure-directories-exist *news-dir*)
  (if (not (uiop:directory-exists-p *news-dir*))
      (error "Cannot create news directory '~A'" *news-dir*)))

(defun main (argv)
  "Main function for the program."
  (let ((program (first argv)))
    (pop argv)
    (handler-case
        (progn 
          (ensure-environment)
          (loop while (not (null argv)) do
               (let ((arg (first argv)))
                 (switch (arg :test 'equal)
                   ("list" (pop argv)
                           (let ((type :unread))
                             (loop while (not (null argv)) do
                                  (setf arg (first argv))
                                  (switch (arg :test 'equal)
                                    ("--unread")
                                    ("--all" (setf type :all))
                                    (t (error 'argument-error :argument arg)))
                                  (pop argv))
                             (list-local-news :type type)))
                   ("sync" (pop argv)
                           (sync-news))))))
      (argument-error (err)
        (format t "Invalid option '~A'. 
Type '~A --help' for help.~%"
                (argument err)
                program))
      (error (err)
        (format t "~A~&" err)))))

(main '("list"))
