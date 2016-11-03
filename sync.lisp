(in-package :pacnews)

(defun read-latest-timestamp ()
  "Read in the timestamp of latest news from local file."
  (with-open-file (stream (concatenate 'string *news-dir* "latest")
                          :direction :io
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    ))


;; (defun dom-recurse-node (node func)
;;   (funcall func node)
;;   (element-map-children (lambda (node)
;;                           (funcall func node)) node))

;; (defun dom-find-node (node func &key (recursive? nil))
;;   "`dom-find-node' Find the DOM with given `func'. `func' must be a function 
;; return exactly T or NIL.
;; If `recursive?' is set to T, it will call itself when the desired node is found.
;; It returns a list of nodes as the found targets, or NIL if nothing is found."
;;   (when (null node)
;;     (return-from dom-find-node nil))
;;   (let ((result '())
;;         (func-win? (funcall func node)))
;;     (when func-win?
;;       (appendf result (list node)))
;;     (when (or (and func-win? recursive?)
;;               (and (not func-win?)))
;;       (element-map-children (lambda (child)
;;                               (appendf result (dom-find-node child func
;;                                                              :recursive? recursive?)))
;;                             node))
;;     result))

;; (let ((result (dom-find-node *dom*
;;                              (lambda (node)
;;                                (and (equal (node-name node) "tr")
;;                                     (let ((attr (element-attribute node "class")))
;;                                       (or (equal attr "even")
;;                                           (equal attr "odd"))))))))
;;   (dolist (node result)
;;     (let ((result (dom-find-node node
;;                                  (lambda (node)
;;                                    (equal (node-name node) "td")))))
;;       (dolist (node result)
;;         (print (node-value (node-first-child node)))))))
