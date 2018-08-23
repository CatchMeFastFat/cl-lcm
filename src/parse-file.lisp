;; file parser
(defun consume-comments (file line)
  "Returns new line without comments"
  (let ((comment-line (search "//" line))
        (comment-region (search "/*" line)))
    (if (and comment-line comment-region)
        (if (> comment-line comment-region)
            (setf  comment-line nil)
            (setf  comment-region nil)))
    (if comment-region
        (let ((comment-end (search "*/" line)))
          (if comment-end
              (setf line
                    (concatenate 'string
                                 (subseq line 0 comment-region)
                                 (subseq line comment-end)))
              (progn (loop while (not comment-end) do
                          (setf comment-end (search "*/" line)))
                     (setf line (subseq line comment-end)))))
        (if comment-line
            (subseq line 0 comment-line)))))
        
(defun parse-message (file-path)
  (with-open-file (my-file file-path
                           :direction :input
                           :if-does-not-exist :error)
    (loop for line = (read-line my-file nil)
       while line do
         (consume-comments (my-file line))
         ()
         


           

