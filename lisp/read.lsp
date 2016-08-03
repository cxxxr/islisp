(defglobal read:read-table (create-vector 256))
(defglobal read:sharp-table (create-vector 256))
(defglobal read:closed-mark (gensym))

(progn
  (for ((i 0 (+ i 1)))
       ((= 256 i))
    (is:vector-set #'read:read-token
                   read:read-table
                   i))
  (is:vector-set #'read:read-line-comment
                 read:read-table
                 (is:convert-char-to-integer #\;))
  (is:vector-set #'read:read-list
                 read:read-table
                 (is:convert-char-to-integer #\())
  (is:vector-set #'read:closed
                 read:read-table
                 (is:convert-char-to-integer #\)))
  (is:vector-set #'read:read-quote
                 read:read-table
                 (is:convert-char-to-integer #\'))
  (is:vector-set #'read:read-unquote
                 read:read-table
                 (is:convert-char-to-integer #\,))
  (is:vector-set #'read:read-quasiquote
                 read:read-table
                 (is:convert-char-to-integer #\`))
  (is:vector-set #'read:read-sharp
                 read:read-table
                 (is:convert-char-to-integer #\#))

  (for ((i 0 (+ i 1)))
       ((= 256 i))
    (is:vector-set #'read:sharp-error
                   read:sharp-table
                   i))
  (is:vector-set #'read:read-block-comment
                 read:sharp-table
                 (is:convert-char-to-integer #\|))
  (is:vector-set #'read:read-function
                 read:sharp-table
                 (is:convert-char-to-integer #\'))
  (is:vector-set #'read:read-vector
                 read:sharp-table
                 (is:convert-char-to-integer #\())
  (is:vector-set #'read:read-binary
                 read:sharp-table
                 (is:convert-char-to-integer #\b))
  (is:vector-set #'read:read-binary
                 read:sharp-table
                 (is:convert-char-to-integer #\B))
  (is:vector-set #'read:read-oct
                 read:sharp-table
                 (is:convert-char-to-integer #\o))
  (is:vector-set #'read:read-oct
                 read:sharp-table
                 (is:convert-char-to-integer #\O))
  (is:vector-set #'read:read-hex
                 read:sharp-table
                 (is:convert-char-to-integer #\x))
  (is:vector-set #'read:read-hex
                 read:sharp-table
                 (is:convert-char-to-integer #\X)))

(defun read:read-line-comment (c input-stream eos-error-p eos-value)
  (read-line input-stream eos-error-p eos-value)
  (read input-stream eos-error-p eos-value))

(defun read:upcase (c)
  (cond ((and (char<= #\a c) (char<= c #\z))
         (is:convert-integer-to-char (- (is:convert-char-to-integer c) 32)))
        (t c)))

(defun read:read-token (c input-stream eos-error-p eos-value)
  (let ((chars nil))
    (for ((c c (if (not breakp) (preview-char input-stream)))
          (firstp t nil)
          (breakp nil))
         (breakp)
      (case c
        ((#\( #\) #\' #\, #\` #\space #\newline)
         (setq breakp t))
        ((#\\)
         (read-char input-stream)
         (setq chars (cons (read-char input-stream) chars)))
        (t
         (if (not firstp)
             (read-char input-stream))
         (setq chars (cons (read:upcase c) chars))
         chars)))
    (setq chars (nreverse chars))
    (let ((str (create-string (is:list-length chars))))
      (for ((rest chars (cdr rest))
            (i 0 (+ i 1)))
           ((null rest))
        (is:string-set (car rest) str i))
      (or (is:parse-number* str)
          (is:convert-string-to-symbol str)))))

(defun read:read-list (c input-stream eos-error-p eos-value)
  (let ((list nil)
        (breakp nil))
    (while (not breakp)
      (let ((x (read input-stream)))
        (if (eq x read:closed-mark)
            (setq breakp t)
            (setq list (cons x list)))))
    (nreverse list)))

(defun read:closed (c input-stream eos-error-p eos-value)
  read:closed-mark)

(defun read:read-quote (c input-stream eos-error-p eos-value)
  (list 'quote (read input-stream)))

(defun read:read-unquote (c input-stream eos-error-p eos-value)
  (cond ((char= #\@ (preview-char input-stream))
         (read-char input-stream)
         (list 'unquote-splicing (read input-stream)))
        (t
         (list 'unquote (read input-stream)))))

(defun read:read-quasiquote (c input-stream eos-error-p eos-value)
  (list 'quasiquote (read input-stream)))

(defun read:read-sharp (c input-stream eos-error-p eos-value)
  (let ((c2 (read-char input-stream)))
    (funcall (is:vector-ref read:sharp-table (is:convert-char-to-integer c2))
             c c2 input-stream eos-error-p eos-value)))

(defun read:read-block-comment (c1 c2 input-stream eos-error-p eos-value)
  (let ((c nil)
        (depth 1))
    (while (< 0 depth)
      (setq c (read-char input-stream))
      (cond ((and (char= c #\#)
                  (char= (read-char input-stream) #\|))
             (incf depth))
            ((and (char= c #\|)
                  (char= (read-char input-stream) #\#))
             (decf depth))))
    (read input_stream eos-error-p eos-value)))

(defun read:read-function (c1 c2 input-stream eos-error-p eos-value)
  (list 'function (read input-stream)))

(defun read:read-vector (c1 c2 input-stream eos-error-p eos-value)
  (let ((loop t)
        (list nil))
    (while loop
      (let ((x (read input-stream)))
        (if (eq x read:closed-mark)
            (setq loop nil)
            (setq list (cons x list)))))
    (cons 'vector (nreverse list))))

(defun read:read-binary (c1 c2 input-stream eos-error-p eos-value))
(defun read:read-oct (c1 c2 input-stream eos-error-p eos-value))
(defun read:read-hex (c1 c2 input-stream eos-error-p eos-value))
(defun read:sharp-error (c1 c2 input-stream eos-error-p eos-value))

(defun read:skip-ws (input-stream eos-error-p eos-value)
  (for ((c (read-char input-stream eos-error-p eos-value)
           (read-char input-stream eos-error-p eos-value)))
       ((not
         (or (char= c #\space)
             (char= c (is:convert-integer-to-char 9))
             (char= c #\newline)))
        c)))

(defun read (&optional (input-stream (standard-input)) (eos-error-p t) eos-value)
  (let ((c (read:skip-ws input-stream eos-error-p eos-value)))
    (funcall (is:vector-ref read:read-table (is:convert-char-to-integer c))
             c input-stream eos-error-p eos-value)))
