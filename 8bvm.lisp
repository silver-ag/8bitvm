"an 8-bit virtual machine, using a nonexistent architecture. see docs directory."
"tested using clisp"

;; for testing and debugging purposes
(defvar dbg t)

;; registers
(defvar *A* #x00) ;; register A
(defvar *B* #x00) ;; register B
(defvar *C* #x00) ;; ...
(defvar *D* #x00) ;; ...
(defvar *I* #x00) ;; register for instruction pointer

;; 8-bit memory space, as a list of bytes
(defvar memory (make-list #x100 :initial-element #x00))

;; useful basic functions
(defun error-quit (errstr)
  (write-line errstr)
  (exit))

;; load program into memory
(let ((program (open (nth 0 *args*) :element-type '(unsigned-byte 8))))
  (loop for i from #x00 to #xff do
    (setf (nth i memory) (read-byte program nil))))


;; instructions
(defun mov-mm (from to)
  (if dbg ((lambda nil (write-string "MOV-MM ") (write from) (write-string " ") (write to) (terpri))))
  (setq *I* (+ 2 *I*))
  (setf (nth to memory) (nth from memory)))

(defun mov-mr (from to)
  (if dbg ((lambda nil (write-string "MOV-MR ") (write from) (write-string " ") (write to) (terpri))))
  (setq *I* (+ 2 *I*))
  (case to
    (#x01 (setq *A* from))
    (#x02 (setq *B* from))
    (#x03 (setq *C* from))
    (#x04 (setq *D* from))
    (#x05 (setq *I* from))))

(defun mov-rm (from to)
  (if dbg ((lambda nil (write-string "MOV-RM ") (write from) (write-string " ") (write to) (terpri))))
  (setq *I* (+ 2 *I*))
  (case from
    (#x01 (setf (nth to memory) *A*))
    (#x02 (setf (nth to memory) *B*))
    (#x03 (setf (nth to memory) *C*))
    (#x04 (setf (nth to memory) *D*))
    (#x05 (setf (nth to memory) *I*))))

(defun mov-rr (from to)
  (if dbg ((lambda nil (write-string "MOV-RR ") (write from) (write-string " ") (write to) (terpri))))
  (setq *I* (+ 2 *I*))
  (case from
    (#x01 (case to
            (#x01 (setq *A* *A*))
            (#x02 (setq *A* *B*))
            (#x03 (setq *A* *C*))
            (#x04 (setq *A* *D*))
            (#x05 (setq *A* *I*))))
    (#x02 (case to
            (#x01 (setq *B* *A*))
            (#x02 (setq *B* *B*))
            (#x03 (setq *B* *C*))
            (#x04 (setq *B* *D*))
            (#x05 (setq *B* *I*))))
    (#x01 (case to
            (#x01 (setq *C* *A*))
            (#x02 (setq *C* *B*))
            (#x03 (setq *C* *C*))
            (#x04 (setq *C* *D*))
            (#x05 (setq *C* *I*))))
    (#x01 (case to
            (#x01 (setq *D* *A*))
            (#x02 (setq *D* *B*))
            (#x03 (setq *D* *C*))
            (#x04 (setq *D* *D*))
            (#x05 (setq *D* *I*))))
    (#x01 (case to
            (#x01 (setq *I* *A*))
            (#x02 (setq *I* *B*))
            (#x03 (setq *I* *C*))
            (#x04 (setq *I* *D*))
            (#x05 (setq *I* *I*))))))

(defun pri ()
  (if dbg (write-line "PRI"))
  (loop for i from #xf0 to #xfe do
    (if (nth i memory)
      (write-char (code-char (nth i memory))))))

(defun hlt ()
  (if dbg (write-line "HLT"))
  (write-line "[execution finished]")
  (exit))

(defun add ()
  (if dbg (write-line "ADD"))
  (setq *D* (+ *A* *B*))
  (if (> *D* #xff)
    (setq *D* (- *D* #xff))))

(defun sub ()
  (if dbg (write-line "SUB"))
  (setq *D* (- *A* *B*))
  (if (< *D* #x00)
    (setq *D* (+ *D* #xff))))

(defun nop ()
  (if dbg (write-line "NOP")))

;; read a byte and deal with it
(defun exec-addr (addr)
  (let ((instruction (nth addr memory)))
    (case instruction
      (#x01 (mov-mm (nth (+ 1 addr) memory) (nth (+ 2 addr) memory)))
      (#x02 (mov-mr (nth (+ 1 addr) memory) (nth (+ 2 addr) memory)))
      (#x03 (mov-rm (nth (+ 1 addr) memory) (nth (+ 2 addr) memory)))
      (#x04 (mov-rr (nth (+ 1 addr) memory) (nth (+ 2 addr) memory)))
      (#x05 (pri))
      (#x06 (hlt))
      (#x07 (add))
      (#x08 (sub))
      (#x09 (nop))
      (#x09 (write-line "OR"))
      (#x0a (write-line "XOR"))
      (#x0b (write-line "AND"))
      (#x0c (write-line "NOT"))
      (otherwise (error-quit "illegal instruction")))))

;; run loop
(defun run-loop nil
  (loop do (exec-addr *I*)
           (setq *I* (1+ *I*))))

;; testing
(run-loop)