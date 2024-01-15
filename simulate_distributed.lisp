(defvar *debug-print* nil)
;; uncomment this line to print the graph states
;; (setq *debug-print* T)

;; util -------------------------------------------------------------------- ;;
;; prints the hash table message-queue
(defmacro print-message-queue ()
  `(maphash
    (lambda (key value) (print (list key value))) message-queue))
;; multiple prints
(defun printm (&rest args)
  (format t "~C~{~A~^ ~}~C" #\linefeed args #\linefeed))
;; assigns the return value of every expression in body to a variable
(defmacro continuous-let (variable &rest body)
  `(let ((,variable nil)) ,@(loop for expr in body collect
        `(setf ,variable ,expr))))
(defmacro continuous-let-until (variable cond init &rest body)
  `(let* ((,variable ,init))
     (loop while (,cond ,variable) do (setf ,variable ,@body))
     ,variable))
;; util function (from let over lambda)
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))
(defun flatten (x)
(labels ((rec (x acc)
                (cond ((null x) acc)
                    ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                    ((atom x) (cons x acc))
                    (t (rec
                            (car x)
                            (rec (cdr x) acc))))))
    (rec x nil)))
;; util -------------------------------------------------------------------- ;;


;; node definition
;; '('nodeid 'nodevalue neighbors)
;; func args: (id value (neighborid1 neighborid2 ...))
(defun step-nodes (nodes func messages colors)
  (let ((message-queue (make-hash-table)))
    (loop
     for node in nodes do
     (destructuring-bind
      (id value neighbors) node
      (setf (second node) (funcall func
               id value neighbors
               (lambda (neighborid message)
                 (push (list id message)
                       (gethash neighborid message-queue '())))
               (gethash id messages) colors))))
    (if *debug-print* (print nodes) NIL)
    message-queue))

;; (id value neighbors send-message incoming-messages) -> (new value)
;; send-message: (neighborid message) -> ()
(defun example-distributed-algorithm
    (id value neighbors send-message incoming-messages)
  (loop
   for
   neighbor
   in
   neighbors
   do
      (funcall send-message neighbor value))
  'green)


(defmacro randoml (list) `(nth (random (length ,list)) ,list))

(defmacro filter-colors (type) 
    `(mapcar #'second (remove-if-not (lambda (m) (equal (car m) ,type)) (mapcar #'second incoming-messages))))

(defmacro send-all (message) `(loop for neighbor in neighbors do (funcall send-message neighbor ,message)))
(defmacro send-all-return (message) `(progn (send-all ,message) ,message))

(defun coloring (id value neighbors send-message incoming-messages colors)
  (let* ((permanent-neighbor-colors (filter-colors 'permanent))
         (candidate-neighbor-colors (filter-colors 'candidate))
         (available-colors (set-difference colors permanent-neighbor-colors))
         (selected-color (randoml available-colors)))
    (case (car value)
        (candidate (if (member (second value) (append candidate-neighbor-colors permanent-neighbor-colors))
                      (send-all-return `(candidate ,selected-color))
                      (send-all-return `(permanent ,(second value)))))
        (permanent (send-all-return value))
        (otherwise (send-all-return `(candidate ,selected-color))))))


;; graph creation macro
;; syntax: node0 -- node1 node2 -- node0
(defmacro create-graph% (nodes &rest definition)
  `(list ,@(loop for node in nodes collect
                 `'(,node nil ,(append
                            (mapcar #'third (remove-if-not (lambda (def) (eq (car def) node)) (group definition 3)))
                            (mapcar #'first (remove-if-not (lambda (def) (eq (third def) node)) (group definition 3))))))))
(defmacro create-graph (&rest definition)
  (let ((nodes (remove-if-not (lambda (n) (not (equal '-- n))) (remove-duplicates (flatten definition)))))
  `(create-graph% ,nodes ,@definition)))
;;
(defun is-graph-permanent? (graph)
  (every (lambda (a) (equal a 'permanent)) (mapcar #'caadr graph)))
(defun !graph-permanent? (graph)
  (not (is-graph-permanent? graph)))

(defun color-graph (graph colors) 
    (continuous-let-until message-queue (lambda (_) (declare (ignore _)) (!graph-permanent? graph))
                    (make-hash-table)
                    (step-nodes graph #'coloring message-queue colors))
  graph)
(defmacro lookup-coloring (nodeid)
  `(cadadr (find-if (lambda (node) (equal (car node) ,nodeid)) graph)))
(defun verify-coloring-node (graph node)
  (if (every (lambda (neighbor) (not (equal (cadadr node) (lookup-coloring neighbor)))) (third node))
      T
      (progn
        (printm "Failed" node)
        NIL)))
(defun verify-coloring (graph)
  (every (lambda (n) (verify-coloring-node graph n)) graph))

(defun mashup-symbol (&rest objects)
  (intern (format nil "~{~a~}" objects)))
(defun random-except (max except)
  (let ((r (random (- max 1))))
    (if (= r except)
        (- max 1)
        r)))
        
(defmacro generate-graph (N E)
  (let ((edges (flatten (loop for i from 0 below E collect
                     (let ((rand-node-1 (random N)))
                       (list (mashup-symbol 'NODE rand-node-1) '-- (mashup-symbol 'NODE (random-except N rand-node-1))))))))
    `(create-graph ,@edges)))
(defun max-degree (graph)
  (apply #'max (mapcar (lambda (n) (length (third n))) graph)))
(defun generate-colors (N)
  (loop for i from 0 below N collect
        (mashup-symbol 'COLOR i)))
(defmacro test-graph (N E)
  `(let ((graph (generate-graph ,N ,E)))
    (verify-coloring (color-graph graph (generate-colors (+ 1 (max-degree graph)))))))

(printm "Testing 100 400: " (test-graph 100 400))
(printm "Testing 1000 4000: " (test-graph 1000 4000))
(printm "Testing 100 800: " (test-graph 100 800))
(printm "Testing 10 100: " (test-graph 10 100))
(printm "Testing 100 200: " (test-graph 100 200))
