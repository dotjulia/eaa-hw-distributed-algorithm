# Simulating distributed algorithms
My homework for the course design and analysis of algorithms

I implemented the distributed algorithm simulation in common lisp.

# Running the tests/simulation
Using any online common lisp compiler:
+ For example #link("https://onecompiler.com/commonlisp/3zzhv4gat") or #link("https://glot.io/snippets/gshu4xlx8b") (these links should already contain the source code)
+ Paste the source
+ Run


Using SBCL locally:
+ Install SBCL (#link("https://www.sbcl.org/platform-table.html"))
+ Clone the repo
+ Run the code: `sbcl --load simulate_distributed.lisp`
+ Ignore the warnings
You will see the following output:
```lisp
Testing 100 400:  T
Testing 1000 4000:  T
Testing 100 800:  T
Testing 10 100:  T
Testing 100 200:  T
```
meaning all the tests passed. You can run custom on the repl (should open after the output) using the following function:
```lisp
* (test-graph 100 200)
```
With 100 being the number of vertices and 200 being the number of randomly generated edges.
In the following sections I will explain how to use more custom examples and roughly how the code works.
# A simple example
A simple example can be called like this:
```lisp
(color-graph (create-graph
               node0 -- node1
               node0 -- node2)
             '(red green blue))
```


The `color-graph` function takes a graph as its first argument and a list of colors as the second argument. (I'll show a function which takes the number of colors later). The syntax of the `create-graph` function works by specifying the edges between nodes, the nodes are defined implicitly by the symbols appearing in the edge list.

When we run this function we could, for example, see this output (if debug output is enabled on line 3):
```lisp
1. ((NODE1 (CANDIDATE RED) (NODE0)) (NODE0 (CANDIDATE RED) (NODE1 NODE2))
    (NODE2 (CANDIDATE RED) (NODE0))) 
2. ((NODE1 (CANDIDATE RED) (NODE0)) (NODE0 (CANDIDATE GREEN) (NODE1 NODE2))
    (NODE2 (CANDIDATE GREEN) (NODE0))) 
3. ((NODE1 (PERMANENT RED) (NODE0)) (NODE0 (CANDIDATE RED) (NODE1 NODE2))
    (NODE2 (CANDIDATE RED) (NODE0))) 
4. ((NODE1 (PERMANENT RED) (NODE0)) (NODE0 (CANDIDATE BLUE) (NODE1 NODE2))
    (NODE2 (CANDIDATE BLUE) (NODE0))) 
5. ((NODE1 (PERMANENT RED) (NODE0)) (NODE0 (CANDIDATE BLUE) (NODE1 NODE2))
    (NODE2 (CANDIDATE GREEN) (NODE0))) 
6. ((NODE1 (PERMANENT RED) (NODE0)) (NODE0 (PERMANENT BLUE) (NODE1 NODE2))
    (NODE2 (PERMANENT GREEN) (NODE0))) 
```
(the iteration numberings were added after the fact, there should be a clear distinction between outputs in the lisp repl)
The output can be interpretet as a list of nodes in the following format:
```
'(<NODEID> (<CANDIDATE|PERMANENT> <COLOR>) (<NEIGHBORID*>))
```
Looking at the above output, we can see that in the first iteration `node1` chose red, `node0` chose red and `node2` chose red. Then in the second iteration, `node1` chose red again and `node0` and `node2` chose green. In iteration 3 we can see that `node1` set red to its permanent color, as no neighboring node had the same candidate color. The next 3 iterations consist of `node0` and `node2` picking the same color 2 times. In the last iteration we can see that all nodes transitioned to the permanent state.

We can check if we have a valid coloring using the `verify-coloring` function:
```lisp
(defmacro lookup-coloring (nodeid)
  `(cadadr (find-if (lambda (node) (equal (car node) ,nodeid)) graph)))
(defun verify-coloring-node (graph node)
  (every (lambda (neighbor) (not (equal (cadadr node) (lookup-coloring neighbor))))
         (third node)))
(defun verify-coloring (graph)
  (every (lambda (n) (verify-coloring-node graph n)) graph))
```

Applied to the last state of the example output above, we can see that it is valid as all neighboring nodes have different colors:
```lisp
* (verify-coloring '((NODE1 (PERMANENT RED) (NODE0)) (NODE0 (PERMANENT BLUE) (NODE1 NODE2)) (NODE2 (PERMANENT GREEN) (NODE0))))
T
```
_`*` indicates the lisp repl_
We can see that the `verify-coloring` worked as it output `T` which stands for true. If we tried an invalid coloring `verify-coloring` would fail:
```lisp
* (verify-coloring '((NODE1 (PERMANENT BLUE) (NODE0)) (NODE0 (PERMANENT BLUE) (NODE1 NODE2)) (NODE2 (PERMANENT GREEN) (NODE0))))
NIL
```

# The algorithm
The `color-graph` function looks like this:
```lisp
(defun color-graph (graph colors) 
    (continuous-let-until message-queue (lambda (_) (declare (ignore _)) (!graph-permanent? graph))
                    (make-hash-table)
                    (step-nodes graph #'coloring message-queue colors)))
```
My `continuous-let-until` repeats the body until a condition is met. The condition `!graph-permanent?` checks if all nodes are not permanent yet.
The actual simulation of the distributed algorithm happens in `step-nodes` while the `coloring` function implements the algorithm run on the graph.

## The coloring algorithm
This is the implementation of the coloring algorithm:
```lisp
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
```
It gets the permanent, candidate and available colors at the beginning. Then selects a random color and differentiates based on the value of the node:
- candidate: if its color appears in the candidate colors of the neighbors. It sets its own color to the randomly selected one specified as a candidate color
- if the nodes color is permanent, it just sends all neighbors that its color is permanent
- otherwise (at the beginning) it sets a random color as its own candidate color.

## The simulation
The simulation of the algorithm on the graph happens in `step-nodes`:

```lisp
(defun step-nodes (nodes func messages colors)
  (let ((message-queue (make-hash-table)))
    (loop for node in nodes do
     (destructuring-bind
      (id value neighbors) node
      (setf (second node) (funcall func
               id value neighbors
               (lambda (neighborid message)
                 (push (list id message)
                       (gethash neighborid message-queue '())))
               (gethash id messages) colors))))
    (print nodes)
    message-queue))
```
This function just loops over the nodes, builds up a `message-queue` using a hash table for the next iteration and calls the `func` function which is the passed algorithm to simulate.

# Testcase with many nodes
To generate a graph with $N$ nodes and $E$ edges I implemented this macro:
```lisp
(defmacro generate-graph (N E)
  (let ((edges (flatten (loop for i from 0 below E collect
    (let ((rand-node-1 (random N)))
      (list (mashup-symbol 'NODE rand-node-1)
            '--
            (mashup-symbol 'NODE (random-except N rand-node-1))))))))
    `(create-graph ,@edges)))
```
This generates $E$ edges choosing from $N$ neighbors. Note that this doesn't guarantee to really return a graph with $N$ nodes if the edge count is small nodes without a connecting edge are omitted). To figure out the $Delta$ of a random graph I implemented the `max-degree` function. There is also a `generate-colors` function to generate $N$ colors.

Example:
```
* (generate-graph 10 40)
((NODE8 NIL (NODE0 NODE1 NODE2 NODE9 NODE4 NODE5 NODE9 NODE5 NODE4))
 (NODE9 NIL (NODE1 NODE0 NODE8 NODE7 NODE4 NODE8))
 (NODE5 NIL (NODE8 NODE7 NODE8 NODE4 NODE7 NODE4))
 (NODE2 NIL (NODE4 NODE3 NODE4 NODE3 NODE3 NODE7 NODE8))
 (NODE3 NIL (NODE6 NODE0 NODE7 NODE2 NODE1 NODE2 NODE2))
 (NODE7 NIL
  (NODE6 NODE9 NODE2 NODE5 NODE6 NODE0 NODE0 NODE0 NODE5 NODE6 NODE6 NODE3))
 (NODE6 NIL (NODE7 NODE7 NODE0 NODE7 NODE4 NODE3 NODE7 NODE1 NODE0))
 (NODE0 NIL (NODE6 NODE8 NODE9 NODE3 NODE7 NODE4 NODE7 NODE7 NODE6))
 (NODE4 NIL (NODE8 NODE6 NODE9 NODE8 NODE0 NODE5 NODE1 NODE2 NODE5 NODE2))
 (NODE1 NIL (NODE6 NODE3 NODE9 NODE8 NODE4)))
* (max-degree *) ;; in cl repls * is the last evaluated value
12
* (generate-colors (+ ** 1)) ;; ** is the value before the last one
(COLOR0 COLOR1 COLOR2 COLOR3 COLOR4 COLOR5 COLOR6 COLOR7 COLOR8 COLOR9 COLOR10 COLOR11 COLOR12)
* (color-graph *** *)
<OMITTED STEPS>
((NODE8 (PERMANENT COLOR10)
  (NODE0 NODE1 NODE2 NODE9 NODE4 NODE5 NODE9 NODE5 NODE4))
 (NODE9 (PERMANENT COLOR5) (NODE1 NODE0 NODE8 NODE7 NODE4 NODE8))
 (NODE5 (PERMANENT COLOR5) (NODE8 NODE7 NODE8 NODE4 NODE7 NODE4))
 (NODE2 (PERMANENT COLOR9) (NODE4 NODE3 NODE4 NODE3 NODE3 NODE7 NODE8))
 (NODE3 (PERMANENT COLOR6) (NODE6 NODE0 NODE7 NODE2 NODE1 NODE2 NODE2))
 (NODE7 (PERMANENT COLOR0)
  (NODE6 NODE9 NODE2 NODE5 NODE6 NODE0 NODE0 NODE0 NODE5 NODE6 NODE6 NODE3))
 (NODE6 (PERMANENT COLOR9)
  (NODE7 NODE7 NODE0 NODE7 NODE4 NODE3 NODE7 NODE1 NODE0))
 (NODE0 (PERMANENT COLOR2)
  (NODE6 NODE8 NODE9 NODE3 NODE7 NODE4 NODE7 NODE7 NODE6))
 (NODE4 (PERMANENT COLOR1)
  (NODE8 NODE6 NODE9 NODE8 NODE0 NODE5 NODE1 NODE2 NODE5 NODE2))
 (NODE1 (PERMANENT COLOR11) (NODE6 NODE3 NODE9 NODE8 NODE4)))
* (verify-coloring *)
T ;; yay :)
```

To make this testing process easier I wrote this helper macro:
```lisp
(defmacro test-graph (N E)
  `(let ((graph (generate-graph ,N ,E)))
    (verify-coloring (color-graph graph (generate-colors (+ 1 (max-degree graph)))))))
```
It can be invoked like this:
```
* (test-graph 100 400) ;; create graph with 100 vertices and 400 edges
T ;; <- means it worked
```
This macro just expands to the forms above:
```lisp
* (macroexpand-1 '(test-graph 5 10))
(LET ((GRAPH (GENERATE-GRAPH 5 10)))
  (VERIFY-COLORING
   (COLOR-GRAPH GRAPH (GENERATE-COLORS (+ 1 (MAX-DEGREE GRAPH))))))
```
