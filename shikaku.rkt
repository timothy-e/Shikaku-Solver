;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname shikaku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require "rectanglelib.rkt")

(define-struct cell (num used?))
;; A Cell is a (make-cell Nat Bool)

;; A Grid is a (listof (listof Cell))
;; requires: the grid contains a non-empty list of non-empty lists
;;           all the same length

(define-struct rect (x y w h))
;; A Rect is a (make-rect Nat Nat Nat Nat)

(define-struct state (grid rects))
;; A State is a a (make-state Grid (listof Rect))

;; A Table is a (listof (listof Any))
;; requires: each sub-list has the same length


;; * * * A * * * A * * * A * * * A * * *


;; (map2d f lolst) produces a list of lists in which f has been
;;    applied to every element of the input
;; map2d: (X -> Y) (listof (listof X)) -> (listof (listof Y))
;; Examples
(check-expect (map2d sub1 '((-3 -2) (1 4 6) (3.2 -1)))
              '((-4 -3) (0 3 5) (2.2 -2)))
(check-expect (map2d sub1 (list empty empty)) (list empty empty))
(check-expect (map2d sub1 empty) empty)

(define (map2d f lolst)
  (map (lambda (lst)
         (map f lst))
       lolst))

;; Tests
(check-expect (map2d number? '((a 2 3 b c) (d e 4 f) (a b c) (1 2 3)))
              (list (list false true true false false)
                    (list false false true false)
                    (list false false false)
                    (list true true true)))
(check-expect (map2d add1 '((3 4 5) (10 9 8)))
              '((4 5 6) (11 10 9)))


;; * * * B * * * B * * * B * * * B * * *


;; (construct-puzzle nats) produces a State representing the intitial
;;    state of a puzzle 
;; construct-puzzle: (listof (listof Nat)) -> State
;; requires: nats must be a non empty list of non empty lists
;; Examples
(check-expect (construct-puzzle '((2 0)
                                  (0 2)
                                  (2 0)))
              (make-state (list (list (make-cell 2 false) (make-cell 0 false))
                                (list (make-cell 0 false) (make-cell 2 false))
                                (list (make-cell 2 false) (make-cell 0 false)))
                          empty))
(check-expect (construct-puzzle '((1)))
              (make-state (list (list (make-cell 1 false)))
                          empty))

(define (construct-puzzle nats)
  (make-state (map2d (lambda (n) (make-cell n false))
                     nats)
              empty))

;; Tests
(check-expect (construct-puzzle '((0 3 0)))
              (make-state (list (list (make-cell 0 false) (make-cell 3 false)
                                      (make-cell 0 false)))
                          empty))
(check-expect (construct-puzzle '((2 0)
                                  (2 0)))
              (make-state (list (list (make-cell 2 false) (make-cell 0 false))
                                (list (make-cell 2 false) (make-cell 0 false)))
                          empty))


;; * * * C * * * C * * * C * * * C * * * C * * *


;; (solved? puzzle-state) produces true if all of the cells in the given
;;    'puzzle-state' are used
;; solved?: State -> Bool
;; Examples
(check-expect (solved? (make-state (list (list (make-cell 2 false)
                                               (make-cell 0 false))
                                         (list (make-cell 0 false)
                                               (make-cell 2 false))
                                         (list (make-cell 2 false)
                                               (make-cell 0 false)))
                                   empty))
              false)
(check-expect (solved? (make-state (list (list (make-cell 2 true)
                                               (make-cell 0 true))
                                         (list (make-cell 0 true)
                                               (make-cell 2 true))
                                         (list (make-cell 2 true)
                                               (make-cell 0 true)))
                                   empty))
              true)
                       
(define (solved? puzzle-state)
  (andmap (lambda (lst) (foldr  (lambda (x y)
                                  (and (cell-used? x) y))
                                true lst))
          (state-grid puzzle-state)))

;; Tests
(check-expect (solved? (make-state (list (list (make-cell 1 true)))
                                   empty))
              true)                       
(check-expect (solved? (make-state (list (list (make-cell 2 false)
                                               (make-cell 0 false))
                                         (list (make-cell 0 true)
                                               (make-cell 2 true))
                                         (list (make-cell 2 true)
                                               (make-cell 0 true)))
                                   empty))
              false)


;; * * * D * * * D * * * D * * * D * * *


;; (get-first-unused puzzle) finds the topmost, leftmost cell of 
;;    'puzzle' that isn't marked as used (in that priority)
;; get-first-unused: Grid -> (list Nat Nat)
;; requires: grid contains at least one unused cell
;; Examples
(check-expect (get-first-unused (list (list (make-cell 2 false)
                                            (make-cell 0 false))
                                      (list (make-cell 0 true)
                                            (make-cell 2 true))
                                      (list (make-cell 2 true)
                                            (make-cell 0 true))))
              (list 0 0))
(check-expect (get-first-unused (list (list (make-cell 2 true)
                                            (make-cell 0 true))
                                      (list (make-cell 0 true)
                                            (make-cell 2 true))
                                      (list (make-cell 2 true)
                                            (make-cell 0 false))))
              (list 1 2))

(define (get-first-unused puzzle)
  (local
    [;; (get-first-row/acc y my-grid) produces the first row of 'my-grid' that
     ;;    doesn't have all cells used, where 'y' is the accumulating row num
     ;; get-first-row/acc: Nat Grid -> (listof Cell)
     (define (get-first-row/acc y my-grid)
       (cond
         [(andmap (lambda (x) (cell-used? x))
                  (first my-grid))
          (get-first-row/acc (add1 y) (rest my-grid))]
         [else ;; an unused val in the row
          (get-first-col/acc 0 y (first my-grid))]))
     ;; (get-first-col/acc x y row) produces the coordinates of the first
     ;;    false value in 'row', where 'x' is the current number being checked
     ;;    and y is the row number
     ;; get-first-col/acc: Nat Nat (listof Cell) -> (list Nat Nat)
     (define (get-first-col/acc x y row)
       (cond
         [(cell-used? (first row))
          (get-first-col/acc (add1 x) y (rest row))]
         [else ;; found it
          (list x y)]))]
    (get-first-row/acc 0 puzzle)))

;; Tests
(check-expect (get-first-unused (list (list (make-cell 1 false))))
              (list 0 0))
(check-expect (get-first-unused (list (list (make-cell 2 true)
                                            (make-cell 0 true))
                                      (list (make-cell 0 false)
                                            (make-cell 2 true))
                                      (list (make-cell 2 true)
                                            (make-cell 0 true))))
              (list 0 1))


;; * * * E * * * E * * * E * * * E * * *

(define 0f (make-cell 0 false))
(define 0t (make-cell 0 true))
(define unsolved-puzzle
  (make-state (list (list 0t 0t 0f 0f 0f (make-cell 4 false))
                    (list 0t (make-cell 2 true) (make-cell 6 false) 0f 0f 0f)
                    (list (make-cell 3 true) 0f 0f 0f 0f 0f (make-cell 4 false))
                    (list 0f 0f 0f (make-cell 2 false) 0f 0f)
                    (list 0f 0f 0f 0f (make-cell 10 false) 0f)
                    (list (make-cell 3 false) 0f 0f 0f 0f 0f))
              (list (make-rect 0 0 1 3)
                    (make-rect 1 0 1 2))))
                                          
                         

;; (neighbours my-state) produces a list of legitimate states that could follow
;;    from 'my-state'
;; neighbours: State -> (listof State)
;; requires: there must be at least one unused cell
;; Examples
(check-expect
 (neighbours unsolved-puzzle)
 (list
  (make-state
   (list (list 0t 0t 0t 0t 0t (make-cell 4 true))
         (list 0t (make-cell 2 true) (make-cell 6 false) 0f 0f 0f)
         (list (make-cell 3 true) 0f 0f 0f 0f 0f (make-cell 4 false))
         (list 0f 0f 0f (make-cell 2 false) 0f 0f)
         (list 0f 0f 0f 0f (make-cell 10 false) 0f)
         (list (make-cell 3 false) 0f 0f 0f 0f 0f))
   (list (make-rect 2 0 4 1)
         (make-rect 0 0 1 3)
         (make-rect 1 0 1 2)))  
  (make-state
   (list (list 0t 0t 0t 0t 0t (make-cell 4 false))
         (list 0t (make-cell 2 true) (make-cell 6 true) 0t 0t 0f)
         (list (make-cell 3 true) 0f 0f 0f 0f 0f (make-cell 4 false))
         (list 0f 0f 0f (make-cell 2 false) 0f 0f)
         (list 0f 0f 0f 0f (make-cell 10 false) 0f)
         (list (make-cell 3 false) 0f 0f 0f 0f 0f))
   (list (make-rect 2 0 3 2)
         (make-rect 0 0 1 3)
         (make-rect 1 0 1 2)))
  (make-state
   (list (list 0t 0t 0t 0t 0f (make-cell 4 false))
         (list 0t (make-cell 2 true) (make-cell 6 true) 0t 0f 0f)
         (list (make-cell 3 true) 0f 0t 0t 0f 0f (make-cell 4 false))
         (list 0f 0f 0f (make-cell 2 false) 0f 0f)
         (list 0f 0f 0f 0f (make-cell 10 false) 0f)
         (list (make-cell 3 false) 0f 0f 0f 0f 0f))
   (list (make-rect 2 0 2 3)
         (make-rect 0 0 1 3)
         (make-rect 1 0 1 2)))
  (make-state
   (list (list 0t 0t 0t 0f 0f (make-cell 4 false))
         (list 0t (make-cell 2 true) (make-cell 6 true) 0f 0f 0f)
         (list (make-cell 3 true) 0f 0t 0f 0f 0f (make-cell 4 false))
         (list 0f 0f 0t (make-cell 2 false) 0f 0f)
         (list 0f 0f 0t 0f (make-cell 10 false) 0f)
         (list (make-cell 3 false) 0f 0t 0f 0f 0f))
   (list (make-rect 2 0 1 6)
         (make-rect 0 0 1 3)
         (make-rect 1 0 1 2)))))
(check-expect (neighbours (make-state (list (list (make-cell 2 true)
                                                  (make-cell 0 true))
                                            (list (make-cell 0 true)
                                                  (make-cell 3 true))
                                            (list (make-cell 0 false)
                                                  (make-cell 0 true)))
                                      (list (make-rect 0 0 2 1)
                                            (make-rect 0 1 2 1))))
              empty)

(define (neighbours my-state)
  (local
    [(define my-grid (state-grid my-state))
     (define top-coords (get-first-unused my-grid))
     (define top-x (first top-coords))
     (define top-y (second top-coords))
     (define sub-w (- (length (first my-grid)) top-x))
     (define sub-h (- (length my-grid) top-y))
     
     ;; (new-states lorect) produces a list of states each with a different
     ;;    valid rectangle added and the appropriate cells marked as used
     ;; new-states: (listof Rect) -> (listof States)
     (define (new-states lorect)
       (local
         [;; (new-state new-rect) produces a state the same as the current  
         ;;    state but with an extra rectangle added and the newly used
         ;;    squares marked as so
         ;; new-state: Rect -> State
         (define (new-state new-rect)
            (make-state (mark-as-used my-grid new-rect)
                        (cons new-rect (state-rects my-state))))]
         
         (cond
           [(empty? lorect) empty]
           [else
            (cons (new-state (first lorect))
                  (new-states (rest lorect)))])))
     
     ;; valid-rect/list lorect) returns a list of all valid rectangles from
     ;;    'lorect' on the grid
     ;; valid-rect/list: (listof Rect) -> (listof Rect)
     (define (valid-rect/list lorect)
       (local
         [;; (valid-rect? my-rect) returns true if my-rect is a valid 
         ;;    addition to my-grid
         ;; valid-rect?: Rect -> Bool
         (define (valid-rect? my-rect)
            (local
              [(define my-section (sub-rect my-grid my-rect))]
              (and (all-cells-unused? my-section)
                   (one-number? my-section)
                   (number-equals-area? my-section))))]
         (filter (lambda (x) (valid-rect? x)) lorect)))
     
     ;; (all-rectangles x y w h) produces a list of rectangles of every possible
     ;;    dimension within the width 'w', height 'h', and starting at (x, y)
     ;; all-rectangles: Nat Nat Nat Nat -> (listof Rect)
     (define (all-rectangles x y w h)
       (foldr (lambda (lst flat-lst)
                (append lst flat-lst))
              empty
              (build-list h (lambda (new-h)
                              (build-list w (lambda (new-w)
                                              (make-rect x y
                                                         (add1 new-w)
                                                         (add1 new-h))))))))]
    
    (new-states (valid-rect/list (all-rectangles top-x
                                                 top-y
                                                 sub-w
                                                 sub-h)))))

;; Tests
(check-expect (neighbours (make-state (list (list (make-cell 2 true)
                                                  (make-cell 0 true))
                                            (list (make-cell 0 false)
                                                  (make-cell 2 false)))
                                      (list (make-rect 0 0 2 1))))
              (list (make-state (list (list (make-cell 2 true)
                                            (make-cell 0 true))
                                      (list (make-cell 0 true)
                                            (make-cell 2 true)))
                                (list (make-rect 0 1 2 1)
                                      (make-rect 0 0 2 1)))))
(check-expect (neighbours (make-state (list (list (make-cell 2 true)
                                                  (make-cell 0 true))
                                            (list (make-cell 0 false)
                                                  (make-cell 2 false))
                                            (list (make-cell 2 false)
                                                  (make-cell 0 false)))
                                      (list (make-rect 0 0 2 1))))
              (list (make-state (list (list (make-cell 2 true)
                                            (make-cell 0 true))
                                      (list (make-cell 0 true)
                                            (make-cell 2 true))
                                      (list (make-cell 2 false)
                                            (make-cell 0 false)))
                                (list (make-rect 0 1 2 1)
                                      (make-rect 0 0 2 1)))
                    (make-state (list (list (make-cell 2 true)
                                            (make-cell 0 true))
                                      (list (make-cell 0 true)
                                            (make-cell 2 false))
                                      (list (make-cell 2 true)
                                            (make-cell 0 false)))
                                (list (make-rect 0 1 1 2)
                                      (make-rect 0 0 2 1)))))
(check-expect
 (lists-equiv? 
  (neighbours
   (make-state (list (list 0f 0f (make-cell 3 false) 0f)
                     (list 0f (make-cell 4 false) 0f (make-cell 2 false))
                     (list (make-cell 3 false) 0f 0f 0f))
               empty))
  (list (make-state (list (list 0t 0t (make-cell 3 true) 0f)
                          (list 0f (make-cell 4 false) 0f (make-cell 2 false))
                          (list (make-cell 3 false) 0f 0f 0f))
                    (list (make-rect 0 0 3 1)))
        (make-state (list (list 0t 0t (make-cell 3 false) 0f)
                          (list 0t (make-cell 4 true) 0f (make-cell 2 false))
                          (list (make-cell 3 false) 0f 0f 0f))
                    (list (make-rect 0 0 2 2)))
        (make-state (list (list 0t 0f (make-cell 3 false) 0f)
                          (list 0t (make-cell 4 false) 0f (make-cell 2 false))
                          (list (make-cell 3 true) 0f 0f 0f))
                    (list (make-rect 0 0 1 3)))))
 true)


;; (mark-as-used my-grid rectangle) produces a grid identical to 'my-grid'
;;    except with the cells in the area covered by 'rectangle' marked as used
;; mark-as-used: Grid Rect -> Grid
;; Examples
(check-expect (mark-as-used (list (list (make-cell 0 false)
                                        (make-cell 0 false))
                                  (list (make-cell 6 false)
                                        (make-cell 0 false))
                                  (list (make-cell 0 false)
                                        (make-cell 0 false)))
                            (make-rect 1 0 1 2))
              (list (list (make-cell 0 false)
                          (make-cell 0 true))
                    (list (make-cell 6 false)
                          (make-cell 0 true))
                    (list (make-cell 0 false)
                          (make-cell 0 false))))
(check-expect (mark-as-used (list (list (make-cell 0 false)
                                        (make-cell 0 false))
                                  (list (make-cell 6 false)
                                        (make-cell 0 false))
                                  (list (make-cell 0 false)
                                        (make-cell 0 false)))
                            (make-rect 1 0 0 0))
              (list (list (make-cell 0 false)
                          (make-cell 0 false))
                    (list (make-cell 6 false)
                          (make-cell 0 false))
                    (list (make-cell 0 false)
                          (make-cell 0 false))))
(check-expect (mark-as-used (list (list (make-cell 0 false)
                                        (make-cell 0 false))
                                  (list (make-cell 6 true)
                                        (make-cell 0 true))
                                  (list (make-cell 0 false)
                                        (make-cell 0 false)))
                            (make-rect 0 1 2 2))
              (list (list (make-cell 0 false)
                          (make-cell 0 false))
                    (list (make-cell 6 true)
                          (make-cell 0 true))
                    (list (make-cell 0 true)
                          (make-cell 0 true))))

(define (mark-as-used my-grid rectangle)
  (local
    [;; (mark-rows x y w h my-grid) produces a grid with the area betwen (x, y)
     ;;    and ((x+w), (y+h)) marked as used
     ;; mark-rows: Nat Nat Nat Nat Grid -> Grid
     (define (mark-rows x y w h my-grid)
       (cond
         [(empty? my-grid) empty]
         [(and (zero? y) (> h 0))
          (cons (mark-elems x w (first my-grid))
                (mark-rows x 0 w (sub1 h) (rest my-grid)))]
         [else (cons (first my-grid)
                     (mark-rows x (sub1 y) w h (rest my-grid)))]))

     ;; (mark-elems x w my-row) produces a list containing all of the
     ;;    elements from 'my-row' except with the elements from index
     ;;    x to index x + w marked as used
     ;; select-elems-from-row: Nat Nat (listof X) -> (listof X)
     (define (mark-elems x w my-row)
       (cond
         [(empty? my-row) empty]
         [(and (zero? x) (> w 0))
          (cons (make-cell (cell-num (first my-row)) true)
                (mark-elems 0 (sub1 w) (rest my-row)))]
         [else (cons (first my-row)
                     (mark-elems (sub1 x) w (rest my-row)))]))]
    
    (mark-rows (rect-x rectangle)
               (rect-y rectangle)
               (rect-w rectangle)
               (rect-h rectangle)
               my-grid)))


;; (sub-rect table rectangle) produces a table containing only the elements from
;;    'table' that are within the coordinates of 'rectangle'
;; sub-rect: Table Rect -> Table
;; Examples
(check-expect (sub-rect '((1 2 3) (4 5 6)) (make-rect 0 0 2 2))
              (list (list 1 2) (list 4 5)))
(check-expect (sub-rect '((1 2 3) (4 5 6)) (make-rect 1 1 2 1))
              (list (list 5 6)))
(check-expect (sub-rect '((1 2 3) (4 5 6)) (make-rect 0 0 10 10))
              '((1 2 3) (4 5 6)))
(check-expect (sub-rect '((1 2 3) (4 5 6)) (make-rect 0 0 0 0))
              empty)
(check-expect (sub-rect empty (make-rect 0 0 3 4))
              empty)
(check-expect (sub-rect (list empty empty) (make-rect 0 0 3 4))
              (list empty empty))

(define (sub-rect table rectangle)
  (local
    [;; (select-rows-from-table x y w h my-table) produces a table containing
     ;;    only the elements from my-table that are betwen (x,y) and (x+w, y+h)
     ;; select-rows-from-grid: Nat Nat Nat Nat Table -> Table
     (define (select-rows-from-table x y w h my-table)
       (cond
         [(or (empty? my-table) (zero? h))
          empty]
         [(zero? y)
          (cons (select-elems-from-row x w (first my-table))
                (select-rows-from-table x 0 w (sub1 h) (rest my-table)))]
         [else (select-rows-from-table x (sub1 y) w h (rest my-table))]))

     ;; (select-elems-from-row x w my-row) produces a list containing only the
     ;;    elements from 'my-row' beginning with index x and ending with index w
     ;; select-elems-from-row: Nat Nat (listof X) -> (listof X)
     (define (select-elems-from-row x w my-row)
       (cond
         [(or (empty? my-row) (zero? w))
          empty]
         [(zero? x)
          (cons (first my-row)
                (select-elems-from-row 0 (sub1 w) (rest my-row)))]
         [else (select-elems-from-row (sub1 x) w (rest my-row))]))]
    
    (select-rows-from-table (rect-x rectangle)
                            (rect-y rectangle)
                            (rect-w rectangle)
                            (rect-h rectangle)
                            table)))


;; (one-number? my-grid) produces true if there is exactly one
;;    cell with a numerical value in the given 'my-grid'
;; one-number?: Grid -> Bool
;; Examples
(check-expect (one-number? (list (list (make-cell 2 true)
                                       (make-cell 0 true))))
              true)
(check-expect (one-number? (list (list (make-cell 2 true))))
              true)
(check-expect (one-number? (list (list (make-cell 2 true)
                                       (make-cell 0 true))
                                 (list (make-cell 0 false)
                                       (make-cell 2 true))
                                 (list (make-cell 2 true)
                                       (make-cell 0 true))))
              false)
(check-expect (one-number? (list (list (make-cell 0 true)
                                       (make-cell 0 true))
                                 (list (make-cell 0 false)
                                       (make-cell 0 true))
                                 (list (make-cell 0 true)
                                       (make-cell 0 true))))
              false)
(check-expect (one-number? (list (list (make-cell 0 false)
                                       (make-cell 0 false))
                                 (list (make-cell 6 false)
                                       (make-cell 0 false))
                                 (list (make-cell 0 false)
                                       (make-cell 0 false))))
              true)

(define (one-number? my-grid)
  (= 1 (foldr (lambda (w x)
                (+ (foldr (lambda (y z)
                            (cond
                              [(not (zero? (cell-num y)))
                               (+ 1 z)]
                              [else z]))
                          0 w) x))
              0 my-grid)))


;; (all-cells-unused? my-grid) produces true if all cells in
;;     'my-grid' are unused
;; all-cells-unused?: Grid -> Bool
;; Examples
(check-expect (all-cells-unused? (list (list (make-cell 2 false))
                                       (list (make-cell 0 false))))
              true)
(check-expect (all-cells-unused? (list (list (make-cell 2 true))))
              false)
(check-expect (all-cells-unused? (list (list (make-cell 2 false)
                                             (make-cell 0 false))
                                       (list (make-cell 0 false)
                                             (make-cell 2 false))))
              true)
(check-expect (all-cells-unused? (list (list (make-cell 2 true)
                                             (make-cell 0 false))
                                       (list (make-cell 0 false)
                                             (make-cell 2 true))
                                       (list (make-cell 2 true)
                                             (make-cell 0 true))))
              false)

(define (all-cells-unused? my-grid)
  (not (ormap (lambda (x)
                (ormap (lambda (y) (cell-used? y)) x))
              my-grid)))


;; (number-equals-area? my-grid) returns true if the non-zero cell value
;;    in 'my-grid' is equal to the area of the grid
;; number-equals-area?: Grid -> Bool
;; Examples
(check-expect (number-equals-area? (list (list (make-cell 2 true)
                                               (make-cell 0 true))))
              true)
(check-expect (number-equals-area? (list (list (make-cell 2 true))))
              false)
(check-expect (number-equals-area? (list (list (make-cell 2 true)
                                               (make-cell 0 true))
                                         (list (make-cell 0 false)
                                               (make-cell 0 true))
                                         (list (make-cell 0 true)
                                               (make-cell 0 true))))
              false)
(check-expect (number-equals-area? (list (list (make-cell 1 true))))
              true)
(check-expect (number-equals-area? (list (list (make-cell 0 false)
                                               (make-cell 0 false))
                                         (list (make-cell 6 false)
                                               (make-cell 0 false))
                                         (list (make-cell 0 false)
                                               (make-cell 0 false))))
              true)

(define (number-equals-area? my-grid)
  (local
    [;; (get-num my-grid) produces the non-zero cell value from 'my-grid'
     ;; get-num: Grid -> Nat
     ;; requires: my-grid must contain exactly one non-zero cell value
     (define (get-num my-grid)
       (local
         [;; (get-num/row lst) produces the non-zero cell value from 'lst'
          ;;    if it exists, otherwise it returns false
          ;; get-num/row: (listof Cell) -> (anyof Nat false)
          (define (get-num/row lst)
            (cond
              [(empty? lst) false]
              [(not (zero? (cell-num (first lst))))
               (cell-num (first lst))]
              [else (get-num/row (rest lst))]))
          
          (define num (get-num/row (first my-grid)))]
         (cond
           [(number? num) num]
           [else (get-num (rest my-grid))])))

     (define area (* (length my-grid) (length (first my-grid))))]
    (= area (get-num my-grid))))



;; * * * F * * * F * * * F * * * F * * *


;; (solve-rectangle-puzzle puzzle) either produces false (if there is no
;;     solution)or the list of rectangles that solves the given puzzle
;; solve-rectangle-puzzle: (listof (listof Nat)) -> (anyof (listof Rect) false)
;; requires: puzzle contains a non-empty list of non-empty lists
;;           all the same length
;; Examples
(check-expect (lists-equiv? (solve-rectangle-puzzle '((0 0 3 0)
                                                      (0 4 0 2)
                                                      (3 0 0 0)))
                            (list (make-rect 0 0 1 3)
                                  (make-rect 1 0 3 1)
                                  (make-rect 1 1 2 2)
                                  (make-rect 3 1 1 2)))
              true)
(check-expect (solve-rectangle-puzzle '((0 5 0)
                                        (2 0 0)))
              false)

(define (solve-rectangle-puzzle puzzle)
  (local
    [(define search-results
       (search solved? neighbours (construct-puzzle puzzle)))]
    (cond
      [(state? search-results)
       (state-rects search-results)]
      [else false])))

;; Tests
(check-expect (lists-equiv? (solve-rectangle-puzzle '((0 0 0 0 0 4)
                                                      (0 2 6 0 0 0)
                                                      (3 2 0 0 0 4)
                                                      (0 0 0 2 0 0)
                                                      (0 0 0 0 10 0)
                                                      (3 0 0 0 0 0)))
                            (list (make-rect 0 0 1 3)
                                  (make-rect 1 0 1 2)
                                  (make-rect 2 0 2 3)
                                  (make-rect 4 0 2 2)
                                  (make-rect 0 3 1 3)
                                  (make-rect 1 2 1 2)
                                  (make-rect 2 3 2 1)
                                  (make-rect 4 2 2 2)
                                  (make-rect 1 4 5 2)))
              true)