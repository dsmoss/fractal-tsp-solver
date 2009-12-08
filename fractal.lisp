;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                        ;;;;
;;;; Generates my fractal (snowflake) path finding (TSP) solution           ;;;;
;;;;                                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *grid-width* 100000)
(defvar *grid-heigth* 100000)
(defvar *max-number-of-points* 1000)

;;; These make up the whole of the search area, which makes the centre regular
 ;; and allows for the snowflake to form properly.
 ;; the idea is as follows:
 ;; |
 ;; |\
 ;; | \
 ;; |  \
 ;; |   \
 ;; |____+
 ;; |####|\
 ;; |####| \
 ;; |####|  \
 ;; +####|___\
(defvar *search-area-width* (* 2 *grid-width*))
(defvar *search-area-heigth* (* 2 *grid-heigth*))

(defun make-random-point (max-x max-y)
    "makes a point in a random position of the grid"
    (complex
        (/ (random (* max-x 1000)) 1000)
        (/ (random (* max-y 1000)) 1000)))


(defun make-random-point-list (max-len)
    "makes a list of random points up to max-len length"
     (let   ((value ()))    ; Make a set of random points
         (dotimes (n (random max-len) value)
            (setq value
                (cons (make-random-point *grid-width* *grid-heigth*) value)))))

(defun get-printable-point-position (point)
    "Gets a rounded-off point that can be used to make a dot on a visual grid"
    (complex
        (round (realpart point))
        (round (imagpart point))))

(defun euclid (point-a point-b)
    "calculates the euclidean distance in between two points"
    (let*
        ((p (- point-a point-b)))
        (sqrt
            (+  (expt (realpart p) 2)
                (expt (imagpart p) 2)))))

(defstruct triangle
    "A triangle consists of 3 points.
    Complex numbers are used to construct the points,
    the real part signifying the X axis,
    and the imaginary part signifying the Y axis."
    a b c)

(defun avg (&rest numbers)
    "Gets the average of the numbers provided"
    (if
        (null numbers) 1 ; prevents divide by 0
        (/ (apply #'+ numbers) (length numbers))))

(defun get-triangle-centre (triangle) 
    "Gets the centre of a triangle"
    (avg (triangle-a triangle)
         (triangle-b triangle)
         (triangle-c triangle)))

(defstruct (triangle-list
        (:include triangle))
    point-list)

(defun triangle-split (triangle)
    "Splits a triangle in two according to the rule:
    { a0->c1; b0->a1,c2; c0->a2; avg(c0,a0)->b1,b2 }"
    (let*  ((old-a-point (triangle-a triangle))
            (old-b-point (triangle-b triangle))
            (old-c-point (triangle-c triangle))
            (new-b-point (avg old-a-point old-c-point)))
        (list
            (make-triangle-list :a old-b-point :b new-b-point :c old-a-point)
            (make-triangle-list :a old-c-point :b new-b-point :c old-b-point))))

(defun triangle-list-split (triangle-list)
    "Split a triangle list and acomodate all the points in their right places"
    (let*  ((triangles  (triangle-split triangle-list))
            (triangle-a (car triangles))
            (triangle-b (cadr triangles))
            (centre-a   (get-triangle-centre triangle-a))
            (centre-b   (get-triangle-centre triangle-b)))
        (dolist (point (triangle-list-point-list triangle-list))
            (if (< (euclid point centre-a) (euclid point centre-b))
                (setf (triangle-list-point-list triangle-a)
                    (cons point (triangle-list-point-list triangle-a)))
                (setf (triangle-list-point-list triangle-b)
                    (cons point (triangle-list-point-list triangle-b)))))
        (let   ((list-a (triangle-list-point-list triangle-a))
                (list-b (triangle-list-point-list triangle-b)))
            (if (= 1 (length list-a))
                (setf (triangle-list-point-list triangle-a) (car list-a)))
            (if (= 1 (length list-b))
                (setf (triangle-list-point-list triangle-b) (car list-b))))
        (list triangle-a triangle-b)))

(defun print-point (out point &rest args)
    "Utility function - Pretty-prints a point"
    (format out "(X:~F, Y:~F)"
        (realpart point)
        (imagpart point))
    args)

(defun pprint-triangle-list (out triangle-list &rest args)
    "Utility function - Pretty-prints a triangle-list object"
    (format out "   TRIANGLE{
        A:~/print-point/
        B:~/print-point/
        C:~/print-point/
        CENTRE:~/print-point/
        POINTS:{~{~/print-point/~^,~%                ~}}~&    }"
        (triangle-a triangle-list)
        (triangle-b triangle-list)
        (triangle-c triangle-list)
        (get-triangle-centre triangle-list)
        (let   ((points (triangle-list-point-list triangle-list)))
            (cond
                ((null points)  ())
                ((listp points) points)
                (t (list points)))))
    args)

(defun print-list-of-triangle-list (lst)
    "Pretty-prints a list of triangle-list objects"
    (format t "(~{~/pprint-triangle-list/~^,~% ~}~&)" lst))

(defun explode (lst)
    "explodes a triangle-list list and gets all
    the points in the order they should be"
    (let ((l (flatten lst)))
        (cond
            ((null l) ())
            ((triangle-list-p l) (explode (triangle-list-split l)))
            ((null (triangle-list-point-list (car l)))
                (explode (cdr l)))
            ((atom (triangle-list-point-list (car l)))
                (cons (car l) (explode (cdr l))))
            (t  (explode (append (triangle-list-split (car l)) (cdr l)))))))


(defun flatten (lst)
    "Flattens a list (removes nesting and nulls)"
    (cond
        ((atom lst) lst)
        ((listp (car lst))
            (append (flatten (car lst)) (flatten (cdr lst))))
        (t  (append (list (car lst)) (flatten (cdr lst))))))

(let   ((triangle (make-triangle-list 
                :a          (complex 0 *search-area-heigth*) 
                :b          0 
                :c          *search-area-width*
                :point-list (make-random-point-list *max-number-of-points*))))
    (print-list-of-triangle-list (explode triangle)))

