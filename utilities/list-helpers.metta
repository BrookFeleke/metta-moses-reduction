;; This file contains the implementation of helper functions for List data structure in MeTTa.
;; The List data structure, unless stated otherwise, is the one defined using `Cons`.

;; Function to calculate the length of a list
(: length (-> (List $t) Number))
(= (length Nil) 0)
(= (length (Cons $x $xs))
    (+ 1 (length $xs))
)

;; Function to retrieve the head of given list and the head of an empty list is empty.
(: (-> (List $t) $t))
(= (head Nil) Nil)
(= (head (Cons $x $xs)) $x)

;; Function to retrieve the tail of given list and tail of an empty list is empty.
;; The tail of a list is the list without the first element.
(: (-> (List $t) (List $t)))
(= (tail Nil) Nil)
(= (tail (Cons $x $xs)) $xs)

;; Function to get an element from a list given an index.
(: index (-> Number (List $t) $t))
(= (index $a Nil) Nil)
(= (index $a (Cons $x $xs))
    (if (== $a 0)
        $x
        (index (- $a 1) $xs)
    )
)

;; Function to apply a function to every element in a list and returns a new updated list.
(: map (-> Atom (List $t) (List $tt)))
(= (map $func Nil) Nil)
(= (map $func (Cons $x $xs))
    (Cons ($func $x) (map $func $xs))
)

;; Function to apply a function to each element in a list and accumulate the results in a list.
;; The list mentioned here isn't the same as the list defined with `Cons` but the `()` atom used
;; in MeTTa to represent a list. It will construct the list by prepending an element to the list using `cons-atom`.
(: consMap (-> Atom $ac (List $t) (List $tt)))
(= (consMap $func $ac Nil) $ac)
(= (consMap $func $ac (Cons $x $xs))
		(let $result ($func $x)
			(consMap $func (cons-atom $result $ac) $xs)
		)
)

;; Function to check the existence of an element in a list given a comparing function.
(: memberWith (-> (-> $t $t Bool) $t (List $t) Bool))
(= (memberWith $f $a Nil) False)
(= (memberWith $f $a (Cons $x $xs))
    (if ($f $a $x)
        True
        (memberWith $f $a $xs)
    )
)

;; Function to check whether an Atom exists in a list.
(: member (-> $t (List $t) Bool))
(= (member $a $list)
    (memberWith == $a $list)
)

;; Function to remove duplicates given a list and a member function.
;; Member Function checks the existence of an element in a list.
;; The function will remove every duplicate occurrence an element.
(: removeDuplicates (-> (-> $t (List $t) Bool) (List $t) (List $t)))
(= (removeDuplicates $memberFunc Nil) Nil)
(= (removeDuplicates $memberFunc (Cons $x $xs))
    (if ($memberFunc $x $xs)
        (removeDuplicates $memberFunc $xs)
        (Cons $x (removeDuplicates $memberFunc $xs))
    )
)

; Function to merge two lists. It will append all the elements of the second list to the end of the first one.
(: extend (-> (List $t) (List $t) (List $t)))
(= (extend Nil $ys) $ys)
(= (extend (Cons $x $xs) $ys) (Cons $x (extend $xs $ys)))


;; A function that checks if at least one True exists from a Boolean list.
;; The function will stop execution as soon as it finds a True value.
;; It will return False if the list is empty.
(: any (-> (List Bool) Bool))
(= (any Nil) False)
(= (any (Cons True $xs)) True)
(= (any (Cons False $xs)) (any $xs))

;; A function that check if all of the elements are True from a Bool List.
;; The function will stop execution as soon as it finds a False value and return False.
;; It will return True if the list is empty.
(: all (-> (List Bool) Bool))
(= (all Nil) True)
(= (all (Cons False $xs)) False)
(= (all (Cons True $xs)) (all $xs))

;; A function to find and remove element from a list.
(: findAndRemove (-> $t (List $t) (List $t)))
(= (findAndRemove $elem Nil) Nil)
(= (findAndRemove $elem (Cons $x $xs))
    (if (== $x $elem) (findAndRemove $elem $xs)
        (Cons $x (findAndRemove $elem $xs)))
)

;; Function to find a given node from a list of nodes and remove it. 
;; Similar to findAndRemove implementation of for the lists except it only works for trees.
(: findAndRemoveTree (-> Tree (List Tree) (List Tree)))
(= (findAndRemoveTree $elem Nil) Nil)
(= (findAndRemoveTree $elem (Cons $x $xs))
    (if (isNodeEqual $x $elem)
        (findAndRemoveTree $elem $xs)
        (Cons $x (findAndRemoveTree $elem $xs))
    )
)

;; A function to compare 2 lists if they are identical. It assumes the lists are sorted.
(: compare (-> (List $t) (List $t) (Bool)))
(= (compare Nil Nil) True)
(= (compare (Cons $x $xs) (Cons $y $ys))
    (if (and (not (== $xs $ys)) (or (== $xs Nil) (== $ys Nil)))
        False
        (if (== $x $y)
            (compare $xs $ys)
            False
        )
    )
)

;; Function to partition the list. 
;; This function will split the list into two parts based on a given pivot element.
(: partition (-> Atom (List $t) ( (List $t) (List $t) )))
(= (partition $_ Nil) ( Nil Nil ))
(= (partition $pivot (Cons $x $xs))
    (let ($left $right) (partition $pivot $xs)
        (let*
            (
              ($cond  (<= $x $pivot))
              ( () (println! ($x "<= " $pivot "= " $cond)))
              ( () (println! ($left "---" $right)))
            )
            (if $cond
                ( (Cons $x $left) $right)
                ($left (Cons $x $right))
            )
        )
    )
)

;; QuickSort implementation
(: quickSort (-> (List $t) (List $t)))
(= (quickSort Nil) Nil)
(= (quickSort (Cons $x $xs))
    (let ($left $right) (partition $x $xs)
        (extend (quickSort $left) (Cons $x (quickSort $right)))
    )
)

;; Function to check if the list doesn't contain duplicate element.
(: checkNotDuplicated (-> (List $t) Bool))
(= (checkNotDuplicated $list)
    (case $list
      (
        (Nil True)
        ( (Cons $x $xs)
          (if (member $x $xs)
              False
              (checkNotDuplicated $xs)
          )
        )
      )
    )
)

;; Function to check if an element isn't duplicated in the rest of the list.
(: checkNotInverseDuplicated (-> (List $t) Bool))
(= (checkNotInverseDuplicated $list)
    (case $list
        (
          (Nil True)
          ( (Cons $x $xs)
            (if (member-not $x $xs)
                False
                (checkNotInverseDuplicated $xs)
            )
          )
        )
    )
)

;; A function that takes two lists and returns their common elements.
(: intersect (-> (List $t) (List $t) (List $t) (List $t)))
(= (intersect Nil Nil $accumulator) $accumulator)
(= (intersect Nil (Cons $y $ys) $accumulator) $accumulator)
(= (intersect (Cons $x $xs) Nil $accumulator) $accumulator)
(= (intersect (Cons $x $xs) (Cons $y $ys) $accumulator)
    (if (member $x (Cons $y $ys))
        (intersect $xs (Cons $y $ys) (extend $accumulator (Cons $x Nil)))
        (intersect $xs (Cons $y $ys) $accumulator)
    )
)

;; Function that takes two lists, an accumulator and a member function then returns 
;; the accumulator containing elements from the first list that 
;; are not found in the second list.
(: setDifference (-> (-> $t (List $t) Bool) (List $t) (List $t) (List $t) (List $t)))
(= (setDifference $memberFunc Nil Nil $accumulator) Nil)
(= (setDifference $memberFunc (Cons $x $xs) Nil $accumulator) (Cons $x $xs))
(= (setDifference $memberFunc Nil (Cons $y $ys) $accumulator) $accumulator)
(= (setDifference $memberFunc (Cons $x $xs) (Cons $y $ys) $accumulator)
    (if ($memberFunc $x (Cons $y $ys))
        (setDifference $memberFunc $xs (Cons $y $ys) $accumulator)
        (setDifference $memberFunc $xs (Cons $y $ys) (extend $accumulator (Cons $x Nil)))
    )
)

;; Funcction that takes two lists and member function then
;; returns a new list with elements from both lists included 
;; while keeping each element unique.
(: setUnion (-> (-> $t (List $t) Bool) (List $t) (List $t) (List $t)))
(= (setUnion $memberFunc $list1 $list2)
    (removeDuplicates $memberFunc (extend $list1 $list2))
)

(: isSubset (-> (List $t) (List $t) Bool))
(= (isSubset $list1 $list2)
    (let* 
      (
        ($min-list
          (if (> (length $list1) (length $list2)) $list2 $list1)
        )
        ($max-list 
            (if (> (length $list1) (length $list2)) $list1 $list2)
        )
      )
      (isSubsetOrdered $min-list $max-list)
    )
)

(: isSubsetOrdered (-> (List $t) (List $t) Bool))
(= (isSubsetOrdered $min-list $max-list)
    (if (== $min-list Nil)
        True
        (case $min-list 
          (
            ( (Cons $x $xs)
              (if (member $x $max-list)
                  (isSubsetOrdered $xs $max-list)
                  False
              )
            )
          )
        )
    )
)

;; A function that takes a list of trees and returns a boolean value indicating 
;; whether there exists an AND node with no children.
(: containsTerminalAndNode (-> (List Tree) Bool))
(= (containsTerminalAndNode Nil) False)
(= (containsTerminalAndNode (Cons $x $xs))
    (case $x
        (
            ( (TreeNode (Value $value $truthValue AND) (Cons $y Nil) Nil) True)
            ( (TreeNode $nodeValue $guardSet $children) (containsTerminalAndNode $xs))
        )
    )
)

;; A function that takes a binary function, an initial accumulator value and a list
;; and flattens the list into a single value by applying the function recursively.
(: foldLeft (-> (-> $t $t $t) $t (List $t) $t))
(= (foldLeft $func $acc Nil) $acc)
(= (foldLeft $func $acc (Cons $x $xs)) (foldLeft $func ($func $acc $x) $xs))

;; An Abstract function to call foldLeft function and pass the first element as the initial accumulator (does not require an accumulator)
(: foldl (-> $t $t $t) (List $t) $t)
(= (foldl $func Nil)Nil)
(= (foldl $func (Cons $x $xs))
    (foldLeft $func $x $xs)
)

;; Function to remove an element at a given index from a List.
(: removeAtIndex (-> Number (List $t) (List $t)))
(= (removeAtIndex $a Nil) Nil)
(= (removeAtIndex $a (Cons $x $xs))
    (if (== $a 0)
         $xs
        (Cons $x (removeAtIndex (- $a 1) $xs))
    )
)

;; A Function to replace an element at a given index in a List.
(: replaceAtIndex (-> Number $t (List $t) (List $t)))
(= (replaceAtIndex $a $t Nil) Nil)
(= (replaceAtIndex $a $t (Cons $x $xs))
    (if (== $a 0)
        (Cons $t  $xs)
        (Cons $x (replaceAtIndex (- $a 1) $t $xs))
    )
)
