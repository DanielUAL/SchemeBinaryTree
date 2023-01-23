;Daniel Najafi - 001467739
;ICSI 311 - Principles of Programming Languages
;Qi Wang - Teacher 
;Scheme - BinarySearchTree - Project 4
;

;Lambda is used to set possible parameters. This list has no parameters since
;the defintion is for an Empty binary search tree. This program will return
;three sets of empty paranthesis. 
(define BSTempty
  (lambda ()
    (list '() '() '())))

;define a headBST.
;This defintion will allow for the user to input one value, this one value is the head of the tree. The output will be whatever the user
;inputted as a parameter, then two empty parentheses to represent the empty left and right side.
(define headBST
  (lambda (headinformation)
    (if (list? headinformation)  ;checks if headinformation is a list
        (list headinformation)   ;If headinformation is a list, then assciate it with the list value
        (list headinformation '() '())))) ;This is the else statement. Sets the list information as a value with two empty parameters.

;This defines a very basic binary search tree. This definition can take three parameters, making a full tree.
;Scheme does not have a traditional way to make a tree, so making a list is the only way to do so. The information is needed to know
;since it settles if upcoming values will be going to the left or right side. This function makes the  left and rightside their own
;BST trees. This is needed since they are simply lists in scheme, so they are essentially entirely new right and left trees on either side of the head.
(define BST
  (lambda (information leftBST rightBST) ;parameters of the values that will be used in this definition
    (list information leftBST rightBST))) ;Each given user paramter will be outputted as each of these values inside of a list

;this gets the leftBST value
(define getleftBST (lambda (tree)   ;this gets leftBST. Parameter is tree
 (cadr tree))) ;cadr gets everything after the first element in a list
;this gets the rightBST
(define getrightBST (lambda (tree)   ;this gets rightBST. Parameter is tree
 (caddr tree))) ;caddr gets everything after the second element in a list
;this gets the information
(define getinformation (lambda (tree) ;this gets information. Parameter is tree
 (car tree))) ;gets the first element

;sets the leftBST
(define setleftBST
  (lambda (leftsetter tree) ;params are leftsetter and the tree
    (list (getinformation tree) leftsetter (getrightBST tree)))) ;This will change the BST's leftvalue to be a new user inputted value. And keep the right and head the same

;sets the rightBST
(define setrightBST
  (lambda (rightsetter tree) ;params are the rightsetter and the tree
    (list (getinformation tree) (getleftBST tree) rightsetter))) ;This will change the BST's rightvalue to be a new user inputted value. And keep the left and head the same

;This checks if a given BST list is empty or not. This code uses lambda BST to establish
;the parameters. 
(define isEmpty ;isEmpty
  (lambda (BST) ;parameter BST
    (cond ((NULL? BST) #true) ;uses cond which is essentially a switch case. If BST is null, true is printed
          (else #false))))  ;if the above conditon is not true, it is false.

;search
;This will search through the tree to see if a given number exists in the tree or not
(define searchAHH
  (lambda (number tree) ;two parameters with number and tree
    (if(null? tree) ;when the tree is null, false is printed. Since if the tree is empty, the number cant exist
       #false
       (if(and(list? tree) (not (null? ( getinformation tree ))))
          (if(= number (getinformation tree)) ;this will use the getter for information to check if the number is equal to the information          
             #true
             (if(> number (getinformation tree));if number is greater than the head, check right
                (searchAHH number (getrightBST tree)) ;search is recursively used to tranverse the right tree
                (if(< number (getinformation tree)) ;if number is less than the head, check left
                   (searchAHH number (getleftBST tree))))))))) ;search is recursively used to tranverse the left tree

;insert
;This will insert values into the BST
(define insert
  (lambda (number tree) ;parameters are number and tree
    (if(isEmpty tree)  ;checks the isEmpty condition
       (headBST number) ;puts the inputted number as the headnode
       (if(searchAHH number tree) ;searches to see if the number is in the tree
          tree        ;if true
          (if(and (< number (getinformation tree)) (not (list? getleftBST))) ;checks if the left side is not a list, and if the number is less than the given 60
              (setleftBST (headBST number) tree) ;this inserts from the left side
              (setrightBST (headBST number) tree)))))) ;this inserts from the right side
                     
;inorder
;This code will take the given numbers and put it in increasing order. To do this, the code will
;recursively call the inorder function for when the getters left and right are used. And only calling
;the list when only getinformation is needed. 
(define inorder
  (lambda (tree) ;Sets parameter tree for this definition
    (if (not (null? tree))  ;if --> the tree is --> not --> null then append will happen. Otherwise then the base casse '() is used
        (append (if(list? tree) ;checks if the tree is a list
                   (inorder (getleftBST tree)) ;uses the getter and recursion to get the value of leftBST in tree
                   '()) ;base case
                (if(list? tree) ;checks if the tree is a list
                   (list (getinformation tree)) ;only needs to u se the information getter and list for the head value
                   '()) ;base case
                (if(list? tree) ;checks if the tree is a list
                   (inorder (getrightBST tree))  ;uses the getter and recursion to get the value of rightBST in tree
                   '())) ;base case
        '() ))) 

;postorder
;This code will take the given numbers and put it in post order. To do this, the code will
;recursively call the postorder function for when the getters left and right are used. And only calling
;the list when only getinformation is needed. 
(define postorder
  (lambda (tree)  ;Sets parameter tree for this definition
    (if (not (null? tree))  ;if --> the tree is --> not --> null then append will happen. Otherwise then the base casse '() is used
        (append (if(list? tree)  ;checks if the tree is a list
                   (postorder (getleftBST tree)) ;uses the getter and recursion to get the value of leftBST in tree
                   '()) ;base case
                (if(list? tree) ;checks if the tree is a list
                   (postorder (getrightBST tree)) ;uses the getter and recursion to get the value of rightBST in tree
                   '()) ;base chase
                (if(list? tree) ;checks if the tree is a list
                   (list (getinformation tree)) ;only needs to u se the information getter and list for the head value
                   '()))
        '() )))

;preorder
;This code will take the given numbers and put it in pre order. To do this, the code will
;recursively call the preorder function for when the getters left and right are used. And only calling
;the list when only getinformation is needed. 
(define preorder
  (lambda (tree)  ;Sets parameter tree for this definition
    (if (not (null? tree))  ;if --> the tree is --> not --> null then append will happen. Otherwise then the base casse '() is used
        (append (if(list? tree)  ;checks if the tree is a list
                   (list (getinformation tree));only needs to u se the information getter and list for the head value
                   '()) ;base case
                (if(list? tree) ;checks if the tree is a list
                   (preorder (getleftBST tree)) ;uses the getter and recursion to get the value of leftBST in tree
                   '()) ;base case 
                (if(list? tree) ;checks if the tree is a list
                   (preorder (getrightBST tree)) ;uses the getter and recursion to get the value of rightBST in tree
                   '()));base case
        '() )))     

;Helper
(BSTempty)
(headBST 60)
(headBST 44)

(BST 60 20 70)
(BST 66 22 77)
;getters
(getleftBST (BST 60 20 70))
(getleftBST (BST 42 8 43))

(getrightBST (BST 60 20 70))
(getrightBST (BST 42 8 43))

(getinformation (BST 60 20 70))
(getinformation (BST 42 8 43))

;setters
(setleftBST (list 40 '() '()) (BST 60 (list 20 '() '()) (list 70 '() '())))
(setleftBST (list 88 '() '()) (BST 42 (list 22 '() '()) (list 52 '() '())))

(setrightBST (list 40 '() '()) (BST 60 (list 20 '() '()) (list 70 '() '())))
(setrightBST (list 88 '() '()) (BST 42 (list 22 '() '()) (list 52 '() '())))

;empty and search
(isEmpty (BST 60 20 70)) 
(isEmpty '())

(searchAHH 20 (BST 60 (list 20 '() '()) (list 70 '() '())))   
(searchAHH 70 (BST 66 (list 22 '() '()) (list 77 '() '())))

;tranversals
(inorder (BST 60 (list 20 '() '()) (list 70 '() '())))
(inorder (BST 42 (list 30 '() '()) (list 72 '() '())))

(postorder (BST 60 (list 20 '() '()) (list 70 '() '())))
(postorder (BST 42 (list 30 '() '()) (list 72 '() '())))

(preorder (BST 60 (list 20 '() '()) (list 70 '() '())))
(preorder (BST 42 (list 30 '() '()) (list 72 '() '())))

(insert 70 (headBST 60))
(insert 40 (headBST 60))

(BSTempty)