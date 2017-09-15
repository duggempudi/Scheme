;TEAM MEMBERS: Srinivasa Reddy Duggempudi(U00830362)
;Assignment-1

#lang racket
;Signature
;map           : Map
;clear         : Map->Map
;containsKey   : Map x Key -> boolean
;containsValue : Map x Value -> boolean
;equals        : Map x Map  -> boolean
;get           : Map x Key  -> value
;isEmpty       : Map  -> boolean
;put           : Map x Key x Value ->Map
;remove        : Map x Key        -> Map
;size          : Map              -> int
;
;The Key and Value in this signatue can be symbols or numbers or strings.
;classification
;
;constructors:map,put
;observers   :containsKey,containsValue,equals,get,isEmpty,size
;nonconstructors : remove,clear
;
;constructors axioms
;forall K1,K2 in keys ,V1 and V2 in values, M in Map:
; put(put(K1,V2,M),K1,V1)=put(M,K1,V1)
; put(put(K2,V2,M),K1,V1)=put(put(K1,V1,M),K2,V2)
;
;nonconstructos axioms
;forall K1,K2 in keys ,V1 and V2 in values, M in Map:
;clear(M)=map
;remove(map,K1)=map
;remove(put(M,K2,V2) ,K1)=
;  if (K1==K2) => M
;  else => put(remove(M,K1),K2,V2)
;observers axioms
;forall K1,K2 in keys ,V1 and V2 in values, M1,M2 in Map:
;containsKey(map,K1)=False
;containsKey(put(M1,K2,V2),K1)=
;  if(K2==K1)=>True
;  else =>containsKey(M1,K1)
;containsValue(map,V1)=False
;containsValue(put(M1,K2,V2),V1)=
;  if(V1==V2)=>True
;  else =>containsValue(M1,V1)
;size(map)=0
;size(put(M1,K1,V1))=
;  if(containsKey(M1,K1))=>size(M1)
;  else => 1+size(M1)
;isEmpty(map)=True
;isEmpty(put(M1,K1,V1))=False
;get(map,K1)="Key Not Present"
;get(put(K2,V2,M1),K1)=
;   if(K1==K2)=>V2
;   else => get(M1,K1)
;equals(map,map)=True
;equals(put(M1,K1,V1),M2)=
; if(size(put(M1,K1,V1))== size(M2))=>
;     if(containsKey(M2,K1)=>
;        if(get(M2,K1)==V1)=>equals(M1,remove(M2,K1))
;        else=>False
;     else=>False
; else=>False
;
;
;
;
;
;
;
;
;





;This function define map.The objective of this function is
;to create an empty map
(define map '())
;Test cases
;map => '()

;This function define clear. The objective of this function
;is to clear the map and provide an empty map
(define (clear mapping)
  '())
;Test cases
;(clear (put map 'a 1)) => '()
;(clear (put (put map 'a 1) 'b 2)) => '()

;This function define containsKey . The objective of this functin
;is to check whether a key occurs in a map
(define (containsKey mapping key)
  (if (null? mapping) #f (if (equal? (caar mapping) key) #t
                             (containsKey (cdr mapping) key)
                             )
      )
  )
;Test cases
;(containsKey (put (put (put map 'a 1) 'b 2) 'c 3)) 'a) => #t
;(containsKey (put (put (put map 'a 1) 'b 2) 'c 3) 'd) => #f

;This function define containsValue. The objective of this function
;is to check whether a value occurs in a map
(define (containsValue mapping value)
  (if (null? mapping) #f (if (equal? (cadar mapping) value) #t
                             (containsValue (cdr mapping) value)
                             )
      )
  )
;Test cases
;(containsValue (put (put (put map 'a 1) 'b 2) 'c 3) 2) => #t
;(containsValue (put (put (put map 'a 1) 'b 2) 'c 3)) 10) => #f

;This is function to define get. The objective of this function
; is to take a key and a mapping and get a value as ouput if present

(define (get mapping key)
  (if (null? mapping) "Key not present" (if (equal? (caar mapping) key)
                                            (cadar mapping)
                                            (get (cdr mapping) key)))
  )
;Test cases
;(get (put (put (put map 'a 1) 'b 2) 'c 3) 'a) => 1
;(get (put (put (put map 'a 1) 'b 5) 'c 4) 'd) => Key not present 

;This is function to define size.The objective of this function
;is to get the size of a map
(define (size mapping)
  (if (null? mapping) 0
      (+ 1 (size (cdr mapping)))
      )
  )
;Test cases
;(size map)=>0
;(size (put (put map 'a 1) 'a 2))=>1

;This function removes a key from a map in present and returns
;the remaining map
(define (remove mapping key)
  (if (null? mapping) '()
      (if (equal? (caar mapping) key)(cdr mapping)
          (cons (car mapping) (remove (cdr mapping) key))
          )
      )
  )
;Test cases
;(remove (put (put (put map 'a 1) 'b 2) 'c 3) 'a)=>'((b 2)(c 3))
;(remove (put (put (put map 'a 1) 'b 2) 'c 3) 'd)=>'((a 1)(b 2)(c 3))
;
;This functin checks whether a map is empty or not
(define (isEmpty mapping)
  (null? mapping))
;Test cases
;(isEmpty map)=>#t
;(isEmpty (put (put (put map 'a 1) 'b 2) 'c 3) 'd)=>#f
;
;This function put key value pairs in maps. If a key is already
;present in map it changes its value
(define (put mapping key value)
  (if (null? mapping) (cons (list key value) '())
      (if (equal? (caar mapping) key) (cons (list key value) (cdr mapping))
          (cons (car mapping) (put (cdr mapping) key value)))
          )
      )
;Test cases
;(define M (put map 'a 1))
;a=> '((a 1))
;(put a 'b 2)=> '((a 1)(b 2))
;(put a 'a 4)=> '((a 4))
;
;This functin checks whether two maps are equal or not.
(define (equals mapping1 mapping2)
  (if (and (isEmpty mapping1) (isEmpty mapping2)) #t
      (if (isEmpty mapping1) #f
          (if (isEmpty mapping2) #f
              (if (equal? (get mapping1 (caar mapping1))
                               (get mapping2 (caar mapping1)
                                    ))
                  (equals (remove mapping1 (caar mapping1))
                          (remove mapping2 (caar mapping1))
                          )
                  #f)
              )
          )
      )
  )
;Test cases
;(define a (put (put map 'a 2) 'b 1))
;a=> '((a 2 )(b 1))
;(define b (put (put map 'b 1) 'a 2))
;b=>'((b 1 )(a 2))
;(equals a b)=> #t
;(define c (put b 'a 3))
;c=>'((b 1)(a 3))
;(equals a c)=>#f
;(equals a map)=>#f


  
