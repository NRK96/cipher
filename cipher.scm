;; Nicholas Keen
;; Schemer II
;; Feb 28 2018
;; Programming Languages

;; op takes an expression and returns the first operator in the expression
(define op 
    (lambda (lst)
        (cond
            ;; empty list
            ((null? lst) '())
            ;; prefix notation means the operator will 
            ;; always be at the head of the list
            (else (car lst))
        )
    )
)

;; lhs takes an expression and returns whatever is on the left hand side
;; of the first operator within that expression
(define lhs
    (lambda (lst)
        (cond
            ;; empty list
            ((null? lst) '())
            ;; move past the operator and returns whatever is right after
            ;; as it will always be the left hand side of the expression
            (else (cadr lst))
        )
    )
)

;; rhs takes an expression and returns whatever is on the right hand side
;; of the first operator within that expression, returns empty on unary ops
(define rhs
    (lambda (lst)
        (cond 
            ;; empty list
            ((null? lst) '())
            ;; sqrt is unary, return empty list
            ((equal? (car lst) (car '(sqrt))) '())
            ;; move past the operator and the next expression as that is the 
            ;; left hand side, the next expression will always be the right
            ;; hand side given prefix notation.
            (else (caddr lst))
        )
    )
)

;; helper method that searches a list and returns true or false
;; true if x is in list lst and false otherwise
(define contains
    (lambda (lst x)
        (cond
            ;; x is not in the list return false
            ((null? lst) #f)
            ;; we found x in the list return true
            ((eq? (car lst) x) #t)
            ;; didn't find x, searche next entry
            (else (contains (cdr lst) x))
        )
    )
)

;; takes a list and returns the set of elements from that list
(define vars
    (lambda (lst)
        (cond
            ;; empty list, we're done, return
            ((null? lst) '())
            ;; if the next element does not appear later in the list,
            ((not(contains (cdr lst) (car lst)))
            ;; add it to the set and continue
             (cons (car lst)(vars (cdr lst))))
            ;; duplicate, don't add it
            (else (vars (cdr lst)))
        )
    )
)

;; recursive helper function that enciphers the message recursively 
;; returning a list of the enciphered characters
(define cipher
    (lambda (c str offset)
        (define var 0)
        (cond
            ;; set the value for the current character
            ((null? str) '())
            (else (set! var (+ (char->integer(car str)) offset)))
        )
        (cond
            ;; if the current value is too high, adjust accordingly
            ((> var 122) (set! var (- var 26)))
        )
        (cond
            ;; end of string, we're done
            ((null? str) '())
            ;; recurse through string and create enciphered message
            ((char-lower-case?(car str))
             (cons(integer->char var)(cipher c (cdr str) offset)))
            ;; not a lower case character, ignore
            (else (cons(car str)(cipher c (cdr str) offset)))
        )
    )
)

;; caesar cipher that takes lower case letters in a message and offsets them
;; by a given character, returning the enciphered message
(define caesar
    (lambda (c str)
        ;; convert string to list
        (define strlst (string->list str))
        ;; create offset value based on given character
        (define offset (- (char->integer c)(char->integer #\a)))
        ;; call cipher and convert the returned list back into a string
        (list->string (cipher c strlst offset))
    )
)

;; helper method for mono-cipher
;; takes a list and a specified character and finds it's position in the list
(define index
    (lambda (lst c num)
        (cond
            ;; empty list return 0
            ((null? lst) 0)
            ;; found it, return index
            ((equal? (car lst) c) (+ num 1))
            ;; still looking, increment index and recurse
            (else (index (cdr lst) c (+ num 1)))
        )
    )
)

;; helper method for mono-cipher
;; searches through a list and finds the element at an index
(define search
    (lambda (lst i num)
        (cond
            ;; empty list, return 0
            ((null? lst) 0)
            ;; at index, return element
            ((= (- i 1) num) (car lst))
            ;; haven't found it, keep looking through list
            (else (search (cdr lst) i (+ num 1)))
        )
    )
)

;; helper method for monoalphabetic-encipher
;; enciphers a given message with a given alphabet
(define mono-cipher
    (lambda (strlst i code encode)
        (cond
            ;; empty list, return 0
            ((null? strlst) 0)
            ;; set index
            (else (set! i (index (string->list code) (car strlst) 0)))
        )
        (cond
            ;; empty list, return empty list
            ((null? strlst) '())
            ;; if lower case, cons together a code letter and 
            ;; the rest of the codeword recursively
            ((char-lower-case? (car strlst)) 
             (cons(search encode i 0)
                  (mono-cipher (cdr strlst) i code encode)))
            ;; not lower case, add to codeword and recurse
            (else (cons (car strlst)(mono-cipher (cdr strlst) i code encode)))
        )
    )
)

;; takes a code alphabet and a word and returns an eciphered codeword
(define monoalphabetic-encipher
    (lambda (code str)
        (define strlst (string->list str))
        (define encode (reverse (string->list code)))
        (define i 0)
        ;; call our helper and convert to a string
        (list->string(mono-cipher strlst i code encode))
    )
)

;; performs the same functionality as the encipher
(define monoalphabetic-decypher
    (lambda (code str)
        (monoalphabetic-encipher code str)
    )
)

;; accepts a string and encodes it, decodes it and 
;; throws the strings into a list
(define make-code
    (lambda (str)
        ;; define our alphabet
        (define alphabet "zyxwvutsrqponmlkjihgfedcba")
        ;; encode our codeword
        (define encode (list->string(string->list(monoalphabetic-encipher alphabet str))))
        ;; decode our encoded word
        (define decode (list->string(string->list(monoalphabetic-decypher alphabet encode))))
        ;; put the results in a list
        (list encode decode)
    )
)