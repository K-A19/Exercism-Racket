#lang racket

#|
Create an implementation of the Atbash cipher, an ancient encryption system created in the Middle East.

The Atbash cipher is a simple substitution cipher that relies on transposing all the letters in the alphabet such that the resulting alphabet is backwards. The first letter is replaced with the last letter, the second with the second-last, and so on.

An Atbash cipher for the Latin alphabet would be as follows:

Plain:  abcdefghijklmnopqrstuvwxyz
Cipher: zyxwvutsrqponmlkjihgfedcba

Ciphertext is written out in groups of fixed length, the traditional group size being 5 letters, leaving numbers unchanged, and punctuation is excluded. This is to make it harder to guess things based on word boundaries. All text will be encoded as lowercase letters.

Examples
- Encoding test gives gvhg
- Encoding x123 yes gives c123b vh
- Decoding gvhg gives test
- Decoding gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt gives thequickbrownfoxjumpsoverthelazydog
The tests for this problem assume that numbers are passed through without being encrypted/decrypted.
|#


(define (encode m)

    ; Creating a variable to store the alphabet for shifting
    (define alphabet "abcdefghijklmnopqrstuvwxyz")

    ; Removes all white space from the sentence to be encoded
    (set! m (string-downcase (regexp-replace* #px"\\s+" m "")))

    ; Recursive algorithm
    (define (recurencode a)

        ; Base check expression for when the string is empty
        (if (= 0 (string-length a)) 

            ""

            
            (string-append 

                ; Shifts the letter if the next character to encoded is alphabetic or just returns it if it's a digit or skips it if it's punctuation
                (cond 
                    [(char-alphabetic? (string-ref a 0)) (string(string-ref alphabet (- 25 (string-find alphabet (string (string-ref a 0) )))))] 
                    [(char-numeric? (string-ref a 0)) (string (string-ref a 0))] 
                    [else ""]
                )

                ; Maintains recursion by using a sustring from the second character onwards
                (recurencode (substring a 1))
            )
        
        )
    )

    ; Creates variables to store the coded data with and without space formatting
    (define spaceless (recurencode m))
    (define coded "")

    ; Inserts spaces after every 5th character
    (for ([i (range (string-length spaceless))])

        (if (and (not(= i 0)) (= 0 (remainder i 5)))

            (set! coded (string-append coded " "))

            ""
        )

        (set! coded (string-append coded (string(string-ref spaceless i))))   
    )

    coded

)

(define (decode m)

    ; Creating a variable to store the alphabet for shifting
    (define alphabet "abcdefghijklmnopqrstuvwxyz")

    ; Removes all white space from the sentence to be encoded
    (set! m (string-downcase (regexp-replace* #px"\\s+" m "")))

    ; Recursive algorithm
    (define (recurdecode a)

        ; Base check expression for when the string is empty
        (if (= 0 (string-length a)) 

            ""

            
            (string-append 

                ; Shifts the letter if the next character to encoded is alphabetic or just returns it if it's a digit or skips it if it's punctuation
                (cond 
                    [(char-alphabetic? (string-ref a 0)) (string(string-ref alphabet (- 25 (string-find alphabet (string (string-ref a 0) )))))] 
                    [(char-numeric? (string-ref a 0)) (string (string-ref a 0))] 
                    [else ""]
                )

                ; Maintains recursion by using a sustring from the second character onwards
                (recurdecode (substring a 1))
            )
        
        )
    )

    (recurdecode m)

)

(encode "Testing,1 2 3, testing.")
(decode "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt")