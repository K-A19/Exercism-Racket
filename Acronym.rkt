#lang racket

(provide acronym)

#| 
Convert a phrase to its acronym.
Help generate some jargon by writing a program that converts a long name like Portable Network Graphics to its acronym (PNG).
Punctuation is handled as follows: hyphens are word separators (like whitespace); all other punctuation can be removed from the input.
|#
(define (acronym input)
    
    ; Defining a variable to store the acronym
    (define acro "")

    ; Regex expression spliting of the string by whitespaces and hyphens
    (define words (regexp-split #px"[\\s\\-]+" input))
    
    ; Iteration over each word split
    (for ([word words])

        ; Essentially a while loop
        (let loop ([test word])
        
            ; Repeats the sequence as long as the first character is not alphabetic in nature
            (when (not (char-alphabetic? (string-ref test 0)))
            
                ; Updates the word to remove the first character if it's not alphabetic
                (set! word (substring test 1))
                (loop (substring test 1))
            
            )
        
        )
    
        ; References the first letter of each word
        (define letter (string (string-ref word 0)))

        ; Updates the acro variabel to contain the next letter for the acronym
        (set! acro (string-append acro letter))
    
    )
    
    ; Returns the acronym in capital letters
    (string-upcase acro)

)

(acronym "_hello -bitch-_radicle")
