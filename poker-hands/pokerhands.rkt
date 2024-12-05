;project by Caitlin Cai

; ************************************************************
; USEFUL CONSTANTS
; ************************************************************

(define ranks '(2 3 4 5 6 7 8 9 10 j q k a))
(define suits '(s h d c))
(define ranknames '(ace two three four five six seven
                        eight nine ten jack queen king ace))

; ************************************************************
; UTILITY HELPER FUNCTIONS
; ************************************************************

; suit-name takes in a suit as a member of suits, and
;           returns the name of the suit
(define (suit-name suit)
  (cond ((equal? suit 's) 'spades)
        ((equal? suit 'h) 'hearts)
        ((equal? suit 'd) 'diamonds)
        (else 'clubs)))

; rank-name takes in a rank as a member of ranks, and
;           returns the name of the rank
(define (rank-name rank)
  (item (numeric-rank rank) ranknames))

; pluralize takes in a rank name, and
;           returns the plural of the name
(define (pluralize r)
  (word r (cond ((member? r '(six)) 'es)
                (else 's))))

; numeric-rank takes in a rank (or card), and
;              returns the corresponding number rank
(define (numeric-rank rank-or-card)
  (let ((rank (if (member? rank-or-card ranks)
                  rank-or-card
                  (rank rank-or-card))))
    (cond ((equal? rank 'a) 14)
          ((equal? rank 'k) 13)
          ((equal? rank 'q) 12)
          ((equal? rank 'j) 11)
          (else rank))))

; rank extracts the rank of a single card using bf
(define (rank card) (bf card))

; ranks-only takes in a hand, and
;            returns the ranks of all cards in a hand
(define (ranks-only hand)
  (every rank hand))

; suit extracts the suit of a single card using first
(define (suit card) (first card))

; second takes in a word or sentence, and
;        returns the second part of the input when possible and
;        the input otherwise
(define (second thing)
  (if (> (count thing) 1)
      (first (bf thing))
      thing))

; third takes in a word or sentence, and
;       returns the third part of the input when possible and
;       the last of the input otherwise
(define (third thing)
  (if (> (count thing) 2)
      (first ((repeated bf 2) thing))
      (last thing)))

; fourth takes in a word or sentence, and
;        returns the fourth part of the input when possible and
;        the last of the input otherwise
(define (fourth thing)
  (if (> (count thing) 3)
      (first ((repeated bf 3) thing))
      (last thing)))

; sort takes in a hand, and
;      returns a hand completely sorted by rank
(define (sort hand)
  ((repeated sort-once (- (count hand) 1)) hand))

; sort-once takes in a hand, and
;           returns the hand sorted once
(define (sort-once hand)
  (cond ((empty? hand) hand)
        ((= (count hand) 1) hand)
        ((> (numeric-rank (first hand))
            (numeric-rank (second hand)))
         (se (first hand) (sort-once (bf hand))))
        (else (se (second hand)
                  (sort-once (se (first hand) (bf (bf hand))))))))

; ************************************************************
; PREDICATES TO DETERMINE THE TYPE OF HAND
; ************************************************************

;the following functions check if a hand qualifies for a category, but does not check for more specific titles
;e.g. '(ha hk hq hj h10) is a straight, a straight flush, and a royal flush ~ on purpose

; NOTE: ALL the following functions assume the hand is sorted.

; Removes singleton cards
(define (remove-singles hand)
  (cond ((<= (count hand) 1) '())
        ((and (>= (count hand) 4)
              (= (numeric-rank (first hand))
                 (numeric-rank (second hand))
                 (numeric-rank (third hand))
                 (numeric-rank (fourth hand))))
         (se (first hand)
             (second hand)
             (third hand)
             (fourth hand)
             (remove-singles (bf (bf (bf hand)))))
         )
        ((and (>= (count hand) 3)
              (= (numeric-rank (first hand))
                 (numeric-rank (second hand))
                 (numeric-rank (third hand))))
         (se (first hand)
             (second hand)
             (third hand)
             (remove-singles (bf (bf (bf hand)))))
         )
        ((and (>= (count hand) 2)
              (= (numeric-rank (first hand))
                 (numeric-rank (second hand))))
         (se (first hand)
             (second hand)
             (remove-singles (bf (bf hand))))
         )
        (else (remove-singles (bf hand)))
        )
  )


; flush? returns #t when all suits are the same and
;                #f otherwise
(define (flush? hand)
  (cond ((= (count hand) 1) #t)
        ((equal? (suit (first hand))
                 (suit (second hand)))
         (flush? (bf hand)))
        (else #f)
        )
  )


; straight? returns #t when all ranks are in sequence and
;                   #f otherwise
; NOTE: suit is not considered
;       assumes a 5-card hand

(define (straight? hand)
  (cond ((<= (count hand) 1) #t)
        ((= (- (numeric-rank (first hand)) 1)
            (numeric-rank (second hand))
            )
         (straight? (bf hand))
         )
        ((and (equal? (rank (first hand)) 'a)
              (eq? (rank (second hand)) 5)
              )
         (straight? (bf hand))
         )
        (else #f)
        )
  )


; straight-flush? returns #t when the hand is a straight,
;                                             a flush, and
;                                             second-high card is not king
;                         #f otherwise
(define (straight-flush? hand)
  (and (straight? hand)
       (flush? hand)
       )
  )


; royal-flush? returns #t when the hand is a straight,
;                                          a flush, and
;                                          second-high card is king
;                      #f otherwise
(define (royal-flush? hand)
  (and (straight? hand)
       (flush? hand)
       (eq? (rank (second hand)) 'k)
       )
  )


; pair? takes a sorted hand as its input and
;       returns #t if there exists exactly one pair and
;               #f otherwise
; NOTE: must assume higher-value hands have been eliminated

(define (pair? hand)
  (= (count (remove-singles hand)) 2))



; two-pair? takes a sorted hand as its input and
;           returns #t if there exists exactly two pairs and
;                   #f otherwise
; NOTE: must assume higher-value hands have been eliminated
(define (two-pair? hand)
  (and (= (count (remove-singles hand)) 4)
       (not (eq? (rank (first (remove-singles hand)))
                 (rank (fourth (remove-singles hand)))
                 )
            )
       )
  )


; three-of-a-kind? takes a sorted hand as its input and
;                  returns #t if there exists exactly one trio and
;                          #f otherwise
; NOTE: must assume higher-value hands have been eliminated
(define (three-of-a-kind? hand)
  (= (count (remove-singles hand)) 3))

; full-house? takes a sorted hand as its input and
;             returns #t if there exists exactly one trio and and one pair and
;                     #f otherwise
; NOTE: must assume higher-value hands have been eliminated
(define (full-house? hand)
  (= (count (remove-singles hand)) 5))

; four-of-a-kind? takes a sorted hand as its input and
;                 returns #t if there exists four of a kind and
;                        #f otherwise

(define (four-of-a-kind? hand)
  (and (= (count (remove-singles hand)) 4)
       (eq? (rank (first (remove-singles hand)))
            (rank (fourth (remove-singles hand)))
            )
       )
  )

; ************************************************************
; OUTPUT FUNCTIONS THAT DESCRIBE THE TYPE OF HAND
; ************************************************************

; royal-flush takes a royal flush as an input and returns
;             the type of royal-flush in the form
;             "royal flush - [suit]"
; NOTE: royal-flush is a flush, a straight, and high card is an ace
(define (royal-flush hand)
  (se '(royal flush -) (suit-name (suit (first hand)))))

; straight-flush takes a straight flush as an input and returns
;                the type of straight flush in the form
;                "[rank]-high straight flush - [suit]"
; NOTE: straight-flush is a flush, a straight, and
;       high card is not an ace
(define (straight-flush hand)
  (cond ((and (eq? (rank (second hand)) 5)
              (eq? (rank (first hand)) 'a))
         (se '(five-high straight flush -)
             (suit-name (first (first hand)))))
        (else (se (word (rank-name (first hand))
                        '-high)
                  '(straight flush - )
                  (suit-name (first (first hand)))))
        )
  )


; flush takes a flush as an input and returns
;       the type of flush in the form
;       "flush - [suit]"
(define (flush hand)
  (se '(flush -) (suit-name (suit (first hand)))))

; straight takes a straight as an input and returns
;          the type of straight in the form
;          "[rank]-high straight"

(define (straight hand)
  (cond ((and (eq? (rank (second hand)) 5)
              (eq? (rank (first hand)) 'a))
         '(five-high straight))
        (else (se (word (rank-name (first hand))
                        '-high) 'straight))))


; pair takes a pair as an input and returns
;      the type of pair in the form
;      "pair of [ranks]"
; NOTE: returns #f if no pair exists

(define (pair hand)
  (cond ((pair? hand)
         (se '(pair of)
             (pluralize (rank-name (first (remove-singles hand))))))
        (else #f)))

; three-of-a-kind takes three-of-a-kind as an input and returns
;                 the type of three-of-a-kind in the form
;                 "three [ranks]"
; NOTE: returns #f if three-of-a-kind does not exist


(define (three-of-a-kind hand)
  (cond ((three-of-a-kind? hand)
         (se 'three
             (pluralize (rank-name (third hand)))))
        (else #f)))


; full-house takes a full house as an input and returns
;            the type of full house in the form
;            "full house - [ranks] full of [ranks]"
(define (full-house hand)
  (cond ((eq? (rank (third hand))
              (rank (first hand)))
         (se '(full house -)
             (pluralize (rank-name (third hand)))
             '(full of)
             (pluralize (rank-name (last hand)))))
        (else (se '(full house -)
                  (pluralize (rank-name (third hand)))
                  '(full of)
                  (pluralize (rank-name (first hand)))))))

  

; four-of-a-kind takes four-of-a-kind as an input and returns
;                the type of four-of-a-kind in the form
;                "four [ranks]"
; NOTE: returns #f if no four-of-a-kind exists
(define (four-of-a-kind hand)
  (cond ((four-of-a-kind? hand)
         (se 'four
             (pluralize (rank-name (first (remove-singles hand))))))
        (else #f)))


; two-pair takes two-pair as an input and returns
;          the type of two-pair in the form
;          "two pair - [ranks] and [ranks]"
; NOTE: returns #f if two-pair does not exist
(define (two-pair hand)
  (cond ((two-pair? hand)
         (se '(two pair -)
             (pluralize (rank-name (first (remove-singles hand))))
             'and
             (pluralize (rank-name (last (remove-singles hand))))))
        (else #f)))


; high-card takes in a hand and returns
;           the high card rank in the form
;           "[rank] high"
(define (high-card hand)
  (se (rank-name (first hand)) 'high))


; ************************************************************
; MAIN PROGRAM
; ************************************************************

; poker-value takes in a hand, sorts the hand, and returns
;             the type of the hand
; NOTE:  the types with all unique ranks are tested first,
;        then the types with duplicate ranks are tested
(define (poker-value hand)
  (let ((sortedhand (sort hand)))
    (cond ((royal-flush? sortedhand) (royal-flush sortedhand))
          ((straight-flush? sortedhand) (straight-flush sortedhand))
          ((flush? sortedhand) (flush sortedhand))
          ((straight? sortedhand) (straight sortedhand))
          (else (let ((handranks (ranks-only sortedhand)))
                  (cond ((four-of-a-kind? sortedhand) (four-of-a-kind
                                                       sortedhand))
                        ((full-house? sortedhand) (full-house
                                                   sortedhand))
                        ((three-of-a-kind? sortedhand) (three-of-a-kind
                                                        sortedhand))
                        ((two-pair? sortedhand) (two-pair
                                                 sortedhand))
                        ((pair? sortedhand) (pair sortedhand))
                        (else (high-card sortedhand))))))))



; ************************************************************
; TESTING CODE
; ************************************************************

; The following is a collection of 21 test-hand pairs in the form
; '[hand hand-description])
(define rf '[(ha hk h10 hj hq) (royal flush - hearts)]) ; royal flush
(define sf '[(h9 hk h10 hj hq) (king-high straight flush - hearts)]) ; straight flush
(define sf5 '[(d2 d5 d3 da d4) (five-high straight flush - diamonds)]) ; 5-high straight flush
(define fourXxxxx '[(d3 h3 h10 s3 c3) (four threes)]) ; four of a kind
(define fourxxxxX '[(hj dj c2 sj cj) (four jacks)]) ; four of a kind
(define fhxxxyy '[(c3 ha sa h3 ca) (full house - aces full of threes)]) ; full house
(define fhxxyyy '[(ca ha s3 h3 c3) (full house - threes full of aces)]) ; full house
(define f '[(d2 d5 d10 dj da) (flush - diamonds)]) ; flush
(define str '[(dq h9 h10 hj h8) (queen-high straight)]) ; straight
(define str5 '[(d4 c5 ca c2 c3) (five-high straight)]) ; 5-high straight
(define threexxxXX '[(c6 d6 h5 h6 h3) (three sixes)]) ; three of a kind
(define threeXxxxX '[(c5 d5 h5 h6 h3) (three fives)]) ; three of a kind
(define threeXXxxx '[(c3 d3 h5 h6 h3) (three threes)]) ; three of a kind
(define twopxxyyX '[(d10 h3 h10 h2 d3) (two pair - tens and threes)]) ; two pair
(define twopxxXyy '[(d10 h3 h10 h2 d2) (two pair - tens and twos)]) ; two pair
(define twopXxxyy '[(d3 h3 h10 h2 d2) (two pair - threes and twos)]) ; two pair
(define pxxXXX '[(d3 h9 h10 hj dj) (pair of jacks)]) ; pair
(define pXxxXX '[(d3 h9 h10 hj d10) (pair of tens)]) ; pair
(define pXXxxX '[(d3 h9 h10 hj d9) (pair of nines)]) ; pair
(define pXXXxx '[(d3 h9 h10 h3 dj) (pair of threes)]) ; pair
(define high '[(ca h2 d3 s4 sk) (ace high)]) ; high card

; all-test-hands is a list containing the 21 test-hand pairs
(define all-test-hands (list rf sf sf5 fourXxxxx fourxxxxX fhxxxyy fhxxyyy
                             f str str5 threexxxXX threeXxxxX threeXXxxx
                             twopxxyyX twopxxXyy twopXxxyy
                             pxxXXX pXxxXX pXXxxX pXXXxx high))

; Special testing code for a royal flush
;(let [(hand (sort (car rf)))]
  ; NOTE: Each of the following five lines is designed to work
  ;       independently.  Uncomment these lines ONE-at-a-time.
  ; (append '(royal flush test hand is) (list hand)))
  ; (cons hand (append '(is) (if (straight? hand) '() '(not)) '(a straight))))
  ; (cons hand (append '(is) (if (flush? hand) '() '(not)) '(a flush))))
; (cons hand (append '(is) (if (straight-flush? hand) '() '(not)) '(a straight flush))))
; (cons hand (append '(is) (if (royal-flush? hand) '() '(not)) '(a royal flush))))
; NOTE: Once all five tests have passed, comment all Special
;       testing code for a royal flush and proceed to the
;       last line below.

; test takes in a hand and a description as a list, and returns
;      a list of the form
;      '(hand #t) when the hand and description match, and
;      '(hand [error] #f) when the hand and description do not match.
; NOTE: The actual sentence describing the hand MUST match what
;       you chose to use in your code.

(define (test testhand)
  (let* [(hand (car testhand))
         (val (poker-value hand))
         (exp-val (car (cdr testhand)))]
    (cons hand
          (if (equal? val exp-val)
              '(#t)
              (list (list 'actual: val 'expected: exp-val)
                    #f)))))

; tests stores the results of the above tests in a list.
; EXAMPLE: if poker-value is successful, tests will look similar to
;          '(([hand] #t) (([hand]) #t))
;          if poker-value is not working, tests will contain #f's for
;          each hand value that is not detected properly, similar to
;          '(([hand] #f) ([hand] #f))
(define tests (map test all-test-hands))

; test-results returns the contents of tests with 'FAIL when
;                      at least one test fails
;                      and 'PASS when all tests are successful!
(define (test-results)
  (let [(errors (filter (lambda (x) (not (last x)))
                        tests))]
    (if (null? errors)
        'PASS
        (list tests 'FAIL))))

; THIS INVOCATION CONTROLS ALL TESTS
; NOTE: Uncomment this ONLY when you are done testing for
;       a royal flush above.
(test-results)