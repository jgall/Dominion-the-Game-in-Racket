#lang racket/gui
(require racket/draw)

;; An attempt to make Dominion the Game

;; A Player is a (make-player [List-of Card] [List-of Card] [List-of Card] Turn-Status)
(define-struct player (hand deck discard status))


;; A Turn-Status is a (make-turn-status Number Number Number)
(define-struct turn-status (actions buys money))

;; add-action : Turn-Status -> Turn-Status
(define (add-action stat)
  (make-turn-status (+ (turn-status-actions stat) 1)
                    (turn-status-buys stat)
                    (turn-status-money stat)))

;; add-buy : Turn-Status -> Turn-Status
(define (add-buy s)
  (make-turn-status (turn-status-actions s)
                    (add1 (turn-status-buys s))
                    (turn-status-money s)))


;; add-money : Turn-Status -> Turn-Status
(define (add-money s)
  (make-turn-status (turn-status-actions s)
                    (turn-status-buys s)
                    (add1 (turn-status-money s))))

;; A Card is one (or more) of
;; - Treasure-Card
;; - Action-Card
;; - Attack-Card
;; - Action-Reaction-Card
;; - Victory-Card

;; card? : any/c -> Boolean
;; is the input a Card?
(define (card? var)
  (or (treasure-card? var)
      (action-card? var)
      (victory-card? var)))

;; A Treasure-Card is a (make-treasure-card String Number Number)
(define-struct treasure-card (name value cost))
(define COPPER (make-treasure-card "Copper" 1 0))
(define SILVER (make-treasure-card "Silver" 2 3))
(define GOLD (make-treasure-card "Gold" 3 6))

;; An Action-Card is a (make-action-card String Number [List-of Local-Functions] [List-of Function]) 
(define-struct action-card (name cost actions specials))

;; TODO: attack cards and reaction cards....


;; A Victory-Card is a (make-victory-card String Number Number)
(define-struct victory-card (name value cost))
(define ESTATE (make-victory-card "Estate" 1 2))
(define DUCHY (make-victory-card "Duchy" 3 5))
(define PROVINCE (make-victory-card "Province" 6 8))

(define (card-name c)
  (cond [(action-card? c) (action-card-name c)]
        [(treasure-card? c) (treasure-card-name c)]
        [(victory-card? c) (victory-card-name c)]))

;; A Function is one of:
;; - Local-Function
;; - World-Function


;; A Local-Function has the signature: Player -> Player

;; CREATE DRAWS

;; draw-one : Player -> Player
(define (draw-one a-player)
  (make-player (cons (first (player-deck a-player)) (player-hand a-player))
               (rest (player-deck a-player))
               (player-discard a-player)
               (player-status a-player)))

;; draw-num : Number Player -> Player
(define (draw-num num p)
  (if (or (empty? (player-deck p))
          (zero? num))
      p
      (draw-num (sub1 num) (draw-one p))))

;; create-draw-cards : Number -> [Player -> Player]
(define (create-draw-cards num)
  (λ (p) (draw-num num p)))

;; CREATE ACTIONS 

;; add-one-action : Player -> Player
(define (add-one-action a-player)
  (make-player (player-hand a-player)
               (player-deck a-player)
               (player-discard a-player)
               (add-action (player-status a-player))))

;; add-num-actions : Number Player -> Player
(define (add-num-actions num p)
  (if (zero? num)
      p
      (add-num-actions (sub1 num) (add-one-action p))))

;; create-add-actions : Number -> [Player -> Player]
(define (create-add-actions num)
  (λ (p) (add-num-actions num p)))

;; CREATE BUYS

;; add-one-buy : Player -> Player
(define (add-one-buy p)
  (make-player (player-hand p)
               (player-deck p)
               (player-discard p)
               (add-buy (player-status p))))

;; add-num-buys : Number Player -> Player
(define (add-num-buys num p)
  (if (zero? num)
      p
      (add-num-buys (sub1 num) (add-one-buy p))))

;; create-add-buys : Number -> [Player -> Player]
(define (create-add-buys num)
  (λ (p) (add-num-buys num p)))

(define VILLAGE (make-action-card "Village" 3 (list (create-draw-cards 1)
                                                    (create-add-actions 2)) '()))


;; (define MILITIA (make-action-card "Militia" 4 (list (create-draw-cards 2)) militia-func))





;; A World-Function has the signature: World -> World

;; shuffle-deck : Player -> Player
(define (discard->deck p)
  (make-player (player-hand p)
               '()
               (shuffle (player-deck p))
               (player-status p)))


;; A World is a (make-world [List-of Player] [List-of Pair] Number)
(define-struct world (players table turn))

(define (end-turn w)
  (make-world (world-players w)
              (world-table w)
              (modulo (add1 (world-turn w)) (length (world-players w)))))

(define (apply-actions ac w)
  (set! world0 (foldr (λ (f) (make-world (list-ref (world-players w) (world-turn w))
                                         (world-table w)
                                         (world-turn w))) w (action-card-actions ac))))



(define player1 (make-player (list ESTATE ESTATE ESTATE COPPER COPPER)
                             (list COPPER COPPER COPPER COPPER COPPER)
                             '()
                             (make-turn-status 1 0 1)))
(define player2 (make-player (list COPPER ESTATE COPPER COPPER COPPER)
                             (list COPPER COPPER ESTATE ESTATE COPPER)
                             '()
                             (make-turn-status 1 0 1)))
(define world0 (make-world (list player1 player2)
                           (list VILLAGE ESTATE PROVINCE DUCHY COPPER SILVER GOLD)
                           0))






(define card-list (list VILLAGE
                        COPPER
                        SILVER
                        GOLD
                        ESTATE
                        DUCHY
                        PROVINCE))


(define (name->bitmap name)
  (make-object bitmap% (string-append "resources/" (string-downcase name) ".jpg")
    'unknown false false 4.0))

(define frame (new frame% [label "Test"]
                   [width 800]
                   [height 600]))

;; AN UPDATING INFO MESSAGE
(define msg (new message% [parent frame]
                 [label "Dominion the Game"]
                 [auto-resize true]))

;; THE BOARD: WHAT PEOPLE CAN BUY
(define (create-buy-buttons alist)
  (if (empty? alist)
      "Board Created..."
      (let ([x (new button% [parent buy-board]
                 [label (name->bitmap (card-name (first alist)))]
                 [callback (λ (button event)
                             (if (action-card? (first alist))
                                 (apply-actions (first alist))
                                 "no action..."))])])
        (create-buy-buttons (rest alist)))))
(define buy-board (new horizontal-panel% [parent frame]
                   [alignment '(center top)]))

(create-buy-buttons card-list)

;; CARDS IN PLAY BY ANYONE
(define in-play-board (new horizontal-panel% [parent frame]
                     [alignment '(center center)]))

;; PLAYERS CARDS
;; display-player-hand : [List-of Card] -> any/c
(define (display-player-hand cards)
  (if (empty? cards)
      "Cards Drawn..."
      (let ([x (new button% [parent player-board]
                    [label (name->bitmap (card-name (first cards)))]
                    [callback (λ (button event)
                                (send msg set-label
                                      (card-name (first cards))))])])
        (display-player-hand (rest cards)))))
(define player-board (new horizontal-panel% [parent frame]
                          [alignment '(center bottom)]))
(display-player-hand (player-hand player1))

;; INFORMATION PANEL
(define bottom-panel (new horizontal-panel% [parent frame]
                          [alignment '(center bottom)]
                          [stretchable-height false]))
(define money-amt (new message% [parent bottom-panel] [label "Money: 0,"]))
(define buy-amt (new message% [parent bottom-panel] [label "Buys: 1"]))
(define to-buy-phase-button (new button% [parent bottom-panel]
                                 [label "Play Treasures"]
                                 [callback (λ (button event)
                                             (send msg set-label "Play Treasures"))]))
(define end-Turn-button (new button% [parent bottom-panel]
                             [label "End Turn"]
                             [callback (λ (button event)
                                         (set! world0 (end-turn world0)))]))

;; CREATES THE GUI
(send frame show true)




