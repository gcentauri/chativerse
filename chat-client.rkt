;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chat-client) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define SIZE 32)
(define COLOR 'aqua)

(define (last lst)
  (cond
    [(empty? (rest lst)) (first lst)]
    [else (last (rest lst))]))

(define (delete-last s)
  (cond
    [(string=? s "") ""]
    [else (substring s 0 (sub1 (string-length s)))]))

(define-struct client [chat-list text])

;; A ChatList is either: 
;;   - empty
;;   - (list String empty)

;; ChatList Message -> Client
(define (receive cl ms)
  (make-client (append (client-chat-list cl) (list ms)) ""))

(check-expect
 (receive (make-client '() "") "foo")
 (make-client (list "foo") ""))

(define (draw-chat chat)
  (cond
    [(empty? chat) empty-image]
    [else (above (text (first chat) SIZE COLOR)
                 (draw-chat (rest chat)))]))

(define (render client)
  (place-image/align
   (above (draw-chat (client-chat-list client))
          (text (client-text client) SIZE COLOR))
   150 600
   "middle" "bottom"
   (empty-scene 300 600)))

(check-expect
 (render (make-client '() ""))
 (empty-scene 300 600))

(define (add-char s c)
  (local
    ((define no-type (list "\r" "cancel" "clear" "shift" "rshift" "control"
                          "rcontrol" "menu" "pause" "capital" "prior" "next"
                          "end" "home" "escape" "select" "print" "execute"
                          "snapshot" "insert" "help" "left" "right" "up" "down")))
    (cond
      [(member c no-type) s]
      [(key=? c "\b") (delete-last s)]
      [else (string-append s c)])))

;; Client -> Client or (make-package Client String)
(define (react client key)
  (local
    ((define chat (client-chat-list client))
     (define msg (client-text client))
     (define text (add-char msg key)))
    (cond
      [(key=? key "\r") (make-package (make-client (append chat (list text)) "") msg)]
      [else (make-client chat text)])))

(big-bang (make-client '() "")
  [on-receive receive]
  [to-draw render]
  [on-key react]
  [register LOCALHOST])