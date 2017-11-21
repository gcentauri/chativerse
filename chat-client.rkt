;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chat-client) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define SIZE 24)
(define COLOR 'aqua)

(define-struct client [chat-list text])
;; A ChatList is either: 
;;   - empty
;;   - (list String empty)
;; Text is a String

;; Receiving Messages from the Universe
;; ====================================

;; receive : Client Message -> Client
;; receives an updated client state and a string message
(define (receive cl ms)
  (make-client (append (client-chat-list cl) (list ms)) ""))

(check-expect (receive (make-client '() "") "foo")
              (make-client (list "foo") ""))

;; Rendering the Chat
;; ==================

;; Listof String -> Image
;; helper for drawing the list of previous chats
(define (draw-chat chat)
  (cond
    [(empty? chat) empty-image]
    [else (above (text (first chat) SIZE COLOR)
                 (draw-chat (rest chat)))]))

;; draw-textbox : Client -> Image
;; helper for drawing the current text
(define (draw-textbox client)
  (text (client-text client) SIZE COLOR))

;; render : Client -> Image
;; function for big-bang to-draw handler
(define (render client)
  (place-image/align
   (above (draw-chat (client-chat-list client))
          (draw-textbox client))
   150 600
   "middle" "bottom"
   (rectangle 300 600 'solid 'black)))

(check-expect (render (make-client '() "")) (rectangle 300 600 'solid 'black))

;; Keyboard Event Handling
;; =======================

;; delete-last : String -> String
;; deletes the last character from a string - helper
(define (delete-last s)
  (cond
    [(string=? s "") ""]
    [else (substring s 0 (sub1 (string-length s)))]))

;; add-char : String Key -> String
;; adds the string corresponding to key event to the current text
(define (add-char s k)
  (local
    ((define non-printing
       (list "\r" "cancel" "clear" "shift" "rshift" "control"
             "rcontrol" "menu" "pause" "capital" "prior" "next"
             "end" "home" "escape" "select" "print" "execute"
             "snapshot" "insert" "help" "left" "right" "up" "down")))
    (cond
      [(member k non-printing) s]
      [(key=? k "\b") (delete-last s)]
      [else (string-append s k)])))

(check-expect (add-char "fo" "o") "foo")
(check-expect (add-char "fo" "\r") "fo")
(check-expect (add-char "foo" "\b") "fo")

;; react : Client Key -> Client or (make-package Client String)
;; big-bang on-key handler. sends a message to server on return key
(define (react client key)
  (local
    ((define chat (client-chat-list client))
     (define msg (client-text client))
     (define text (add-char msg key)))
    (cond
      [(key=? key "\r") (make-package (make-client (append chat (list text)) "") msg)]
      [else (make-client chat text)])))

;; Main Program
;; ============
;; new-client! : String -> World
;; launches a new chat client with name "user"
(define (new-client! user)
  (big-bang (make-client '() "")
            [on-receive receive]
            [to-draw render]
            [on-key react]
            [name user]
            [register LOCALHOST]))

(launch-many-worlds (new-client! "grant")
                    (new-client! "alisa"))