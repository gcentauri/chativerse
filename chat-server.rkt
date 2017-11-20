;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chat-server) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Bundle is
;   (make-bundle UniverseState [Listof mail?] [Listof iworld?])
     
; UniverseState iworld? -> Bundle
; next list of worlds when world iw is joining
; the universe in state s
(define (add-client univ client)
  (local ((define univ* (append univ (list client))))
    (make-bundle univ*
                 (list (make-mail client "welcome"))
                 empty)))

; an obvious example for adding a world:
(check-expect
  (add-client '() iworld1)
  (make-bundle (list iworld1)
               (list (make-mail iworld1 "welcome"))
               '()))
     
; UniverseState iworld? W2U -> Bundle
; next list of worlds when world iw is sending message m to
; the universe in state s
(define (process univ client msg)
  (make-bundle univ
               (map (λ (w) (make-mail w msg)) univ)
               empty))

(check-expect
 (process (list iworld1 iworld2 iworld3) iworld1 "hello")
 (make-bundle (list iworld1 iworld2 iworld3)
              (list (make-mail iworld1 "hello")
                    (make-mail iworld2 "hello")
                    (make-mail iworld3 "hello"))
              empty))

(define go!
  (universe '()
  [on-new add-client]
  [on-msg process]
  ))