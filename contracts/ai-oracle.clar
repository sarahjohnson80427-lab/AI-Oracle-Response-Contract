(define-constant err-not-owner (err u100))
(define-constant err-invalid-fee (err u101))
(define-constant err-invalid-oracle (err u102))
(define-constant err-question-not-found (err u103))
(define-constant err-already-answered (err u104))
(define-constant err-invalid-signature (err u105))
(define-constant err-not-open (err u106))
(define-constant err-expired (err u107))
(define-constant err-not-asker (err u108))
(define-constant err-already-refunded (err u109))

(define-data-var owner principal tx-sender)
(define-data-var oracle-key (buff 33) 0x00)
(define-data-var oracle-fee uint u0)
(define-data-var oracle-active bool false)
(define-data-var question-nonce uint u0)
(define-data-var expiry-blocks uint u144)

(define-map questions
  uint
  {
    asker: principal,
    bounty: uint,
    question-hash: (buff 32),
    asked-at: uint,
    answered: bool,
    refunded: bool
  }
)

(define-map answers
  uint
  {
    responder: principal,
    answer-hash: (buff 32),
    answered-at: uint
  }
)

(define-map oracle-stats
  principal
  {
    total-answered: uint,
    total-earned: uint
  }
)

(define-public (set-oracle (pubkey (buff 33)) (fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
    (asserts! (> fee u0) err-invalid-fee)
    (var-set oracle-key pubkey)
    (var-set oracle-fee fee)
    (var-set oracle-active true)
    (ok true)
  )
)

(define-public (disable-oracle)
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
    (var-set oracle-active false)
    (ok true)
  )
)

(define-public (set-expiry (blocks uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
    (var-set expiry-blocks blocks)
    (ok true)
  )
)

(define-public (ask-question (question (buff 256)))
  (let
    (
      (qid (+ (var-get question-nonce) u1))
      (fee (var-get oracle-fee))
      (hash (sha256 question))
      (height stacks-block-height)
    )
    (begin
      (asserts! (var-get oracle-active) err-not-open)
      (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
      (map-set questions qid
        {
          asker: tx-sender,
          bounty: fee,
          question-hash: hash,
          asked-at: height,
          answered: false,
          refunded: false
        }
      )
      (var-set question-nonce qid)
      (ok qid)
    )
  )
)

(define-public (submit-answer
  (qid uint)
  (answer (buff 256))
  (signature (buff 65))
)
  (let
    (
      (q (map-get? questions qid))
      (ahash (sha256 answer))
      (pubkey (var-get oracle-key))
      (height stacks-block-height)
    )
    (begin
      (asserts! (is-some q) err-question-not-found)
      (asserts! (not (get answered (unwrap! q err-question-not-found))) err-already-answered)
      (asserts!
        (< height (+ (get asked-at (unwrap! q err-question-not-found)) (var-get expiry-blocks)))
        err-expired
      )
      (asserts!
        (secp256k1-verify ahash signature pubkey)
        err-invalid-signature
      )
      (map-set answers qid
        {
          responder: tx-sender,
          answer-hash: ahash,
          answered-at: height
        }
      )
      (map-set questions qid
        {
          asker: (get asker (unwrap! q err-question-not-found)),
          bounty: (get bounty (unwrap! q err-question-not-found)),
          question-hash: (get question-hash (unwrap! q err-question-not-found)),
          asked-at: (get asked-at (unwrap! q err-question-not-found)),
          answered: true,
          refunded: false
        }
      )
      (let
        (
          (stats (default-to { total-answered: u0, total-earned: u0 } (map-get? oracle-stats tx-sender)))
          (new-count (+ (get total-answered stats) u1))
          (new-earned (+ (get total-earned stats) (get bounty (unwrap! q err-question-not-found))))
        )
        (map-set oracle-stats tx-sender
          {
            total-answered: new-count,
            total-earned: new-earned
          }
        )
      )
      (try!
        (stx-transfer?
          (get bounty (unwrap! q err-question-not-found))
          (as-contract tx-sender)
          tx-sender
        )
      )
      (ok true)
    )
  )
)

(define-public (refund-question (qid uint))
  (let
    (
      (q (map-get? questions qid))
      (height stacks-block-height)
    )
    (begin
      (asserts! (is-some q) err-question-not-found)
      (asserts! (is-eq tx-sender (get asker (unwrap! q err-question-not-found))) err-not-asker)
      (asserts! (not (get answered (unwrap! q err-question-not-found))) err-already-answered)
      (asserts! (not (get refunded (unwrap! q err-question-not-found))) err-already-refunded)
      (asserts!
        (>= height (+ (get asked-at (unwrap! q err-question-not-found)) (var-get expiry-blocks)))
        err-not-open
      )
      (map-set questions qid
        {
          asker: (get asker (unwrap! q err-question-not-found)),
          bounty: (get bounty (unwrap! q err-question-not-found)),
          question-hash: (get question-hash (unwrap! q err-question-not-found)),
          asked-at: (get asked-at (unwrap! q err-question-not-found)),
          answered: false,
          refunded: true
        }
      )
      (try!
        (stx-transfer?
          (get bounty (unwrap! q err-question-not-found))
          (as-contract tx-sender)
          tx-sender
        )
      )
      (ok true)
    )
  )
)

(define-read-only (get-question (qid uint))
  (map-get? questions qid)
)

(define-read-only (get-answer (qid uint))
  (map-get? answers qid)
)

(define-read-only (get-oracle)
  {
    public-key: (var-get oracle-key),
    fee: (var-get oracle-fee),
    active: (var-get oracle-active)
  }
)

(define-read-only (get-oracle-stats (oracle principal))
  (map-get? oracle-stats oracle)
)

(define-read-only (get-question-count)
  (var-get question-nonce)
)
