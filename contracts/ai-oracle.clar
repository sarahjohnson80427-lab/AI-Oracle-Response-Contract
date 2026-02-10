(define-constant err-not-owner (err u100))
(define-constant err-question-not-found (err u101))
(define-constant err-already-answered (err u102))
(define-constant err-invalid-signature (err u103))
(define-constant err-expired (err u104))
(define-constant err-not-asker (err u105))
(define-constant err-already-refunded (err u106))
(define-constant err-oracle-not-registered (err u107))
(define-constant err-invalid-bid (err u108))
(define-constant err-bid-not-found (err u109))
(define-constant err-not-selected-oracle (err u110))
(define-constant err-no-bid-selected (err u111))
(define-constant err-already-registered (err u112))
(define-constant err-invalid-pubkey (err u113))
(define-constant err-bounty-question (err u115))
(define-constant err-no-bounty-or-bid (err u116))

(define-data-var owner principal tx-sender)
(define-data-var question-nonce uint u0)
(define-data-var expiry-blocks uint u144)
(define-data-var bid-nonce uint u0)

(define-map oracle-registry
  principal
  {
    pubkey: (buff 33),
    registered-at: uint,
    active: bool,
  }
)

(define-map questions
  uint
  {
    asker: principal,
    question-hash: (buff 32),
    asked-at: uint,
    answered: bool,
    refunded: bool,
    selected-bid: (optional uint),
    bounty-amount: uint,
  }
)

(define-map question-bids
  uint
  {
    question-id: uint,
    oracle: principal,
    bid-amount: uint,
    submitted-at: uint,
  }
)

(define-map answers
  uint
  {
    responder: principal,
    answer-hash: (buff 32),
    answered-at: uint,
  }
)

(define-map oracle-stats
  principal
  {
    total-answered: uint,
    total-earned: uint,
  }
)

(define-public (register-oracle (pubkey (buff 33)))
  (let (
      (height stacks-block-height)
      (existing (map-get? oracle-registry tx-sender))
    )
    (begin
      (asserts! (is-none existing) err-already-registered)
      (asserts! (not (is-eq pubkey 0x00)) err-invalid-pubkey)
      (map-set oracle-registry tx-sender {
        pubkey: pubkey,
        registered-at: height,
        active: true,
      })
      (ok true)
    )
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
  (let (
      (qid (+ (var-get question-nonce) u1))
      (hash (sha256 question))
      (height stacks-block-height)
    )
    (begin
      (map-set questions qid {
        asker: tx-sender,
        question-hash: hash,
        asked-at: height,
        answered: false,
        refunded: false,
        selected-bid: none,
        bounty-amount: u0,
      })
      (var-set question-nonce qid)
      (ok qid)
    )
  )
)

(define-public (ask-bounty-question
    (question (buff 256))
    (amount uint)
  )
  (let (
      (qid (+ (var-get question-nonce) u1))
      (hash (sha256 question))
      (height stacks-block-height)
    )
    (begin
      (asserts! (> amount u0) err-invalid-bid)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set questions qid {
        asker: tx-sender,
        question-hash: hash,
        asked-at: height,
        answered: false,
        refunded: false,
        selected-bid: none,
        bounty-amount: amount,
      })
      (var-set question-nonce qid)
      (ok qid)
    )
  )
)

(define-public (submit-bid
    (qid uint)
    (bid-amount uint)
  )
  (let (
      (q (map-get? questions qid))
      (oracle-info (map-get? oracle-registry tx-sender))
      (height stacks-block-height)
      (new-bid-id (+ (var-get bid-nonce) u1))
    )
    (begin
      (asserts! (is-some q) err-question-not-found)
      (asserts! (is-some oracle-info) err-oracle-not-registered)
      (asserts! (get active (unwrap! oracle-info err-oracle-not-registered))
        err-oracle-not-registered
      )
      (asserts! (is-eq (get bounty-amount (unwrap! q err-question-not-found)) u0)
        err-bounty-question
      )
      (asserts! (not (get answered (unwrap! q err-question-not-found)))
        err-already-answered
      )
      (asserts!
        (< height
          (+ (get asked-at (unwrap! q err-question-not-found))
            (var-get expiry-blocks)
          ))
        err-expired
      )
      (asserts! (> bid-amount u0) err-invalid-bid)
      (map-set question-bids new-bid-id {
        question-id: qid,
        oracle: tx-sender,
        bid-amount: bid-amount,
        submitted-at: height,
      })
      (var-set bid-nonce new-bid-id)
      (ok new-bid-id)
    )
  )
)

(define-public (accept-bid
    (qid uint)
    (bid-id uint)
  )
  (let (
      (q (map-get? questions qid))
      (bid (map-get? question-bids bid-id))
      (height stacks-block-height)
    )
    (begin
      (asserts! (is-some q) err-question-not-found)
      (asserts! (is-some bid) err-bid-not-found)
      (asserts! (is-eq tx-sender (get asker (unwrap! q err-question-not-found)))
        err-not-asker
      )
      (asserts! (not (get answered (unwrap! q err-question-not-found)))
        err-already-answered
      )
      (asserts! (is-eq (get question-id (unwrap! bid err-bid-not-found)) qid)
        err-bid-not-found
      )
      (asserts!
        (< height
          (+ (get asked-at (unwrap! q err-question-not-found))
            (var-get expiry-blocks)
          ))
        err-expired
      )
      (try! (stx-transfer? (get bid-amount (unwrap! bid err-bid-not-found)) tx-sender
        (as-contract tx-sender)
      ))
      (map-set questions qid {
        asker: (get asker (unwrap! q err-question-not-found)),
        question-hash: (get question-hash (unwrap! q err-question-not-found)),
        asked-at: (get asked-at (unwrap! q err-question-not-found)),
        answered: false,
        refunded: false,
        selected-bid: (some bid-id),
        bounty-amount: (get bounty-amount (unwrap! q err-question-not-found)),
      })
      (ok true)
    )
  )
)

(define-public (submit-answer
    (qid uint)
    (answer (buff 256))
    (signature (buff 65))
  )
  (let (
      (q (unwrap! (map-get? questions qid) err-question-not-found))
      (ahash (sha256 answer))
      (height stacks-block-height)
      (selected-bid-id (get selected-bid q))
      (bounty (get bounty-amount q))
      (oracle-info (unwrap! (map-get? oracle-registry tx-sender) err-oracle-not-registered))
      (pubkey (get pubkey oracle-info))
      (payment-amount (if (is-some selected-bid-id)
        (let ((bid (unwrap!
            (map-get? question-bids (unwrap! selected-bid-id err-no-bid-selected))
            err-bid-not-found
          )))
          (asserts! (is-eq tx-sender (get oracle bid)) err-not-selected-oracle)
          (get bid-amount bid)
        )
        (begin
          (asserts! (> bounty u0) err-no-bounty-or-bid)
          bounty
        )
      ))
    )
    (begin
      (asserts! (get active oracle-info) err-oracle-not-registered)
      (asserts! (not (get answered q)) err-already-answered)
      (asserts! (< height (+ (get asked-at q) (var-get expiry-blocks)))
        err-expired
      )
      (asserts! (secp256k1-verify ahash signature pubkey) err-invalid-signature)
      (map-set answers qid {
        responder: tx-sender,
        answer-hash: ahash,
        answered-at: height,
      })
      (map-set questions qid {
        asker: (get asker q),
        question-hash: (get question-hash q),
        asked-at: (get asked-at q),
        answered: true,
        refunded: false,
        selected-bid: selected-bid-id,
        bounty-amount: bounty,
      })
      (let (
          (stats (default-to {
            total-answered: u0,
            total-earned: u0,
          }
            (map-get? oracle-stats tx-sender)
          ))
          (new-count (+ (get total-answered stats) u1))
          (new-earned (+ (get total-earned stats) payment-amount))
        )
        (map-set oracle-stats tx-sender {
          total-answered: new-count,
          total-earned: new-earned,
        })
      )
      (try! (stx-transfer? payment-amount (as-contract tx-sender) tx-sender))
      (ok true)
    )
  )
)

(define-public (refund-question (qid uint))
  (let (
      (q (unwrap! (map-get? questions qid) err-question-not-found))
      (height stacks-block-height)
      (selected-bid-id (get selected-bid q))
      (bounty (get bounty-amount q))
      (refund-amount (if (is-some selected-bid-id)
        (let ((bid (unwrap!
            (map-get? question-bids (unwrap! selected-bid-id err-no-bid-selected))
            err-bid-not-found
          )))
          (get bid-amount bid)
        )
        (begin
          (asserts! (> bounty u0) err-no-bounty-or-bid)
          bounty
        )
      ))
    )
    (begin
      (asserts! (is-eq tx-sender (get asker q)) err-not-asker)
      (asserts! (not (get answered q)) err-already-answered)
      (asserts! (not (get refunded q)) err-already-refunded)
      (asserts! (>= height (+ (get asked-at q) (var-get expiry-blocks)))
        err-expired
      )
      (map-set questions qid {
        asker: (get asker q),
        question-hash: (get question-hash q),
        asked-at: (get asked-at q),
        answered: false,
        refunded: true,
        selected-bid: selected-bid-id,
        bounty-amount: bounty,
      })
      (try! (stx-transfer? refund-amount (as-contract tx-sender) tx-sender))
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

(define-read-only (get-question-bid (bid-id uint))
  (map-get? question-bids bid-id)
)

(define-read-only (get-oracle-info (oracle principal))
  (map-get? oracle-registry oracle)
)

(define-read-only (get-oracle-stats (oracle principal))
  (map-get? oracle-stats oracle)
)

(define-read-only (get-question-count)
  (var-get question-nonce)
)

(define-read-only (get-bid-count)
  (var-get bid-nonce)
)
