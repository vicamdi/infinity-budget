;; InfinityBudget - DAO Governance Platform Smart Contract
;; A simplified implementation of the InfinityBudget system with reputation-weighted voting,
;; quadratic funding mechanisms, and adaptive resource allocation

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-voting-closed (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-unauthorized (err u106))
(define-constant err-invalid-amount (err u107))

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var min-reputation-to-propose uint u100)
(define-data-var voting-period uint u1008)
(define-data-var total-treasury uint u0)

;; Data Maps
(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 256),
    funding-requested: uint,
    votes-for: uint,
    votes-against: uint,
    quadratic-score: uint,
    status: (string-ascii 20),
    created-at: uint,
    ends-at: uint,
    funds-allocated: uint
  }
)

(define-map user-reputation
  principal
  { 
    reputation-score: uint, 
    total-contributions: uint 
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { 
    vote-weight: uint, 
    vote-type: bool, 
    voted-at: uint 
  }
)

(define-map expert-validations
  { proposal-id: uint, expert: principal }
  { 
    technical-score: uint, 
    validated-at: uint 
  }
)

(define-map project-performance
  uint
  { 
    impact-score: uint,
    milestones-completed: uint,
    total-milestones: uint,
    community-rating: uint
  }
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-user-reputation (user principal))
  (default-to 
    { reputation-score: u0, total-contributions: u0 }
    (map-get? user-reputation user)
  )
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-treasury-balance)
  (ok (var-get total-treasury))
)

(define-read-only (get-project-performance (proposal-id uint))
  (map-get? project-performance proposal-id)
)

(define-read-only (calculate-quadratic-vote-weight (reputation uint))
  (if (<= reputation u100)
    u10
    (if (<= reputation u400)
      u20
      (if (<= reputation u900)
        u30
        u40
      )
    )
  )
)

;; Private functions

(define-private (update-reputation-internal (user principal) (points uint))
  (let
    (
      (current-rep (get-user-reputation user))
      (new-score (+ (get reputation-score current-rep) points))
      (new-contributions (+ (get total-contributions current-rep) u1))
    )
    (map-set user-reputation
      user
      { 
        reputation-score: new-score,
        total-contributions: new-contributions
      }
    )
    new-score
  )
)

;; Public functions

(define-public (submit-proposal (title (string-ascii 256)) (funding-requested uint))
  (let
    (
      (proposer-rep (get reputation-score (get-user-reputation tx-sender)))
      (proposal-id (+ (var-get proposal-nonce) u1))
      (current-height block-height)
    )
    (asserts! (>= proposer-rep (var-get min-reputation-to-propose)) err-unauthorized)
    (asserts! (> funding-requested u0) err-invalid-amount)
    
    (map-set proposals
      proposal-id
      {
        proposer: tx-sender,
        title: title,
        funding-requested: funding-requested,
        votes-for: u0,
        votes-against: u0,
        quadratic-score: u0,
        status: "active",
        created-at: current-height,
        ends-at: (+ current-height (var-get voting-period)),
        funds-allocated: u0
      }
    )
    
    (var-set proposal-nonce proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) err-not-found))
      (voter-rep (get reputation-score (get-user-reputation tx-sender)))
      (vote-weight (calculate-quadratic-vote-weight voter-rep))
      (existing-vote (get-vote proposal-id tx-sender))
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (< block-height (get ends-at proposal)) err-voting-closed)
    (asserts! (is-eq (get status proposal) "active") err-voting-closed)
    
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote-weight: vote-weight, vote-type: vote-for, voted-at: block-height }
    )
    
    (map-set proposals
      proposal-id
      (merge proposal {
        votes-for: (if vote-for 
          (+ (get votes-for proposal) vote-weight)
          (get votes-for proposal)
        ),
        votes-against: (if vote-for
          (get votes-against proposal)
          (+ (get votes-against proposal) vote-weight)
        ),
        quadratic-score: (+ (get quadratic-score proposal) vote-weight)
      })
    )
    
    (ok true)
  )
)

(define-public (expert-validate (proposal-id uint) (technical-score uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) err-not-found))
      (expert-rep (get reputation-score (get-user-reputation tx-sender)))
    )
    (asserts! (>= expert-rep u500) err-unauthorized)
    (asserts! (<= technical-score u100) err-invalid-amount)
    
    (map-set expert-validations
      { proposal-id: proposal-id, expert: tx-sender }
      { technical-score: technical-score, validated-at: block-height }
    )
    
    (ok true)
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) err-not-found))
      (votes-for (get votes-for proposal))
      (votes-against (get votes-against proposal))
    )
    (asserts! (>= block-height (get ends-at proposal)) err-voting-closed)
    (asserts! (is-eq (get status proposal) "active") err-already-exists)
    
    (if (> votes-for votes-against)
      (let
        (
          (funding-amount (get funding-requested proposal))
        )
        (asserts! (>= (var-get total-treasury) funding-amount) err-insufficient-funds)
        (var-set total-treasury (- (var-get total-treasury) funding-amount))
        
        (map-set proposals
          proposal-id
          (merge proposal {
            status: "approved",
            funds-allocated: funding-amount
          })
        )
        
        (update-reputation-internal (get proposer proposal) u50)
        (ok true)
      )
      (begin
        (map-set proposals
          proposal-id
          (merge proposal { status: "rejected" })
        )
        (ok false)
      )
    )
  )
)

(define-public (update-project-performance 
  (proposal-id uint) 
  (impact-score uint) 
  (milestones-completed uint)
  (total-milestones uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get proposer proposal)) err-unauthorized)
    (asserts! (<= impact-score u100) err-invalid-amount)
    (asserts! (<= milestones-completed total-milestones) err-invalid-amount)
    
    (map-set project-performance
      proposal-id
      {
        impact-score: impact-score,
        milestones-completed: milestones-completed,
        total-milestones: total-milestones,
        community-rating: u0
      }
    )
    
    (if (>= impact-score u80)
      (update-reputation-internal tx-sender u100)
      (if (>= impact-score u60)
        (update-reputation-internal tx-sender u50)
        (update-reputation-internal tx-sender u25)
      )
    )
    
    (ok true)
  )
)

(define-public (deposit-to-treasury (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set total-treasury (+ (var-get total-treasury) amount))
    (ok true)
  )
)

(define-public (bootstrap-reputation (user principal) (initial-points uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set user-reputation
      user
      { 
        reputation-score: initial-points,
        total-contributions: u0
      }
    )
    (ok true)
  )
)

(define-public (set-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (set-min-reputation (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-reputation-to-propose new-min)
    (ok true)
  )
)