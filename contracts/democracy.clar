;; DaoCracy Core Governance Contract

;; Define constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PROPOSAL (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-INVALID-VOTING-PERIOD (err u105))
(define-constant ERR-INVALID-QUORUM (err u106))

;; Proposal Status Constants
(define-constant PROPOSAL-STATUS-PENDING u0)
(define-constant PROPOSAL-STATUS-ACTIVE u1)
(define-constant PROPOSAL-STATUS-PASSED u2)
(define-constant PROPOSAL-STATUS-REJECTED u3)
(define-constant PROPOSAL-STATUS-EXECUTED u4)

;; Governance Parameters
(define-constant MAX-DESCRIPTION-LENGTH u500)
(define-constant MAX-FUNCTION-NAME-LENGTH u100)
(define-constant MIN-VOTING-PERIOD u10)
(define-constant MAX-VOTING-PERIOD u1000)
(define-constant MAX-GOVERNANCE-MINT u10000)
(define-constant MAX-QUORUM u1000000)

;; DAO Proposal Structure
(define-map Proposals
  { proposal-id: uint }
  {
    creator: principal,
    description: (string-utf8 500),
    voting-start: uint,
    voting-end: uint,
    status: uint,
    votes-for: uint,
    votes-against: uint,
    required-quorum: uint,
    target-contract: (optional principal),
    executable-function: (optional (string-utf8 100))
  }
)

;; Governance Token Map
(define-map GovernanceTokens
  principal
  uint
)

;; Track Next Proposal ID
(define-data-var next-proposal-id uint u0)

;; Validation Functions
(define-private (is-valid-description (desc (string-utf8 500)))
  (and 
    (> (len desc) u0)
    (<= (len desc) MAX-DESCRIPTION-LENGTH)
  )
)

(define-private (is-valid-voting-period (period uint))
  (and 
    (>= period MIN-VOTING-PERIOD)
    (<= period MAX-VOTING-PERIOD)
  )
)

(define-private (is-valid-amount (amount uint))
  (and 
    (> amount u0)
    (<= amount MAX-GOVERNANCE-MINT)
  )
)

(define-private (is-valid-quorum (quorum uint))
  (and 
    (> quorum u0)
    (<= quorum MAX-QUORUM)
  )
)

(define-private (is-valid-target-contract (contract (optional principal)))
  (match contract
    some-contract (not (is-eq some-contract tx-sender))
    true
  )
)

(define-private (is-valid-executable-function (func (optional (string-utf8 100))))
  (match func
    some-func (and 
      (> (len some-func) u0)
      (<= (len some-func) MAX-FUNCTION-NAME-LENGTH)
    )
    true
  )
)

;; Read-only functions to get proposal and token balance
(define-read-only (get-proposal (proposal-id uint))
  (map-get? Proposals { proposal-id: proposal-id })
)

(define-read-only (get-token-balance (account principal))
  (default-to u0 (map-get? GovernanceTokens account))
)

;; Mint Governance Tokens
(define-public (mint-governance-tokens (amount uint) (recipient principal))
  (begin
    ;; Validate inputs
    (asserts! (is-authorized-governance-creator tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-valid-amount amount) ERR-INVALID-AMOUNT)
    (asserts! (not (is-eq recipient tx-sender)) ERR-UNAUTHORIZED)
    
    ;; Mint tokens
    (map-set GovernanceTokens 
      recipient 
      (+ (get-token-balance recipient) amount)
    )
    (ok amount)
  )
)

;; Create a new DAO Proposal
(define-public (create-proposal 
  (description (string-utf8 500))
  (voting-period uint)
  (required-quorum uint)
  (target-contract (optional principal))
  (executable-function (optional (string-utf8 100)))
)
  (let 
    (
      (proposal-id (var-get next-proposal-id))
      (current-block block-height)
    )
    ;; Validate inputs
    (asserts! (is-valid-description description) ERR-INVALID-PROPOSAL)
    (asserts! (is-valid-voting-period voting-period) ERR-INVALID-VOTING-PERIOD)
    (asserts! (is-valid-quorum required-quorum) ERR-INVALID-QUORUM)
    (asserts! (is-valid-target-contract target-contract) ERR-UNAUTHORIZED)
    (asserts! (is-valid-executable-function executable-function) ERR-INVALID-PROPOSAL)
    (asserts! (> (get-token-balance tx-sender) u0) ERR-UNAUTHORIZED)
    
    ;; Create proposal mapping
    (map-set Proposals 
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        description: description,
        voting-start: current-block,
        voting-end: (+ current-block voting-period),
        status: PROPOSAL-STATUS-PENDING,
        votes-for: u0,
        votes-against: u0,
        required-quorum: required-quorum,
        target-contract: target-contract,
        executable-function: executable-function
      }
    )
    
    ;; Increment proposal ID
    (var-set next-proposal-id (+ proposal-id u1))
    
    (ok proposal-id)
  )
)

;; Vote on a Proposal
(define-public (vote-on-proposal 
  (proposal-id uint)
  (vote-type bool)
)
  (let 
    (
      (voter-balance (get-token-balance tx-sender))
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
    
    ;; Validate voting conditions
    (asserts! (> voter-balance u0) ERR-UNAUTHORIZED)
    (asserts! 
      (and 
        (<= (get voting-start proposal) block-height)
        (>= (get voting-end proposal) block-height)
      ) 
      ERR-INVALID-PROPOSAL
    )
    
    ;; Update proposal votes
    (if vote-type
      (map-set Proposals 
        { proposal-id: proposal-id }
        (merge proposal {
          votes-for: (+ (get votes-for proposal) voter-balance)
        })
      )
      (map-set Proposals 
        { proposal-id: proposal-id }
        (merge proposal {
          votes-against: (+ (get votes-against proposal) voter-balance)
        })
      )
    )
    
    (ok true)
  )
)

;; Execute Proposal
(define-public (execute-proposal (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    )
    
    ;; Validate proposal execution conditions
    (asserts! 
      (> block-height (get voting-end proposal)) 
      ERR-INVALID-PROPOSAL
    )
    
    ;; Check if proposal passes quorum
    (if 
      (and 
        (>= (get votes-for proposal) (get required-quorum proposal))
        (> (get votes-for proposal) (get votes-against proposal))
      )
      (begin
        ;; Update proposal status to Passed
        (map-set Proposals 
          { proposal-id: proposal-id }
          (merge proposal { status: PROPOSAL-STATUS-PASSED })
        )
        
        ;; Optional: Execute associated contract function
        (match (get target-contract proposal)
          contract-address 
            (match (get executable-function proposal)
              func-name (ok true)
              (ok true)
            )
          (ok true)
        )
      )
      ;; If proposal fails
      (begin
        (map-set Proposals 
          { proposal-id: proposal-id }
          (merge proposal { status: PROPOSAL-STATUS-REJECTED })
        )
        (ok false)
      )
    )
  )
)

;; Authorization helper
(define-private (is-authorized-governance-creator (sender principal))
  (is-eq sender CONTRACT-OWNER)
)