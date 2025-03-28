;; DaoCracy Core Governance Contract

;; Define constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PROPOSAL (err u103))

;; DAO Proposal Status
(define-enum ProposalStatus
  Pending
  Active
  Passed
  Rejected
  Executed
)

;; DAO Proposal Structure
(define-map Proposals
  { proposal-id: uint }
  {
    creator: principal,
    description: (string-utf8 500),
    voting-start: uint,
    voting-end: uint,
    status: ProposalStatus,
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
    (try! (is-authorized-governance-creator tx-sender))
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
    (asserts! (> (get-token-balance tx-sender) u0) ERR-UNAUTHORIZED)
    
    ;; Create proposal mapping
    (map-set Proposals 
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        description: description,
        voting-start: current-block,
        voting-end: (+ current-block voting-period),
        status: Pending,
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
    
    ;; Validate voting period and voter's token balance
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
          (merge proposal { status: Passed })
        )
        
        ;; Optional: Execute associated contract function
        (match (get target-contract proposal)
          contract-address 
            (match (get executable-function proposal)
              func-name 
                ;; Placeholder for potential cross-contract call
                (ok true)
              none (ok true)
            )
          none (ok true)
        )
      )
      ;; If proposal fails
      (begin
        (map-set Proposals 
          { proposal-id: proposal-id }
          (merge proposal { status: Rejected })
        )
        (ok false)
      )
    )
  )
)

;; Authorization helper
(define-private (is-authorized-governance-creator (sender principal))
  (if (is-eq sender CONTRACT-OWNER)
    (ok true)
    ERR-UNAUTHORIZED
  )
)