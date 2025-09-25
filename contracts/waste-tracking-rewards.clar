;; Waste Tracking and Rewards Smart Contract
;; Track waste disposal, measure recycling rates, and reward sustainable practices

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-invalid-waste-type (err u104))
(define-constant err-already-claimed (err u105))
(define-constant err-insufficient-balance (err u106))

;; Waste types
(define-constant waste-general u1)
(define-constant waste-recyclable u2)
(define-constant waste-organic u3)
(define-constant waste-hazardous u4)

;; Reward rates (in microSTX per kg)
(define-constant recyclable-reward u10000) ;; 0.01 STX per kg
(define-constant organic-reward u5000) ;; 0.005 STX per kg
(define-constant general-penalty u1000) ;; Small penalty for general waste

;; Data Variables
(define-data-var disposal-counter uint u0)
(define-data-var total-waste-tracked uint u0)
(define-data-var total-recycled uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var reward-pool uint u0)

;; Data Maps

;; Waste disposal records
(define-map waste-disposals uint {
    user: principal,
    waste-type: uint,
    weight: uint,
    disposal-block: uint,
    location: (string-ascii 128),
    verified: bool,
    reward-amount: uint,
    reward-claimed: bool
})

;; User statistics
(define-map user-stats principal {
    total-disposals: uint,
    total-recycled-weight: uint,
    total-organic-weight: uint,
    total-general-weight: uint,
    total-rewards: uint,
    recycling-rate: uint,
    sustainability-score: uint
})

;; Municipal locations
(define-map locations (string-ascii 128) {
    manager: principal,
    location-type: uint,
    active: bool,
    total-waste: uint,
    recycling-rate: uint
})

;; Monthly recycling targets
(define-map recycling-targets {year: uint, month: uint} {
    target-rate: uint,
    actual-rate: uint,
    bonus-pool: uint,
    participants: uint
})

;; Helper Functions

;; Calculate recycling rate
(define-private (calculate-recycling-rate (recycled uint) (total uint))
    (if (> total u0)
        (/ (* recycled u100) total)
        u0
    )
)

;; Calculate reward amount based on waste type and weight
(define-private (calculate-reward (waste-type uint) (weight uint))
    (if (is-eq waste-type waste-recyclable)
        (* weight recyclable-reward)
        (if (is-eq waste-type waste-organic)
            (* weight organic-reward)
            u0 ;; No reward for general/hazardous waste
        )
    )
)

;; Update user statistics
(define-private (update-user-stats (user principal) (waste-type uint) (weight uint) (reward uint))
    (let 
        ((current-stats (default-to {total-disposals: u0, total-recycled-weight: u0, total-organic-weight: u0, total-general-weight: u0, total-rewards: u0, recycling-rate: u0, sustainability-score: u0} 
                                   (map-get? user-stats user))))
        
        (let 
            ((new-recycled (if (is-eq waste-type waste-recyclable) 
                              (+ (get total-recycled-weight current-stats) weight)
                              (get total-recycled-weight current-stats)))
             (new-organic (if (is-eq waste-type waste-organic)
                             (+ (get total-organic-weight current-stats) weight)
                             (get total-organic-weight current-stats)))
             (new-general (if (is-eq waste-type waste-general)
                             (+ (get total-general-weight current-stats) weight)
                             (get total-general-weight current-stats)))
             (total-waste (+ new-recycled (+ new-organic new-general)))
             (new-rate (calculate-recycling-rate new-recycled total-waste))
             (new-score (+ (get sustainability-score current-stats) (if (> reward u0) u5 u1))))
            
            (map-set user-stats user {
                total-disposals: (+ (get total-disposals current-stats) u1),
                total-recycled-weight: new-recycled,
                total-organic-weight: new-organic,
                total-general-weight: new-general,
                total-rewards: (+ (get total-rewards current-stats) reward),
                recycling-rate: new-rate,
                sustainability-score: new-score
            })
        )
    )
)

;; Public Functions

;; Register a disposal location
(define-public (register-location (location-name (string-ascii 128)) (location-type uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set locations location-name {
            manager: tx-sender,
            location-type: location-type,
            active: true,
            total-waste: u0,
            recycling-rate: u0
        })
        (ok true)
    )
)

;; Record waste disposal
(define-public (record-waste-disposal (waste-type uint) (weight uint) (location (string-ascii 128)))
    (let 
        ((new-disposal-id (+ (var-get disposal-counter) u1))
         (reward-amount (calculate-reward waste-type weight)))
        
        (asserts! (<= waste-type u4) err-invalid-waste-type)
        (asserts! (> weight u0) err-invalid-amount)
        
        ;; Record disposal
        (map-set waste-disposals new-disposal-id {
            user: tx-sender,
            waste-type: waste-type,
            weight: weight,
            disposal-block: stacks-block-height,
            location: location,
            verified: false,
            reward-amount: reward-amount,
            reward-claimed: false
        })
        
        ;; Update statistics
        (update-user-stats tx-sender waste-type weight reward-amount)
        (var-set disposal-counter new-disposal-id)
        (var-set total-waste-tracked (+ (var-get total-waste-tracked) weight))
        
        (if (is-eq waste-type waste-recyclable)
            (var-set total-recycled (+ (var-get total-recycled) weight))
            true
        )
        
        (ok new-disposal-id)
    )
)

;; Verify waste disposal (by location manager or owner)
(define-public (verify-disposal (disposal-id uint))
    (let 
        ((disposal-data (unwrap! (map-get? waste-disposals disposal-id) err-not-found)))
        
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized) ;; Simplified verification
        (asserts! (not (get verified disposal-data)) err-already-claimed)
        
        ;; Mark as verified
        (map-set waste-disposals disposal-id 
            (merge disposal-data {verified: true})
        )
        
        (ok true)
    )
)

;; Claim rewards for verified disposal
(define-public (claim-reward (disposal-id uint))
    (let 
        ((disposal-data (unwrap! (map-get? waste-disposals disposal-id) err-not-found))
         (reward-amount (get reward-amount disposal-data)))
        
        (asserts! (is-eq tx-sender (get user disposal-data)) err-unauthorized)
        (asserts! (get verified disposal-data) err-unauthorized)
        (asserts! (not (get reward-claimed disposal-data)) err-already-claimed)
        (asserts! (>= (var-get reward-pool) reward-amount) err-insufficient-balance)
        
        ;; Transfer reward
        (if (> reward-amount u0)
            (try! (as-contract (stx-transfer? reward-amount tx-sender (get user disposal-data))))
            true
        )
        
        ;; Update disposal record
        (map-set waste-disposals disposal-id 
            (merge disposal-data {reward-claimed: true})
        )
        
        ;; Update statistics
        (var-set reward-pool (- (var-get reward-pool) reward-amount))
        (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward-amount))
        
        (ok reward-amount)
    )
)

;; Fund reward pool (by municipality)
(define-public (fund-reward-pool (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set reward-pool (+ (var-get reward-pool) amount))
        (ok true)
    )
)

;; Set monthly recycling target
(define-public (set-recycling-target (year uint) (month uint) (target-rate uint) (bonus-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= target-rate u100) err-invalid-amount)
        
        (map-set recycling-targets {year: year, month: month} {
            target-rate: target-rate,
            actual-rate: u0,
            bonus-pool: bonus-amount,
            participants: u0
        })
        (ok true)
    )
)

;; Calculate and distribute monthly bonuses
(define-public (distribute-monthly-bonus (year uint) (month uint))
    (let 
        ((target-data (unwrap! (map-get? recycling-targets {year: year, month: month}) err-not-found))
         (current-rate (calculate-recycling-rate (var-get total-recycled) (var-get total-waste-tracked))))
        
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        ;; Update actual rate
        (map-set recycling-targets {year: year, month: month}
            (merge target-data {
                actual-rate: current-rate
            })
        )
        
        (ok current-rate)
    )
)

;; Bulk record disposal (for municipal systems)
(define-public (bulk-record-disposal (disposals (list 10 {waste-type: uint, weight: uint, location: (string-ascii 128)})))
    (let 
        ((results (map process-single-disposal disposals)))
        (ok (len results))
    )
)

;; Helper for bulk disposal
(define-private (process-single-disposal (disposal {waste-type: uint, weight: uint, location: (string-ascii 128)}))
    (let 
        ((new-disposal-id (+ (var-get disposal-counter) u1))
         (waste-type (get waste-type disposal))
         (weight (get weight disposal))
         (location (get location disposal))
         (reward-amount (calculate-reward waste-type weight)))
        
        (map-set waste-disposals new-disposal-id {
            user: tx-sender,
            waste-type: waste-type,
            weight: weight,
            disposal-block: stacks-block-height,
            location: location,
            verified: true, ;; Auto-verified for bulk
            reward-amount: reward-amount,
            reward-claimed: false
        })
        
        (var-set disposal-counter new-disposal-id)
        (var-set total-waste-tracked (+ (var-get total-waste-tracked) weight))
        
        (if (is-eq waste-type waste-recyclable)
            (var-set total-recycled (+ (var-get total-recycled) weight))
            true
        )
        
        new-disposal-id
    )
)

;; Read-Only Functions

;; Get disposal details
(define-read-only (get-disposal-details (disposal-id uint))
    (map-get? waste-disposals disposal-id)
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
    (map-get? user-stats user)
)

;; Get platform statistics
(define-read-only (get-platform-stats)
    {
        total-disposals: (var-get disposal-counter),
        total-waste: (var-get total-waste-tracked),
        total-recycled: (var-get total-recycled),
        recycling-rate: (calculate-recycling-rate (var-get total-recycled) (var-get total-waste-tracked)),
        rewards-distributed: (var-get total-rewards-distributed),
        reward-pool-balance: (var-get reward-pool)
    }
)

;; Get location details
(define-read-only (get-location-info (location-name (string-ascii 128)))
    (map-get? locations location-name)
)

;; Get monthly target
(define-read-only (get-recycling-target (year uint) (month uint))
    (map-get? recycling-targets {year: year, month: month})
)

;; Check reward eligibility
(define-read-only (check-reward-eligibility (disposal-id uint))
    (match (map-get? waste-disposals disposal-id)
        disposal-data (ok {
            eligible: (and (get verified disposal-data) (not (get reward-claimed disposal-data))),
            reward-amount: (get reward-amount disposal-data),
            verified: (get verified disposal-data),
            claimed: (get reward-claimed disposal-data)
        })
        err-not-found
    )
)
