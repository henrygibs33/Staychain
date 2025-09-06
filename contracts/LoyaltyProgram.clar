;; LoyaltyProgram - Reward system for frequent guests and hosts
;; Encourages platform engagement through points, tiers, and exclusive benefits

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-insufficient-points (err u401))
(define-constant err-invalid-parameters (err u402))
(define-constant err-reward-not-found (err u403))
(define-constant err-tier-not-found (err u404))
(define-constant err-already-claimed (err u405))
(define-constant err-reward-unavailable (err u406))

;; Data variables
(define-data-var next-reward-id uint u1)
(define-data-var loyalty-pool-balance uint u0)

;; Loyalty tiers configuration
(define-map loyalty-tiers
    uint
    {
        tier-name: (string-ascii 20),
        points-required: uint,
        booking-discount: uint,
        priority-support: bool,
        exclusive-properties: bool,
        tier-bonus-rate: uint
    }
)

;; User loyalty accounts
(define-map loyalty-accounts
    principal
    {
        total-points: uint,
        current-tier: uint,
        lifetime-bookings: uint,
        lifetime-hosting: uint,
        points-earned-this-period: uint,
        points-redeemed: uint,
        tier-achieved-at: uint,
        last-activity: uint
    }
)

;; Point earning activities
(define-map point-activities
    (string-ascii 20)
    {
        activity-name: (string-ascii 20),
        points-per-action: uint,
        is-active: bool,
        max-daily-points: uint
    }
)

;; User daily point tracking
(define-map daily-point-tracking
    {user: principal, date: uint}
    {
        points-earned-today: uint,
        activities-completed: (list 5 (string-ascii 20))
    }
)

;; Redeemable rewards
(define-map loyalty-rewards
    uint
    {
        reward-name: (string-ascii 50),
        description: (string-ascii 150),
        points-cost: uint,
        reward-type: (string-ascii 15),
        discount-percentage: uint,
        is-available: bool,
        tier-requirement: uint,
        max-redemptions: uint,
        total-redeemed: uint
    }
)

;; User reward redemptions
(define-map user-redemptions
    {user: principal, reward-id: uint}
    {
        redeemed-at: uint,
        redemption-count: uint,
        expires-at: uint
    }
)

;; Public functions

;; Initialize loyalty tiers (admin only)
(define-public (init-loyalty-tiers)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set loyalty-tiers u1 
            {tier-name: "Bronze", points-required: u0, booking-discount: u5, 
             priority-support: false, exclusive-properties: false, tier-bonus-rate: u100})
        (map-set loyalty-tiers u2 
            {tier-name: "Silver", points-required: u1000, booking-discount: u10, 
             priority-support: true, exclusive-properties: false, tier-bonus-rate: u110})
        (map-set loyalty-tiers u3 
            {tier-name: "Gold", points-required: u2500, booking-discount: u15, 
             priority-support: true, exclusive-properties: true, tier-bonus-rate: u125})
        (map-set loyalty-tiers u4 
            {tier-name: "Platinum", points-required: u5000, booking-discount: u20, 
             priority-support: true, exclusive-properties: true, tier-bonus-rate: u150})
        (ok true)
    )
)

;; Initialize point activities (admin only)
(define-public (init-point-activities)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set point-activities "booking"
            {activity-name: "booking", points-per-action: u100, is-active: true, max-daily-points: u500})
        (map-set point-activities "hosting"
            {activity-name: "hosting", points-per-action: u150, is-active: true, max-daily-points: u750})
        (map-set point-activities "review"
            {activity-name: "review", points-per-action: u50, is-active: true, max-daily-points: u200})
        (map-set point-activities "referral"
            {activity-name: "referral", points-per-action: u300, is-active: true, max-daily-points: u600})
        (ok true)
    )
)

;; Award points for activity
(define-public (award-points 
    (user principal)
    (activity (string-ascii 20))
    (multiplier uint))
    (let
        (
            (activity-data (unwrap! (map-get? point-activities activity) err-invalid-parameters))
            (user-account (default-to 
                {total-points: u0, current-tier: u1, lifetime-bookings: u0, lifetime-hosting: u0,
                 points-earned-this-period: u0, points-redeemed: u0, tier-achieved-at: stacks-block-height, 
                 last-activity: u0}
                (map-get? loyalty-accounts user)))
            (base-points (get points-per-action activity-data))
            (tier-data (unwrap! (map-get? loyalty-tiers (get current-tier user-account)) err-tier-not-found))
            (tier-bonus (get tier-bonus-rate tier-data))
            (points-to-award (/ (* (* base-points multiplier) tier-bonus) u100))
            (today-block (/ stacks-block-height u144))
            (daily-tracking (default-to 
                {points-earned-today: u0, activities-completed: (list)}
                (map-get? daily-point-tracking {user: user, date: today-block})))
        )
        (asserts! (get is-active activity-data) err-invalid-parameters)
        (asserts! (> multiplier u0) err-invalid-parameters)
        
        ;; Check daily limits
        (asserts! (<= (+ (get points-earned-today daily-tracking) points-to-award) 
                     (get max-daily-points activity-data)) err-invalid-parameters)
        
        ;; Update user loyalty account
        (map-set loyalty-accounts user
            (merge user-account {
                total-points: (+ (get total-points user-account) points-to-award),
                points-earned-this-period: (+ (get points-earned-this-period user-account) points-to-award),
                last-activity: stacks-block-height
            })
        )
        
        ;; Update daily tracking
        (map-set daily-point-tracking {user: user, date: today-block}
            {
                points-earned-today: (+ (get points-earned-today daily-tracking) points-to-award),
                activities-completed: (unwrap-panic (as-max-len? 
                    (append (get activities-completed daily-tracking) activity) u5))
            }
        )
        
        ;; Check for tier upgrade
        (try! (check-tier-upgrade user))
        (ok points-to-award)
    )
)

;; Check and upgrade user tier
(define-public (check-tier-upgrade (user principal))
    (let
        (
            (user-account (unwrap! (map-get? loyalty-accounts user) err-not-authorized))
            (current-tier (get current-tier user-account))
            (total-points (get total-points user-account))
            (new-tier (calculate-tier-from-points total-points))
        )
        (if (> new-tier current-tier)
            (begin
                (map-set loyalty-accounts user
                    (merge user-account {
                        current-tier: new-tier,
                        tier-achieved-at: stacks-block-height
                    })
                )
                (ok new-tier)
            )
            (ok current-tier)
        )
    )
)

;; Calculate tier based on points
(define-private (calculate-tier-from-points (points uint))
    (if (>= points u5000) u4
        (if (>= points u2500) u3
            (if (>= points u1000) u2 u1)))
)

;; Create redeemable reward (admin only)
(define-public (create-reward
    (reward-name (string-ascii 50))
    (description (string-ascii 150))
    (points-cost uint)
    (reward-type (string-ascii 15))
    (discount-percentage uint)
    (tier-requirement uint)
    (max-redemptions uint))
    (let
        (
            (reward-id (var-get next-reward-id))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (> points-cost u0) err-invalid-parameters)
        (asserts! (<= discount-percentage u100) err-invalid-parameters)
        (asserts! (<= tier-requirement u4) err-invalid-parameters)
        
        (map-set loyalty-rewards reward-id
            {
                reward-name: reward-name,
                description: description,
                points-cost: points-cost,
                reward-type: reward-type,
                discount-percentage: discount-percentage,
                is-available: true,
                tier-requirement: tier-requirement,
                max-redemptions: max-redemptions,
                total-redeemed: u0
            }
        )
        (var-set next-reward-id (+ reward-id u1))
        (ok reward-id)
    )
)

;; Redeem loyalty reward
(define-public (redeem-reward (reward-id uint))
    (let
        (
            (reward (unwrap! (map-get? loyalty-rewards reward-id) err-reward-not-found))
            (user-account (unwrap! (map-get? loyalty-accounts tx-sender) err-not-authorized))
            (user-redemption (default-to 
                {redeemed-at: u0, redemption-count: u0, expires-at: u0}
                (map-get? user-redemptions {user: tx-sender, reward-id: reward-id})))
        )
        (asserts! (get is-available reward) err-reward-unavailable)
        (asserts! (>= (get total-points user-account) (get points-cost reward)) err-insufficient-points)
        (asserts! (>= (get current-tier user-account) (get tier-requirement reward)) err-not-authorized)
        (asserts! (< (get total-redeemed reward) (get max-redemptions reward)) err-reward-unavailable)
        
        ;; Deduct points from user account
        (map-set loyalty-accounts tx-sender
            (merge user-account {
                total-points: (- (get total-points user-account) (get points-cost reward)),
                points-redeemed: (+ (get points-redeemed user-account) (get points-cost reward))
            })
        )
        
        ;; Record redemption
        (map-set user-redemptions {user: tx-sender, reward-id: reward-id}
            {
                redeemed-at: stacks-block-height,
                redemption-count: (+ (get redemption-count user-redemption) u1),
                expires-at: (+ stacks-block-height u4320) ;; 30 days
            }
        )
        
        ;; Update reward usage
        (map-set loyalty-rewards reward-id
            (merge reward {total-redeemed: (+ (get total-redeemed reward) u1)})
        )
        
        (ok true)
    )
)

;; Fund loyalty pool for rewards
(define-public (fund-loyalty-pool (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-parameters)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set loyalty-pool-balance (+ (var-get loyalty-pool-balance) amount))
        (ok true)
    )
)

;; Read-only functions

;; Get user loyalty account
(define-read-only (get-loyalty-account (user principal))
    (map-get? loyalty-accounts user)
)

;; Get tier information
(define-read-only (get-tier-info (tier uint))
    (map-get? loyalty-tiers tier)
)

;; Get reward details
(define-read-only (get-reward (reward-id uint))
    (map-get? loyalty-rewards reward-id)
)

;; Get user redemption history
(define-read-only (get-user-redemption (user principal) (reward-id uint))
    (map-get? user-redemptions {user: user, reward-id: reward-id})
)

;; Calculate discount for user
(define-read-only (calculate-loyalty-discount (user principal) (base-amount uint))
    (match (map-get? loyalty-accounts user)
        account-data
        (match (map-get? loyalty-tiers (get current-tier account-data))
            tier-data
            (let
                (
                    (discount-rate (get booking-discount tier-data))
                    (discount-amount (/ (* base-amount discount-rate) u100))
                )
                (some {
                    original-amount: base-amount,
                    discount-amount: discount-amount,
                    final-amount: (- base-amount discount-amount),
                    tier: (get current-tier account-data)
                })
            )
            none
        )
        none
    )
)

;; Check tier benefits eligibility
(define-read-only (get-user-benefits (user principal))
    (match (map-get? loyalty-accounts user)
        account-data
        (match (map-get? loyalty-tiers (get current-tier account-data))
            tier-data
            (some {
                tier: (get current-tier account-data),
                tier-name: (get tier-name tier-data),
                booking-discount: (get booking-discount tier-data),
                priority-support: (get priority-support tier-data),
                exclusive-properties: (get exclusive-properties tier-data),
                points-to-next-tier: (calculate-points-to-next-tier account-data)
            })
            none
        )
        none
    )
)

;; Calculate points needed for next tier
(define-private (calculate-points-to-next-tier (account-data (tuple (total-points uint) (current-tier uint) (lifetime-bookings uint) (lifetime-hosting uint) (points-earned-this-period uint) (points-redeemed uint) (tier-achieved-at uint) (last-activity uint))))
    (let
        (
            (current-tier (get current-tier account-data))
            (current-points (get total-points account-data))
            (next-tier (+ current-tier u1))
        )
        (if (<= next-tier u4)
            (match (map-get? loyalty-tiers next-tier)
                next-tier-data
                (- (get points-required next-tier-data) current-points)
                u0
            )
            u0
        )
    )
)

;; Get loyalty program statistics
(define-read-only (get-loyalty-stats)
    (ok {
        total-rewards: (- (var-get next-reward-id) u1),
        loyalty-pool-balance: (var-get loyalty-pool-balance)
    })
)

;; Administrative functions

;; Update tier requirements (admin only)
(define-public (update-tier-requirements 
    (tier uint)
    (points-required uint)
    (booking-discount uint))
    (let
        (
            (tier-data (unwrap! (map-get? loyalty-tiers tier) err-tier-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (<= booking-discount u50) err-invalid-parameters)
        (map-set loyalty-tiers tier
            (merge tier-data {
                points-required: points-required,
                booking-discount: booking-discount
            })
        )
        (ok true)
    )
)

;; Toggle reward availability (admin only)
(define-public (toggle-reward-availability (reward-id uint))
    (let
        (
            (reward (unwrap! (map-get? loyalty-rewards reward-id) err-reward-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set loyalty-rewards reward-id
            (merge reward {is-available: (not (get is-available reward))})
        )
        (ok true)
    )
)
