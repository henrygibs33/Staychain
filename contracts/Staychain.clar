(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_BOOKING_NOT_FOUND (err u102))
(define-constant ERR_INVALID_DATES (err u103))
(define-constant ERR_PROPERTY_NOT_AVAILABLE (err u104))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u105))
(define-constant ERR_BOOKING_NOT_ACTIVE (err u106))
(define-constant ERR_ALREADY_REVIEWED (err u107))
(define-constant ERR_CANNOT_REVIEW_OWN_PROPERTY (err u108))
(define-constant ERR_DISPUTE_NOT_FOUND (err u109))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u110))
(define-constant ERR_INVALID_DISPUTE_STATUS (err u111))
(define-constant ERR_NOT_DISPUTING_PARTY (err u112))
(define-constant ERR_ALREADY_VOTED (err u113))
(define-constant ERR_DISPUTE_NOT_OPEN (err u114))
(define-constant ERR_INSUFFICIENT_ARBITRATOR_STAKE (err u115))
(define-constant ERR_NOT_ARBITRATOR (err u116))
(define-constant ERR_INVALID_PRICING_RULE (err u117))
(define-constant ERR_PRICING_RULE_NOT_FOUND (err u118))
(define-constant ERR_INVALID_DEMAND_THRESHOLD (err u119))
(define-constant ERR_INVALID_PRICE_MULTIPLIER (err u120))

(define-data-var next-property-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var arbitrator-stake-required uint u1000000)
(define-data-var next-pricing-rule-id uint u1)

(define-map properties
  { property-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    price-per-night: uint,
    location: (string-ascii 100),
    max-guests: uint,
    is-active: bool,
    total-bookings: uint,
    average-rating: uint
  }
)

(define-map bookings
  { booking-id: uint }
  {
    property-id: uint,
    guest: principal,
    check-in: uint,
    check-out: uint,
    total-amount: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map property-availability
  { property-id: uint, date: uint }
  { is-available: bool }
)

(define-map reviews
  { property-id: uint, reviewer: principal }
  {
    rating: uint,
    comment: (string-ascii 300),
    booking-id: uint,
    created-at: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    name: (string-ascii 50),
    total-properties: uint,
    total-bookings: uint,
    host-rating: uint,
    guest-rating: uint
  }
)

(define-map disputes
  { dispute-id: uint }
  {
    booking-id: uint,
    initiator: principal,
    respondent: principal,
    dispute-type: (string-ascii 30),
    description: (string-ascii 500),
    evidence-hash: (string-ascii 64),
    status: (string-ascii 20),
    escrow-amount: uint,
    created-at: uint,
    resolution-deadline: uint,
    final-decision: (string-ascii 20),
    arbitrator-count: uint
  }
)

(define-map dispute-votes
  { dispute-id: uint, arbitrator: principal }
  {
    vote: (string-ascii 20),
    reasoning: (string-ascii 300),
    voted-at: uint
  }
)

(define-map arbitrators
  { arbitrator: principal }
  {
    stake-amount: uint,
    cases-resolved: uint,
    reputation-score: uint,
    is-active: bool,
    registered-at: uint
  }
)

(define-map dispute-arbitrator-assignments
  { dispute-id: uint, arbitrator: principal }
  { assigned-at: uint }
)

;; Dynamic pricing system maps
(define-map property-pricing-rules
  { property-id: uint }
  {
    base-price: uint,
    demand-multiplier: uint,
    seasonal-multiplier: uint,
    peak-season-start: uint,
    peak-season-end: uint,
    is-dynamic-pricing-enabled: bool,
    last-price-update: uint
  }
)

(define-map property-demand-metrics
  { property-id: uint }
  {
    booking-requests-30d: uint,
    successful-bookings-30d: uint,
    average-stay-duration: uint,
    demand-score: uint,
    last-metrics-update: uint
  }
)

(define-map daily-booking-stats
  { property-id: uint, date: uint }
  {
    booking-requests: uint,
    successful-bookings: uint,
    average-price: uint
  }
)

(define-map market-pricing-data
  { location: (string-ascii 100) }
  {
    average-market-price: uint,
    property-count: uint,
    last-updated: uint
  }
)


(define-read-only (is-date-available (property-id uint) (date uint))
  (default-to true (get is-available (map-get? property-availability { property-id: property-id, date: date })))
)

(define-private (update-user-property-count (user principal))
  (let
    (
      (profile (default-to { name: "", total-properties: u0, total-bookings: u0, host-rating: u0, guest-rating: u0 } (map-get? user-profiles { user: user })))
    )
    (map-set user-profiles
      { user: user }
      (merge profile { total-properties: (+ (get total-properties profile) u1) })
    )
  )
)

(define-public (create-property (title (string-ascii 100)) (description (string-ascii 500)) (price-per-night uint) (location (string-ascii 100)) (max-guests uint))
  (let
    (
      (property-id (var-get next-property-id))
    )
    (map-set properties
      { property-id: property-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        price-per-night: price-per-night,
        location: location,
        max-guests: max-guests,
        is-active: true,
        total-bookings: u0,
        average-rating: u0
      }
    )
    (var-set next-property-id (+ property-id u1))
    (update-user-property-count tx-sender)
    (ok property-id)
  )
)



(define-private (block-property-dates (property-id uint) (check-in uint) (check-out uint))
  (if (>= check-in check-out)
    true
    (begin
      (map-set property-availability
        { property-id: property-id, date: check-in }
        { is-available: false }
      )
    ;;   (block-property-dates property-id (+ check-in u1) check-out)
    )
  )
)

(define-private (unblock-property-dates (property-id uint) (check-in uint) (check-out uint))
  (if (>= check-in check-out)
    true
    (begin
      (map-delete property-availability { property-id: property-id, date: check-in })
    ;;   (unblock-property-dates property-id (+ check-in u1) check-out)
    )
  )
)



(define-private (update-user-booking-count (user principal))
  (let
    (
      (profile (default-to { name: "", total-properties: u0, total-bookings: u0, host-rating: u0, guest-rating: u0 } (map-get? user-profiles { user: user })))
    )
    (map-set user-profiles
      { user: user }
      (merge profile { total-bookings: (+ (get total-bookings profile) u1) })
    )
  )
)

(define-private (update-property-booking-count (property-id uint))
  (let
    (
      (property (unwrap-panic (map-get? properties { property-id: property-id })))
    )
    (map-set properties
      { property-id: property-id }
      (merge property { total-bookings: (+ (get total-bookings property) u1) })
    )
  )
)

(define-private (update-property-rating (property-id uint) (new-rating uint))
  (let
    (
      (property (unwrap-panic (map-get? properties { property-id: property-id })))
      (current-rating (get average-rating property))
      (total-bookings (get total-bookings property))
      (updated-rating (if (is-eq current-rating u0)
                        new-rating
                        (/ (+ (* current-rating total-bookings) new-rating) (+ total-bookings u1))))
    )
    (map-set properties
      { property-id: property-id }
      (merge property { average-rating: updated-rating })
    )
  )
)

(define-private (is-property-available (property-id uint) (check-in uint) (check-out uint))
  (if (>= check-in check-out)
    true
    (and
      (is-date-available property-id check-in)
    ;;   (is-property-available property-id (+ check-in u1) check-out)
    )
  )
)

(define-public (book-property (property-id uint) (check-in uint) (check-out uint) (guests uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (nights (- check-out check-in))
      ;; Use dynamic pricing if available, otherwise fall back to base price
      (current-price (let ((dynamic-price (calculate-dynamic-price property-id check-in)))
        (if (> dynamic-price u0) dynamic-price (get price-per-night property))
      ))
      (total-amount (* nights current-price))
      (booking-id (var-get next-booking-id))
    )
    (asserts! (> check-out check-in) ERR_INVALID_DATES)
    (asserts! (<= guests (get max-guests property)) ERR_INVALID_DATES)
    (asserts! (get is-active property) ERR_PROPERTY_NOT_AVAILABLE)
    (asserts! (is-property-available property-id check-in check-out) ERR_PROPERTY_NOT_AVAILABLE)
    
    ;; Update demand metrics before processing payment
    (update-demand-metrics property-id true)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    (map-set bookings
      { booking-id: booking-id }
      {
        property-id: property-id,
        guest: tx-sender,
        check-in: check-in,
        check-out: check-out,
        total-amount: total-amount,
        status: "confirmed",
        created-at: stacks-block-height
      }
    )
    (block-property-dates property-id check-in check-out)
    (var-set next-booking-id (+ booking-id u1))
    (update-property-booking-count property-id)
    (update-user-booking-count tx-sender)
    (ok booking-id)
  )
)

(define-public (cancel-booking (booking-id uint))
  (let
    (
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) ERR_BOOKING_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get guest booking)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status booking) "confirmed") ERR_BOOKING_NOT_ACTIVE)
    (asserts! (> (get check-in booking) stacks-block-height) ERR_BOOKING_NOT_ACTIVE)
    (try! (as-contract (stx-transfer? (get total-amount booking) tx-sender (get guest booking))))
    (map-set bookings
      { booking-id: booking-id }
      (merge booking { status: "cancelled" })
    )
    (unblock-property-dates (get property-id booking) (get check-in booking) (get check-out booking))
    (ok true)
  )
)

(define-public (complete-booking (booking-id uint))
  (let
    (
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) ERR_BOOKING_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: (get property-id booking) }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status booking) "confirmed") ERR_BOOKING_NOT_ACTIVE)
    (asserts! (>= stacks-block-height (get check-out booking)) ERR_BOOKING_NOT_ACTIVE)
    (try! (as-contract (stx-transfer? (get total-amount booking) tx-sender (get owner property))))
    (map-set bookings
      { booking-id: booking-id }
      (merge booking { status: "completed" })
    )
    (ok true)
  )
)

(define-public (add-review (property-id uint) (booking-id uint) (rating uint) (comment (string-ascii 300)))
  (let
    (
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) ERR_BOOKING_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get guest booking)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get property-id booking) property-id) ERR_BOOKING_NOT_FOUND)
    (asserts! (is-eq (get status booking) "completed") ERR_BOOKING_NOT_ACTIVE)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_DATES)
    (asserts! (is-none (map-get? reviews { property-id: property-id, reviewer: tx-sender })) ERR_ALREADY_REVIEWED)
    (asserts! (not (is-eq tx-sender (get owner property))) ERR_CANNOT_REVIEW_OWN_PROPERTY)
    (map-set reviews
      { property-id: property-id, reviewer: tx-sender }
      {
        rating: rating,
        comment: comment,
        booking-id: booking-id,
        created-at: stacks-block-height
      }
    )
    (update-property-rating property-id rating)
    (ok true)
  )
)

(define-public (update-property (property-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (price-per-night uint) (max-guests uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (map-set properties
      { property-id: property-id }
      (merge property {
        title: title,
        description: description,
        price-per-night: price-per-night,
        max-guests: max-guests
      })
    )
    (ok true)
  )
)

(define-public (toggle-property-status (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (map-set properties
      { property-id: property-id }
      (merge property { is-active: (not (get is-active property)) })
    )
    (ok true)
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-review (property-id uint) (reviewer principal))
  (map-get? reviews { property-id: property-id, reviewer: reviewer })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)


(define-read-only (get-total-properties)
  (- (var-get next-property-id) u1)
)

(define-read-only (get-total-bookings)
  (- (var-get next-booking-id) u1)
)

(define-public (register-arbitrator)
  (let
    (
      (stake-required (var-get arbitrator-stake-required))
    )
    (try! (stx-transfer? stake-required tx-sender (as-contract tx-sender)))
    (map-set arbitrators
      { arbitrator: tx-sender }
      {
        stake-amount: stake-required,
        cases-resolved: u0,
        reputation-score: u100,
        is-active: true,
        registered-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (deregister-arbitrator)
  (let
    (
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR_NOT_ARBITRATOR))
    )
    (asserts! (get is-active arbitrator-data) ERR_NOT_ARBITRATOR)
    (try! (as-contract (stx-transfer? (get stake-amount arbitrator-data) tx-sender tx-sender)))
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (create-dispute (booking-id uint) (dispute-type (string-ascii 30)) (description (string-ascii 500)) (evidence-hash (string-ascii 64)))
  (let
    (
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) ERR_BOOKING_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: (get property-id booking) }) ERR_PROPERTY_NOT_FOUND))
      (dispute-id (var-get next-dispute-id))
      (escrow-amount (get total-amount booking))
      (respondent (if (is-eq tx-sender (get guest booking)) (get owner property) (get guest booking)))
    )
    (asserts! (or (is-eq tx-sender (get guest booking)) (is-eq tx-sender (get owner property))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status booking) "confirmed") ERR_BOOKING_NOT_ACTIVE)
    (asserts! (is-none (map-get? disputes { dispute-id: dispute-id })) ERR_DISPUTE_ALREADY_EXISTS)
    (map-set disputes
      { dispute-id: dispute-id }
      {
        booking-id: booking-id,
        initiator: tx-sender,
        respondent: respondent,
        dispute-type: dispute-type,
        description: description,
        evidence-hash: evidence-hash,
        status: "open",
        escrow-amount: escrow-amount,
        created-at: stacks-block-height,
        resolution-deadline: (+ stacks-block-height u1440),
        final-decision: "",
        arbitrator-count: u0
      }
    )
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (submit-evidence (dispute-id uint) (evidence-hash (string-ascii 64)))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get initiator dispute)) (is-eq tx-sender (get respondent dispute))) ERR_NOT_DISPUTING_PARTY)
    (asserts! (is-eq (get status dispute) "open") ERR_DISPUTE_NOT_OPEN)
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute { evidence-hash: evidence-hash })
    )
    (ok true)
  )
)

(define-public (assign-arbitrator-to-dispute (dispute-id uint) (arbitrator principal))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: arbitrator }) ERR_NOT_ARBITRATOR))
    )
    (asserts! (is-eq (get status dispute) "open") ERR_DISPUTE_NOT_OPEN)
    (asserts! (get is-active arbitrator-data) ERR_NOT_ARBITRATOR)
    (asserts! (< (get arbitrator-count dispute) u3) ERR_INVALID_DISPUTE_STATUS)
    (asserts! (is-none (map-get? dispute-arbitrator-assignments { dispute-id: dispute-id, arbitrator: arbitrator })) ERR_ALREADY_VOTED)
    (map-set dispute-arbitrator-assignments
      { dispute-id: dispute-id, arbitrator: arbitrator }
      { assigned-at: stacks-block-height }
    )
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute { arbitrator-count: (+ (get arbitrator-count dispute) u1) })
    )
    (ok true)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote (string-ascii 20)) (reasoning (string-ascii 300)))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR_NOT_ARBITRATOR))
    )
    (asserts! (is-eq (get status dispute) "open") ERR_DISPUTE_NOT_OPEN)
    (asserts! (get is-active arbitrator-data) ERR_NOT_ARBITRATOR)
    (asserts! (is-some (map-get? dispute-arbitrator-assignments { dispute-id: dispute-id, arbitrator: tx-sender })) ERR_NOT_ARBITRATOR)
    (asserts! (is-none (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: tx-sender })) ERR_ALREADY_VOTED)
    (asserts! (< stacks-block-height (get resolution-deadline dispute)) ERR_BOOKING_NOT_ACTIVE)
    (map-set dispute-votes
      { dispute-id: dispute-id, arbitrator: tx-sender }
      {
        vote: vote,
        reasoning: reasoning,
        voted-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-private (count-votes-for-decision (dispute-id uint) (decision (string-ascii 20)))
  u1
)

(define-public (resolve-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (booking (unwrap! (map-get? bookings { booking-id: (get booking-id dispute) }) ERR_BOOKING_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: (get property-id booking) }) ERR_PROPERTY_NOT_FOUND))
      (initiator-favor-votes (count-votes-for-decision dispute-id "initiator"))
      (respondent-favor-votes (count-votes-for-decision dispute-id "respondent"))
      (final-decision (if (> initiator-favor-votes respondent-favor-votes) "initiator" "respondent"))
    )
    (asserts! (is-eq (get status dispute) "open") ERR_INVALID_DISPUTE_STATUS)
    (asserts! (>= stacks-block-height (get resolution-deadline dispute)) ERR_BOOKING_NOT_ACTIVE)
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute { 
        status: "resolved",
        final-decision: final-decision
      })
    )
    (if (is-eq final-decision "initiator")
      (try! (as-contract (stx-transfer? (get escrow-amount dispute) tx-sender (get initiator dispute))))
      (try! (as-contract (stx-transfer? (get escrow-amount dispute) tx-sender (get respondent dispute))))
    )
    (ok final-decision)
  )
)

(define-public (update-arbitrator-reputation (arbitrator principal) (dispute-id uint))
  (let
    (
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: arbitrator }) ERR_NOT_ARBITRATOR))
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    )
    (asserts! (is-eq (get status dispute) "resolved") ERR_INVALID_DISPUTE_STATUS)
    (map-set arbitrators
      { arbitrator: arbitrator }
      (merge arbitrator-data { 
        cases-resolved: (+ (get cases-resolved arbitrator-data) u1),
        reputation-score: (+ (get reputation-score arbitrator-data) u10)
      })
    )
    (ok true)
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-dispute-vote (dispute-id uint) (arbitrator principal))
  (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: arbitrator })
)

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-total-disputes)
  (- (var-get next-dispute-id) u1)
)

(define-read-only (is-arbitrator-assigned (dispute-id uint) (arbitrator principal))
  (is-some (map-get? dispute-arbitrator-assignments { dispute-id: dispute-id, arbitrator: arbitrator }))
)

;; Dynamic pricing engine functions
(define-public (setup-dynamic-pricing (property-id uint) (base-price uint) (demand-multiplier uint) (seasonal-multiplier uint) (peak-start uint) (peak-end uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    ;; Validate property ownership
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    ;; Validate pricing parameters
    (asserts! (> base-price u0) ERR_INVALID_PRICING_RULE)
    (asserts! (and (>= demand-multiplier u50) (<= demand-multiplier u300)) ERR_INVALID_PRICE_MULTIPLIER)
    (asserts! (and (>= seasonal-multiplier u50) (<= seasonal-multiplier u300)) ERR_INVALID_PRICE_MULTIPLIER)
    (asserts! (< peak-start peak-end) ERR_INVALID_PRICING_RULE)
    
    ;; Create pricing rule for property
    (map-set property-pricing-rules
      { property-id: property-id }
      {
        base-price: base-price,
        demand-multiplier: demand-multiplier,
        seasonal-multiplier: seasonal-multiplier,
        peak-season-start: peak-start,
        peak-season-end: peak-end,
        is-dynamic-pricing-enabled: true,
        last-price-update: stacks-block-height
      }
    )
    
    ;; Initialize demand metrics
    (map-set property-demand-metrics
      { property-id: property-id }
      {
        booking-requests-30d: u0,
        successful-bookings-30d: u0,
        average-stay-duration: u0,
        demand-score: u100,
        last-metrics-update: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (toggle-dynamic-pricing (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (pricing-rule (unwrap! (map-get? property-pricing-rules { property-id: property-id }) ERR_PRICING_RULE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    
    (map-set property-pricing-rules
      { property-id: property-id }
      (merge pricing-rule { 
        is-dynamic-pricing-enabled: (not (get is-dynamic-pricing-enabled pricing-rule))
      })
    )
    (ok true)
  )
)

;; Calculate current dynamic price based on demand and seasonality
(define-read-only (calculate-dynamic-price (property-id uint) (check-date uint))
  (match (map-get? property-pricing-rules { property-id: property-id })
    pricing-rule
    (let
      (
        (base-price (get base-price pricing-rule))
        (demand-metrics (default-to 
          { booking-requests-30d: u0, successful-bookings-30d: u0, average-stay-duration: u0, demand-score: u100, last-metrics-update: u0 }
          (map-get? property-demand-metrics { property-id: property-id })
        ))
        (is-peak-season (and 
          (>= check-date (get peak-season-start pricing-rule))
          (<= check-date (get peak-season-end pricing-rule))
        ))
        (seasonal-multiplier (if is-peak-season (get seasonal-multiplier pricing-rule) u100))
        (demand-score (get demand-score demand-metrics))
        (demand-multiplier (get demand-multiplier pricing-rule))
        
        ;; Apply demand-based pricing adjustment
        (demand-adjusted-price (if (> demand-score u150)
          (/ (* base-price demand-multiplier) u100)
          (if (< demand-score u75)
            (/ (* base-price u80) u100)  ;; Reduce price for low demand
            base-price
          )
        ))
        
        ;; Apply seasonal adjustment
        (final-price (/ (* demand-adjusted-price seasonal-multiplier) u100))
      )
      
      (if (get is-dynamic-pricing-enabled pricing-rule)
        final-price
        base-price
      )
    )
    u0  ;; Return 0 if no pricing rule exists
  )
)

;; Update demand metrics when booking request is made
(define-private (update-demand-metrics (property-id uint) (booking-successful bool))
  (let
    (
      (current-metrics (default-to 
        { booking-requests-30d: u0, successful-bookings-30d: u0, average-stay-duration: u0, demand-score: u100, last-metrics-update: u0 }
        (map-get? property-demand-metrics { property-id: property-id })
      ))
      (new-requests (+ (get booking-requests-30d current-metrics) u1))
      (new-successful (if booking-successful (+ (get successful-bookings-30d current-metrics) u1) (get successful-bookings-30d current-metrics)))
      (demand-ratio (if (> new-requests u0) (/ (* new-successful u100) new-requests) u0))
      (new-demand-score (+ u50 (/ demand-ratio u2)))  ;; Score between 50-100 based on success ratio
    )
    
    (map-set property-demand-metrics
      { property-id: property-id }
      (merge current-metrics {
        booking-requests-30d: new-requests,
        successful-bookings-30d: new-successful,
        demand-score: new-demand-score,
        last-metrics-update: stacks-block-height
      })
    )
    true
  )
)

;; Update market pricing data for location
(define-public (update-market-pricing (location (string-ascii 100)) (average-price uint) (property-count uint))
  (begin
    (map-set market-pricing-data
      { location: location }
      {
        average-market-price: average-price,
        property-count: property-count,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Get market comparison data
(define-read-only (get-market-comparison (property-id uint))
  (match (map-get? properties { property-id: property-id })
    property
    (match (map-get? market-pricing-data { location: (get location property) })
      market-data
      (let
        (
          (current-price (calculate-dynamic-price property-id stacks-block-height))
          (market-average (get average-market-price market-data))
          (price-difference (if (> current-price market-average) 
            (- current-price market-average)
            (- market-average current-price)
          ))
          (percentage-diff (if (> market-average u0) (/ (* price-difference u100) market-average) u0))
        )
        (some {
          current-price: current-price,
          market-average: market-average,
          percentage-difference: percentage-diff,
          is-above-market: (> current-price market-average)
        })
      )
      none
    )
    none
  )
)

;; Batch update pricing for multiple properties
(define-public (batch-update-pricing (property-ids (list 10 uint)))
  (let
    (
      (results (map update-single-property-pricing property-ids))
    )
    (ok (len results))
  )
)

(define-private (update-single-property-pricing (property-id uint))
  (let
    (
      (pricing-rule (map-get? property-pricing-rules { property-id: property-id }))
    )
    (match pricing-rule
      rule
      (begin
        (map-set property-pricing-rules
          { property-id: property-id }
          (merge rule { last-price-update: stacks-block-height })
        )
        true
      )
      false
    )
  )
)

;; Get comprehensive pricing analytics
(define-read-only (get-pricing-analytics (property-id uint))
  (let
    (
      (pricing-rule (map-get? property-pricing-rules { property-id: property-id }))
      (demand-metrics (map-get? property-demand-metrics { property-id: property-id }))
      (current-price (calculate-dynamic-price property-id stacks-block-height))
    )
    {
      pricing-rule: pricing-rule,
      demand-metrics: demand-metrics,
      current-price: current-price,
      market-comparison: (get-market-comparison property-id)
    }
  )
)

;; Read-only functions for pricing data
(define-read-only (get-property-pricing-rule (property-id uint))
  (map-get? property-pricing-rules { property-id: property-id })
)

(define-read-only (get-property-demand-metrics (property-id uint))
  (map-get? property-demand-metrics { property-id: property-id })
)

(define-read-only (get-market-pricing-data (location (string-ascii 100)))
  (map-get? market-pricing-data { location: location })
)


