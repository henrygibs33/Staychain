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

(define-data-var next-property-id uint u1)
(define-data-var next-booking-id uint u1)

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
      (total-amount (* nights (get price-per-night property)))
      (booking-id (var-get next-booking-id))
    )
    (asserts! (> check-out check-in) ERR_INVALID_DATES)
    (asserts! (<= guests (get max-guests property)) ERR_INVALID_DATES)
    (asserts! (get is-active property) ERR_PROPERTY_NOT_AVAILABLE)
    (asserts! (is-property-available property-id check-in check-out) ERR_PROPERTY_NOT_AVAILABLE)
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

