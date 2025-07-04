;; Maintenance Scheduler Contract
;; Optimizes and manages maintenance schedules

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_SCHEDULE_NOT_FOUND (err u201))
(define-constant ERR_INVALID_SCHEDULE (err u202))
(define-constant ERR_SCHEDULE_CONFLICT (err u203))

;; Schedule data structure
(define-map maintenance-schedules
  { schedule-id: uint }
  {
    asset-id: (string-ascii 64),
    scheduled-by: principal,
    maintenance-type: (string-ascii 32),
    scheduled-date: uint,
    estimated-duration: uint,
    priority: uint,
    status: (string-ascii 16),
    assigned-technician: (optional principal),
    estimated-cost: uint
  }
)

;; Resource allocation
(define-map resource-allocation
  { date: uint, resource-type: (string-ascii 32) }
  { allocated-capacity: uint, available-capacity: uint }
)

;; Schedule counters
(define-data-var schedule-count uint u0)

;; Create maintenance schedule
(define-public (create-schedule
  (asset-id (string-ascii 64))
  (maintenance-type (string-ascii 32))
  (scheduled-date uint)
  (estimated-duration uint)
  (priority uint)
  (estimated-cost uint))
  (let ((schedule-id (var-get schedule-count)))
    (asserts! (> scheduled-date block-height) ERR_INVALID_SCHEDULE)
    (asserts! (<= priority u5) ERR_INVALID_SCHEDULE)
    (asserts! (> estimated-duration u0) ERR_INVALID_SCHEDULE)

    ;; Check resource availability
    (asserts! (check-resource-availability scheduled-date estimated-duration) ERR_SCHEDULE_CONFLICT)

    (map-set maintenance-schedules
      { schedule-id: schedule-id }
      {
        asset-id: asset-id,
        scheduled-by: tx-sender,
        maintenance-type: maintenance-type,
        scheduled-date: scheduled-date,
        estimated-duration: estimated-duration,
        priority: priority,
        status: "scheduled",
        assigned-technician: none,
        estimated-cost: estimated-cost
      }
    )

    ;; Update resource allocation
    (update-resource-allocation scheduled-date "technician" estimated-duration)

    (var-set schedule-count (+ schedule-id u1))
    (ok schedule-id)
  )
)

;; Assign technician to schedule
(define-public (assign-technician
  (schedule-id uint)
  (technician principal))
  (let ((schedule (unwrap! (map-get? maintenance-schedules { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND)))
    (asserts! (is-eq (get scheduled-by schedule) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status schedule) "scheduled") ERR_INVALID_SCHEDULE)

    (map-set maintenance-schedules
      { schedule-id: schedule-id }
      (merge schedule { assigned-technician: (some technician) })
    )
    (ok true)
  )
)

;; Update schedule status
(define-public (update-schedule-status
  (schedule-id uint)
  (new-status (string-ascii 16)))
  (let ((schedule (unwrap! (map-get? maintenance-schedules { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND)))
    (asserts! (or (is-eq (get scheduled-by schedule) tx-sender)
                  (is-eq (get assigned-technician schedule) (some tx-sender))) ERR_UNAUTHORIZED)

    (map-set maintenance-schedules
      { schedule-id: schedule-id }
      (merge schedule { status: new-status })
    )
    (ok true)
  )
)

;; Check resource availability
(define-private (check-resource-availability (date uint) (duration uint))
  (match (map-get? resource-allocation { date: date, resource-type: "technician" })
    allocation (>= (get available-capacity allocation) duration)
    true ;; No allocation exists, assume available
  )
)

;; Update resource allocation
(define-private (update-resource-allocation (date uint) (resource-type (string-ascii 32)) (duration uint))
  (match (map-get? resource-allocation { date: date, resource-type: resource-type })
    allocation (map-set resource-allocation
                 { date: date, resource-type: resource-type }
                 (merge allocation {
                   allocated-capacity: (+ (get allocated-capacity allocation) duration),
                   available-capacity: (- (get available-capacity allocation) duration)
                 }))
    (map-set resource-allocation
      { date: date, resource-type: resource-type }
      { allocated-capacity: duration, available-capacity: (- u480 duration) }) ;; 8 hours = 480 minutes
  )
)

;; Get schedule information
(define-read-only (get-schedule (schedule-id uint))
  (map-get? maintenance-schedules { schedule-id: schedule-id })
)

;; Get resource allocation for date
(define-read-only (get-resource-allocation (date uint) (resource-type (string-ascii 32)))
  (map-get? resource-allocation { date: date, resource-type: resource-type })
)

;; Get schedules by asset
(define-read-only (get-schedules-by-asset (asset-id (string-ascii 64)))
  ;; This would require iteration in a real implementation
  ;; For now, return a simple response
  (ok "schedules-found")
)

;; Optimize schedule based on priority and resources
(define-public (optimize-schedules (date-range-start uint) (date-range-end uint))
  (begin
    ;; This would implement optimization algorithm
    ;; For now, return success
    (ok "schedules-optimized")
  )
)

;; Get total schedule count
(define-read-only (get-schedule-count)
  (var-get schedule-count)
)
