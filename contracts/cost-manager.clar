;; Cost Manager Contract
;; Manages maintenance costs and budgets

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_BUDGET_NOT_FOUND (err u301))
(define-constant ERR_INSUFFICIENT_BUDGET (err u302))
(define-constant ERR_INVALID_AMOUNT (err u303))

;; Budget data structure
(define-map budgets
  { budget-id: uint }
  {
    owner: principal,
    asset-id: (string-ascii 64),
    budget-period: (string-ascii 16), ;; monthly, quarterly, yearly
    allocated-amount: uint,
    spent-amount: uint,
    remaining-amount: uint,
    start-date: uint,
    end-date: uint
  }
)

;; Cost tracking
(define-map cost-records
  { cost-id: uint }
  {
    asset-id: (string-ascii 64),
    budget-id: uint,
    cost-type: (string-ascii 32),
    amount: uint,
    date: uint,
    description: (string-ascii 256),
    recorded-by: principal
  }
)

;; Cost categories
(define-map cost-categories
  { category: (string-ascii 32) }
  { total-spent: uint, transaction-count: uint }
)

;; Counters
(define-data-var budget-count uint u0)
(define-data-var cost-count uint u0)

;; Create budget
(define-public (create-budget
  (asset-id (string-ascii 64))
  (budget-period (string-ascii 16))
  (allocated-amount uint)
  (start-date uint)
  (end-date uint))
  (let ((budget-id (var-get budget-count)))
    (asserts! (> allocated-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> end-date start-date) ERR_INVALID_AMOUNT)

    (map-set budgets
      { budget-id: budget-id }
      {
        owner: tx-sender,
        asset-id: asset-id,
        budget-period: budget-period,
        allocated-amount: allocated-amount,
        spent-amount: u0,
        remaining-amount: allocated-amount,
        start-date: start-date,
        end-date: end-date
      }
    )

    (var-set budget-count (+ budget-id u1))
    (ok budget-id)
  )
)

;; Record cost
(define-public (record-cost
  (asset-id (string-ascii 64))
  (budget-id uint)
  (cost-type (string-ascii 32))
  (amount uint)
  (description (string-ascii 256)))
  (let ((budget (unwrap! (map-get? budgets { budget-id: budget-id }) ERR_BUDGET_NOT_FOUND))
        (cost-id (var-get cost-count)))
    (asserts! (is-eq (get owner budget) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (>= (get remaining-amount budget) amount) ERR_INSUFFICIENT_BUDGET)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)

    ;; Record the cost
    (map-set cost-records
      { cost-id: cost-id }
      {
        asset-id: asset-id,
        budget-id: budget-id,
        cost-type: cost-type,
        amount: amount,
        date: block-height,
        description: description,
        recorded-by: tx-sender
      }
    )

    ;; Update budget
    (map-set budgets
      { budget-id: budget-id }
      (merge budget {
        spent-amount: (+ (get spent-amount budget) amount),
        remaining-amount: (- (get remaining-amount budget) amount)
      })
    )

    ;; Update cost category totals
    (update-cost-category cost-type amount)

    (var-set cost-count (+ cost-id u1))
    (ok cost-id)
  )
)

;; Update cost category totals
(define-private (update-cost-category (category (string-ascii 32)) (amount uint))
  (match (map-get? cost-categories { category: category })
    existing (map-set cost-categories
               { category: category }
               {
                 total-spent: (+ (get total-spent existing) amount),
                 transaction-count: (+ (get transaction-count existing) u1)
               })
    (map-set cost-categories
      { category: category }
      { total-spent: amount, transaction-count: u1 })
  )
)

;; Get budget information
(define-read-only (get-budget (budget-id uint))
  (map-get? budgets { budget-id: budget-id })
)

;; Get cost record
(define-read-only (get-cost-record (cost-id uint))
  (map-get? cost-records { cost-id: cost-id })
)

;; Get cost category totals
(define-read-only (get-cost-category (category (string-ascii 32)))
  (map-get? cost-categories { category: category })
)

;; Calculate budget utilization percentage
(define-read-only (get-budget-utilization (budget-id uint))
  (match (map-get? budgets { budget-id: budget-id })
    budget (let ((utilization (/ (* (get spent-amount budget) u100) (get allocated-amount budget))))
             (ok utilization))
    ERR_BUDGET_NOT_FOUND
  )
)

;; Check if budget is exceeded
(define-read-only (is-budget-exceeded (budget-id uint))
  (match (map-get? budgets { budget-id: budget-id })
    budget (ok (is-eq (get remaining-amount budget) u0))
    ERR_BUDGET_NOT_FOUND
  )
)

;; Get total costs for asset
(define-read-only (get-asset-total-costs (asset-id (string-ascii 64)))
  ;; This would require iteration in a real implementation
  ;; For now, return a placeholder
  (ok u0)
)

;; Generate cost report
(define-public (generate-cost-report (budget-id uint))
  (let ((budget (unwrap! (map-get? budgets { budget-id: budget-id }) ERR_BUDGET_NOT_FOUND)))
    (asserts! (is-eq (get owner budget) tx-sender) ERR_UNAUTHORIZED)
    ;; This would generate a comprehensive cost report
    (ok "cost-report-generated")
  )
)

;; Get budget count
(define-read-only (get-budget-count)
  (var-get budget-count)
)

;; Get cost count
(define-read-only (get-cost-count)
  (var-get cost-count)
)
