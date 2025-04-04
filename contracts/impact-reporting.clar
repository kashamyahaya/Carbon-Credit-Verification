;; impact-reporting.clar
;; Tracks environmental benefits of reforestation

(define-map impact-reports
  { project-id: uint, report-id: uint }
  {
    timestamp: uint,
    carbon-sequestered: uint,
    biodiversity-score: uint,
    water-quality-impact: uint,
    soil-health-impact: uint,
    community-benefits: (string-utf8 200),
    reporter: principal
  }
)

(define-map project-report-count
  { project-id: uint }
  { count: uint }
)

(define-map authorized-reporters
  { reporter: principal }
  { authorized: bool }
)

(define-data-var admin principal tx-sender)

(define-public (add-reporter (reporter principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u1))
    (map-set authorized-reporters
      { reporter: reporter }
      { authorized: true }
    )
    (ok true)
  )
)

(define-read-only (is-authorized-reporter (reporter principal))
  (default-to false (get authorized (map-get? authorized-reporters { reporter: reporter })))
)

(define-public (submit-impact-report
    (project-id uint)
    (carbon-sequestered uint)
    (biodiversity-score uint)
    (water-quality-impact uint)
    (soil-health-impact uint)
    (community-benefits (string-utf8 200)))
  (let (
    (report-count (default-to u0 (get count (map-get? project-report-count { project-id: project-id }))))
    (next-report-id (+ report-count u1))
  )
    (asserts! (is-authorized-reporter tx-sender) (err u2))
    (map-set impact-reports
      { project-id: project-id, report-id: next-report-id }
      {
        timestamp: block-height,
        carbon-sequestered: carbon-sequestered,
        biodiversity-score: biodiversity-score,
        water-quality-impact: water-quality-impact,
        soil-health-impact: soil-health-impact,
        community-benefits: community-benefits,
        reporter: tx-sender
      }
    )
    (map-set project-report-count
      { project-id: project-id }
      { count: next-report-id }
    )
    (ok next-report-id)
  )
)

(define-read-only (get-impact-report (project-id uint) (report-id uint))
  (map-get? impact-reports { project-id: project-id, report-id: report-id })
)

(define-read-only (get-report-count (project-id uint))
  (default-to u0 (get count (map-get? project-report-count { project-id: project-id })))
)

(define-read-only (get-total-carbon-impact (project-id uint))
  (let (
    (report-count (get-report-count project-id))
    (total-carbon u0)
  )
    (fold add-carbon-impact (list u1 u2 u3 u4 u5) { project-id: project-id, total: u0 })
  )
)

(define-private (add-carbon-impact (report-id uint) (state { project-id: uint, total: uint }))
  (let (
    (project-id (get project-id state))
    (current-total (get total state))
    (report (map-get? impact-reports { project-id: project-id, report-id: report-id }))
  )
    (if (is-some report)
      { project-id: project-id, total: (+ current-total (default-to u0 (get carbon-sequestered report))) }
      state
    )
  )
)
