;; Decentralized University - Student-governed educational institution with tokenized degrees
;; A comprehensive smart contract for managing a decentralized university system

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-course-not-active (err u106))
(define-constant err-already-enrolled (err u107))
(define-constant err-not-enrolled (err u108))
(define-constant err-course-full (err u109))
(define-constant err-invalid-grade (err u110))

;; Data Variables
(define-data-var next-student-id uint u1)
(define-data-var next-course-id uint u1)
(define-data-var next-degree-id uint u1)
(define-data-var university-treasury uint u0)

;; Data Maps
(define-map students
  { student-id: uint }
  {
    address: principal,
    name: (string-ascii 50),
    email: (string-ascii 100),
    enrolled-courses: (list 20 uint),
    completed-courses: (list 50 uint),
    total-credits: uint,
    gpa: uint,
    governance-tokens: uint,
    registration-block: uint
  }
)

(define-map courses
  { course-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    instructor: principal,
    credits: uint,
    max-students: uint,
    enrolled-count: uint,
    tuition-cost: uint,
    is-active: bool,
    creation-block: uint
  }
)

(define-map enrollments
  { student-id: uint, course-id: uint }
  {
    enrollment-block: uint,
    completion-block: (optional uint),
    grade: (optional uint),
    is-completed: bool
  }
)

(define-map degrees
  { degree-id: uint }
  {
    name: (string-ascii 100),
    required-credits: uint,
    required-courses: (list 20 uint),
    token-symbol: (string-ascii 10),
    is-active: bool
  }
)

(define-map student-degrees
  { student-id: uint, degree-id: uint }
  {
    completion-block: uint,
    nft-id: uint,
    verification-hash: (buff 32)
  }
)

(define-map student-address-to-id
  { address: principal }
  { student-id: uint }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    votes-for: uint,
    votes-against: uint,
    voting-end-block: uint,
    is-executed: bool,
    proposal-type: (string-ascii 20)
  }
)

(define-map governance-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

;; Read-only functions
(define-read-only (get-student (student-id uint))
  (map-get? students { student-id: student-id })
)

(define-read-only (get-course (course-id uint))
  (map-get? courses { course-id: course-id })
)

(define-read-only (get-enrollment (student-id uint) (course-id uint))
  (map-get? enrollments { student-id: student-id, course-id: course-id })
)

(define-read-only (get-degree (degree-id uint))
  (map-get? degrees { degree-id: degree-id })
)

(define-read-only (get-student-by-address (address principal))
  (match (map-get? student-address-to-id { address: address })
    entry (get-student (get student-id entry))
    none
  )
)

(define-read-only (get-university-treasury)
  (var-get university-treasury)
)

(define-read-only (get-next-student-id)
  (var-get next-student-id)
)

(define-read-only (get-next-course-id)
  (var-get next-course-id)
)

(define-read-only (calculate-gpa (completed-courses (list 50 uint)) (grades (list 50 uint)))
  (let
    (
      (total-points (fold + grades u0))
      (course-count (len completed-courses))
    )
    (if (> course-count u0)
      (/ (* total-points u100) course-count)
      u0
    )
  )
)

;; Public functions for student management
(define-public (register-student (name (string-ascii 50)) (email (string-ascii 100)))
  (let
    (
      (student-id (var-get next-student-id))
      (existing-student (map-get? student-address-to-id { address: tx-sender }))
    )
    (asserts! (is-none existing-student) err-already-exists)
    (map-set students
      { student-id: student-id }
      {
        address: tx-sender,
        name: name,
        email: email,
        enrolled-courses: (list),
        completed-courses: (list),
        total-credits: u0,
        gpa: u0,
        governance-tokens: u100,
        registration-block: block-height
      }
    )
    (map-set student-address-to-id
      { address: tx-sender }
      { student-id: student-id }
    )
    (var-set next-student-id (+ student-id u1))
    (ok student-id)
  )
)

;; Public functions for course management
(define-public (create-course 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (credits uint)
  (max-students uint)
  (tuition-cost uint)
)
  (let
    (
      (course-id (var-get next-course-id))
    )
    (map-set courses
      { course-id: course-id }
      {
        name: name,
        description: description,
        instructor: tx-sender,
        credits: credits,
        max-students: max-students,
        enrolled-count: u0,
        tuition-cost: tuition-cost,
        is-active: true,
        creation-block: block-height
      }
    )
    (var-set next-course-id (+ course-id u1))
    (ok course-id)
  )
)

(define-public (enroll-in-course (course-id uint))
  (let
    (
      (student-data (unwrap! (get-student-by-address tx-sender) err-not-found))
      (student-id (unwrap! (get student-id (map-get? student-address-to-id { address: tx-sender })) err-not-found))
      (course-data (unwrap! (get-course course-id) err-not-found))
      (existing-enrollment (map-get? enrollments { student-id: student-id, course-id: course-id }))
    )
    (asserts! (get is-active course-data) err-course-not-active)
    (asserts! (< (get enrolled-count course-data) (get max-students course-data)) err-course-full)
    (asserts! (is-none existing-enrollment) err-already-enrolled)
    
    ;; Transfer tuition cost to university treasury
    (try! (stx-transfer? (get tuition-cost course-data) tx-sender (as-contract tx-sender)))
    (var-set university-treasury (+ (var-get university-treasury) (get tuition-cost course-data)))
    
    ;; Create enrollment record
    (map-set enrollments
      { student-id: student-id, course-id: course-id }
      {
        enrollment-block: block-height,
        completion-block: none,
        grade: none,
        is-completed: false
      }
    )
    
    ;; Update course enrollment count
    (map-set courses
      { course-id: course-id }
      (merge course-data { enrolled-count: (+ (get enrolled-count course-data) u1) })
    )
    
    ;; Update student enrolled courses
    (let
      (
        (updated-enrolled-courses 
          (unwrap! (as-max-len? (append (get enrolled-courses student-data) course-id) u20) err-course-full)
        )
      )
      (map-set students
        { student-id: student-id }
        (merge student-data { enrolled-courses: updated-enrolled-courses })
      )
    )
    
    (ok true)
  )
)

(define-public (submit-grade (student-id uint) (course-id uint) (grade uint))
  (let
    (
      (course-data (unwrap! (get-course course-id) err-not-found))
      (enrollment-data (unwrap! (get-enrollment student-id course-id) err-not-enrolled))
      (student-data (unwrap! (get-student student-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get instructor course-data)) err-unauthorized)
    (asserts! (<= grade u100) err-invalid-grade)
    (asserts! (not (get is-completed enrollment-data)) err-already-exists)
    
    ;; Update enrollment with grade and completion
    (map-set enrollments
      { student-id: student-id, course-id: course-id }
      (merge enrollment-data 
        {
          completion-block: (some block-height),
          grade: (some grade),
          is-completed: true
        }
      )
    )
    
    ;; Update student completed courses and credits
    (let
      (
        (updated-completed-courses 
          (unwrap! (as-max-len? (append (get completed-courses student-data) course-id) u50) err-course-full)
        )
        (new-total-credits (+ (get total-credits student-data) (get credits course-data)))
        (governance-token-reward (if (>= grade u70) u10 u5))
      )
      (map-set students
        { student-id: student-id }
        (merge student-data 
          {
            completed-courses: updated-completed-courses,
            total-credits: new-total-credits,
            governance-tokens: (+ (get governance-tokens student-data) governance-token-reward)
          }
        )
      )
    )
    
    (ok true)
  )
)

;; Degree management functions
(define-public (create-degree
  (name (string-ascii 100))
  (required-credits uint)
  (required-courses (list 20 uint))
  (token-symbol (string-ascii 10))
)
  (let
    (
      (degree-id (var-get next-degree-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set degrees
      { degree-id: degree-id }
      {
        name: name,
        required-credits: required-credits,
        required-courses: required-courses,
        token-symbol: token-symbol,
        is-active: true
      }
    )
    (var-set next-degree-id (+ degree-id u1))
    (ok degree-id)
  )
)

(define-public (award-degree (student-id uint) (degree-id uint))
  (let
    (
      (student-data (unwrap! (get-student student-id) err-not-found))
      (degree-data (unwrap! (get-degree degree-id) err-not-found))
      (completed-courses (get completed-courses student-data))
      (verification-hash (keccak256 (concat (unwrap-panic (to-consensus-buff? student-id)) 
                                           (unwrap-panic (to-consensus-buff? degree-id)))))
    )
    (asserts! (>= (get total-credits student-data) (get required-credits degree-data)) err-insufficient-balance)
    (asserts! (get is-active degree-data) err-not-found)
    
    ;; Award the degree
    (map-set student-degrees
      { student-id: student-id, degree-id: degree-id }
      {
        completion-block: block-height,
        nft-id: (+ (* student-id u1000) degree-id),
        verification-hash: verification-hash
      }
    )
    
    ;; Award bonus governance tokens
    (map-set students
      { student-id: student-id }
      (merge student-data { governance-tokens: (+ (get governance-tokens student-data) u50) })
    )
    
    (ok true)
  )
)

;; Governance functions
(define-public (create-proposal
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type (string-ascii 20))
)
  (let
    (
      (student-data (unwrap! (get-student-by-address tx-sender) err-not-found))
      (proposal-id (+ (var-get next-course-id) (var-get next-student-id)))
    )
    (asserts! (>= (get governance-tokens student-data) u10) err-insufficient-balance)
    
    (map-set governance-proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        votes-for: u0,
        votes-against: u0,
        voting-end-block: (+ block-height u144), ;; ~24 hours
        is-executed: false,
        proposal-type: proposal-type
      }
    )
    
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let
    (
      (student-data (unwrap! (get-student-by-address tx-sender) err-not-found))
      (proposal-data (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) err-not-found))
      (voting-power (get governance-tokens student-data))
      (existing-vote (map-get? governance-votes { proposal-id: proposal-id, voter: tx-sender }))
    )
    (asserts! (< block-height (get voting-end-block proposal-data)) err-unauthorized)
    (asserts! (is-none existing-vote) err-already-exists)
    (asserts! (> voting-power u0) err-insufficient-balance)
    
    ;; Record the vote
    (map-set governance-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote, voting-power: voting-power }
    )
    
    ;; Update proposal vote counts
    (if vote
      (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal-data { votes-for: (+ (get votes-for proposal-data) voting-power) })
      )
      (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal-data { votes-against: (+ (get votes-against proposal-data) voting-power) })
      )
    )
    
    (ok true)
  )
)

;; Utility functions
(define-public (update-course-status (course-id uint) (is-active bool))
  (let
    (
      (course-data (unwrap! (get-course course-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get instructor course-data)) err-unauthorized)
    
    (map-set courses
      { course-id: course-id }
      (merge course-data { is-active: is-active })
    )
    (ok true)
  )
)

(define-public (withdraw-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get university-treasury)) err-insufficient-balance)
    
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (var-set university-treasury (- (var-get university-treasury) amount))
    (ok true)
  )
)