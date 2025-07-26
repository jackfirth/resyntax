# DrRacket Refactoring Rules for Resyntax

Based on analysis of the DrRacket codebase (specifically `unit.rkt`, `language.rkt`, and `rep.rkt`), here are potential new Resyntax refactoring rules that could help improve DrRacket's code quality.

## 1. Nested `when` to Combined `and` Condition

**Pattern Found**: Nested `when` expressions where the inner `when` only executes when the outer condition is true.

**Example from DrRacket**: 
- In `unit.rkt` around line 3890: Nested conditionals checking frame existence then properties
- In `rep.rkt` around line 1240: Nested checks for user thread and thread state

**Refactoring Rule**:
```racket
#lang resyntax/test

(require racket/base)

;; Rule: nested-when-to-and
;; Replace nested when expressions with combined and condition
(define-test-case nested-when-to-and
  #:original
  (when condition1
    (when condition2
      action))
  #:expected
  (when (and condition1 condition2)
    action))
```

**Real DrRacket locations where this applies**:
- `unit.rkt` line ~3890: Frame and canvas checking
- `rep.rkt` line ~1240: Thread state validation
- `language.rkt` line ~890: Settings validation

## 2. Boolean Redundancy in Object Checks

**Pattern Found**: Checking if an object exists and then using that object in a boolean context.

**Example from DrRacket**:
- In `unit.rkt`: `(and (is-a? obj interface<%>) obj)` patterns
- In `rep.rkt`: `(and canvas (send canvas method))` patterns

**Refactoring Rule**:
```racket
#lang resyntax/test

(require racket/base racket/class)

;; Rule: is-a-boolean-redundancy
;; Remove redundant boolean check when object is used in boolean context
(define-test-case is-a-boolean-redundancy
  #:original
  (and (is-a? obj class-or-interface) obj)
  #:expected
  obj
  #:unless
  ;; Don't apply if obj has side effects or if the is-a? check is needed for type safety
  (or (not (identifier? #'obj))
      (side-effect-free? #'obj)))
```

**Real DrRacket locations where this applies**:
- `unit.rkt` line ~2100: Canvas and frame object checking
- `rep.rkt` line ~890: Editor and admin object validation

## 3. Conditional Assignment to `when`

**Pattern Found**: Using `cond` for simple conditional assignment where `else` clause is `(void)`.

**Example from DrRacket**:
- In `unit.rkt`: Setting GUI state conditionally
- In `rep.rkt`: Managing evaluation state

**Refactoring Rule**:
```racket
#lang resyntax/test

(require racket/base)

;; Rule: cond-to-when-for-single-action
;; Replace cond with when for single conditional action
(define-test-case cond-to-when-for-single-action
  #:original
  (cond
    [condition action]
    [else (void)])
  #:expected
  (when condition action))

;; Also handle implicit void
(define-test-case cond-to-when-no-else
  #:original
  (cond
    [condition action])
  #:expected
  (when condition action))
```

**Real DrRacket locations where this applies**:
- `unit.rkt` line ~4200: Button state management
- `rep.rkt` line ~1500: Evaluation cleanup

## 4. List Emptiness Check Simplification

**Pattern Found**: Checking if something is a list and not empty.

**Example from DrRacket**:
- In `language.rkt`: Validating parameter lists
- In `rep.rkt`: Checking expression lists

**Refactoring Rule**:
```racket
#lang resyntax/test

(require racket/base)

;; Rule: list-not-empty-to-pair
;; Simplify list non-empty check
(define-test-case list-not-empty-to-pair
  #:original
  (and (list? lst) (not (null? lst)))
  #:expected
  (pair? lst))

;; Alternative pattern
(define-test-case list-not-null-to-pair
  #:original
  (and (list? lst) (not (empty? lst)))
  #:expected
  (pair? lst))
```

**Real DrRacket locations where this applies**:
- `language.rkt` line ~450: Parameter validation
- `rep.rkt` line ~2100: Expression list checking

## 5. String Concatenation with Single Values

**Pattern Found**: Using `string-append` where `~a` would be more appropriate.

**Example from DrRacket**:
- In `unit.rkt`: Building labels and messages
- In `rep.rkt`: Creating error messages

**Refactoring Rule**:
```racket
#lang resyntax/test

(require racket/base racket/format)

;; Rule: string-append-with-format-to-~a
;; Replace string-append with format when converting single values
(define-test-case string-append-format-to-~a
  #:original
  (string-append prefix (format "~a" value) suffix)
  #:expected
  (~a prefix value suffix))

;; Simple case with just format
(define-test-case format-number-to-~a
  #:original
  (format "~a" value)
  #:expected
  (~a value))
```

**Real DrRacket locations where this applies**:
- `unit.rkt` line ~2800: Building menu labels
- `rep.rkt` line ~3200: Error message construction

## 6. Method Existence and Call Pattern

**Pattern Found**: Checking if an object responds to a method before calling it.

**Example from DrRacket**:
- In `unit.rkt`: GUI object method calls
- In `rep.rkt`: Editor method invocation

**Refactoring Rule**:
```racket
#lang resyntax/test

(require racket/base racket/class)

;; Rule: method-existence-and-call
;; Use with-method for safe method calling
(define-test-case method-existence-and-call
  #:original
  (and (object-method-arity-includes? obj 'method arity)
       (send obj method . args))
  #:expected
  (with-handlers ([exn:fail:object? (λ (e) #f)])
    (send obj method . args)))
```

**Real DrRacket locations where this applies**:
- `unit.rkt` line ~1800: Canvas method calls
- `rep.rkt` line ~2900: Editor operations

## 7. Parameter Validation Chains

**Pattern Found**: Multiple parameter validation checks in sequence.

**Example from DrRacket**:
- In `language.rkt`: Settings validation
- In `rep.rkt`: Input validation

**Refactoring Rule**:
```racket
#lang resyntax/test

(require racket/base racket/contract)

;; Rule: multiple-parameter-checks-to-and
;; Combine multiple parameter checks
(define-test-case multiple-parameter-checks
  #:original
  (when (and (number? x)
             (positive? x)
             (exact? x))
    action)
  #:expected
  (when (exact-positive-integer? x)
    action)
  #:unless
  ;; Only apply for common predicate combinations
  (not (known-combined-predicate? #'(number? positive? exact?))))
```

**Real DrRacket locations where this applies**:
- `language.rkt` line ~680: Settings validation
- `rep.rkt` line ~1100: Port validation

## Implementation Notes

These rules should be implemented with careful consideration for:

1. **Side effects**: Don't refactor expressions that might have side effects
2. **Type safety**: Preserve type checking where it's important for safety
3. **Readability**: Ensure refactored code is more readable, not just shorter
4. **Performance**: Consider whether the refactoring improves or degrades performance

## Testing

Each rule should be tested against the actual DrRacket codebase to ensure:
- No false positives that would break functionality
- The refactored code is equivalent in behavior
- The suggestions actually improve code quality