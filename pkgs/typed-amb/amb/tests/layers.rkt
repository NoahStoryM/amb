#lang racket/base

(require rackunit)

(test-case "typed/amb/base loads the core without the standard mergers or surface syntax"
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require 'typed/amb/base #f)
    (check-true (module-declared? 'amb/base))
    (check-false (module-declared? 'amb/merge))
    (check-false (module-declared? 'amb/main))
    (check-false (module-declared? 'amb/private/choice))
    (check-false (module-declared? 'amb/private/observe))))

(test-case "typed/amb/merge loads the mergers without search effects"
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require 'typed/amb/merge #f)
    (check-true (module-declared? 'amb/merge))
    (check-false (module-declared? 'amb/base))
    (check-false (module-declared? 'amb/private/core))
    (check-false (module-declared? 'amb/main))))
