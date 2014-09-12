(define (quotething x)
  `(hello ,x))
quotething

(quotething 'blah)
(hello blah)
(hello (unquote x))

