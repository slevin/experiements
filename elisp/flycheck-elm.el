(flycheck-define-checker elm
  "An Elm syntax checker

Uses Elm compiler. See URL
`http://elm-lang.org'."
  :command ("elm"
            "-o"        ; only javascript
            source)
  :error-patterns
  ((error line-start "Parse error at (line " line ", column " column "):\n"
          (message) line-end)
   (error line-start "Error on line " line ", column " column " to " (one-or-more digit) ":\n"
          (message) line-end)
   (error line-start "Type error on line " line ", column " column " to " (one-or-more digit)":\n"
          (message (one-or-more (or not-newline "\n")))
          line-end)
   (error line-start "Type Error: "
          (message (one-or-more (or not-newline "\n")))
          line-end))
  :modes (elm-mode))

(add-to-list 'flycheck-checkers 'elm)
