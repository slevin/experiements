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
          (message
           (and (one-or-more not-newline)
                (zero-or-more "\n"
                              (zero-or-more " ")
                              (one-or-more not-newline))))
          line-end))
  :modes (elm-mode))

(add-to-list 'flycheck-checkers 'elm)
