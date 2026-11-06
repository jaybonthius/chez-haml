(lambda (formatter cell-value row col)
  (html
    (:tr
      (:td ((formatter col) (cell-value row col))))))
