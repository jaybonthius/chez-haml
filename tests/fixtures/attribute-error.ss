(lambda (formatter cell-value row col)
  (haml
    (:tr
      (:td ((formatter col) (cell-value row col))))))
