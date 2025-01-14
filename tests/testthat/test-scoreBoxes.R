test_that("scoreBoxes evaluates metrics correctly", {
  # Mock results for boxes
  box_results <- list(
    list(
      iteration = 1,
      boxes = list(
        cond1 = list(
          list(box = matrix(c("cond1_G1_1", "cond1_G2_1", NA, NA), nrow = 2, byrow = TRUE)),
          list(box = matrix(c("cond1_G1_2", "cond1_G3_1", NA, NA), nrow = 2, byrow = TRUE))
        ),
        cond2 = list(
          list(box = matrix(c("cond2_G2_1", "cond2_G3_1", NA, NA), nrow = 2, byrow = TRUE)),
          list(box = matrix(c("cond2_G1_1", "cond2_G2_2", NA, NA), nrow = 2, byrow = TRUE))
        )
      )
    ),
    list(
      iteration = 2,
      boxes = list(
        cond1 = list(
          list(box = matrix(c("cond1_G1_1", "cond1_G3_1", NA, NA), nrow = 2, byrow = TRUE)),
          list(box = matrix(c("cond1_G2_1", "cond1_G1_2", NA, NA), nrow = 2, byrow = TRUE))
        ),
        cond2 = list(
          list(box = matrix(c("cond2_G1_1", "cond2_G3_1", NA, NA), nrow = 2, byrow = TRUE)),
          list(box = matrix(c("cond2_G2_1", "cond2_G1_2", NA, NA), nrow = 2, byrow = TRUE))
        )
      )
    )
  )

  # Evaluate metrics
  scores <- scoreBoxes(box_results)

  # Validate structure
  expect_type(scores, "list")
  expect_equal(ncol(scores), 7) # Columns: Experiment, Entropy, Genotypes, etc.
  expect_equal(nrow(scores), length(box_results)) # Rows: One per iteration

  # Check values for all rows
  expect_true(all(scores$BoxEntropy.avg > 0))
  expect_true(all(scores$BoxGenotypes.avg > 0))
  expect_true(all(scores$BoxSimpsonsIndex.avg > 0))
})
