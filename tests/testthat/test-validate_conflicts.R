test_that("validate_conflicts detects adjacency conflicts", {
  box <- matrix(c("cond1_G1_1", "cond1_G2_1", NA, NA, "cond1_G1_2", NA), nrow = 3, byrow = TRUE)

  # Conflict exists
  expect_false(validate_conflicts(box))

  # No conflicts
  box <- matrix(c("cond1_G1_1", "cond1_G2_1", NA, NA, NA, "cond1_G1_2"), nrow = 3, byrow = TRUE)
  expect_true(validate_conflicts(box))
})
