test_that("is_adjacent works correctly", {
  box <- matrix(NA, nrow = 5, ncol = 5)
  box[1, 1] <- "cond1_G1_1"

  # No conflicts
  expect_false(is_adjacent(box, 1, 2, "cond1_G2_1"))

  # Conflict in the same column
  box[3, 1] <- "cond1_G1_2"
  expect_true(is_adjacent(box, 2, 1, "cond1_G1_2"))

  # Conflict in the same row
  box[1, 3] <- "cond1_G1_3"
  expect_true(is_adjacent(box, 2, 1, "cond1_G1_2"))

  # Conflict diagonally
  box[2, 2] <- "cond1_G1_4"
  expect_true(is_adjacent(box, 1, 1, "cond1_G1_1"))
})
