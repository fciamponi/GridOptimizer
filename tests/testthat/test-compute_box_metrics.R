test_that("compute_box_metrics calculates correct metrics", {
  box <- matrix(c("cond1_G1_1", "cond1_G2_1", NA, NA, "cond1_G3_1", NA), nrow = 3, byrow = TRUE)

  metrics <- compute_box_metrics(box)

  expect_equal(metrics$unique_genotype_count, 3)
  expect_true(metrics$entropy > 0)
  expect_true(metrics$simpsons_index > 0)
})
