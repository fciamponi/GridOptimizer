test_that("calculate_box_metrics computes correct aggregated metrics", {
  boxes <- list(
    list(box = matrix(c("cond1_G1_1", "cond1_G2_1", NA, NA), nrow = 2, byrow = TRUE)),
    list(box = matrix(c("cond1_G1_2", "cond1_G3_1", NA, NA), nrow = 2, byrow = TRUE))
  )
  genotypes <- c("G1", "G2", "G3")

  metrics <- calculate_box_metrics(boxes, genotypes)

  expect_true(metrics$avg_entropy > 0)
  expect_true(metrics$avg_simpsons_index > 0)
  expect_equal(metrics$randomness_percentage, metrics$avg_entropy / log2(length(genotypes)) * 100)
})
