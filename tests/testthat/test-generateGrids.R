test_that("generateGrids generates valid boxes", {
  # Mock sample data
  samples <- data.frame(Genotype = c("G1", "G2", "G3"), Reps = c(2, 2, 1))
  conditions <- c("cond1", "cond2")

  # Generate boxes
  results <- generateGrids(
    samples = samples,
    genotypes = "Genotype",
    reps = "Reps",
    conditions = conditions,
    boxRows = 2,
    boxCols = 2,
    nIterations = 10,
    nCores = 1
  )

  # Check structure
  expect_type(results, "list")
  expect_true(length(results) > 0)

  # Validate generated boxes
  all_valid <- all(sapply(results[[1]]$boxes, function(c_boxes) {
    all(sapply(c_boxes, function(b) validate_conflicts(b$box)))
  }))
  expect_true(all_valid)

  # Check conditions and genotypes
  condition_boxes <- results[[1]]$boxes
  for (condition in conditions) {
    expect_true(condition %in% names(condition_boxes))
    genotypes <- unique(unlist(lapply(condition_boxes[[condition]], function(b) {
      sapply(na.omit(as.vector(b$box)), function(label) unlist(strsplit(label, "_"))[2])
    })))
    expect_true(all(genotypes %in% samples$Genotype))
  }
})
