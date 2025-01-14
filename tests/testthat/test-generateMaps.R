test_that("generateMaps creates PDF maps from updated sample placement", {
  # Create a mock updated sample placement dataset
  mock_data <- data.frame(
    Sample = c("G1", "G2", "G3", "G4", "G5"),
    Replicate = c(1, 2, 1, 3, 1),
    Condition = c("cond1", "cond1", "cond2", "cond2", "cond3"),
    Box = c(1, 1, 1, 2, 2),
    Row = c(1, 2, 3, 1, 2),
    Column = c(1, 2, 3, 1, 2)
  )

  # Generate the PDF maps
  output_file <- tempfile(fileext = ".pdf")
  generateMaps(
    data = mock_data,
    samples = "Sample",
    rep = "Replicate",
    cond = "Condition",
    box = "Box",
    row = "Row",
    col = "Column",
    file_name = output_file
  )

  # Check if the file was created
  expect_true(file.exists(paste0(output_file, ".pdf")))

  # Cleanup
  file.remove(paste0(output_file, ".pdf"))
})
