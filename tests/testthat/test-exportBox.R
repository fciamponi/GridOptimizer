test_that("exportBox generates Excel and PDF files", {
  skip_on_cran() # Skip file creation tests on CRAN

  # Mock box results
  boxes <- list(
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
    )
  )

  # Temporary directory for output files
  temp_dir <- tempdir()
  excel_file <- file.path(temp_dir, "test_export.xlsx")
  pdf_file <- file.path(temp_dir, "test_export.pdf")

  # Call exportBox
  exportBox(1, boxes, file_name = file.path(temp_dir, "test_export"))

  # Validate file existence
  expect_true(file.exists(excel_file))
  expect_true(file.exists(pdf_file))

  # Validate Excel content
  wb <- openxlsx::loadWorkbook(excel_file) # Use the correct file path
  sheet_names <- openxlsx::getSheetNames(excel_file) # Pass the actual Excel file path
  expect_true("Sample Placement" %in% sheet_names)
  expect_true("Genotype Counts" %in% sheet_names)

  sample_placement <- openxlsx::read.xlsx(excel_file, sheet = "Sample Placement")
  expect_gt(nrow(sample_placement), 0) # Ensure data exists
  expect_true(all(c("Genotype", "Condition", "Replicate", "Box", "Row", "Column") %in% colnames(sample_placement)))

  genotype_counts <- openxlsx::read.xlsx(excel_file, sheet = "Genotype Counts")
  expect_gt(nrow(genotype_counts), 0) # Ensure data exists
  expect_true("Genotype" %in% colnames(genotype_counts))

  # Clean up temporary files
  unlink(c(excel_file, pdf_file))
})
