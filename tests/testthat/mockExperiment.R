# Mock data for testing GridOptimizer

# Create a data frame representing samples with genotypes and replicates
mock_samples <- data.frame(
  Genotype = paste0("G", 1:10),
  Reps = c(3, 5, 2, 4, 6, 3, 2, 5, 4, 3) # Number of replicates for each genotype
)

# Define conditions for the experiment
mock_conditions <- c("cond1", "cond2", "cond3")

# Box dimensions
box_rows <- 3
box_cols <- 3

# Number of iterations and cores for parallel processing
n_iterations <- 10
n_cores <- 2

# Generate mock experiment input data using the generateGrids function
mock_box_results <- generateGrids(
  samples = mock_samples,
  genotypes = "Genotype",
  reps = "Reps",
  conditions = mock_conditions,
  boxRows = box_rows,
  boxCols = box_cols,
  nIterations = n_iterations,
  nCores = n_cores
)

# Score the generated boxes to evaluate metrics
mock_scores <- scoreGrids(mock_box_results)

# Export the first experiment to Excel and PDF files
exportGrid(
  experiment = 1,
  boxes = mock_box_results,
  file_name = "mock_experiment_results"
)

# The files "mock_experiment_results.xlsx" and "mock_experiment_results.pdf" will be generated in the working directory.

## Generating Updated Maps from Modified Sample Placement

# After exporting the initial results, users can modify the "Sample Placement" sheet
# and use it to generate updated box maps in PDF format.

# Example: Load the modified "Sample Placement" sheet
updated_sample_placement <- read.xlsx('./mock_experiment_results.xlsx', sheet = "Sample Placement")

# Generate updated PDF maps
generateMaps(
  data = updated_sample_placement,
  samples = "Genotype",
  rep = "Replicate",
  cond = "Condition",
  box = "Box",
  row = "Row",
  col = "Column",
  file_name = "updated_box_maps"
)

# The output PDF file "updated_box_maps.pdf" will be saved in the working directory.
