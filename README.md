# GridOptimizer

GridOptimizer is an R package for generating, visualizing, and analyzing genotypic sample placements in experimental conditions. It supports conflict resolution and robust metrics calculation for experimental designs.

---

## Features

- Generate experimental grids with replicates across conditions.
- Export results to Excel and PDF formats.
- Score grids using metrics like entropy and Simpson's index.
- Update and visualize sample placements from modified Excel sheets.

---

## Installation

### From GitHub
You can install the latest version of the package directly from GitHub using `devtools`:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install GridOptimizer from GitHub
devtools::install_github("fciamponi/GridOptimizer")
```

### From Source
1. Download the `.tar.gz` file from the releases section.
2. Install it in RStudio:

   ```r
   install.packages("/path/to/GridOptimizer_0.1.0.tar.gz", repos = NULL, type = "source")
   ```

---

## Usage

### Example Workflow

#### 1. Generate grids
```r
samples <- data.frame(
  Genotype = c("G1", "G2", "G3"),
  Reps = c(3, 5, 2)
)
conditions <- c("cond1", "cond2", "cond3")

grids <- generateGrids(
  samples = samples,
  sample_label = "Genotype",
  reps = "Reps",
  conditions = conditions,
  gridRows = 3,
  gridCols = 3,
  nIterations = 10,
  nCores = 2
)
```

#### 2. Score Boxes
```r
scores <- scoreGrids(grids)
```

#### 3. Export Results
```r
exportGrid(1, grids, file_name = "experiment_results")
```

#### 4. Generate Maps from Updated Sample Placement
```r
updated_map <- read.xlsx("experiment_results.xlsx", sheet_name = "Sample Placement")
generateMaps(
  data = updated_map,
  samples = "Genotype",
  rep = "Replicate",
  cond = "Condition",
  box = "Box",
  row = "Row",
  col = "Column",
  file_name = "updated_maps"
)
```

---

## Documentation

Complete documentation and vignettes are available [here](https://github.com/fciamponi/GridOptimizer/tree/main/vignettes).

You can also access the package documentation within R:

```r
help(package = "GridOptimizer")
```

---

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create a new branch (`git checkout -b feature-name`).
3. Make your changes and commit (`git commit -m "Add feature X"`).
4. Push to the branch (`git push origin feature-name`).
5. Create a pull request.

Please ensure all tests pass before submitting your PR:

```r
devtools::test()
```

---

## License

This project is licensed under the GPL3 License. See the [LICENSE](LICENSE.md) file for details.

---

## Acknowledgments

Thanks to all contributors and users who have supported this project!
