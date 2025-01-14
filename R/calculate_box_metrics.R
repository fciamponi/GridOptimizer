#' Calculate Metrics for Multiple Boxes
#'
#' @description
#' Computes aggregated metrics (entropy, unique genotypes, Simpson's Diversity Index, randomness percentage)
#' across multiple boxes.
#'
#' @param boxes A list of boxes (matrices) to analyze.
#' @param genotypes A vector of all possible genotypes.
#'
#' @return A list containing average and standard deviation of entropy, genotype counts, and Simpson's Diversity Index.
#'
#' @examples
#' boxes <- list(
#'   list(box = matrix(c("cond1_G1_1", "cond1_G2_1", NA, NA), nrow = 2, byrow = TRUE)),
#'   list(box = matrix(c("cond1_G1_2", "cond1_G3_1", NA, NA), nrow = 2, byrow = TRUE))
#' )
#' genotypes <- c("G1", "G2", "G3")
#' calculate_box_metrics(boxes, genotypes)
#'
#' @export
calculate_box_metrics <- function(boxes, genotypes) {
  max_entropy <- log2(length(genotypes))

  box_metrics <- lapply(boxes, function(b) {
    entropy_and_genotype <- compute_box_metrics(b$box)
    return(list(
      entropy = entropy_and_genotype$entropy,
      unique_genotype_count = entropy_and_genotype$unique_genotype_count,
      simpsons_index = entropy_and_genotype$simpsons_index
    ))
  })

  entropies <- sapply(box_metrics, function(m) m$entropy)
  unique_genotypes <- sapply(box_metrics, function(m) m$unique_genotype_count)
  simpsons_indices <- sapply(box_metrics, function(m) m$simpsons_index)

  avg_entropy <- mean(entropies, na.rm = TRUE)
  randomness_percentage <- (avg_entropy / max_entropy) * 100

  return(list(
    avg_entropy = avg_entropy,
    sd_entropy = sd(entropies, na.rm = TRUE),
    avg_unique_genotypes = mean(unique_genotypes, na.rm = TRUE),
    sd_unique_genotypes = sd(unique_genotypes, na.rm = TRUE),
    avg_simpsons_index = mean(simpsons_indices, na.rm = TRUE),
    sd_simpsons_index = sd(simpsons_indices, na.rm = TRUE),
    randomness_percentage = randomness_percentage
  ))
}
