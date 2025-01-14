#' Compute Box Metrics
#'
#' @description
#' Computes entropy, unique genotype count, and Simpson's Diversity Index for a box.
#'
#' @param box A matrix representing the box.
#'
#' @return A list containing entropy, unique genotype count, and Simpson's Diversity Index.
#'
#' @examples
#' box <- matrix(c("cond1_G1_1", "cond1_G2_1", "cond1_G1_2", NA), nrow = 2, ncol = 2)
#' compute_box_metrics(box)
#'
#' @export
compute_box_metrics <- function(box) {
  genotypes <- sapply(na.omit(as.vector(box)), function(label) {
    unlist(strsplit(label, "_"))[2]
  })

  freq_table <- table(genotypes)
  probs <- freq_table / sum(freq_table)

  # Shannon Entropy
  max_entropy <- log2(length(probs))
  entropy <- -sum(probs * log2(probs), na.rm = TRUE) / max_entropy

  # Unique Genotype Count
  unique_genotype_count <- length(unique(genotypes))

  # Simpson's Diversity Index (1 - D)
  simpsons_index <- 1 - sum(probs^2)

  return(list(
    entropy = entropy,
    unique_genotype_count = unique_genotype_count,
    simpsons_index = simpsons_index
  ))
}
