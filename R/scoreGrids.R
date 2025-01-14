#' Score Boxes
#'
#' @description
#' Evaluates generated boxes and computes metrics for each experiment.
#'
#' @param box_results A list of box generation results.
#'
#' @return A data frame with metrics for each experiment, including entropy, unique genotype counts,
#' and Simpson's Diversity Index.
#'
#' @examples
#' box_results <- list(list(
#'   iteration = 1,
#'   boxes = list(
#'     cond1 = list(
#'       list(box = matrix(c("cond1_G1_1", "cond1_G2_1", NA, NA), nrow = 2, ncol = 2))
#'     )
#'   )
#' ))
#' scoreGrids(box_results)
#'
#' @import dplyr
#' @importFrom grDevices dev.off pdf
#' @importFrom stats na.omit sd setNames

#' @export
scoreGrids <- function(box_results) {
  scores <- lapply(box_results, function(result) {
    all_boxes <- unlist(result$boxes, recursive = FALSE)

    genotypes <- unique(unlist(lapply(all_boxes, function(b) {
      sapply(na.omit(as.vector(b$box)), function(label) unlist(strsplit(label, "_"))[2])
    })))

    metrics <- calculate_box_metrics(all_boxes, genotypes)
    data.frame(
      Experiment = result$iteration,
      BoxEntropy.avg = metrics$avg_entropy,
      BoxEntropy.std = metrics$sd_entropy,
      BoxGenotypes.avg = metrics$avg_unique_genotypes,
      BoxGenotypes.std = metrics$sd_unique_genotypes,
      BoxSimpsonsIndex.avg = metrics$avg_simpsons_index,
      BoxSimpsonsIndex.std = metrics$sd_simpsons_index
    )
  })

  scores_df <- do.call(rbind, scores)
  return(scores_df)
}
