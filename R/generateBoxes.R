#' Generate Boxes
#'
#' @description
#' Generates boxes of genotypic samples based on given conditions and parameters.
#'
#' @param samples Data frame with genotype and replicate information.
#' @param genotypes Column name for genotype data.
#' @param reps Column name for replicate data.
#' @param conditions Experimental conditions to consider.
#' @param boxRows Number of rows in each box.
#' @param boxCols Number of columns in each box.
#' @param nIterations Number of iterations to attempt box generation.
#' @param nCores Number of cores for parallel execution.
#' @param availableBoxes Optional; maximum number of boxes allowed.
#'
#' @return A list of generated boxes with conflict resolution applied.
#'
#' @examples
#' samples <- data.frame(Genotype = c("G1", "G2", "G3"), Reps = c(2, 2, 1))
#' conditions <- c("cond1", "cond2")
#' boxes <- generateBoxes(samples, "Genotype", "Reps", conditions, 2, 2, 10, 1)
#'
#' @import dplyr
#' @import parallel
#' @importFrom grDevices dev.off pdf
#' @importFrom stats na.omit sd setNames

#' @export
generateBoxes <- function(samples, genotypes, reps, conditions, boxRows, boxCols, nIterations, nCores, availableBoxes = NULL) {
  # Validate inputs
  if (!all(c(genotypes, reps) %in% colnames(samples))) {
    stop("The input dataset must contain the specified columns for genotypes and reps.")
  }

  total_samples <- sum(samples[[reps]]) * length(conditions)
  box_slots <- boxRows * boxCols
  boxes_per_condition <- ceiling(total_samples / box_slots)
  required_boxes <- boxes_per_condition * length(conditions)

  if (!is.null(availableBoxes) && required_boxes > availableBoxes) {
    stop(paste("Insufficient available boxes. Required:", required_boxes, "Available:", availableBoxes))
  }

  samples$genotypes <- samples[[genotypes]]
  samples$reps <- samples[[reps]]

  condition_samples <- lapply(conditions, function(condition) {
    samples_expanded <- do.call(rbind, lapply(seq_along(samples$genotypes), function(i) {
      data.frame(
        Genotype = samples$genotypes[i],
        Replicate = seq_len(samples$reps[i]),
        Condition = condition,
        Label = paste(condition, samples$genotypes[i], seq_len(samples$reps[i]), sep = "_")
      )
    }))
    return(samples_expanded)
  })

  evaluate_solution <- function(iteration, retries = 3) {
    all_genotypes <- unique(unlist(lapply(condition_samples, function(s) s$Genotype)))

    for (attempt in seq_len(retries)) {
      all_condition_boxes <- list()

      for (condition_idx in seq_along(condition_samples)) {
        samples <- condition_samples[[condition_idx]]
        shuffled_samples <- samples[sample(nrow(samples)), ]

        nSamples <- nrow(shuffled_samples)
        boxes_per_condition <- ceiling(nSamples / box_slots)

        condition_boxes <- replicate(boxes_per_condition, {
          list(box = matrix(NA, nrow = boxRows, ncol = boxCols),
               genotypes = setNames(rep(0, length(all_genotypes)), all_genotypes))
        }, simplify = FALSE)

        for (sample_idx in seq_len(nSamples)) {
          replicate_label <- shuffled_samples$Label[sample_idx]
          genotype <- shuffled_samples$Genotype[sample_idx]

          valid_boxes <- which(sapply(condition_boxes, function(b) any(is.na(b$box))))
          if (length(valid_boxes) == 0) stop("No empty boxes available for placement.")

          genotype_counts <- sapply(condition_boxes[valid_boxes], function(b) b$genotypes[genotype])
          min_genotype_count <- min(genotype_counts, na.rm = TRUE)
          candidate_boxes <- valid_boxes[genotype_counts == min_genotype_count]

          target_box_idx <- if (length(candidate_boxes) > 1) {
            box_fill_counts <- sapply(condition_boxes[candidate_boxes], function(b) sum(!is.na(b$box)))
            candidate_boxes[which.min(box_fill_counts)]
          } else candidate_boxes[1]

          target_box <- condition_boxes[[target_box_idx]]$box
          empty_slot_indices <- which(is.na(target_box), arr.ind = TRUE)
          if (nrow(empty_slot_indices) == 0) next

          empty_slot_idx <- empty_slot_indices[1, ]
          condition_boxes[[target_box_idx]]$box[empty_slot_idx[1], empty_slot_idx[2]] <- replicate_label
          condition_boxes[[target_box_idx]]$genotypes[genotype] <- condition_boxes[[target_box_idx]]$genotypes[genotype] + 1
        }

        for (i in seq_along(condition_boxes)) {
          condition_boxes[[i]]$box <- resolve_conflicts(condition_boxes[[i]]$box)
        }

        all_condition_boxes[[conditions[condition_idx]]] <- condition_boxes
      }

      if (all(sapply(all_condition_boxes, function(c_boxes) {
        all(sapply(c_boxes, function(b) validate_conflicts(b$box)))
      }))) {
        return(list(iteration = iteration, boxes = all_condition_boxes))
      }
    }

    return(NULL)
  }

  results <- parallel::mclapply(seq_len(nIterations), evaluate_solution, mc.cores = nCores)
  valid_results <- Filter(Negate(is.null), results)
  return(valid_results)
}
