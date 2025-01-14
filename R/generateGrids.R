#' Generate Boxes
#'
#' @description
#' Generates boxes of genotypic samples based on given conditions and parameters.
#'
#' @param samples Data frame with genotype and replicate information.
#' @param sample_labels Column name for genotype data.
#' @param reps Column name for replicate data.
#' @param conditions Experimental conditions to consider.
#' @param gridRows Number of rows in each box.
#' @param gridCols Number of columns in each box.
#' @param nIterations Number of iterations to attempt box generation.
#' @param nCores Number of cores for parallel execution.
#' @param availableGrids Optional; maximum number of grids allowed.
#'
#' @return A list of generated boxes with conflict resolution applied.
#'
#' @examples
#' samples <- data.frame(Genotype = c("G1", "G2", "G3"), Reps = c(2, 2, 1))
#' conditions <- c("cond1", "cond2")
#' boxes <- generateGrids(samples, "Genotype", "Reps", conditions, 2, 2, 10, 1)
#'
#' @import dplyr
#' @import parallel
#' @import progress
#' @importFrom grDevices dev.off pdf
#' @importFrom stats na.omit sd setNames

#' @export
generateGrids <- function(samples, sample_labels, reps, conditions, gridRows, gridCols, nIterations, nCores, availableGrids = NULL) {
  # Validate inputs
  if (!all(c(sample_labels, reps) %in% colnames(samples))) {
    stop("The input dataset must contain the specified columns for sample_labels and reps.")
  }

  # Check if the progress package is installed
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("The 'progress' package is required. Please install it using install.packages('progress').")
  }

  total_samples <- sum(samples[[reps]]) * length(conditions)
  box_slots <- gridRows * gridCols
  boxes_per_condition <- ceiling(total_samples / box_slots)
  required_boxes <- boxes_per_condition * length(conditions)

  if (!is.null(availableGrids) && required_boxes > availableGrids) {
    stop(paste("Insufficient available boxes. Required:", required_boxes, "Available:", availableGrids))
  }

  samples$sample_labels <- samples[[sample_labels]]
  samples$reps <- samples[[reps]]

  condition_samples <- lapply(conditions, function(condition) {
    samples_expanded <- do.call(rbind, lapply(seq_along(samples$sample_labels), function(i) {
      data.frame(
        Genotype = samples$sample_labels[i],
        Replicate = seq_len(samples$reps[i]),
        Condition = condition,
        Label = paste(condition, samples$sample_labels[i], seq_len(samples$reps[i]), sep = "_")
      )
    }))
    return(samples_expanded)
  })

  # Initialize the progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent | Iteration :current/:total | ETA: :eta",
    total = nIterations,
    clear = FALSE,
    width = 60
  )

  evaluate_solution <- function(iteration, retries = 3) {
    all_sample_labels <- unique(unlist(lapply(condition_samples, function(s) s$Genotype)))

    for (attempt in seq_len(retries)) {
      all_condition_boxes <- list()

      for (condition_idx in seq_along(condition_samples)) {
        samples <- condition_samples[[condition_idx]]
        shuffled_samples <- samples[sample(nrow(samples)), ]

        nSamples <- nrow(shuffled_samples)
        boxes_per_condition <- ceiling(nSamples / box_slots)

        condition_boxes <- replicate(boxes_per_condition, {
          list(box = matrix(NA, nrow = gridRows, ncol = gridCols),
               sample_labels = setNames(rep(0, length(all_sample_labels)), all_sample_labels))
        }, simplify = FALSE)

        for (sample_idx in seq_len(nSamples)) {
          replicate_label <- shuffled_samples$Label[sample_idx]
          genotype <- shuffled_samples$Genotype[sample_idx]

          valid_boxes <- which(sapply(condition_boxes, function(b) any(is.na(b$box))))
          if (length(valid_boxes) == 0) stop("No empty boxes available for placement.")

          genotype_counts <- sapply(condition_boxes[valid_boxes], function(b) b$sample_labels[genotype])
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
          condition_boxes[[target_box_idx]]$sample_labels[genotype] <- condition_boxes[[target_box_idx]]$sample_labels[genotype] + 1
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

  # results <- parallel::mclapply(seq_len(nIterations), function(iteration) {
  #   pb$tick()
  #   evaluate_solution(iteration)
  # }, mc.cores = nCores)


  # Set up parallel processing
  cl <- makeCluster(nCores)  # Create a cluster
  clusterExport(
    cl,
    varlist = c("samples", "condition_samples", "evaluate_solution",
                "box_slots", "gridRows", "gridCols",
                "resolve_conflicts", "validate_conflicts", "conditions"),
    envir = environment()  # Use the correct environment
  )
  clusterExport(cl, varlist = c("is_adjacent"), envir = asNamespace("GridOptimizer"))
  clusterEvalQ(cl, library(progress))

  # Parallel execution
  results <- parLapply(cl, seq_len(nIterations), evaluate_solution)

  stopCluster(cl)  # Stop the cluster

  valid_results <- Filter(Negate(is.null), results)
  return(valid_results)
}
