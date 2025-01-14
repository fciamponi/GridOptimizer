#' Resolve Adjacency Conflicts
#'
#' @description
#' Resolves adjacency conflicts in a box by swapping labels between cells.
#'
#' @param box A matrix representing the box.
#' @param max_iterations Maximum number of iterations to attempt resolution.
#'
#' @return A matrix with resolved conflicts.
#'
#' @examples
#' box <- matrix(c(NA, "cond1_G1_1", "cond1_G1_2", NA), nrow = 2, ncol = 2)
#' resolve_conflicts(box, max_iterations = 100)
#'
#' @export
resolve_conflicts <- function(box, max_iterations = 1000) {
  filled_positions <- which(!is.na(box), arr.ind = TRUE)

  has_conflict <- function(row, col) {
    label <- box[row, col]
    if (is.na(label)) return(FALSE)
    is_adjacent(box, row, col, label)
  }

  iteration <- 0
  while (iteration < max_iterations) {
    iteration <- iteration + 1
    conflict_found <- FALSE

    for (pos_idx in seq_len(nrow(filled_positions))) {
      pos <- filled_positions[pos_idx, ]
      row <- pos[1]
      col <- pos[2]
      label <- box[row, col]

      if (has_conflict(row, col)) {
        conflict_found <- TRUE

        other_positions <- filled_positions[filled_positions[, 1] != row | filled_positions[, 2] != col, , drop = FALSE]
        if (nrow(other_positions) == 0) next

        for (swap_idx in seq_len(nrow(other_positions))) {
          swap_row <- other_positions[swap_idx, 1]
          swap_col <- other_positions[swap_idx, 2]

          temp <- box[row, col]
          box[row, col] <- box[swap_row, swap_col]
          box[swap_row, swap_col] <- temp

          if (!has_conflict(row, col) && !has_conflict(swap_row, swap_col)) break

          box[swap_row, swap_col] <- box[row, col]
          box[row, col] <- temp
        }
      }
    }

    if (!conflict_found) break
  }

  if (iteration == max_iterations) {
    warning("Maximum iterations reached. Conflicts may remain unresolved.")
  }

  return(box)
}

