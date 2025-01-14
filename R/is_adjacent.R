#' Check for Adjacency Conflicts
#'
#' @description
#' Checks if a specific cell in a box matrix has adjacency conflicts
#' based on the given genotype label.
#'
#' @param box A matrix representing the box.
#' @param row Row index of the cell to check.
#' @param col Column index of the cell to check.
#' @param label The genotype label in the cell.
#'
#' @return Logical value: `TRUE` if there is a conflict, `FALSE` otherwise.
#'
#' @examples
#' box <- matrix(NA, nrow = 5, ncol = 5)
#' box[1, 1] <- "cond1_G1_1"
#' is_adjacent(box, 1, 2, "cond1_G1_2")
#'
#' @export
is_adjacent <- function(box, row, col, label) {
  if (is.na(label)) return(FALSE)
  genotype <- unlist(strsplit(label, "_"))[2]
  rows <- nrow(box)
  cols <- ncol(box)

  for (r in 1:rows) {
    if (r != row && !is.na(box[r, col]) &&
        grepl(paste0("_", genotype, "_"), box[r, col])) {
      return(TRUE)
    }
  }

  for (c in 1:cols) {
    if (c != col && !is.na(box[row, c]) &&
        grepl(paste0("_", genotype, "_"), box[row, c])) {
      return(TRUE)
    }
  }

  for (dr in c(-1, 1)) {
    for (dc in c(-1, 1)) {
      nr <- row + dr
      nc <- col + dc
      if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols &&
          !is.na(box[nr, nc]) &&
          grepl(paste0("_", genotype, "_"), box[nr, nc])) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}
