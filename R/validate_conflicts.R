#' Validate Adjacency Conflicts
#'
#' @description
#' Validates if any adjacency conflicts exist in a box.
#'
#' @param box A matrix representing the box.
#'
#' @return Logical value: `TRUE` if no conflicts exist, `FALSE` otherwise.
#'
#' @examples
#' box <- matrix(c(NA, "cond1_G1_1", "cond1_G1_2", NA), nrow = 2, ncol = 2)
#' validate_conflicts(box)
#'
#' @export
validate_conflicts <- function(box) {
  rows <- nrow(box)
  cols <- ncol(box)

  for (row in 1:rows) {
    for (col in 1:cols) {
      label <- box[row, col]
      if (!is.na(label) && is_adjacent(box, row, col, label)) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}
