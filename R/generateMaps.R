# Declare global variables
utils::globalVariables(c("Column", "Row", "Label"))

#' Generate Maps from Updated Sample Placement
#'
#' @description
#' This function reads a modified "Sample Placement" sheet and generates updated
#' PDF maps for each box. It processes the user's modifications to the box layout
#' and creates visualizations for all the updated boxes.
#'
#' @param data A data frame (e.g., read from the "Sample Placement" sheet).
#' @param samples The column name containing sample names.
#' @param rep The column name containing replicate numbers.
#' @param cond The column name containing condition labels.
#' @param box The column name containing box identifiers.
#' @param row The column name containing row numbers.
#' @param col The column name containing column numbers.
#' @param file_name The base name for the output PDF file.
#' @param font_size Optional font size for the labels on the maps.
#'
#' @return A PDF file with visualizations for all boxes.
#'
#' @examples
#' mock_data <- data.frame(
#' Sample = c("G1", "G2", "G3", "G4", "G5"),
#' Replicate = c(1, 2, 1, 3, 1),
#' Condition = c("cond1", "cond1", "cond2", "cond2", "cond3"),
#' Box = c(1, 1, 1, 2, 2),
#' Row = c(1, 2, 3, 1, 2),
#' Column = c(1, 2, 3, 1, 2)
#' )
#'
#' temp_file <- file.path(tempdir(), "updated_map")
#' generateMaps(
#'   data = mock_data,
#'   samples = "Sample",
#'   rep = "Replicate",
#'   cond = "Condition",
#'   box = "Box",
#'   row = "Row",
#'   col = "Column",
#'   file_name = temp_file
#' )
#'
#' @import ggplot2
#'
#' @export
generateMaps <- function(data, samples, rep, cond, box, row, col, file_name = "updated_maps", font_size = NULL) {
  # Validate input
  required_columns <- c(samples, rep, cond, box, row, col)
  if (!all(required_columns %in% names(data))) {
    stop("The data frame must contain the required columns: ", paste(required_columns, collapse = ", "))
  }
  
  # Convert any NA values to "Empty" in the input data
  data[is.na(data)] <- "Empty"
  
  # Create plots for each box
  plots <- list()
  unique_boxes <- unique(data[[box]])
  
  for (box_id in unique_boxes) {
    # Subset data for the current box
    box_data <- subset(data, data[[box]] == box_id)
    condition_label <- unique(box_data[[cond]])[1]  # Assume one condition per box
    n_rows <- max(as.numeric(box_data[[row]]), na.rm = TRUE)
    n_cols <- max(as.numeric(box_data[[col]]), na.rm = TRUE)
    
    # Build the matrix representation of the box
    box_matrix <- matrix("Empty", nrow = n_rows, ncol = n_cols)
    for (i in 1:nrow(box_data)) {
      box_matrix[as.numeric(box_data[[row]][i]), as.numeric(box_data[[col]][i])] <-
        paste(box_data[[samples]][i], box_data[[rep]][i], sep = " - ")  # Match "sample - rep" format
    }
    
    # Prepare plot data (swap Row and Column for correct orientation)
    plot_data <- expand.grid(
      Row = 1:ncol(box_matrix),  # Columns become Rows
      Column = 1:nrow(box_matrix)  # Rows become Columns
    )
    plot_data$Label <- as.vector(box_matrix)
    
    # Reformat labels, ensuring "Empty" remains as is
    plot_data$Label <- sapply(plot_data$Label, function(label) {
      if (label == "Empty") {
        return("Empty")  # Keep "Empty" as is
      } else {
        label  # Use "sample - rep" format
      }
    })
    
    # Determine font size
    label_size <- if (!is.null(font_size)) {
      font_size
    } else {
      max(15 / max(nrow(box_matrix), ncol(box_matrix)), 3)
    }
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = Column, y = Row, label = Label)) +
      geom_tile(fill = "white", color = "black") +
      geom_text(size = label_size) +
      scale_y_reverse(breaks = 1:ncol(box_matrix), labels = 1:ncol(box_matrix)) +
      scale_x_continuous(breaks = 1:nrow(box_matrix), labels = 1:nrow(box_matrix)) +
      theme_minimal() +
      labs(
        title = paste("Box", box_id, "-", condition_label),
        x = "Column",
        y = "Row"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank()
      )
    
    plots[[length(plots) + 1]] <- p
  }
  
  # Save plots to a single PDF file
  pdf_file <- paste0(file_name, ".pdf")
  pdf(pdf_file, width = 11.69, height = 8.27)  # A4 Landscape
  for (p in plots) {
    print(p)
  }
  dev.off()
  
  message("PDF maps generated: ", pdf_file)
}
