# Declare global variables
utils::globalVariables(c("Column", "Row", "Label"))

#' Export Box Results
#'
#' @description
#' Exports box results to Excel and optionally generates PDF for visualization.
#'
#' @param experiment The experiment number to export.
#' @param boxes A list of box generation results.
#' @param file_name Base name for output files.
#'
#' @return Generates an Excel file with the results and optionally a PDF file.
#'
#' @examples
#' samples <- data.frame(Genotype = c("G1", "G2", "G3"), Reps = c(2, 2, 1))
#' conditions <- c("cond1", "cond2")
#' boxes <- generateGrids(samples, "Genotype", "Reps", conditions, 2, 2, 10, 1)
#' temp_file <- file.path(tempdir(), "experiment_results")
#' exportGrid(1, boxes, file_name = temp_file)
#'
#' @import openxlsx
#' @import ggplot2
#' @import reshape2
#' @importFrom stats na.omit sd setNames
#' @export
exportGrid <- function(experiment, boxes, file_name = "experiment_output") {
  if (experiment > length(boxes) || is.null(boxes[[experiment]])) {
    stop(paste("Invalid experiment number. Please select a number between 1 and", length(boxes)))
  }
  
  experiment_boxes <- boxes[[experiment]]$boxes
  
  sheet1_data <- data.frame(
    Genotype = character(),
    Condition = character(),
    Replicate = numeric(),
    Box = numeric(),
    Row = numeric(),
    Column = numeric()
  )
  
  genotype_list <- c()
  count_matrix <- list()
  
  sequential_box_number <- 1
  
  for (condition in names(experiment_boxes)) {
    condition_boxes <- experiment_boxes[[condition]]
    
    for (box_idx in seq_along(condition_boxes)) {
      box <- condition_boxes[[box_idx]]$box
      box[is.na(box)] <- "Empty"  # Replace NA with "Empty"
      box_number <- sequential_box_number
      sequential_box_number <- sequential_box_number + 1
      
      genotype_counts <- table(unlist(sapply(box, function(cell) {
        if (!is.na(cell)) unlist(strsplit(cell, "_"))[2] else NA
      })))
      genotype_list <- sort(unique(c(genotype_list, names(genotype_counts))))
      count_matrix[[paste0("Box", box_number, "_", condition)]] <- genotype_counts
      
      for (col in 1:ncol(box)) {
        for (row in 1:nrow(box)) {
          label <- box[row, col]
          if (!is.na(label)) {
            parts <- unlist(strsplit(label, "_"))
            sheet1_data <- rbind(sheet1_data, data.frame(
              Genotype = parts[2],
              Condition = parts[1],
              Replicate = as.numeric(parts[3]),
              Box = box_number,
              Row = row,
              Column = col
            ))
          }
        }
      }
    }
  }
  
  genotype_counts_df <- data.frame(
    Genotype = genotype_list
  )
  for (box_condition in names(count_matrix)) {
    counts <- count_matrix[[box_condition]]
    genotype_counts_df[[box_condition]] <- sapply(genotype_list, function(genotype) {
      if (genotype %in% names(counts)) counts[genotype] else 0
    })
  }
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "Sample Placement")
  writeData(wb, "Sample Placement", sheet1_data)
  
  addWorksheet(wb, "Genotype Counts")
  writeData(wb, "Genotype Counts", genotype_counts_df)
  
  pdf_file <- paste0(file_name, ".pdf")
  pdf(pdf_file, width = 11.69, height = 8.27)  # A4 Landscape size
  
  # Add individual sheets for each box
  sequential_box_number <- 1
  for (condition in names(experiment_boxes)) {
    condition_boxes <- experiment_boxes[[condition]]
    for (box_idx in seq_along(condition_boxes)) {
      box <- condition_boxes[[box_idx]]$box
      box[is.na(box)] <- "Empty"  # Replace NA with "Empty"
      box_with_labels <- cbind(Row = 1:nrow(box), as.data.frame(box))
      colnames(box_with_labels) <- c("Row/Column", 1:ncol(box))
      
      # Prepare plot data for ggplot
      plot_data <- reshape2::melt(box_with_labels, id.vars = "Row/Column")
      colnames(plot_data) <- c("Row", "Column", "Label")
      
      # Update Label format to "sample - replicate"
      plot_data$Label <- sapply(plot_data$Label, function(label) {
        if (label == "Empty") return("Empty")
        parts <- unlist(strsplit(label, "_"))
        paste(parts[2], parts[3], sep = " - ")
      })
      
      # Generate the plot
      p <- ggplot(plot_data, aes(x = as.numeric(Column), y = as.numeric(Row), label = Label)) +
        geom_tile(fill = "white", color = "black") +
        geom_text(size = 4) +
        scale_y_reverse(breaks = 1:nrow(box_with_labels), labels = 1:nrow(box_with_labels)) +
        scale_x_continuous(breaks = 1:ncol(box_with_labels), labels = 1:ncol(box_with_labels)) +
        labs(
          title = paste("Box", sequential_box_number, "-",condition),
          x = "Column",
          y = "Row"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          panel.grid = element_blank()
        )
      
      # Print each plot to a new page in the PDF
      print(p)
      sequential_box_number <- sequential_box_number + 1
    }
  }
  
  dev.off()
  
  excel_file <- paste0(file_name, ".xlsx")
  saveWorkbook(wb, excel_file, overwrite = TRUE)
  
  message("Excel and PDF outputs generated:",
          "\nExcel file: ", excel_file,
          "\nPDF file: ", pdf_file)
}
