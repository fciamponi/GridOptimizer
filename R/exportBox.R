#' Export Box Results
#'
#' @description
#' Exports box results to Excel and PDF formats for visualization and analysis.
#'
#' @param experiment The experiment number to export.
#' @param boxes A list of box generation results.
#' @param file_name Base name for output files.
#' @param font_size Optional font size for PDF plots.
#'
#' @return Generates Excel and PDF files with the results.
#'
#' @examples
#' # Example assumes a list of boxes exists.
#' exportBox(1, boxes, file_name = "experiment_results")
#'
#' @import openxlsx
#' @import ggplot2
#' @export
exportBox <- function(experiment, boxes, file_name = "experiment_output", font_size = NULL) {
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
  plots <- list()

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

      plot_data <- expand.grid(
        Row = 1:nrow(box),
        Column = 1:ncol(box)
      )
      plot_data$Label <- as.vector(t(box))
      plot_data <- na.omit(plot_data)

      plot_data$Label <- sapply(plot_data$Label, function(label) {
        if (label == "Empty") {
          return("Empty")  # Keep "Empty" as is
        } else {
          parts <- unlist(strsplit(label, "_"))
          return(paste(parts[2], parts[3]))
        }
      })

      label_size <- if (!is.null(font_size)) {
        font_size
      } else {
        max(15 / max(nrow(box), ncol(box)), 3)
      }

      p <- ggplot(plot_data, aes(x = Column, y = Row, label = Label)) +
        geom_tile(fill = "white", color = "black") +
        geom_text(size = label_size) +
        scale_y_reverse(breaks = 1:nrow(box), labels = 1:nrow(box)) +
        scale_x_continuous(breaks = 1:ncol(box), labels = 1:ncol(box)) +
        theme_minimal() +
        labs(
          title = paste("Box", box_number, "-", condition),
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

  # Add individual sheets for each box
  sequential_box_number <- 1
  for (condition in names(experiment_boxes)) {
    condition_boxes <- experiment_boxes[[condition]]
    for (box_idx in seq_along(condition_boxes)) {
      box <- condition_boxes[[box_idx]]$box
      box[is.na(box)] <- "Empty"  # Replace NA with "Empty"
      box_with_labels <- cbind(Row = 1:nrow(box), as.data.frame(box))
      colnames(box_with_labels) <- c("Row/Column", 1:ncol(box))
      sheet_name <- paste0("Box ", sequential_box_number)
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, box_with_labels)
      sequential_box_number <- sequential_box_number + 1
    }
  }

  excel_file <- paste0(file_name, ".xlsx")
  saveWorkbook(wb, excel_file, overwrite = TRUE)

  pdf_file <- paste0(file_name, ".pdf")
  pdf(pdf_file, width = 11.69, height = 8.27)
  for (p in plots) {
    print(p)
  }
  dev.off()

  message("Excel and PDF outputs generated:\n",
          "Excel file: ", excel_file, "\n",
          "PDF file: ", pdf_file)
}
