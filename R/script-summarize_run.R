#' Summarize an rENM run directory
#'
#' Traverses the full directory tree for a species run and writes a
#' technical summary of the structure, contents, and storage footprint.
#'
#' @param alpha_code Character. Four-letter bird banding code.
#'
#' @details
#' This function:
#'   - recursively traverses project_dir/runs/<alpha_code>
#'   - lists all folders and files
#'   - records file sizes and modification times
#'   - summarizes storage usage by directory and file type
#'   - calculates useful run metrics
#'   - writes a text summary to:
#'
#'       project_dir/framework/<alpha_code>_run_summary.txt
#'
#' Metrics reported include:
#'   - total directory size
#'   - number of folders
#'   - number of files
#'   - largest files
#'   - file type distributions
#'   - storage by extension
#'   - empty folders
#'   - recent modification times
#'
#' @return Invisibly returns the path to the summary file.
#'
#' @examples
#' summarize_run("CASP")
#'
#' @export

summarize_run <- function(alpha_code) {
  
  # ---------------------------------------------------------------------------
  # --- Validate input --------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  if (!is.character(alpha_code) || nchar(alpha_code) != 4) {
    stop("alpha_code must be a 4-character species code.")
  }
  
  alpha_code <- toupper(alpha_code)
  
  # ---------------------------------------------------------------------------
  # --- Define paths ----------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  project_dir <- rENM.core::rENM_project_dir()
  
  run_dir <- file.path(
    project_dir,
    "runs",
    alpha_code
  )
  
  framework_dir <- file.path(
    project_dir,
    "framework"
  )
  
  if (!dir.exists(run_dir)) {
    stop("Run directory does not exist:\n", run_dir)
  }
  
  if (!dir.exists(framework_dir)) {
    dir.create(framework_dir, recursive = TRUE)
  }
  
  output_file <- file.path(
    framework_dir,
    paste0(alpha_code, "_run_summary.txt")
  )
  
  # ---------------------------------------------------------------------------
  # --- Helper functions ------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  format_size <- function(bytes) {
    
    units <- c("B", "KB", "MB", "GB", "TB")
    
    if (is.na(bytes) || bytes == 0) {
      return("0 B")
    }
    
    power <- floor(log(bytes, 1024))
    power <- min(power, length(units) - 1)
    
    size <- bytes / (1024 ^ power)
    
    paste0(format(round(size, 2), nsmall = 2), " ", units[power + 1])
  }
  
  separator <- paste(rep("=", 72), collapse = "")
  
  # ---------------------------------------------------------------------------
  # --- Traverse directory ----------------------------------------------------
  # ---------------------------------------------------------------------------
  
  all_paths <- list.files(
    run_dir,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  
  file_paths <- all_paths[file.info(all_paths)$isdir == FALSE]
  dir_paths  <- all_paths[file.info(all_paths)$isdir == TRUE]
  
  file_info <- file.info(file_paths)
  
  # ---------------------------------------------------------------------------
  # --- File metadata ---------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  files_df <- data.frame(
    path = file_paths,
    relative_path = sub(
      paste0("^", normalizePath(run_dir), "/?"),
      "",
      normalizePath(file_paths)
    ),
    size_bytes = file_info$size,
    modified = file_info$mtime,
    extension = tolower(tools::file_ext(file_paths)),
    stringsAsFactors = FALSE
  )
  
  files_df$extension[files_df$extension == ""] <- "[none]"
  
  total_size <- sum(files_df$size_bytes, na.rm = TRUE)
  
  # ---------------------------------------------------------------------------
  # --- Directory summaries ---------------------------------------------------
  # ---------------------------------------------------------------------------
  
  dir_summary <- lapply(dir_paths, function(d) {
    
    contained_files <- files_df[
      dirname(files_df$path) == d,
      ,
      drop = FALSE
    ]
    
    data.frame(
      directory = sub(
        paste0("^", normalizePath(run_dir), "/?"),
        "",
        normalizePath(d)
      ),
      file_count = nrow(contained_files),
      total_size = sum(contained_files$size_bytes, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  dir_summary <- do.call(rbind, dir_summary)
  
  # ---------------------------------------------------------------------------
  # --- File extension statistics ---------------------------------------------
  # ---------------------------------------------------------------------------
  
  ext_summary <- aggregate(
    size_bytes ~ extension,
    data = files_df,
    FUN = sum
  )
  
  ext_counts <- aggregate(
    size_bytes ~ extension,
    data = files_df,
    FUN = length
  )
  
  names(ext_counts)[2] <- "count"
  
  ext_summary <- merge(
    ext_summary,
    ext_counts,
    by = "extension"
  )
  
  ext_summary <- ext_summary[
    order(ext_summary$size_bytes, decreasing = TRUE),
  ]
  
  # ---------------------------------------------------------------------------
  # --- Largest files ---------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  largest_files <- files_df[
    order(files_df$size_bytes, decreasing = TRUE),
  ]
  
  largest_files <- head(largest_files, 20)
  
  # ---------------------------------------------------------------------------
  # --- Empty directories -----------------------------------------------------
  # ---------------------------------------------------------------------------
  
  empty_dirs <- dir_paths[
    lengths(lapply(dir_paths, list.files)) == 0
  ]
  
  # ---------------------------------------------------------------------------
  # --- Modification statistics ----------------------------------------------
  # ---------------------------------------------------------------------------
  
  oldest_file <- files_df[
    which.min(files_df$modified),
  ]
  
  newest_file <- files_df[
    which.max(files_df$modified),
  ]
  
  # ---------------------------------------------------------------------------
  # --- Write report ----------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  con <- file(output_file, open = "wt")
  
  writeLines(separator, con)
  writeLines("rENM RUN DIRECTORY SUMMARY", con)
  writeLines(separator, con)
  
  writeLines("", con)
  
  writeLines(paste("Species code :", alpha_code), con)
  writeLines(paste("Run directory:", run_dir), con)
  writeLines(paste("Generated    :", Sys.time()), con)
  
  writeLines("", con)
  writeLines(separator, con)
  writeLines("GENERAL METRICS", con)
  writeLines(separator, con)
  
  writeLines(paste("Total folders      :", length(dir_paths)), con)
  writeLines(paste("Total files        :", nrow(files_df)), con)
  writeLines(paste("Total storage used :", format_size(total_size)), con)
  
  writeLines("", con)
  
  writeLines(
    paste(
      "Oldest file:",
      oldest_file$relative_path,
      "|",
      oldest_file$modified
    ),
    con
  )
  
  writeLines(
    paste(
      "Newest file:",
      newest_file$relative_path,
      "|",
      newest_file$modified
    ),
    con
  )
  
  # ---------------------------------------------------------------------------
  # --- Extension summary -----------------------------------------------------
  # ---------------------------------------------------------------------------
  
  writeLines("", con)
  writeLines(separator, con)
  writeLines("FILE TYPES", con)
  writeLines(separator, con)
  
  for (i in seq_len(nrow(ext_summary))) {
    
    line <- sprintf(
      "%-12s %8d files   %12s",
      ext_summary$extension[i],
      ext_summary$count[i],
      format_size(ext_summary$size_bytes[i])
    )
    
    writeLines(line, con)
  }
  
  # ---------------------------------------------------------------------------
  # --- Largest files ---------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  writeLines("", con)
  writeLines(separator, con)
  writeLines("LARGEST FILES", con)
  writeLines(separator, con)
  
  for (i in seq_len(nrow(largest_files))) {
    
    line <- sprintf(
      "%-12s %s",
      format_size(largest_files$size_bytes[i]),
      largest_files$relative_path[i]
    )
    
    writeLines(line, con)
  }
  
  # ---------------------------------------------------------------------------
  # --- Directory structure ---------------------------------------------------
  # ---------------------------------------------------------------------------
  
  writeLines("", con)
  writeLines(separator, con)
  writeLines("DIRECTORY STRUCTURE", con)
  writeLines(separator, con)
  
  for (d in sort(unique(dirname(files_df$relative_path)))) {
    
    writeLines("", con)
    writeLines(paste0("[", d, "]"), con)
    
    subfiles <- files_df[
      dirname(files_df$relative_path) == d,
    ]
    
    subfiles <- subfiles[
      order(subfiles$relative_path),
    ]
    
    for (i in seq_len(nrow(subfiles))) {
      
      line <- sprintf(
        "  %-12s %s",
        format_size(subfiles$size_bytes[i]),
        basename(subfiles$relative_path[i])
      )
      
      writeLines(line, con)
    }
  }
  
  # ---------------------------------------------------------------------------
  # --- Empty directories -----------------------------------------------------
  # ---------------------------------------------------------------------------
  
  writeLines("", con)
  writeLines(separator, con)
  writeLines("EMPTY DIRECTORIES", con)
  writeLines(separator, con)
  
  if (length(empty_dirs) == 0) {
    
    writeLines("None", con)
    
  } else {
    
    for (d in empty_dirs) {
      
      writeLines(
        sub(
          paste0("^", normalizePath(run_dir), "/?"),
          "",
          normalizePath(d)
        ),
        con
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # --- Directory summaries ---------------------------------------------------
  # ---------------------------------------------------------------------------
  
  writeLines("", con)
  writeLines(separator, con)
  writeLines("DIRECTORY STORAGE SUMMARY", con)
  writeLines(separator, con)
  
  dir_summary <- dir_summary[
    order(dir_summary$total_size, decreasing = TRUE),
  ]
  
  for (i in seq_len(nrow(dir_summary))) {
    
    line <- sprintf(
      "%-12s %6d files   %s",
      format_size(dir_summary$total_size[i]),
      dir_summary$file_count[i],
      dir_summary$directory[i]
    )
    
    writeLines(line, con)
  }
  
  # ---------------------------------------------------------------------------
  # --- Close connection ------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  close(con)
  
  message("Run summary written to:\n", output_file)
  
  invisible(output_file)
}