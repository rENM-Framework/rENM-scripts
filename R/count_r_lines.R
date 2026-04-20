#' rENM Ffamework codebase summary and line count audit
#'
#' This script provides a reproducible, audit-ready summary of the size and
#' structure of an rENM framework codebase composed of multiple R packages.
#' It computes line-count metrics across all .R scripts located in each
#' package's /R directory and aggregates the results into both human-readable
#' and machine-readable outputs.
#'
#' The script performs the following steps:
#'   (1) Traverses each package directory and identifies all .R source files
#'   (2) Classifies each line as code, comment, or blank
#'   (3) Computes per-file and per-package summaries
#'   (4) Aggregates results across all packages into a grand total
#'   (5) Writes a structured audit report (.txt) and a tabular summary (.csv)
#'
#' Details:
#' Line classification is defined as follows:
#'   - Blank lines: empty or whitespace-only lines
#'   - Comment lines: lines beginning with "#"
#'   - Code lines: all remaining lines
#'
#' Inline comments (e.g., "x <- 1 # note") are treated as code lines, which
#' is consistent with standard LOC (lines-of-code) accounting practices.
#'
#' This script is designed to support:
#'   - reproducibility audits
#'   - software documentation metrics
#'   - reporting for publications and technical appendices
#'   - tracking codebase growth and structure over time
#'
#' All paths are normalized to ensure cross-platform compatibility and
#' CRAN-compliant behavior (no hard-coded absolute paths).
#'
#' Input:
#'   project_directory : character
#'       Path to the root directory where output files will be written.
#'
#'   pkg_dirs : character vector
#'       Vector of paths to package directories. Each directory must contain
#'       an /R subdirectory with .R source files.
#'
#' Output:
#'   framework_summary.txt
#'       A human-readable audit report containing:
#'         - per-package summaries
#'         - per-file breakdowns
#'         - grand totals across all packages
#'
#'   framework_summary_totals.csv
#'       A machine-readable table with per-package totals and a final
#'       "ALL_PACKAGES" aggregate row.
#'
#' Notes:
#'   - Compatible with rENM_project_dir() for dynamic project resolution
#'   - Designed for integration into rENM.ai pipelines and reporting workflows
#'   - Output format follows rENM logging conventions (72-character separators)
#'
#' Author: rENM Framework
#' Date: 2026-04-20
#' ============================================================================


# ---- Function: count_r_lines -----------------------------------
count_r_lines <- function(pkg_dir) {

  pkg_dir <- normalizePath(pkg_dir, mustWork = TRUE)
  r_dir <- file.path(pkg_dir, "R")

  if (!dir.exists(r_dir)) {
    warning("No /R directory found in: ", pkg_dir)
    return(NULL)
  }

  files <- list.files(
    r_dir,
    pattern = "\\.[Rr]$",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(files) == 0) {
    warning("No .R files found in: ", r_dir)
    return(NULL)
  }

  classify_lines <- function(lines) {
    trimmed <- trimws(lines)

    blank <- trimmed == ""
    comment <- grepl("^#", trimmed)
    code <- !(blank | comment)

    list(
      total = length(lines),
      blank = sum(blank),
      comment = sum(comment),
      code = sum(code)
    )
  }

  per_file <- lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    stats <- classify_lines(lines)

    data.frame(
      file = basename(f),
      total = stats$total,
      code = stats$code,
      comment = stats$comment,
      blank = stats$blank,
      stringsAsFactors = FALSE
    )
  })

  per_file_df <- do.call(rbind, per_file)
  totals <- colSums(per_file_df[, c("total", "code", "comment", "blank")])

  list(
    totals = totals,
    by_file = per_file_df
  )
}

# ---- User Setup -------------------------------------------------

project_directory <- "~/rENM"
project_directory <- normalizePath(project_directory, mustWork = TRUE)

summary_file <- file.path(project_directory, "framework", "framework_summary_detail.txt")
csv_file     <- file.path(project_directory, "framework", "framework_summary_totals.csv")

pkg_dirs <- c(
  "~/rENM/framework/rENM.core",
  "~/rENM/framework/rENM.data",
  "~/rENM/framework/rENM.model",
  "~/rENM/framework/rENM.analysis",
  "~/rENM/framework/rENM.ai",
  "~/rENM/framework/rENM.reports"
)

pkg_dirs  <- normalizePath(pkg_dirs, mustWork = TRUE)
pkg_names <- basename(pkg_dirs)

# ---- Initialize outputs ----------------------------------------

cat("rENM Framework Codebase Summary\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    paste(rep("=", 72), collapse = ""), "\n\n",
    file = summary_file)

totals_list <- list()

# ---- Process packages ------------------------------------------

for (i in seq_along(pkg_dirs)) {

  res <- count_r_lines(pkg_dirs[i])
  if (is.null(res)) next

  totals <- res$totals
  totals_list[[pkg_names[i]]] <- totals

  cat("Package:", pkg_names[i], "\n",
      paste(rep("-", 72), collapse = ""), "\n",
      file = summary_file,
      append = TRUE)

  cat(
    sprintf("Total lines    : %d\n", totals["total"]),
    sprintf("Code lines    : %d\n", totals["code"]),
    sprintf("Comment lines : %d\n", totals["comment"]),
    sprintf("Blank lines   : %d\n\n", totals["blank"]),
    file = summary_file,
    append = TRUE
  )

  write.table(
    res$by_file,
    file = summary_file,
    append = TRUE,
    row.names = FALSE,
    quote = FALSE,
    sep = "\t"
  )

  cat("\n\n", file = summary_file, append = TRUE)
}

# ---- Grand totals ----------------------------------------------

totals_df <- do.call(rbind, totals_list)
totals_df <- as.data.frame(totals_df)
totals_df$package <- rownames(totals_df)

totals_df <- totals_df[, c("package", "total", "code", "comment", "blank")]

grand_totals <- colSums(totals_df[, c("total", "code", "comment", "blank")])

grand_row <- data.frame(
  package = "ALL_PACKAGES",
  total   = grand_totals["total"],
  code    = grand_totals["code"],
  comment = grand_totals["comment"],
  blank   = grand_totals["blank"]
)

totals_df <- rbind(totals_df, grand_row)

cat(paste(rep("=", 72), collapse = ""), "\n",
    "GRAND TOTALS\n",
    paste(rep("=", 72), collapse = ""), "\n",
    file = summary_file,
    append = TRUE)

cat(
  sprintf("Total lines    : %d\n", grand_totals["total"]),
  sprintf("Code lines    : %d\n", grand_totals["code"]),
  sprintf("Comment lines : %d\n", grand_totals["comment"]),
  sprintf("Blank lines   : %d\n\n", grand_totals["blank"]),
  file = summary_file,
  append = TRUE
)

# ---- Write CSV --------------------------------------------------
write.csv(totals_df, csv_file, row.names = FALSE)

# ---- Final message ---------------------------------------------

cat("Framework summary written to:\n", summary_file, "\n")
cat("CSV totals written to:\n", csv_file, "\n")
