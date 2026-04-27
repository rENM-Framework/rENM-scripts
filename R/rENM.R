#' Run the end-to-end rENM pipeline for a single bird species (new)
#'
#' Executes the full retrospective ecological niche modeling (rENM)
#' workflow for a single species identified by its four-letter bird
#' banding code. The function wraps the major data acquisition,
#' occurrence processing, variable preparation, time-series modeling,
#' trend analysis, AI reporting, and final report assembly steps
#' implemented across the rENM framework packages.
#'
#' @details
#' This function serves as a top-level orchestration wrapper for the
#' installed rENM framework packages:
#'
#' \itemize{
#'   \item \code{rENM.core}
#'   \item \code{rENM.data}
#'   \item \code{rENM.model}
#'   \item \code{rENM.analysis}
#'   \item \code{rENM.ai}
#'   \item \code{rENM.reports}
#'   \item \code{rENM.dev}
#' }
#'
#' For the supplied species alpha code, the function runs the complete
#' pipeline in sequence, including:
#'
#' \enumerate{
#'   \item extraction and preparation of eBird occurrence records,
#'   \item spatial thinning and record limiting,
#'   \item range-based extent determination,
#'   \item extraction and staging of MERRA environmental variables,
#'   \item stochastic variable screening,
#'   \item construction of a 5-year-binned rENM time series,
#'   \item climatic suitability trend analyses,
#'   \item centroid, velocity, and hotspot analyses,
#'   \item compilation of report tables, pages, and summary graphics,
#'   \item assembly and submission of an AI-ready suitability package,
#'   \item rendering of returned AI documents, and
#'   \item assembly of the final report.
#' }
#'
#' The function time-stamps the start and end of the run, computes total
#' elapsed time, displays these values on the console, and appends them
#' to the species log file at:
#'
#' \preformatted{
#' <project-dir>/runs/<alpha_code>/_log.txt
#' }
#'
#' The project directory is resolved with
#' \code{rENM.core::rENM_project_dir()}, avoiding hard-coded paths and
#' supporting CRAN-compliant package behavior.
#'
#' Log entries are written in the framework's standard timestamped format
#' and bracketed with 72-character separator lines for readability.
#'
#' The function uses \code{on.exit()} to guarantee that end time and
#' elapsed time are recorded even if the pipeline terminates early, and
#' \code{tryCatch()} to log any error before re-throwing it.
#'
#' @param alpha_code Character scalar. The four-letter bird banding code
#'   for the target species. The value is normalized to uppercase before
#'   processing.
#'
#' @return Invisibly returns a named list containing:
#' \itemize{
#'   \item \code{alpha_code}: normalized four-letter species code,
#'   \item \code{start_time}: POSIXct start time,
#'   \item \code{end_time}: POSIXct end time,
#'   \item \code{elapsed_time}: difftime object giving total elapsed time,
#'   \item \code{log_file}: full path to the run log file,
#'   \item \code{status}: character string giving the final run status.
#' }
#'
#' The primary purpose of the function is side-effect execution of the
#' complete rENM workflow and generation of its associated outputs.
#'
#' @importFrom utils flush.console
#'
#' @examples
#' \dontrun{
#' rENM("CASP")
#' }
#'
#' @seealso
#' \code{\link[rENM.core:rENM_project_dir]{rENM.core::rENM_project_dir}}
#'
#' @export
rENM <- function(alpha_code) {

  if (!is.character(alpha_code) || length(alpha_code) != 1L || is.na(alpha_code)) {
    stop("'alpha_code' must be a single non-missing character value.", call. = FALSE)
  }

  alpha_code <- toupper(trimws(alpha_code))

  if (nchar(alpha_code) != 4L) {
    stop("'alpha_code' must be a four-letter bird banding code.", call. = FALSE)
  }

  .required_pkgs <- c(
    "rENM.core",
    "rENM.data",
    "rENM.model",
    "rENM.analysis",
    "rENM.ai",
    "rENM.reports",
    "rENM.dev"
  )

  missing_pkgs <- .required_pkgs[
    !vapply(.required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0L) {
    stop(
      "The following required packages are not installed: ",
      paste(missing_pkgs, collapse = ", "),
      call. = FALSE
    )
  }

  project_dir <- rENM.core::rENM_project_dir()
  run_dir     <- file.path(project_dir, "runs", alpha_code)
  log_file    <- file.path(run_dir, "_log.txt")

  if (!dir.exists(run_dir)) {
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  }

  separator <- paste(rep("-", 72L), collapse = "")

  .timestamp <- function(x) {
    format(x, "%Y-%m-%d %H:%M:%S %Z")
  }

  .log_write <- function(..., append = TRUE) {
    cat(..., file = log_file, sep = "", append = append)
  }

  .log_line <- function(text, time = Sys.time()) {
    stamp <- paste0("[", .timestamp(time), "] ")
    .log_write(stamp, text, "\n")
  }

  start_time <- Sys.time()
  end_time <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = attr(start_time, "tzone"))
  elapsed_time <- NA
  status <- "started"

  on.exit({
    if (is.na(end_time)) {
      end_time <<- Sys.time()
    }
    if (is.na(elapsed_time)[1]) {
      elapsed_time <<- end_time - start_time
    }

    cat("[", .timestamp(end_time), "] ", "Finished rENM() for ", alpha_code,
        " with status: ", status, ".\n", sep = "")
    cat("Start time:   ", .timestamp(start_time), "\n", sep = "")
    cat("End time:     ", .timestamp(end_time), "\n", sep = "")
    cat("Elapsed time: ", format(elapsed_time), "\n", sep = "")
    cat(separator, "\n", sep = "")
    utils::flush.console()

    .log_write(separator, "\n")
    .log_line(paste0("Finished rENM() for ", alpha_code, " with status: ", status, "."), time = end_time)
    .log_line(paste0("Run start time: ", .timestamp(start_time)), time = end_time)
    .log_line(paste0("Run end time: ", .timestamp(end_time)), time = end_time)
    .log_line(paste0("Total elapsed time: ", format(elapsed_time)), time = end_time)
    .log_write(separator, "\n")
  }, add = TRUE)

  cat(separator, "\n", sep = "")
  cat("[", .timestamp(start_time), "] ", "Starting rENM() for ", alpha_code, " ...\n", sep = "")
  utils::flush.console()

  .log_write(separator, "\n")
  .log_line(paste0("Starting rENM() for ", alpha_code, " ..."), time = start_time)
  .log_line(paste0("Run start time: ", .timestamp(start_time)), time = start_time)
  .log_write(separator, "\n")

  tryCatch({

    # --------------------------------------------------------------------------

    # --- DATA ASSEMBLY ---

    # extract ebird occurrence records from the base collection
    rENM.data::get_ebird_occurrences(alpha_code)

    # de-duplicate occurrence records
    rENM.data::remove_duplicate_occurrences(alpha_code)

    # thin records in the _occs/tmp/ run collection [parallel version]
    rENM.data::thin_occurrences2(alpha_code)

    # thin records to upper bound
    rENM.data::limit_record_count(alpha_code)

    # move filtered records to the <alpha_code>/_occs/ run collection and tidy up
    rENM.data::tidy_occurrences(alpha_code)

    # set model extent using the usgs gap range
    rENM.data::find_range_extent(alpha_code)

    # extract merra variables and move to the run directory
    rENM.data::get_merra_variables(alpha_code)

    # --- TIME SERIES CONSTRUCTION ---

    # stage occurrence data
    rENM.model::stage_occurrences(alpha_code)

    # down select variables (maxnet version)
    rENM.model::screen_by_convergence2(alpha_code)

    # stage screened variables
    rENM.model::stage_screened_variables(alpha_code)

    # reduce covariance among staged variables
    # rENM.model::reduce_covariance(alpha_code)

    # create an rENM time series
    rENM.model::create_timeseries(alpha_code)

    # --- TIME SERIES ANALYSIS ---

    # compute the thiel-sen trend across the time series
    rENM.analysis::find_suitability_trend(alpha_code)

    # find proportions of pos, neg, and zero values across modeled extent
    rENM.analysis::find_trend_percentages(alpha_code)

    # find proportions of pos, neg, and zero values across USGS GAP range
    rENM.analysis::find_range_change_percentages(alpha_code)

    # create state-level suitability trend analysis and map for a species
    rENM.analysis::create_state_trend_analysis(alpha_code)

    # analyze temporal trends in suitability centroids
    rENM.analysis::analyze_weighted_centroids(alpha_code)

    # finds bioclimatic velocity from weighted centroids
    rENM.analysis::find_bioclimatic_velocity(alpha_code)

    # plot and save suitability trend map with centroids
    rENM.analysis::save_trend_plot_with_centroids(alpha_code)

    # gather variable rankings
    rENM.analysis::gather_variable_contributions(alpha_code)

    # summarize variable contributions
    rENM.analysis::summarize_variable_contributions(alpha_code)

    # create a climatic suitability trend acceleration/deceleration map
    rENM.analysis::create_suitability_change_map(alpha_code)

    # create a hot spot map
    rENM.analysis::create_hot_spot_map(alpha_code)

    # gather time series suitability maps
    rENM.reports::gather_suitability_maps(alpha_code)

    # --- REPORT GENERATION ---

    # gather time series suitability maps
    rENM.reports::gather_suitability_maps(alpha_code)

    # gather range maps
    rENM.reports::gather_range_maps(alpha_code)

    # gather suitability trend and hot spot stats
    rENM.reports::gather_suitability_trend_stats(alpha_code)

    # create a suitability trend summary table
    rENM.reports::create_suitability_trend_summary_table(alpha_code)

    # create a suitability time series summary page
    rENM.reports::assemble_suitability_timeseries_page(alpha_code)

    # create a range time series summary page
    rENM.reports::assemble_range_timeseries_page(alpha_code)

    # create a suitability trends, acceleration/deceleration page
    rENM.reports::assemble_suitability_trends_page(alpha_code)

    # create a state trends and state hot spot page
    rENM.reports::assemble_state_trends_page(alpha_code)

    # create variable trend summary table
    rENM.reports::create_variable_trend_summary_table(alpha_code)

    # create variable trends page
    rENM.reports::assemble_variable_trends_page(alpha_code)

    # gather trend and rate-of-change maps for the top 10 variables
    rENM.reports::gather_top_variable_trend_maps(alpha_code)

    # create summary page for the top 10 variables
    rENM.reports::assemble_variable_trend_maps_page(alpha_code)

    # create centroid trend summary table
    rENM.reports::create_centroid_trend_summary_table(alpha_code)

    # create a suitability trends with centroids page
    rENM.reports::assemble_centroid_trends_page(alpha_code)

    # --- GenAI ANALYSIS ---

    # build a data package for submission to ChatGPT
    rENM.ai::assemble_ai_package(alpha_code)

    # submit the package using the OpenAI Reponses API
    rENM.ai::submit_ai_package(alpha_code)

    # render the returned .docx file into a .pdf file
    rENM.ai::render_ai_docx(alpha_code)

    # --- CREATE FINAL REPORT ---

    # create a final report comprising all summary pages
    rENM.reports::assemble_final_report(alpha_code)

    # --------------------------------------------------------------------------

    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    status <- "success"

    invisible(list(
      alpha_code   = alpha_code,
      start_time   = start_time,
      end_time     = end_time,
      elapsed_time = elapsed_time,
      log_file     = log_file,
      status       = status
    ))

  }, error = function(e) {

    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    status <- "error"

    msg <- conditionMessage(e)

    cat("[", .timestamp(end_time), "] ", "ERROR in rENM() for ", alpha_code, ": ", msg, "\n", sep = "")
    utils::flush.console()

    .log_write(separator, "\n")
    .log_line(paste0("ERROR in rENM() for ", alpha_code, ": ", msg), time = end_time)
    .log_line(paste0("Run start time: ", .timestamp(start_time)), time = end_time)
    .log_line(paste0("Run end time: ", .timestamp(end_time)), time = end_time)
    .log_line(paste0("Elapsed time before failure: ", format(elapsed_time)), time = end_time)
    .log_write(separator, "\n")

    stop(e)
  })
}

# ------------------------------------------------------------------------------

# load rENM framework packages
library(rENM.core)
library(rENM.data)
library(rENM.model)
library(rENM.analysis)
library(rENM.ai)
library(rENM.reports)

# alpha_code <- "BCRF"
# alpha_code <- "CASP"
# alpha_code <- "GRRO"

rENM(alpha_code)
