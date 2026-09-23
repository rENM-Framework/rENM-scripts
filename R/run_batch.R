# ---------------------------------------------------------------------------
# run_batch.R -- run the rENM pipeline over several species unattended
#
# Usage
#   source("~/rENM/run_batch.R")                      # runs the list below
#   run_rENM_batch(c("PIJA", "GRWA"))                 # or call it directly
#   run_rENM_batch(c("PIJA"), seed = NULL)            # non-reproducible run
#   run_rENM_batch(c("PIJA"), ai = NULL)              # no generated narrative
#
# rENM() logs its own failure and then re-raises, so an unguarded loop stops
# at the first bad species and abandons the rest. Each call here is wrapped,
# so one failure costs one species instead of the batch.
#
# Progress is written to runs/_batch_log.txt as well as the console. An
# unattended run outlives the console buffer, and the file is what you read
# the next morning.
# ---------------------------------------------------------------------------

# ---- Batch runner ---------------------------------------------------------

#' @param species   Character vector of four-letter alpha codes.
#' @param seed      Passed to rENM(). Keep the default for reproducible runs;
#'                  NULL restores the original stochastic behaviour.
#' @param ai        Passed to rENM(). One of "chatgpt", "claude", or NULL.
#'                  The default matches rENM()'s own. NULL skips the provider
#'                  call and substitutes a coversheet, which makes the batch
#'                  deterministic end to end and costs nothing per species.
#' @param log_file  Where to append progress. NULL disables file logging.
#' @param stop_on_error  TRUE aborts the batch at the first failure. The
#'                  default carries on, which is what an overnight run wants.
#'
#' @return Invisibly, a data frame of species, elapsed minutes and status.
run_rENM_batch <- function(species,
                           seed          = 42,
                           ai            = "chatgpt",
                           log_file      = file.path(
                             rENM.core::rENM_project_dir(), "runs",
                             "_batch_log.txt"
                           ),
                           stop_on_error = FALSE) {

  stopifnot(is.character(species), length(species) > 0)
  species <- toupper(trimws(species))

  # Checked here rather than left to rENM(), which validates per call: a bad
  # value would otherwise fail once per species instead of once per batch.
  if (!is.null(ai) &&
      !(is.character(ai) && length(ai) == 1L && ai %in% c("chatgpt", "claude"))) {
    stop("'ai' must be one of \"chatgpt\", \"claude\", or NULL.", call. = FALSE)
  }

  say <- function(txt) {
    cat(txt, "\n", sep = "")
    utils::flush.console()
    if (!is.null(log_file)) {
      try(cat(txt, "\n", file = log_file, append = TRUE, sep = ""),
          silent = TRUE)
    }
  }

  rule       <- strrep("=", 72)
  batch_t0   <- Sys.time()
  status     <- character(length(species))
  minutes    <- numeric(length(species))

  say("")
  say(rule)
  say(sprintf("[%s] BATCH START -- %d species: %s",
              format(batch_t0, "%Y-%m-%d %H:%M:%S"),
              length(species), paste(species, collapse = ", ")))
  say(sprintf("             seed = %s",
              if (is.null(seed)) "none (runs are not reproducible)" else seed))
  say(sprintf("             ai   = %s",
              if (is.null(ai)) "none (coversheet only)" else ai))
  say(rule)

  for (i in seq_along(species)) {
    sp <- species[i]
    t0 <- Sys.time()

    say("")
    say(sprintf("[%s] START %s  (%d of %d)",
                format(t0, "%Y-%m-%d %H:%M:%S"), sp, i, length(species)))

    result <- tryCatch(
      {
        rENM::rENM(sp, seed = seed, ai = ai)
        "OK"
      },
      error = function(e) paste("FAILED:", conditionMessage(e))
    )

    minutes[i] <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    status[i]  <- result

    say(sprintf("[%s] END   %s  %.1f min  %s",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                sp, minutes[i], result))

    if (stop_on_error && !identical(result, "OK")) {
      say(sprintf("Stopping: %s failed and stop_on_error = TRUE.", sp))
      break
    }
  }

  out <- data.frame(
    species = species,
    minutes = round(minutes, 1),
    status  = status,
    stringsAsFactors = FALSE
  )

  n_ok    <- sum(out$status == "OK")
  elapsed <- as.numeric(difftime(Sys.time(), batch_t0, units = "mins"))

  say("")
  say(rule)
  say("BATCH SUMMARY")
  for (i in seq_len(nrow(out))) {
    say(sprintf("  %-6s %7.1f min  %s",
                out$species[i], out$minutes[i], out$status[i]))
  }
  say(sprintf("  %d of %d succeeded in %.1f min total",
              n_ok, nrow(out), elapsed))
  say(rule)

  invisible(out)
}

# ---- Species to run -------------------------------------------------------
# Edit this list, then source the file. Sourcing with a `species` object
# already defined leaves it alone, so the function can be called directly
# without the list below firing.

if (!exists("species", inherits = FALSE)) {
  species <- c("PIJA", "GRWA", "GRVI", "CASP", "BCRF", "GRRO")
  batch_results <- run_rENM_batch(species)
}
