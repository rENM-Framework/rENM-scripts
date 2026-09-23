# rENM Framework Scripts

![rENM](https://img.shields.io/badge/rENM-framework-blue) ![module](https://img.shields.io/badge/module-scripts-informational)

**Utility and orchestration scripts for the rENM Framework**

## Overview

This repository contains R and bash scripts that support work on the rENM Framework — a modular suite of R packages for reconstructing and analyzing long-term ecological niche dynamics using historical species occurrence records and environmental data.

## Contents

### R scripts

| File | Description |
|----|----|
| `R/count_r_lines.R` | Codebase audit — counts and classifies lines across all rENM package source files |
| `R/run_batch.R` | Batch orchestration - runs the rENM pipeline over several species unattended |
| `R/summarize_run.R` | Directory summary - traverses a species run directory and writes a technical summary |

### Bash scripts

| File | Description |
|----|----|
| `bash/create_project_directory.sh` | Create the rENM project directory structure |
| `bash/install_example_data.sh` | Download and install the rENM example dataset |

## Getting started

Run the bash scripts in order to initialize a working project directory and install the example data before running any R scripts:

``` bash
bash bash/create_project_directory.sh
bash bash/install_example_data.sh
```

By default both scripts target `~/rENMtest`. Edit the `PROJECT_DIRECTORY` variable at the top of each script to use a different location.

## Running the pipeline

The pipeline lives in the `rENM` package, not in this repository. After installing the framework packages and configuring your project directory, a single species runs from the R console:

``` r
library(rENM)
rENM("CASP")
```

`rENM()` runs all stages in sequence and logs progress and elapsed time to `<project_dir>/runs/<alpha_code>/_log.txt`. It takes a `seed` argument, defaulting to 42, which makes a run reproducible, and an `ai` argument selecting `"chatgpt"`, `"claude"`, or `NULL` for no generated narrative. Enter `?rENM` for details, and see the [User Manual](https://github.com/rENM-Framework/rENM-documentation) for installation, configuration, and a step-by-step walkthrough.

## Batch runs

`run_batch.R` runs the pipeline over several species unattended:

``` r
source("R/run_batch.R")
run_rENM_batch(c("PIJA", "GRWA", "CASP"))
run_rENM_batch(c("PIJA"), seed = NULL)   # non-reproducible run
```

`rENM()` logs a failure and then re-raises it, so an unguarded loop would stop at the first bad species and abandon the rest. Each call is wrapped here, so one failure costs one species rather than the batch. Progress is written to `runs/_batch_log.txt` as well as the console, since an unattended run outlives the console buffer.

## Codebase audit

`count_r_lines.R` produces a line-count summary across all rENM package source files, classifying each line as code, comment, or blank. Edit the `project_directory` and `pkg_dirs` variables at the top of the User Setup section to match your local paths, then source the script:

``` r
source("R/count_r_lines.R")
```

Output is written to:

- `<project_dir>/framework/framework_summary_detail.txt` — per-file breakdown
- `<project_dir>/framework/framework_summary_totals.csv` — per-package totals

## Contact

John Schnase — [rENM.Framework\@gmail.com](mailto:rENM.Framework@gmail.com)

## License

See `LICENSE` for details.

------------------------------------------------------------------------

**rENM Framework** — A modular system for reconstructing and analyzing long-term ecological niche dynamics.
