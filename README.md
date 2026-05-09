# rENM Framework Scripts

![rENM](https://img.shields.io/badge/rENM-framework-blue) ![module](https://img.shields.io/badge/module-scripts-informational)

**Utility and orchestration scripts for the rENM Framework**

## Overview

This repository contains R and bash scripts that support the rENM Framework — a modular suite of R packages for reconstructing and analyzing long-term ecological niche dynamics using historical species occurrence records and environmental data.

## Contents

### R scripts

| File | Description |
|----|----|
| `R/rENM.R` | End-to-end pipeline orchestration — runs the complete rENM workflow for a single species |
| `R/count_r_lines.R` | Codebase audit — counts and classifies lines across all rENM package source files |

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

`rENM.R` wraps the complete six-package rENM pipeline into a single function call. After installing the rENM framework packages and configuring your project directory, source the script and call `rENM()` with a four-letter bird banding code:

``` r
source("R/rENM.R")
rENM("CASP")
```

Edit the `alpha_code` variable near the bottom of the script to change the target species. The pipeline runs all stages in sequence — data assembly, time-series modeling, trend analysis, report generation, and AI synthesis — and logs progress and elapsed time to `<project_dir>/runs/<alpha_code>/_log.txt`.

Individual pipeline steps can be commented out to run a partial workflow. `reduce_covariance()` and `submit_to_claude()` are already commented out in the default configuration; uncomment them as needed.

## Codebase audit

`count_r_lines.R` produces a line-count summary across all rENM package source files, classifying each line as code, comment, or blank. Edit the `project_directory` and `pkg_dirs` variables at the top of the User Setup section to match your local paths, then source the script:

``` r
source("R/count_r_lines.R")
```

Output is written to:

-   `<project_dir>/framework/framework_summary_detail.txt` — per-file breakdown
-   `<project_dir>/framework/framework_summary_totals.csv` — per-package totals

## Contact

John Schnase — [rENM.Framework\@gmail.com](mailto:rENM.Framework@gmail.com){.email}

## License

See `LICENSE` for details.

------------------------------------------------------------------------

**rENM Framework** — A modular system for reconstructing and analyzing long-term ecological niche dynamics.
