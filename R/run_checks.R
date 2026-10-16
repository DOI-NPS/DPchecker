#' Run all congruence checks
#'
#' @param check_metadata_only Only run checks on the metadata and skip anything involving data files.
#' @param output_filename Optional. If specified, saves results of congruence checks to this file. If omitted, prints results to console. If the file already exists, results will be appended to the existing file.
#' @param output_dir Location in which to save the output file, if using.
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#' @param skip_cols String. Defaults to NA. A list of one or more columns in the data to skip when testing whether the dates within data fall within the dates range specified in metadata. Useful if, for instance, there are columns within the data associated with the QA/QC process and these dates are expected to fall outside the date range specified for the data.
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' dir <- DPchecker_example("BICY_veg")
#' run_congruence_checks(dir)
#'
run_congruence_checks <- function(directory = here::here(),
                                  metadata = load_metadata(directory),
                                  check_metadata_only = FALSE,
                                  skip_cols = NA,
                                  output_filename,
                                  output_dir = here::here()) {
  is_eml(metadata) # Throw an error if metadata isn't an emld object

  err_count <- 0
  warn_count <- 0
  total_count <- 10 # Don't forget to update this number when adding more checks!
  # (why? doesn't appear to be used anywhere else...)

  if (!missing(output_filename)) {
    output_dir <- normalizePath(output_dir,
                                winslash = .Platform$file.sep,
                                mustWork = TRUE
    )
    output_path <- file.path(output_dir, output_filename)
    open_mode <- if (file.exists(output_path)) {
      "at" # if file exists, use append mode
    } else {
      "wt" # If the file doesn't already exist, use write mode
    }
    file <- file(output_path, open = open_mode)
    sink(file)
    sink(file, type = "message")
    if (open_mode == "at") {
      cli::cli_verbatim("\n\n\n") # If appending to existing log, add a few newlines to make it more readable
    }
    cli::cli_rule(center = "{Sys.time()}")
    cli::cli_inform("The following checks were run using DPchecker version {packageVersion('DPchecker')}.")
  }

  if (check_metadata_only) {
    cli::cli_h1("Running metadata-only checks (skipping checks against data files)")
  } else {
    cli::cli_h1("Running all congruence checks")
  }

  cli::cli_h2("Reading metadata")
  tryCatch(invisible(metadata), # load_metadata from the function args actually gets evaluated here
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             cli::cli_abort(c("x" = "You must correct the above issue before the congruence checks can run."), call = NULL)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  cli::cli_h2("Checking metadata compliance")
  tryCatch(test_validate_schema(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_alert_danger("Schema validation failed. Run {.fn test_validate_schema} for details.")
             cli::cli_abort(c("x" = "Metadata schema must validate before the rest of the congruence checks can run."), call = NULL)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_alert_warning("Schema validation warnings exist. Run {.fn test_validate_schema} for details.", call = NULL)
           }
  )
  tryCatch(test_dup_meta_entries(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             cli::cli_abort(c("x" = "You must remove duplicate data table names from metadata before the rest of the congruence checks can run."), call = NULL)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_metadata_version(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_delimiter(metadata),
           error = function(e) {
             cli::cli_bullets(c(e$message, e$body))
             err_count <<- err_count + 1
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_header_num(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_footer(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_taxonomic_cov(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_geographic_cov(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_content_units(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_doi(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_doi_format(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_datatable_urls(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_datatable_urls_doi(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           })
  tryCatch(test_datatable_url_attributes(metadata),
           error = function(e) {
             err_count <<-err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           })
  tryCatch(test_publisher(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_valid_fieldnames(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_valid_filenames(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_pii_meta_emails(directory),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  cli::cli_h2("Checking that metadata contains required elements for DataStore extraction")

  tryCatch(test_creator(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_pub_date(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_dp_title(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_keywords(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_by_for_nps(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_publisher_name(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_publisher_state(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_publisher_city(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_dp_abstract(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_methods(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_file_descript(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_access(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_cui_label(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_cui_email(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_cui_designator(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_license(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_int_rights(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_attribute_defs(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_storage_type(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  cli::cli_h2("Checking additional/optional metadata elements")

  tryCatch(test_orcid_exists(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_orcid_format(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_orcid_resolves(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_orcid_match(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_notes(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )
  tryCatch(test_project(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  if (!check_metadata_only) {
    cli::cli_h2("Checking that metadata is consistent with data file(s)")
    tryCatch(test_file_name_match(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               cli::cli_abort(c("x" = "Files documented in metadata must match files present in package before the rest of the congruence checks can run."), call = NULL)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             }
    )
    tryCatch(test_fields_match(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               cli::cli_abort(c("x" = "Columns documented in metadata must match columns present in data files before the rest of the congruence checks can run."), call = NULL)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             }
    )
    tryCatch(test_missing_data(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             }
    )
    tryCatch(test_numeric_fields(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             }
    )
    tryCatch(test_dates_parse(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             }
    )
    tryCatch(test_date_range(directory, metadata, skip_cols = skip_cols),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             }
    )

    cli::cli_h2("Checking data and metadata compliance")
    tryCatch(test_pii_data_emails(directory),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_verbatim(c(w$message, w$body))
             }
    )
  }

  cli::cli_h2("Summary")
  if (err_count > 0) {
    cli::cli_alert_danger("{err_count} errors to address")
  }
  if (warn_count > 0) {
    cli::cli_alert_warning("{warn_count} warnings to look into")
  }
  if (warn_count + err_count == 0) {
    check_type <- if (check_metadata_only) {
      "metadata"
    } else {
      "congruence"
    }
    cli::cli_alert_success("Success! All {check_type} checks passed.")
  }

  if (!missing(output_filename)) {
    sink(type = "message")
    sink()
    close(file)
    file.show(output_path) # Opens log file. May want to add option in future to not do this
  }
  return(invisible(c("errors" = err_count, "warnings" = warn_count)))
}

#' Generate path to example data
#'
#' @param dp_name Name of data package.
#'
#' @return Path to example data, if dp_name is specified.
#' @export
#'
#' @examples
#' DPchecker_example()
#' DPchecker_example("BUIS_herps")
DPchecker_example <- function(dp_name = c("BICY_veg", "BUIS_herps")) {
  dp_name <- match.arg(dp_name)
  message("Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.")
  system.file("extdata", dp_name, package = "DPchecker", mustWork = TRUE)
}
