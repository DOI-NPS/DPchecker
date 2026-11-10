#' Run all congruence checks
#'
#' @param check_metadata_only Only run checks on the metadata and skip anything involving data files.
#' @param output_filename Optional. If specified, saves results of congruence checks to this file. If omitted, prints results to console. If the file already exists, results will be appended to the existing file.
#' @param output_dir Location in which to save the output file, if using.
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#' @param skip_cols String. Defaults to NA. A list of one or more columns in the data to skip when testing whether the dates within data fall within the dates range specified in metadata. Useful if, for instance, there are columns within the data associated with the QA/QC process and these dates are expected to fall outside the date range specified for the data.
#'
#' @return Invisibly returns a named integer vector
#'   `c(errors = <n>, warnings = <n>)` (unchanged from prior versions for
#'   any existing caller). Additionally carries a `"checks"` attribute: a
#'   list with one element per check that actually ran, each of the form
#'   `list(check = <name>, status = "pass"/"warn"/"fail", message = <chr>)`,
#'   in execution order. `message` is the raw condition text the check
#'   raised (`c(cond$message, cond$body)`) for a warn/fail, or NULL for a
#'   pass - i.e. the same content printed to the console, minus cli's
#'   visual formatting. The attribute is purely additive - it does not
#'   affect the vector's value, names, class, or indexing, so existing
#'   console/script use is unaffected. Intended for programmatic consumers
#'   (e.g. a Shiny GUI) that need a per-check readout - including the
#'   reason for a warn/fail - rather than parsing console output.
#'
#'   If a GATEKEEPER check fails (load_metadata, validate_schema,
#'   dup_meta_entries, file_name_match, fields_match), the function aborts
#'   as before - it does NOT return. In that case the same per-check list
#'   (including the failing gatekeeper, with its message, and every check
#'   that ran before it) is attached to the thrown condition as a `$checks`
#'   field, recoverable via tryCatch(error = function(e) e$checks). The
#'   console output of the abort is unchanged (the extra condition field is
#'   metadata, not message text; the message is still rendered with cli
#'   formatting via rlang::abort(..., use_cli_format = TRUE)).
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

  # Structured per-check log for programmatic consumers (e.g. breezeml's
  # congruence tab). record_check(name) is called once immediately after
  # each check's tryCatch(...) block; it reads the running err_count/
  # warn_count totals, diffs them against their values at the previous
  # record_check() call, determines pass/warn/fail, and appends one record.
  #
  # The record also carries `message`: the raw condition text the check
  # raised. record_check() can't see the condition object itself (e/w only
  # exist inside the handlers), so each handler stashes c(cond$message,
  # cond$body) into `.last_msg` (the SAME content it already prints via
  # cli::cli_bullets); record_check() reads it into the record and clears
  # it for the next check. A passing check leaves .last_msg NULL, so its
  # record's message is NULL. This is purely additive - the cli output the
  # console user sees is untouched (the .last_msg assignment is a side
  # assignment next to the existing cli call).
  #
  # GATEKEEPER CHECKS (the ones whose error handler aborts) need special
  # handling: the abort unwinds the stack, so the post-block record_check()
  # never runs on a failure, AND the local check_log would otherwise be
  # lost. So each gatekeeper's error handler, right before aborting, (a)
  # appends its own "fail" record INLINE (including the message), and (b)
  # passes the partial check_log on the thrown condition via rlang::abort(
  # ..., checks = check_log) so the caller can recover it as e$checks.
  # (Note: cli::cli_abort() does NOT forward extra fields onto the
  # condition - verified - so rlang::abort() is used directly, with
  # use_cli_format = TRUE and a c("x" = ...) message so the console output
  # is byte-identical to the old cli::cli_abort(c("x" = ...)) call.)
  # On a SUCCESSFUL gatekeeper check the inline append never runs (no
  # error), so the normal post-block record_check() still logs its pass/
  # warn - mutually exclusive, so no double-logging.
  check_log <- list()
  .prev_err <- 0
  .prev_warn <- 0
  .last_msg <- NULL
  record_check <- function(name) {
    status <- if (err_count > .prev_err) {
      "fail"
    } else if (warn_count > .prev_warn) {
      "warn"
    } else {
      "pass"
    }
    .prev_err <<- err_count
    .prev_warn <<- warn_count
    check_log[[length(check_log) + 1]] <<- list(check = name, status = status, message = .last_msg)
    .last_msg <<- NULL
  }

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
             check_log[[length(check_log) + 1]] <<- list(check = "load_metadata", status = "fail", message = c(e$message, e$body))
             rlang::abort(c("x" = "You must correct the above issue before the congruence checks can run."), checks = check_log, call = NULL, use_cli_format = TRUE)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("load_metadata")
  cli::cli_h2("Checking metadata compliance")
  tryCatch(test_validate_schema(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             check_log[[length(check_log) + 1]] <<- list(check = "validate_schema", status = "fail", message = c(e$message, e$body))
             rlang::abort(c("x" = "Metadata schema must validate before the rest of the congruence checks can run."), checks = check_log, call = NULL, use_cli_format = TRUE)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("validate_schema")
  tryCatch(test_dup_meta_entries(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             check_log[[length(check_log) + 1]] <<- list(check = "dup_meta_entries", status = "fail", message = c(e$message, e$body))
             rlang::abort(c("x" = "You must remove duplicate data table names from metadata before the rest of the congruence checks can run."), checks = check_log, call = NULL, use_cli_format = TRUE)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("dup_meta_entries")
  tryCatch(test_metadata_version(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("metadata_version")
  tryCatch(test_delimiter(metadata),
           error = function(e) {
             cli::cli_bullets(c(e$message, e$body))
             err_count <<- err_count + 1
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("delimiter")
  tryCatch(test_header_num(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("header_num")
  tryCatch(test_footer(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("footer")
  tryCatch(test_taxonomic_cov(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("taxonomic_cov")
  tryCatch(test_geographic_cov(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("geographic_cov")
  tryCatch(test_content_units(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("content_units")
  tryCatch(test_doi(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("doi")
  tryCatch(test_doi_format(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("doi_format")
  tryCatch(test_datatable_urls(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("datatable_urls")
  tryCatch(test_datatable_urls_doi(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           })
  record_check("datatable_urls_doi")
  tryCatch(test_datatable_url_attributes(metadata),
           error = function(e) {
             err_count <<-err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           })
  record_check("datatable_url_attributes")
  tryCatch(test_publisher(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("publisher")
  tryCatch(test_valid_fieldnames(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("valid_fieldnames")
  tryCatch(test_valid_filenames(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("valid_filenames")
  tryCatch(test_pii_meta_emails(directory),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("pii_meta_emails")

  cli::cli_h2("Checking that metadata contains required elements for DataStore extraction")

  tryCatch(test_creator(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("creator")
  tryCatch(test_pub_date(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("pub_date")
  tryCatch(test_dp_title(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("dp_title")
  tryCatch(test_keywords(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("keywords")
  tryCatch(test_by_for_nps(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("by_for_nps")
  tryCatch(test_publisher_name(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("publisher_name")
  tryCatch(test_publisher_state(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("publisher_state")
  tryCatch(test_publisher_city(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("publisher_city")
  tryCatch(test_dp_abstract(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("dp_abstract")
  tryCatch(test_methods(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("methods")
  tryCatch(test_file_descript(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("file_descript")
  tryCatch(test_access(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("access")
  tryCatch(test_cui_label(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("cui_label")
  tryCatch(test_cui_email(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("cui_email")
  tryCatch(test_cui_designator(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("cui_designator")
  tryCatch(test_license(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("license")
  tryCatch(test_int_rights(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("int_rights")
  tryCatch(test_attribute_defs(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("attribute_defs")
  tryCatch(test_storage_type(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("storage_type")

  cli::cli_h2("Checking additional/optional metadata elements")

  tryCatch(test_orcid_exists(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("orcid_exists")
  tryCatch(test_orcid_format(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("orcid_format")
  tryCatch(test_orcid_resolves(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("orcid_resolves")
  tryCatch(test_orcid_match(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("orcid_match")
  tryCatch(test_notes(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("notes")
  tryCatch(test_project(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             .last_msg <<- c(e$message, e$body)
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
             .last_msg <<- c(w$message, w$body)
           }
  )
  record_check("project")

  if (!check_metadata_only) {
    cli::cli_h2("Checking that metadata is consistent with data file(s)")
    tryCatch(test_file_name_match(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               check_log[[length(check_log) + 1]] <<- list(check = "file_name_match", status = "fail", message = c(e$message, e$body))
               rlang::abort(c("x" = "Files documented in metadata must match files present in package before the rest of the congruence checks can run."), checks = check_log, call = NULL, use_cli_format = TRUE)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
               .last_msg <<- c(w$message, w$body)
             }
    )
    record_check("file_name_match")
    tryCatch(test_fields_match(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               check_log[[length(check_log) + 1]] <<- list(check = "fields_match", status = "fail", message = c(e$message, e$body))
               rlang::abort(c("x" = "Columns documented in metadata must match columns present in data files before the rest of the congruence checks can run."), checks = check_log, call = NULL, use_cli_format = TRUE)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
               .last_msg <<- c(w$message, w$body)
             }
    )
    record_check("fields_match")
    tryCatch(test_missing_data(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               .last_msg <<- c(e$message, e$body)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
               .last_msg <<- c(w$message, w$body)
             }
    )
    record_check("missing_data")
    tryCatch(test_numeric_fields(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               .last_msg <<- c(e$message, e$body)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
               .last_msg <<- c(w$message, w$body)
             }
    )
    record_check("numeric_fields")
    tryCatch(test_dates_parse(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               .last_msg <<- c(e$message, e$body)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
               .last_msg <<- c(w$message, w$body)
             }
    )
    record_check("dates_parse")
    tryCatch(test_date_range(directory, metadata, skip_cols = skip_cols),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               .last_msg <<- c(e$message, e$body)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
               .last_msg <<- c(w$message, w$body)
             }
    )
    record_check("date_range")

    cli::cli_h2("Checking data and metadata compliance")
    tryCatch(test_pii_data_emails(directory),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               .last_msg <<- c(e$message, e$body)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_verbatim(c(w$message, w$body))
               .last_msg <<- c(w$message, w$body)
             }
    )
    record_check("pii_data_emails")
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

  # Return the same named count vector as before, with the structured
  # per-check log attached as an attribute (purely additive - see @return).
  out <- c("errors" = err_count, "warnings" = warn_count)
  attr(out, "checks") <- check_log
  return(invisible(out))
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
