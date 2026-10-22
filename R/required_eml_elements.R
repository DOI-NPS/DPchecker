#' Test publication date presence and ISO-8601 formatting
#'
#' @description `test_pub_date()` tests for presence and ISO-8601 formatting of the publication date. Also tests whether the publication date is within a reasonable bounds; i.e. not a date prior to the beginning of the data package Reference Type (after 2022) and not in a future year. If the publication date is missing or improperly formatted, the test fails with an error. If the publication date is outside a reasonable range, the test fails with a warning. Otherwiset the test passes.
#'
#' @param metadata The metadata object returned by `load_metadata`. If parameter is not provided, it defaults to calling `load_metadata` in the current project directory.
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata(DPchecker_example("BICY_Veg"))
#' test_pub_date(meta)
#' }
test_pub_date <- function(metadata = load_metadata(directory)) {
  is_eml(metadata) # Throw an error if metadata isn't an emld object
  missing_date <- is.null(metadata$dataset$pubDate)
  # error if no publication date found
  z <- NULL # error tracking
  if (missing_date) {
    cli::cli_abort(c("x" = "Metadata does not contain publication date."))
    z <- "error"
  }
  # if publication date found, check formatting
  if (!missing_date) {
    date <- metadata$dataset$pubDate
    pub_year <- suppressWarnings(as.numeric(substr(date, 1, 4)))
    curr_year <- as.numeric(substr(Sys.Date(), 1, 4))
    dash1 <- substr(date, 5, 5)
    pub_month <- suppressWarnings(as.numeric(substr(date, 6, 7)))
    dash2 <- substr(date, 8, 8)
    pub_day <- suppressWarnings(as.numeric(substr(date, 9, 10)))

    # test for improbable years or non-numerics in year:
    if (is.na(pub_year)) {
      cli::cli_abort(c("x" = "Publication year is not in the correct ISO 8601 format (YYYY-MM-DD)."))
      z <- "error"
    }
    if (!is.na(pub_year)) {
      if (pub_year <= 2022) {
        cli::cli_warn(c("!" = "Publication date, {pub_year}, predates the Data Package Reference Type."))
        z <- "error"
      }
      if (pub_year > curr_year) {
        cli::cli_warn(c("!" = "Publication date, {pub_year}, exceeds the current year."))
        z <- "error"
      }
    }
    if (!identical(dash1, "-")) {
      cli::cli_abort(c(
        "x" =
          "Publication date separator, {dash1}, is not in the correct ISO 8601 format (YYYY-MM-DD)."
      ))
      z <- "error"
    }
    if (is.na(pub_month)) {
      cli::cli_abort(c(
        "x" =
          "Publication month is not in the correct ISO 8601 format (YYY-MM-DD)"
      ))
      z <- "error"
    }
    if (!is.na(pub_month)) {
      if (pub_month > 12) {
        cli::cli_abort(c("x" = "Publication month, {pub_month}, is >12 and is not in the correct ISO 8601 format (YYYY-MM-DD)."))
        z <- "error"
      }
    }
    if (!identical(dash2, "-")) {
      cli::cli_abort(c(
        "x" =
          "Publication date separator, {dash2}, is not in the correct ISO 8601 format (YYYY-MM-DD)."
      ))
      z <- "error"
    }
    if (is.na(pub_day)) {
      cli::cli_abort(c(
        "x" =
          "Publication day is not in the correct ISO 8601 format (YYYY-MM-DD)."
      ))
      z <- "error"
    }
    if (!is.na(pub_day)) {
      if (pub_day > 31) {
        cli::cli_abort(c(
          "x" =
            "Publication day, {pub_day}, is >31 and is not in the correct ISO 8601 format (YYYY-MM-DD)."
        ))
        z <- "error"
      }
    }
    if (is.null(z)) {
      cli::cli_inform(c(
        "v" = "Publication date is present and in the correct ISO 8601 format."
      ))
    }
  }
  return(invisible(metadata))
}

#' Test data package title
#'
#' @description `test_dp_title()` tests EML metadata for presence of a data package title. The test fails with an error if the title is absent or the title is greater than 300 characters (a data.gov requirement). The test fails with a warning if the title is > 20 or < 5 words. Otherwise, the test passes.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_dp_title()
#' }
test_dp_title <- function(metadata = load_metadata(directory)) {
  is_eml(metadata) # Throw an error if metadata isn't an emld object
  title <- metadata$dataset$title
  missing_title <- is.null(title)
  # error if no publication title found
  if (missing_title) {
    cli::cli_abort(c("x" = "Metadata does not contain a data package title. Use {.fn EMLeditor::set_title} to add one."))
  }
  # if title present, check certain parameters:
  if (!missing_title) {
    if (nchar(title) > 300) {
      cli::cli_abort(c("x" = "Titles must be less than 300 Characters. Please revise your title using {.fn EMLeditor::set_title}."))
    }
    x <- sapply(strsplit(title, " "), length)
    if (x < 5) {
      cli::cli_inform(c(
        "!" = "Data package title is <5 words. Consider a more informative title. Use {.fn EMLeditor::set_title} to revise."
      ))
    }
    is_upper <- stringr::str_detect(strsplit(title,
                                          " ")[[1]],
                                                "^[[:upper:]]+$")
    if (any(is_upper == TRUE)) {
      cli::cli_inform(c(
        "!" = "Data package title appears to contain acronyms. Consider revising to spell out acronyms. Use {.fn EMLeditor::set_title} to revise."
      ))
    } else {
      cli::cli_inform(c(
        "v" = "Data package title is present in metadata."
      ))
    }
  }
  return(invisible(metadata))
}

#' Test for "by or for NPS"
#'
#' @details `test_by_for_nps()` test for presence of the "by or for NPS" field. The test fails with an error if the information is missing from metadata. The test fails with a warning if the metadata indicate that the data were not created by or for the NPS (as this is expected to be relatively rare). Otherwise the test passes.
#'
#' @inheritParams test_pub_date
#' @return invisibly returns `metadata`
#'
#' @examples
#' \dontrun{
#' test_by_for_nps()
#' }
test_by_for_nps <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  nps <- EMLeditor::get_eml_simple(metadata, "byOrForNPS")
  missing_nps <- is.null(nps)
  # error if no by or for nps
  if (missing_nps) {
    cli::cli_abort(c("x" = "Metadata does not contain information about whether the data package was created by or for the NPS.  Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if (!missing_nps) {
    if (nps == "TRUE") {
      cli::cli_inform(c(
        "v" = "Metadata states data was created by or for NPS."
      ))
    } else if (nps == "FALSE") {
      cli::cli_warn(c(
        "!" = "Metadata states data was NOT created by or for NPS. Are you sure this is correct?  Use {.fn EMLeditor::set_publisher} to revise."
      ))
    } else if (nps == "NULL") {
      cli::cli_abort(c("x" = 'Metadata indicates "by or for NPS" field set to NULL.  Use {.fn EMLeditor::set_publisher} to revise..'))
    }
  }
  return(invisible(metadata))
}


#' Tests EML metadata for the publisher name
#'
#' @details `test_publisher_name()` test for the presence of the data package publisher name. It fails with an error if the publisher name is missing, and fails with a warning if the publisher name is not "National Park Service" (as this is expected to be rare). Passes test if publisher name is "National Park Service"
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#'
#' @examples
#' \dontrun{
#' test_publisher_name()
#' }
test_publisher_name <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  publisher_name <- metadata$dataset$publisher$organizationName
  missing_name <- is.null(publisher_name)
  if (missing_name) {
    cli::cli_abort(c("x" = "Metadata does not contain the publisher name. Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if (!missing_name) {
    if (publisher_name == "National Park Service") {
      cli::cli_inform(c(
        "v" = "Metadata indicates the publisher is the National Park Service."
      ))
    } else if (publisher_name != "National Park Service") {
      cli::cli_warn(c("!" = "Metadata indicates the publisher is {
                      crayon::bold$red(publisher_name)},
                      not the National Park Service. Are you sure?
                      Use {.fn EMLeditor::set_publisher} to revise."))
    }
  }
  return(invisible(metadata))
}


#' Test EML metadata for publisher state
#'
#' @description `test_publisher_state()` inspects the publisher address in EML metadata. The test faisl with an error if the administrativeArea (state) element is empty. The test fails with a warning if it is not "CO" (again, because this is expected to be exceedingly rare). The test passes if it is "CO".
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_publisher_state()
#' }
test_publisher_state <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  pub_state <- metadata$dataset$publisher$address$administrativeArea
  missing_state <- is.null(pub_state)
  if (missing_state) {
    cli::cli_abort(c("x" = "Metadata does not contain the publisher state. Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if (!missing_state) {
    if (pub_state == "CO") {
      cli::cli_inform(c("v" = "Metadata indicates the publisher state is CO."))
    } else if (pub_state != "CO") {
      cli::cli_warn(c("!" = "Metadata indicates the publisher state, {
                      crayon::bold$red(pub_state)}, is not CO. Are you sure?
                       Use {.fn EMLeditor::set_publisher} to revise."))
    }
  }
  return(invisible(metadata))
}

#' Test EML metadata for publisher city
#'
#' @description `test_publisher_city()` inspects the publisher address in EML metadata. It fails with an an error if the city element is empty. It fails with a warning if it is not "Fort Collins". The test passes if it is "Fort Collins"
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_publisher_city()
#' }
test_publisher_city <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  pub_city <- metadata$dataset$publisher$address$city
  missing_city <- is.null(pub_city)
  if (missing_city) {
    cli::cli_abort(c("x" = "Metadata does not contain the publisher city.
                     Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if (!missing_city) {
    if (pub_city == "Fort Collins") {
      cli::cli_inform(c(
        "v" = "Metadata indicates the publisher city is Fort Collins."
      ))
    }
    if (pub_city != "Fort Collins") {
      cli::cli_warn(c("!" = "Metadata indicates the publisher city,
                      {crayon::bold$red(pub_city)}, is not Fort Collins.
                       Use {.fn EMLeditor::set_publisher} to revise."))
    }
  }
  return(invisible(metadata))
}


#' Test EML abstract
#'
#' @description `test_dp_abstract()` inspects EML for presence of a data package abstract. The test Fails with an error if the abstract is absent or greater than 10,000 characters (required for data.gov). If the abstract is present, the test fails with a warning if the abstract is <20 words, >250 words, or contains a subset of common characters that indicate improper formatting. Otherwise the test passes.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_dp_abstract()
#' }
test_dp_abstract <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  abstract <- metadata$dataset$abstract
  missing_abstract <- is.null(abstract)
  if (missing_abstract) {
    cli::cli_abort(c("x" = "Metadata does not contain an abstract for the data package.
                     Use {.fn EMLeditor::set_abstract} to add one."))
  }
  if (!missing_abstract) {
    # get rid of <para> tags, empty paragraphs, etc
    abstract <- unlist(abstract)
    abs <- NULL
    for (i in seq_along(abstract)) {
      if (nchar(abstract[i]) > 0) {
        abs <- append(abs, abstract[[i]])
      }
    }
   if (nchar(abs) > 10000) {
     cli::cli_abort(c(
      "x" = "The abstract must not exceed 10,000 characters.
      Please revise using {.fn EMLeditor::set_abstract}."))
   }
    x <- sapply(stringr::str_count(abs, "\\w+"), sum)
    # x <- sapply(strsplit(abs, "" ), length)
    x <- sum(x) # sums up all words across potential multiple paragraphs
    z <- NULL # tracking for warnings
    if (x < 20) {
      cli::cli_warn(c(
        "!" = "The data package abstract is less than 20 words.
        Are you sure this is informative enough?
        Use {.fn EMLeditor::set_abstract} to revise."
      ))
      z <- "warn"
    }
    if (x > 250) {
      cli::cli_warn(c(
        "!" = "The data package abstract is longer than 250 words.
        Consider using {.fn EMLeditor::set_abstract} and
        revising for a more concise abstract."
      ))
      z <- "warn"
    }
    if (sum(stringr::str_detect(abs, "&amp;#13;")) > 0) {
      cli::cli_warn(c(
        "!" = "The data package abstract contains non-standard characters: {
                      crayon::bold$red('&amp;#13;')}.
        Use {.fn EMLeditor::set_abstract} to revise."
      ))
      z <- "warn"
    }
    if (sum(grepl("[\r\n]", abs)) > 0) {
      cli::cli_warn(c("!" = "The data package abstract contains non-standard end of line characters such as \\r, \\n, or \\n\\r.
                      Use {.fn EMLeditor::set_abstract} to revise."))
      z <- "warn"
    }
    if (sum(grepl("   ", abs)) > 0) {
      cli::cli_warn(c(
        "!" = "The data package abstract contains extra (more than two in a row) spaces.
        Use {.fn EMLeditor::set_abstract} to revise."
      ))
      z <- "warn"
    }
    if (length(seq_along(abs)) > 3) {
      cli::cli_warn(c(
        "!" = "The data package abstract contains more than three paragraphs.
        Consider using {.fn EMLeditor::set_abstract} to revise."
      ))
    }
    # if no other warnings generated:
    if (is.null(z)) {
      cli::cli_inform(c(
        "v" = "The Metadata contains a well formatted abstract for the data package."
      ))
    }
  }
  return(invisible(metadata))
}

#' Test presence of file descriptions
#'
#' @description `test_file_descript()` tests for the presence of file descriptions (entityDescription) fields. It fails with an error if any one or all of the entityDescription fields are empty in a dataTable. It fails with an error if any two or more file descriptions are identical. The test fails with a warning if for each file description that is longer than 15 words or shorter than three words. Otherwise the test passes.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_file_descript()
#' }
test_file_descript <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  data_tbl$`@context` <- NULL
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }
  metadata_file_desc <- NULL
  for (i in seq_along(data_tbl)) {
    metadata_file_desc <- append(
      metadata_file_desc,
      data_tbl[[i]][["entityDescription"]]
    )
  }
  # if no file descriptions at all:
  if (is.null(metadata_file_desc)) {
    cli::cli_abort(c("x" = "Metadata does not contain data file descriptions (<entityDescription>)."))
  }
  if (!is.null(metadata_file_desc)) {
    # if missing individual file descriptions:
    z <- NULL # warn counter
    for (i in seq_along(metadata_file_desc)) {
      if (is.null(metadata_file_desc[i])) {
        cli::cli_abort(c("x" = "Metadata lacks a file description for file number {i}."))
        z <- "error"
      }
    }
    # if all dataTables have file descriptions (note: does not check against the
    # actual number of .csvs in the data package, for that see:
    # `test_file_name_match()`)
    if (is.null(z)) {
      # find duplicates:
      dups <- metadata_file_desc[duplicated(metadata_file_desc)]
      if (length(dups) > 0) {
        cli::cli_abort(c("x" = "Metadata file description check failed. Some descriptions are used multiple times to describe different files."))
      } else if (length(dups) == 0) {
        cli::cli_inform(c("v" = "All dataTables listed in metadata have a unique file description."))
      }
      for (i in seq_along(metadata_file_desc)) {
        x <- stringr::str_count(metadata_file_desc[i], "\\w+")
        if (x < 3) {
          cli::cli_warn(c(
            "!" = "Data file {i} description is less than 3 words long. Consider a more informative descrption."
          ))
        }
        if (x > 15) {
          cli::cli_warn(c("!" = "Data file {i} description is greater than 15 words. Consider a more concise description."))
        }
      }
    }
  }
}


#' Test for CUI dissemination code
#'
#' @description `test_access()` examines EML metadata for the presence of
#' a access information in additonalMetadata. The function fails with an
#' error if access information does not exist. The function passes with a
#' warning if the access information exists but is deprecated. The
#' function fails with an error if the information exists but is invalid.
#' The function passes with a warning if the access is correct but not public.
#' The function passes without a warning if a current, valid public access
#' statement exists.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' test_access()
#' }
test_access <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  valid_codes <- c("PUBLIC", "PUBVER", "NOCON",
                   "DL ONLY", "FEDCON", "FED ONLY")
  valid_access <- c("PUBLIC", "INTERNAL", "RESTRICTED")
  # get all additionalMetadata elelements and all children elements
  diss_code <- EMLeditor::get_eml_simple(metadata, "CUI")
  access_code <- EMLeditor::get_eml_simple(metadata, "accessLevel")


  if (is.null(diss_code) && is.null(access_code)) {
    cli::cli_abort(c("x" = paste0("Metadata does not contain permissions ",
                                  "information. Use ",
                                  "{.fn EMLeditor::set_permissions}.")))
  }
  # check for legacy CUI:
  if (!is.null(diss_code)) {
    if (diss_code %in% valid_codes) {
      cli::cli_warn(c("!" = paste0("The permissions specified in metadata are",
                                   " deprecated. Please update the permissions",
                                   " using {.fn EMLeditor::set_permissions}.")))
      return(invisible(NULL))
    }
  }
  # test current permissions:
  if (!is.null(access_code)) {
    if (access_code %in% valid_access) {
        cli::cli_inform(c("v" = paste0("Metadata contains the valid accesss ",
                                       "code {access_code}.")))
    }
    if (access_code != "PUBLIC") {
        cli::cli_warn(c("!" = paste0("Metadata access code ",
                                     "{access_code} indicates the data ",
                                     "package is NOT public. Are you sure?")))
    }
  }
  return(invisible(NULL))
}

#' Test for CUI label
#'
#' Fails if there is no permissions information or the permissions information
#' is deprecated. Passes if the permissions information is public (label not
#' required). Fails if permissions are not set to public and an CUI label is
#' absent. Fails if the permissions are not set to public and the CUI label is
#' invalid. Passes if the permissions are not set to public but a valid CUI
#' label exists.
#'
#' @inheritParams test_pub_date
#'
#' @returns NULL(invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' test_cui_label()
#' }
test_cui_label <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)

  dist <- EMLeditor::get_eml_simple(metadata, "distribution")

  if (is.null(dist)) {
    cli::cli_abort(c(x = paste0("Your metadata lacks permissions information. ",
                                "Use {.fn EMLeditor::set_permissions} to add ",
                                "permissions information.")))
  }

  if (!"accessLevel" %in% names((dist))) {
    cli::cli_abort(c(x = paste0("Your metadata lacks permissions or contains",
                                " deprecated CUI information. Use {.fn ",
                                "EMLeditor::set_permissions} to update your",
                                " permissions.")))
  }
  if ("accessLevel" %in% names(dist)) {
    if(dist[["accessLevel"]] == "PUBLIC") {
      cli::cli_inform(c("v" = paste0("Permissions are set to PUBLIC. A CUI ",
                                     "label is not required.")))
      return(invisible(NULL))
    } else {
      label <- dist[["label"]]
      if (is.null(label)) {
        cli::cli_abort(c("x" = paste0("Your permissions are not set to public.",
                                      " This requires a CUI label. Use {.fn ",
                                      "EMLeditor::set_permissions to add a ",
                                      "CUI label.")))
      }
      if (!is.null(label)) {
        legal <- NPSdatastore::get_legal_authority()
        if (!(label %in% legal$label)) {
          cli::cli_abort(c("x" = paste0("Your permissions label is not valid.",
                                        " use {.fn EMLeditor::set_permissions}",
                                        "to set a valid permissions label.")))
        } else {
          cli::cli_inform(c("v" = paste0("Your permissions label is valid.")))
        }
      }
    }
  }
}

#' Tests for presence of a email in permissions/cui designation
#'
#' This function
#'
#' @inheritParams test_pub_date
#'
#' @returns null (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' test_cui_email()
#' }
test_cui_email <- function(metadata = load_metadata(directory))  {
  is_eml(metadata)

  dist <- EMLeditor::get_eml_simple(metadata, "distribution")

  if (is.null(dist)) {
    cli::cli_abort(c(x = paste0("Your metadata lacks permissions information. ",
                                "Use {.fn EMLeditor::set_permissions} to add ",
                                "permissions information.")))
  }

  if (!"accessLevel" %in% names((dist))) {
    cli::cli_abort(c(x = paste0("Your metadata lacks permissions or contains",
                                " deprecated CUI information. Use {.fn ",
                                "EMLeditor::set_permissions} to update your",
                                " permissions.")))
  }
  if ("accessLevel" %in% names(dist)) {
    if(dist[["accessLevel"]] == "PUBLIC") {
      cli::cli_inform(c("v" = paste0("Permissions are set to PUBLIC. An email ",
                                     "associated with the permissions is not ",
                                     "required.")))
      return(invisible(NULL))
    } else {
      contact <- dist[["contactEmail"]]
      if (is.null(contact)) {
        cli::cli_abort(c("x" = paste0("Your permissions are not set to public.",
                                      " This requires a contact email. Use ",
                                      "{.fn EMLeditor::set_permissions to add ",
                                      "a contact email.")))
      } else {
        if (grepl('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
                  contact)) {
          cli::cli_inform(c("v" = paste0("Your permissions contain an valid-",
                                         "looking email address, {(EMLeditor",
                                         ":::.strong_good(contact))}.")))
          return(invisible(NULL))
        } else {
          cli::cli_abort(c("x" = paste0("Your permissions contain an invalid ",
                                        "looking email address, {contact}. ",
                                        "use {.fn EMLeditor::set_permissions}",
                                        "to supply a valid email address.")))
          return(invisible(NULL))
        }
      }
    }
  }
}

test_cui_designator <- function(metadata = load_metadata(directory))  {
  is_eml(metadata)

  dist <- EMLeditor::get_eml_simple(metadata, "distribution")

  if (is.null(dist)) {
    cli::cli_abort(c(x = paste0("Your metadata lacks permissions information. ",
                                "Use {.fn EMLeditor::set_permissions} to add ",
                                "permissions information.")))
  }

  if (!"accessLevel" %in% names((dist))) {
    cli::cli_abort(c(x = paste0("Your metadata lacks permissions or contains",
                                " deprecated CUI information. Use {.fn ",
                                "EMLeditor::set_permissions} to update your",
                                " permissions.")))
  }
  if ("accessLevel" %in% names(dist)) {
    if(dist[["accessLevel"]] == "PUBLIC") {
      cli::cli_inform(c("v" = paste0("Permissions are set to PUBLIC. A ",
                                     "designator for the authority to withhold",
                                     " is not required.")))
      return(invisible(NULL))
    } else {
      authority <- dist[["authorityDesignator"]]
      if (is.null(authority)) {
        cli::cli_abort(c("x" = paste0("Your permissions are not set to public.",
                                      " This requires designating a specific ",
                                      "person (\"authority designator\") who ",
                                      "is responsible for designating the data",
                                      " non-public. Use ",
                                      "{.fn EMLeditor::set_permissions to add ",
                                      "a person's name who has determined that",
                                      "the data contain information that ",
                                      "warrants witholding it from the ",
                                      "public.")))
      } else {
        cli::cli_inform(c("v" = paste0("Your permissions contain a ",
                                      "designated authority who is responsible",
                                      " for having determined that the data ",
                                      "should be withheld from the public. ",
                                      "This person is: ",
                                      "{(EMLeditor:::.strong_good(authority))}."
                                      )))
        return(invisible(NULL))
      }
    }
  }
}

#' Test for presence of a license name
#'
#' @description `test_license()` examines the licenseName element of EML metadata. If there is no license name, the test fails with ab error. If the license name does not match a list of valid license names, the test fails. If the metadata contain a valid license name, but the license name and CUI dissemination code do not agree, the test fails with an error. Otherwise, the test passes.  Additionally, if the license name is not "Public Domain" or "Creative Commons Zero v1.0 Universal", the function will produce a warning that the data package is not public.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_license()
#' }
test_license <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  license_list <- c(
    "Public Domain",
    "Creative Commons Zero v1.0 Universal",
    "Unlicensed (not for public dissemination)"
  )

  license <- metadata$dataset$licensed$licenseName
  if (is.null(license)) {
    cli::cli_abort(c("x" = paste0("Metadata does not contain a license name. ",
                                  "Use {.fn EMLeditor::set_int_rights} to add ",
                                  "a license name.")))
  }
  if (!is.null(license)) {
    if (!license %in% license_list) {
      cli::cli_abort(c("x" = paste0("The metadata does not contain a valid ",
                                    "license name. Use {.fn EMLeditor::",
                                    "set_int_rights} to add a valid license ",
                                    "name.")))
    }
    if (license %in% license_list) {
      #legacy CUI information:
      diss_code <- EMLeditor::get_eml_simple(metadata, "CUI")
      if (!is.null(diss_code)) {
        cli::cli_abort(c("x" = paste0("Metadata contains deprecated ",
                                       "permissions statement that cannot be ",
                                       "matched to an existing license. ",
                                       "use {.fn EMLeditor::set_permissions} ",
                                       "to update your permissions and then ",
                                       "use {.fn EMLeditor::set_int_rights} ",
                                       "to update corresponding intellectual ",
                                       "rights and license names.")))
        return(invisible(metadata))
      }
      #missing permissions cannot be matched to a license
      access_level <- EMLeditor::get_eml_simple(metadata, "accessLevel")
      if (is.null(access_level)) {
        cli::cli_abort(c("x" = paste0("Metadata does not contain permissions",
                                       "to match to a license. Use {.fn ",
                                       "EMLeditor::set_permissions} to add ",
                                       "permissions and then {.fn ",
                                       "EMLeditor::set_int_rights} to add ",
                                       "a corresponding intellectural rights ",
                                       "statement and license name.")))
        return(invisible(metadata))
      }
      if (access_level == "PUBLIC" &&
          license == "No License/Controlled Unclassified Information") {
        cli::cli_abort(c("x" = paste0("Metadata license and access level ",
                                      "do not agree. Use {.fn EMLeditor",
                                      "::set_int_rights} and/or {.fn EMLeditor",
                                      "::set_permissions} to update your ",
                                      "access level and/or metadata license.")))
        return(invisible(metadata))
      } else if (access_level != "PUBLIC" &&
                 license != "Unlicensed (not for public dissemination)") {
        cli::cli_abort(c("x" = paste0("Metadata license and access level do ",
                                      "not agree. Use {.fn EMLeditor::",
                                      "set_int_rights} and/or {.fn EMLeditor::",
                                      "set_permissions} to update your access ",
                                      "level and/or metadata license.")))
      }
    }
    if (access_level == "PUBLIC" && (license == "Creative Commons Zero v1.0 Universal" | license == "Public Domain")) {
        cli::cli_inform(c("v" = paste0("Metadata contains a valid license ",
                                       "and license name that match the ",
                                       "access level.")))
      return(invisible(metadata))
    }
    if (license == "Unlicensed (not for public dissemination)") {
      cli::cli_inform(c("v" = paste0("Metadata contains a valid license ",
                                     "and license name that match the ",
                                     "access level.")))
      cli::cli_warn(c("!" = paste0("Metadata license name indicates that ",
                                   "the data package is NOT public. Are you ",
                                   "sure?")))
    }
  }
  return(invisible(metadata))
}

#' Test for presence of Intellectual Rights
#'
#' @description `test_int_rights()` tests for the presence of text within the intellectualRights element in EML formatted metadata. If text if present, the test passes. Otherwise, the test fails. `test_int_rights()` makes no attempt to parse the text or test whether it properly coincides with the CUI dissemination codes or licenseName in the metadata. This is a simple presence/absence test.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_int_rights()
#' }
test_int_rights <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  int_rights <- metadata$dataset$intellectualRights
  if (is.null(int_rights)) {
    cli::cli_abort(c("x" = paste0("Metadata lacks an Intellectual Rights ",
                                  "statemet. Use {.fn EMLeditor::",
                                  "set_int_rights}.")))
  } else {
    cli::cli_inform(c(
      "v" = "Metadata contains an Intellectual Rights statement."
    ))
  }
  return(invisible(metadata))
}

#' Test metadata for attribute definitions
#'
#' @description `test_attribute_defs()` extracts all attributeNames and attributeDefinitions from EML metadata. It tests to make sure there are the same number of attributeNames and attributeDefinitions. If true, the test passes. If not, it fails with an error. This test ONLY looks at the metadata, it does NOT look at the data files. Passing this test does not mean that all data columns have attributes in the metadata associated with them. To test that, see `test_fields_match()`.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_attribute_defs()
#' }
test_attribute_defs <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get list of columns for each table in the metadata
  metadata_attrs <- lapply(
    data_tbl,
    function(tbl) {
      EMLeditor::get_eml_simple(tbl, "attributeName")
    }
  )
  metadata_attrs$`@context` <- NULL
  # get attribute definitions:
  attr_defs <- lapply(
    data_tbl,
    function(tbl) {
      list(EMLeditor::get_eml_simple(
        tbl,
        "attributeDefinition"
      ))
    }
  )
  attr_defs$`@context` <- NULL
  # unlist:
  metadata_attrs <- unlist(metadata_attrs)
  attr_defs <- unlist(attr_defs)
  # comparisons:
  if (identical(seq_along(metadata_attrs), seq_along(attr_defs))) {
    cli::cli_inform(c(
      "v" = "All attributes listed in metadata have attribute definitions."
    ))
  }
  if (!identical(seq_along(metadata_attrs), seq_along(attr_defs))) {
    cli::cli_abort(c(
      "x" = "Some metadata attributes are missing definitions (or vice versa)."
    ))
  }
  return(invisible(metadata))
}

#' Tests for attribute storage type
#'
#' @description `test_storage_type()` checks to see if there are the same number of attributes (attributeName) and storageTypes in the metadata. Equal numbers of elements will pass; unequal numbers will fail the test with an error. `test_storage_type()` does NOT attempt to verify if the number of storageType elements matches the number of columns in the data package data files (for that functionality, use `test_fields_match()`).
#'
#' `test_storage_type()` also verifies that the storageType is valid; i.e. is a member of an accepted list of possible storage types. Currently these are: string, float, date, factor, dateTime, or characters. Validity for this test is based solely on observed ezEML/EAL output (in theory any string in storageType is schema-valid). Invalid storageTypes will result in a warning.
#'
#' `test_storage_type()` does NOT attempt to verify that the value in storageType logically matches the type data in the corresponding column.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_storage_type()
#' }
test_storage_type <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get list of columns for each table in the metadata
  metadata_attrs <- lapply(
    data_tbl,
    function(tbl) {
      EMLeditor::get_eml_simple(
        tbl,
        "attributeName"
      )
    }
  )
  metadata_attrs$`@context` <- NULL
  metadata_attrs <- unlist(metadata_attrs)
  # get attribute storage types:
  attr_storage_type <- lapply(
    data_tbl,
    function(tbl) {
      list(EMLeditor::get_eml_simple(tbl, "storageType"))
    }
  )
  # if ezEML with multiple .csv (remove typeSystem="XML Schema Datatypes"):
  if (sum(grepl("XML Schema Datatype", attr_storage_type)) > 1) {
    attr_storage_type <- attr_storage_type[-length(seq_along(attr_storage_type))]
    attr_storage_type <- unlist(attr_storage_type)
    attr_storage_type <- attr_storage_type[names(attr_storage_type) %in% c("")]
  }
  # if ezEML with only one .csv (remove typeSystem= "XML Schema Datatypes":
  else if (sum(grepl("XML Schema Datatype", attr_storage_type)) == 1) {
    attr_storage_type <- unlist(attr_storage_type)
    attr_storage_type <- attr_storage_type[names(attr_storage_type) %in% c("")]
  }
  # else not an ezEML product:
  else {
    attr_storage_type$`@context` <- NULL
    attr_storage_type <- unlist(attr_storage_type)
  }

  # comparisons:
  if (identical(seq_along(metadata_attrs), seq_along(attr_storage_type))) {
    cli::cli_inform(c("v" = "All attributes listed in metadata have a storage type associated with them."))
  } else {
    cli::cli_abort(c("x" = "Metadata attribute and storage type mis-match: attributes must have exactly one storage type."))
  }
  attr_storage_list <- c(
    "string",
    "float",
    "date",
    "factor",
    "character",
    "dateTime",
    "integer"
  )
  if (sum(!attr_storage_type %in% attr_storage_list) > 0) {
    cli::cli_warn(c("!" = "Some attribute storage types are not accepted values."))
  } else {
    cli::cli_inform(c("v" = "All attribute storage types are valid values."))
  }
  return(invisible(metadata))
}

#' Test for presence of creator and surName
#'
#' @description `test_creator()` examines the Creator element in metadata. If the creator element is missing, the test fails with an error. If the creator element is present, the function looks for individual creators. If individual creators are present and any of them lacks a surName, the test fails with an error. If any individual creators have a surName with more than two words, the test fails with a warning. Otherwise, the test passes.
#'
#' @inheritParams test_pub_date
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#' \dontrun{
#' test_creator()
#' }
test_creator <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  # get creators
  creator <- metadata[["dataset"]][["creator"]]

  if (is.null(creator)) {
    cli::cli_abort(c("x" = "Metadata lacks a Creator. Use EMLassemblyline to add individuals as creators or use {.fn EMLeditor::set_creator_orgs} to add an organization as a creator."))
    return(invisible(metadata))
  }

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if (sum(names_list %in% names(creator)) > 0) {
    creator <- list(creator)
  }

  # get surNames from individual creators:
  surName <- NULL
  for (i in seq_along(creator)) {
    if ("individualName" %in% names(creator[[i]])) {
      # check for orcid directory id:
      last_name <- creator[[i]][["individualName"]][["surName"]]
      # if any surName is missing, fail with an error:
      if (is.null(last_name)) {
        cli::cli_abort(c("x" = "An individual Creator in metadata lacks a surName. Use EMLassemblyline to insure all individual creators have surNames.\n"))
        return(metadata(invisible))
      } else {
        surName <- append(surName, last_name)
      }
    }
  }

  for (i in seq_along(surName)) {
    word_count <- stringr::str_count(surName[i], "\\W+") + 1
    if (word_count > 2) {
      cli::cli_warn(c("!" = "At least one Individual Creator in metadata has a surName with more than two words. Could this be mistake? Use {.fn EMLeditor::set_creator_orgs} to include an organization as a creator and {.fn EMLeditor::set_creator_order} to re-order or remove creators."))
      return(invisible(metadata))
    }
  }

  cli::cli_inform(c("v" = "Any individual Creators in metadata have a surNames with less than three words."))

  return(invisible(metadata))
}

#' Test for Keywords
#'
#' @description `test_keywords()` tests to see whether metadata contains at least one "Keywords Set". Fails with error if any keyword is longer than 150 characters (data.gov requirement).
#'
#' @inheritParams test_pub_date
#'
#' @return invisible(metadatda)
#' @export
#'
#' @examples
#' \dontrun{
#' test_keywords()
#' }
test_keywords <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  # get creators
  keywords <- metadata[["dataset"]][["keywordSet"]]
  if (is.null(keywords)) {
    cli::cli_abort(c("x" = "No keywords detected. Metadata must contain at least one keyword."))
  } else {
    if (any(nchar(unlist(keywords)) > 150)) {
      cli::cli_abort(c("x" = "Each keyword cannot exceed 150 characters."))
    }
    cli::cli_inform(c("v" = "Metadata contains keyword(s)."))
  }
}
