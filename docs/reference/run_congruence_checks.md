# Run all congruence checks

Run all congruence checks

## Usage

``` r
run_congruence_checks(
  directory = here::here(),
  metadata = load_metadata(directory),
  check_metadata_only = FALSE,
  skip_cols = NA,
  output_filename,
  output_dir = here::here()
)
```

## Arguments

- directory:

  the directory where the data file(s) are found (i.e. your data
  package). Defaults to the current working directory. On exit, returns
  to the current working directory.

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

- check_metadata_only:

  Only run checks on the metadata and skip anything involving data
  files.

- skip_cols:

  String. Defaults to NA. A list of one or more columns in the data to
  skip when testing whether the dates within data fall within the dates
  range specified in metadata. Useful if, for instance, there are
  columns within the data associated with the QA/QC process and these
  dates are expected to fall outside the date range specified for the
  data.

- output_filename:

  Optional. If specified, saves results of congruence checks to this
  file. If omitted, prints results to console. If the file already
  exists, results will be appended to the existing file.

- output_dir:

  Location in which to save the output file, if using.

## Value

Invisibly returns `metadata`.

## Examples

``` r
dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
run_congruence_checks(dir)
#> 
#> ── Running all congruence checks ───────────────────────────────────────────────
#> 
#> ── Reading metadata ──
#> 
#> ── Checking metadata compliance ──
#> 
#> ✔ Your metadata is schema valid.
#> ✔ Each data file name is used exactly once in the metadata file.
#> ✔ Your EML version is supported.
#> ✔ Metadata indicates that each data file contains a field delimiter that is a
#>   single character
#> ✔ Metadata indicates that each data file contains exactly one header row.
#> ✔ Metadata indicates data files do not have footers.
#> ✔ Metadata contains taxonomic coverage element.
#> ✔ Metadata contains geographic coverage element
#> ! Metadata does not contain park content unit links., Use
#> `EMLeditor::set_content_units()` to add content unit links.
#> ✔ Metadata contains a digital object identifier, "doi:
#>   https://doi.org/10.57830/2295086".
#> ✔ Metadata Digital Object Identifier appears to be properly formatted.
#> ✔ Metadata contains URLs for all data tables.
#> ✔ Data table URLs are properly formmatted and correspond to the specified DOI.
#> ! One or more of data file URLs elements in metadata lack attributes. Either
#> use `EMLeditor::set_data_urls()` to add the appropriate attribute for DataStore
#> or make sure the URL provided is a direct download link.
#> ✔ Metadata contains publisher element.
#> ✔ Field names begin with a letter and do not contain spaces or special
#>   characters.
#> ✔ File names begin with a letter and do not contain spaces or special
#>   characters.
#> ✔ Metadata does not appear to contain any personal emails.
#> 
#> ── Checking that metadata contains required elements for DataStore extraction ──
#> 
#> ✔ Any individual Creators in metadata have a surNames with less than three
#>   words.
#> ! Publication date, 2022, predates the Data Package Reference Type.
#> ! Data package title appears to contain acronyms. Consider revising to spell
#>   out acronyms. Use `EMLeditor::set_title()` to revise.
#> ✔ Metadata contains keyword(s).
#> ✔ Metadata states data was created by or for NPS.
#> ✔ Metadata indicates the publisher is the National Park Service.
#> ✔ Metadata indicates the publisher state is CO.
#> ✔ Metadata indicates the publisher city is Fort Collins.
#> ! The data package abstract contains non-standard characters: &amp;#13;. Use
#> `EMLeditor::set_abstract()` to revise.
#> ! The metadata methods contains non-standard characters such as \r or
#> &amp;#13;. Use `EMLeditor::set_methods()` to revise.
#> ✔ All dataTables listed in metadata have a unique file description.
#> ! Data file 3 description is greater than 15 words. Consider a more concise
#> description.
#> ! The permissions specified in metadata are deprecated. Please update the
#> permissions using `EMLeditor::set_permissions()`.
#> ✖ Your metadata lacks permissions or contains deprecated CUI information. Use
#>   `EMLeditor::set_permissions()` to update your permissions.
#> ✖ Your metadata lacks permissions or contains deprecated CUI information. Use
#>   `EMLeditor::set_permissions()` to update your permissions.
#> ✖ Your metadata lacks permissions or contains deprecated CUI information. Use
#>   `EMLeditor::set_permissions()` to update your permissions.
#> ✖ Metadata does not contain a license name. Use `EMLeditor::set_int_rights()`
#>   to add a license name.
#> ✔ Metadata contains an Intellectual Rights statement.
#> ✔ All attributes listed in metadata have attribute definitions.
#> ✔ All attributes listed in metadata have a storage type associated with them.
#> ✔ All attribute storage types are valid values.
#> 
#> ── Checking additional/optional metadata elements ──
#> 
#> ✔ All individual creators have associated ORCiDs.
#> ✔ All Creator ORCiDs are properly formatted.
#> ✔ All Creator ORCiDs resolved to a valid ORCiD profile.
#> ✔ All Creator ORCiDs resolve to an ORCiD profile that matches the Creator last
#>   name.
#> ! Metadata does not contain additionalInfo (notes). Use
#> `EMLeditor::set_additional_info()` to add notes.
#> ! No project associated with the metadata. To add a DataStore project, use
#> `EMLeditor::set_project()`.
#> 
#> ── Checking that metadata is consistent with data file(s) ──
#> 
#> ✔ All data files are listed in metadata and all metadata files names refer to
#>   data files.
#> ✔ All columns in data match all columns in metadata.
#> ✖ Undocumented missing data detected. Please document all missing data in
#>   metadata:
#>   ---> Mini_BICY_Veg_Intercept_Cleaned.csv contains missing data without a
#>   corresponding missing data code in metadata.
#>   ---> Mini_BICY_Veg_Transect_Cleaned.csv contains missing data without a
#>   corresponding missing data code in metadata.
#> ✔ Columns indicated as numeric in metadata contain only numeric values and
#>   valid missing value codes.
#> ✔ Metadata and data date formatting is in congruence.
#> ✔ Columns indicated as date/time in metadata are within the stated temporal
#>   coverage range.
#> 
#> ── Checking data and metadata compliance ──
#> 
#> ✔ Data files do not appear to contain any personal emails.
#> 
#> ── Summary ──
#> 
#> ✖ 5 errors to address
#> ! 9 warnings to look into
```
