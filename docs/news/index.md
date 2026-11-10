# Changelog

## DPchecker 1.2.1

### 2026-10-26

- update `run_congruence_checks` to output appropriate content for
  breezeml shiny app

## DPchecker 1.2.0

### 2026-10-16

- remove `test_permissions`
- add `test_access`, `test_cui_label`, `test_cui_email`, and
  `test_cui_designator`

### 2026-10-06

- remove `test_cui_dissemination` and update with `test_permissions`
- add unit tests for `set_permissions`
- add set_permissions to `run_congruence_checks` list

### 2026-04-22

- Documentation (URLs mostly) and metadata (code.json added) updates in
  preparation for migration to DGEC.

### 2025-07-11

- Fix bug in
  [`test_datatable_urls_doi()`](../reference/test_datatable_urls_doi.md)
  and
  [`test_datatable_url_attributes()`](../reference/test_datatable_url_attributes.md)
- Fix bug in [`test_dp_abstract()`](../reference/test_dp_abstract.md)

## DPchecker 1.1.0

### 2025-05-12

- Add links to Bug Reports (issues) and Source code to github.io main
  page \## 2025-04-23
- Remove `test_public_points()` function. DataStore previously made GPS
  coordinates public, even when data files were restricted-access, so
  this function was written to alert users of that fact. DataStore now
  applies the same access rules to GPS coordinates and data files, so
  this check is no longer needed.

## DPchecker 1.0.1

### 2025-04-24

- add function `test_datatable_url_attributes` to test for the
  appropriate attribute in the xml designation in metadata for urls.
- Added unit tests for `test_datatable_url_attributes`,
  `test_datatable_urls_doi`, and `test_datatable_urls`

### 2025-04-16

- remove arcticdatautils as a dependency and replace functionality with
  equivalent functions from EMLeditor to reduce the total number of
  package dependencies.

### 2025-04-01

- fix bug that caused `test_datatable_urls_doi` to produce unexpected
  errors when checking metadata generated from ezEML.

### 2025-03-25

- fix bug that caused several functions to fail to detect certain .csv
  files

### 2025-03-10

- updated license to MIT, which works for JOSS, NPS, and R! \##
  2025-03-10
- Updated license to OSI-approved “Zero-Clause BSD” in support of JOSS
  submission.
- update R-CMD-check.yml to include error-on: ‘“error”’. This should
  allow R CMD CHECK to pass when there are warnings/notes and only fail
  on actual errors.

### 2025-03-07

- Add [`test_content_units()`](../reference/test_content_units.md)
  function to test for the presence of NPS content unit links. Add
  [`test_content_units()`](../reference/test_content_units.md) function
  to list of functions run by
  [`run_congruence_checks()`](../reference/run_congruence_checks.md).
  Add unit tests for
  [`test_content_units()`](../reference/test_content_units.md). Add
  documentation about
  [`test_content_units()`](../reference/test_content_units.md) to the
  Articles.
- Add EMLeditor as a dependency to support unit tests for
  [`test_content_units()`](../reference/test_content_units.md). \##
  2025-02-25
- Update `CONTRIBUTING.md` \## 2025-02-22
- Add `CONTRIBUTING.md` file.

### 2025-02-07

- Bug fix for test_int_rights: update CC0 license name text to match
  EMLeditor’s input and DataStore backend.

## DPchecker 1.0.0

### 2024-11-05

- Bugfix for [`test_storage_type()`](../reference/test_storage_type.md)
  data packages built in ezEML that have only 1 .csv file.
- Add [`test_project()`](../reference/test_project.md) function to test
  for DataStore projects.
- Add unit tests for all optional eml elements
- Update documentation to reflect new
  [`test_project()`](../reference/test_project.md) function.

## DPchecker 0.3.4

2024-07-29 \* Update license to CC0 \* Bug fix from
[`test_missing_data()`](../reference/test_missing_data.md)

2024-06-24 \* Fixed bug in
[`test_missing_data()`](../reference/test_missing_data.md) where if the
order of files listed in metadata did not match the order of files
produced by [`list.files()`](https://rdrr.io/r/base/list.files.html) the
function would evaluate the wrong file and produce inadvertent and
unhelpful errors. 2024-02-05 \* Fix bug in
[`test_date_range()`](../reference/test_date_range.md) that was adding
UTC to temporalCoverage \*
[`test_missing_data()`](../reference/test_missing_data.md) now also
handles the missing data codes “blank” and “empty”. \* Update
[`test_missing_data()`](../reference/test_missing_data.md) to default to
flag whole files, not each column that has undocumented missing data.
This condenses the error output when running
[`run_congruence_checks()`](../reference/run_congruence_checks.md). When
trouble shooting and attempting to pinpoint data that lack missing
values, [`test_missing_data()`](../reference/test_missing_data.md) can
be run with the parameter detail_level = “columns”. 2024-01-26 \*
Bugfixes for [`test_dates_parse()`](../reference/test_dates_parse.md)
and [`test_date_range()`](../reference/test_date_range.md): now ignore
files that have times but no dates or date times. \* Bugfixes for
[`test_valid_fieldnames()`](../reference/test_valid_fieldnames.md),
[`test_valid_filenames()`](../reference/test_valid_filenames.md),
[`test_numeric_fields()`](../reference/test_numeric_fields.md),
[`test_dates_parse()`](../reference/test_dates_parse.md), and
[`test_date_range()`](../reference/test_date_range.md) - all the same
bug; must be something deep in a dependency chain changed. \* Bugfix
attempt for [`test_fields_match()`](../reference/test_fields_match.md)
reportedly needs more testing \* Add function
[`test_missing_data()`](../reference/test_missing_data.md) which scans
data for NAs not documented in metadata

## DPchecker 0.3.3

- Bug fixes for [`test_date_range()`](../reference/test_date_range.md)
  and [`test_dates_parse()`](../reference/test_dates_parse.md).
- Adjusted
  [`test_datatable_urls()`](../reference/test_datatable_urls.md) and
  [`test_datatable_urls_doi()`](../reference/test_datatable_urls_doi.md)
  so that they work properly when there are no data table urls present
  in metadata.
- Move convert_datetime_format to QCkit; add QCkit as re-export to
  DPchecker
- Updated tabular_data_congruence.R for speed and stability as per
  codefactor suggests

## DPchecker 0.3.2

- Adjusted [`test_date_range()`](../reference/test_date_range.md) so
  that it can handle data columns that contain both dates and times.
  Times are truncated to midnight such that if the data collected on the
  first day indicated is considered “in range” and data collected on the
  last day indicated is considered “in range”.
- Added `test_public_points()` to the list of functions in the
  DPchecker.Rmd file.
- Added `test_public_points()` to the list of functions run by
  [`run_congruence_checks()`](../reference/run_congruence_checks.md).
- Added `test_public_points()` function to test whether metadata
  contains GPS coordinates if the package is not public.
- Added [`test_keywords()`](../reference/test_keywords.md) to the list
  of function in the DPchecker.Rmd file.
- Added [`test_keywords()`](../reference/test_keywords.md) to the list
  of functions run by
  [`run_congruence_checks()`](../reference/run_congruence_checks.md).
- Added new function, [`test_keywords()`](../reference/test_keywords.md)
  to test for presence of keywords in metadata; something that is
  required for EML extraction on DataStore.
- Updated
  [`test_valid_filenames()`](../reference/test_valid_filenames.md) to
  accept filenames with hyphens (in addition to filenames with
  alpha-numerics and underscore). Filenames still must start with a
  letter.
- Added the [`test_dates_parse()`](../reference/test_dates_parse.md)
  function to the list of functions in the DPchecker.Rmd file.
- Added the [`test_dates_parse()`](../reference/test_dates_parse.md)
  function to the list of functions that are run when the function
  [`run_congruence_checks()`](../reference/run_congruence_checks.md) is
  called.
- Added the function
  [`test_dates_parse()`](../reference/test_dates_parse.md) to test
  whether the date formats supplied in the metadata match the values
  supplied in the data files.
- Added a `NEWS.md` file to track changes to the DPchecker R package.
