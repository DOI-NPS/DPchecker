# Test for CUI dissemination code

`test_access()` examines EML metadata for the presence of access
information in additonalMetadata. The function fails with an error if
access information does not exist. The function passes with a warning if
the access information exists but is deprecated. The function fails with
an error if the information exists but is invalid. The function passes
with a warning if the access is correct but not public. The function
passes without a warning if a current, valid public access statement
exists.

## Usage

``` r
test_access(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns NULL

## Examples

``` r
if (FALSE) { # \dontrun{
test_access()
} # }
```
