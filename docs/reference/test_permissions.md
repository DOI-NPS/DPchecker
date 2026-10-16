# Test for CUI dissemination code

`test_permissions()` examines EML metadata for the presence of a
permissions information in additonalMetadata. The function fails with an
error if permissions information does not exist. The function passes
with a warning if the permissions information exists but is deprecated.
The function fails with an error if the information exists but is
invalid. The function passes if a current, valid permissions statement
exists. If the permissions are not public, the function also produces a
warning.

## Usage

``` r
test_permissions(metadata = load_metadata(directory))
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
test_permissions()
} # }
```
