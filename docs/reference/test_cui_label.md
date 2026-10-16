# Test for CUI label

Fails if there is no permissions information or the permissions
information is deprecated. Passes if the permissions information is
public (label not required). Fails if permissions are not set to public
and an CUI label is absent. Fails if the permissions are not set to
public and the CUI label is invalid. Passes if the permissions are not
set to public but a valid CUI label exists.

## Usage

``` r
test_cui_label(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

NULL(invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
test_cui_label()
} # }
```
