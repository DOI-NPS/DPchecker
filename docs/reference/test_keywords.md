# Test for Keywords

`test_keywords()` tests to see whether metadata contains at least one
"Keywords Set". Fails with error if any keyword is longer than 150
characters (data.gov requirement).

## Usage

``` r
test_keywords(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisible(metadata)

## Examples

``` r
if (FALSE) { # \dontrun{
test_keywords()
} # }
```
