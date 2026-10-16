# Test EML abstract

`test_dp_abstract()` inspects EML for presence of a data package
abstract. The test Fails with an error if the abstract is absent or
greater than 10,000 characters (required for data.gov). If the abstract
is present, the test passes with a warning if the abstract is \<20 words,
\>250 words, or contains a subset of common characters that indicate
improper formatting. Otherwise the test passes.

## Usage

``` r
test_dp_abstract(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns `metadata`

## Examples

``` r
if (FALSE) { # \dontrun{
test_dp_abstract()
} # }
```
