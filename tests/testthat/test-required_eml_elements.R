good_dir <- testthat::test_path("good")
bad_dir <- testthat::test_path("bad")
bicy_meta <- DPchecker::load_metadata(test_path("good", "BICY_good"))
buis_meta <- DPchecker::load_metadata(test_path("good", "BUIS_good"))

# a couple of house-keeping functions for mockr interactivity:
return_val_2 <- function() {2}
return_val_1 <- function() {1}

# ---- test pub date
test_that("test_pub_date works on good pub dates", {
  msg <- "Publication date, 2022, predates the Data Package Reference Type."
  expect_warning(
    test_pub_date(metadata = bicy_meta),
    msg)
})

# ----- test_access
test_that("test_set_permissions warns on legacy permissions", {
  expect_warning(test_access(metadata = bicy_meta))
})

# waiting on EMLeditor review before a bug fix PR can be merged to test properly:
# test_that("test_set_permissions passes on valid access code", {
#     new_meta <- EMLeditor::set_permissions(bicy_meta, "PUBLIC", force = TRUE)
#     expect_message(test_permissions(new_meta))
#   })






