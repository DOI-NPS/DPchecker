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
test_that("test_access warns on legacy permissions", {
  expect_warning(test_access(metadata = bicy_meta))
})

test_that("test_access passes for properly specified public access", {
  meta2 <- EMLeditor::set_permissions(bicy_meta, "PUBLIC", force = TRUE)
  expect_no_error(test_access(meta2))
})

test_that("test_access passes for properly specified internal access", {
  meta2 <- EMLeditor::set_permissions(bicy_meta,
                                      "INTERNAL",
                                      legal_authority_id = 1,
                                      contact_email = "no_one@nps.gov",
                                      authority_designator = "fake name",
                                      force = TRUE)
  expect_no_error(suppressWarnings(test_access(meta2)))
})

# ---- test cui label
test_that("test_cui_label error on deprecated CUI", {
  expect_error(test_cui_label(metadata = bicy_meta))
})

test_that("test_cui_label passes with public permissions", {
  meta2 <- EMLeditor::set_permissions(bicy_meta, "PUBLIC", force = TRUE)
  expect_no_error(test_cui_label(meta2))
})

test_that("test_cui_label passes for properly formatted internal permissions", {
  meta2 <- EMLeditor::set_permissions(bicy_meta,
                                      "INTERNAL",
                                      legal_authority_id = 1,
                                      contact_email = "no_one@nps.gov",
                                      authority_designator = "fake name",
                                      force = TRUE)
  expect_no_error(test_cui_label(meta2))
})

# ---- test cui email
test_that("test_cui_email error on deprecated CUI", {
  expect_error(test_cui_email(metadata = bicy_meta))
})

test_that("test_cui_email passes with public permissions", {
  meta2 <- EMLeditor::set_permissions(bicy_meta, "PUBLIC", force = TRUE)
  expect_no_error(test_cui_email(meta2))
})

test_that("test_cui_email passes for properly formatted internal permissions", {
  meta2 <- EMLeditor::set_permissions(bicy_meta,
                                      "INTERNAL",
                                      legal_authority_id = 1,
                                      contact_email = "no_one@nps.gov",
                                      authority_designator = "fake name",
                                      force = TRUE)
  expect_no_error(test_cui_email(meta2))
})

# ---- test_cui_designator
test_that("test_cui_designator errors on deprecated CUI", {
  expect_error(test_cui_designator(metadata = bicy_meta))
})

test_that("test_cui_designator passes with public permissions", {
  meta2 <- EMLeditor::set_permissions(bicy_meta, "PUBLIC", force = TRUE)
  expect_no_error(test_cui_designator(meta2))
})

test_that("test_cui_designator passes with properly formatted internal permissions", {
  meta2 <- EMLeditor::set_permissions(bicy_meta,
                                      "INTERNAL",
                                      legal_authority_id = 1,
                                      contact_email = "no_one@nps.gov",
                                      authority_designator = "fake name",
                                      force = TRUE)
  expect_no_error(test_cui_designator(meta2))
})


# waiting on EMLeditor review before a bug fix PR can be merged to test properly:
# test_that("test_set_permissions passes on valid access code", {
#     new_meta <- EMLeditor::set_permissions(bicy_meta, "PUBLIC", force = TRUE)
#     expect_message(test_permissions(new_meta))
#   })






