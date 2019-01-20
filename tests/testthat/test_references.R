# test_references.R

context("Reference listing")

test_that("references() generates a named list of references", {

  ref <- references()

  expect_equal(class(ref), "list")
  expect_equal(class(ref[2]), "list")
  expect_equal(class(ref[[2]]), "character")

})
