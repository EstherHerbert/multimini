test_that("errors given", {
  expect_error(minimise(patients, groups = 1, factors = c("sex", "site")))
})
