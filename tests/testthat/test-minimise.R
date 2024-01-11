test_that("errors given", {
  expect_error(minimise(patients, groups = 1, factors = c("sex", "site")))
})

test_that("seed works", {
  expect_identical(
    minimise(patients, groups = 2, factors = c("sex", "site"), seed = 456),
    minimise(patients, groups = 2, factors = c("sex", "site"), seed = 456)
  )
  expect_identical(
    minimise(patients, groups = 2, factors = c("sex"), stratify = "site",
             seed = 254, burnin = 5),
    minimise(patients, groups = 2, factors = c("sex"), stratify = "site",
             seed = 254, burnin = 5)
  )
  expect_false(
    identical(
      minimise(patients, groups = 2, factors = c("sex"), stratify = "site",
               burnin = 5),
      minimise(patients, groups = 2, factors = c("sex"), stratify = "site",
               burnin = 5)
    )
  )
})
