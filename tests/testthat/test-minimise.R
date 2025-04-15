test_that("errors given for wrong inputs", {
  expect_error(minimise(patients, groups = 1, factors = c("sex", "site")))
  expect_error(minimise(patients, groups = 2, factors = "sex", minprob = 1.1))
  expect_error(minimise(patients, groups = 2, factors = c("sex", "age")))
  expect_error(minimise(patients, groups = 2, factors = "stage",
                        ratio = c(1,2,1)))
  expect_error(minimise(patients, factors = "sex", burnin = 200))
  temp <- within(patients, rm(eligible))
  expect_error(minimise(temp, factors = "sex", check.eligibility = T))
})

test_that("seed works", {
  expect_identical(
    minimise(patients, groups = 2, factors = c("sex", "site"), seed = 456),
    minimise(patients, groups = 2, factors = c("sex", "site"), seed = 456)
  )
  expect_false(
    identical(
      minimise(patients, groups = 2, factors = c("sex"), burnin = 5),
      minimise(patients, groups = 2, factors = c("sex"), burnin = 5)
    )
  )
})

test_that("eligibility correct", {
  expect_true({
    minimise(patients, factors = "sex", check.eligibility = T) %>%
      as.data.frame() %>%
      with(purrr::map2(Group, eligible, ~.x %in%.y)) %>%
      unlist() %>%
      all()
  })
})
