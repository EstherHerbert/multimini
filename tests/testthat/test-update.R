test_that("errors for incorrect input", {
  mini <- minimise(patients, factors = "sex", burnin = 10)
  update_data <- data.frame(site = 1:10)

  expect_error(update(mini, update_data))

  })

test_that("original minimisation maintained", {
  mini <- minimise(patients, factors = "sex", burnin = 10)
  update_data <- data.frame(site = 1:10, sex = sample(c("M", "F"), 10, T))
  expect_equal(update(mini, update_data)[1:150,], mini)
})

test_that("seed maintained", {
  mini <- minimise(patients, factors = "sex", seed = 1253)
  update_data <- data.frame(site = 1:10, sex = sample(c("M", "F"), 10, T))
  expect_equal(seed(update(mini, update_data)), 1253)
  expect_equal(update(mini, update_data), update(mini, update_data))
})

test_that("attributes maintained", {
  mini <- minimise(patients, factors = "sex")
  update_data <- data.frame(site = 1:10, sex = sample(c("M", "F"), 10, T))
  expect_equal(class(mini), class(update(mini, update_data)))
  expect_equal(groups(mini), groups(update(mini, update_data)))
  expect_equal(factors(mini), factors(update(mini, update_data)))
  expect_equal(burnin(mini), burnin(update(mini, update_data)))
  expect_equal(minprob(mini), minprob(update(mini, update_data)))
  expect_equal(ratio(mini), ratio(update(mini, update_data)))
})

