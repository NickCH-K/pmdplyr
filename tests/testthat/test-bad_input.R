# This tests errors for incorrect inputs in all files
# Note that the data.table warning is not tested so as to avoid needing the package


### MUTATE_CASCADE
cascade_data <- pibble(
  t = 1:3,
  x = 1:3,
  .t = t
)

test_that("mutate_cascade input failstates", {
  expect_error(
    cascade_data %>%
      mutate_cascade(a = 1, .backwards = 2),
    ".backwards must be TRUE or FALSE"
  )
  expect_error(
    cascade_data %>%
      mutate_cascade(a = 1, .skip = 2),
    ".skip must be TRUE or FALSE"
  )
  expect_error(
    cascade_data %>%
      mutate_cascade(a = 1, .group_i = 2),
    ".group_i must be TRUE or FALSE"
  )
  expect_error(cascade_data %>%
    as_pibble(.i = t) %>%
    mutate_cascade(a = 1),
  regexp = "requires that .t be declared"
  )
})

test_that("mutate_subset input failstates", {
  expect_error(
    as.matrix(cascade_data) %>%
      mutate_subset(y = mean(x), .filter = t <= 2),
    "Requires data to be a data frame or tibble."
  )
})
