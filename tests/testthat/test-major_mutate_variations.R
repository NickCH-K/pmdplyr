cascade_data <- pibble(
  t = 1:3,
  x = 1:3,
  .t = t
)

test_that("mutate_cascade with tlag works", {
  expect_equal(
    (cascade_data %>%
      mutate_cascade(x = x + tlag(x, .quick = TRUE)))$x,
    c(1, 3, 6)
  )
})

test_that("mutate_subset works", {
  expect_equal(
    (cascade_data %>%
      mutate_subset(y = mean(x), .filter = t <= 2))$y,
    c(1.5, 1.5, 1.5)
  )
})
