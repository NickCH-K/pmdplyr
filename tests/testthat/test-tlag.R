df <- pibble(
  t = c(1, 1, 2, 2),
  x = 1:4,
  .t = t
)

test_that("tlag works", {
  expect_equal(
    (df %>% dplyr::mutate(y = tlag(x, .resolve = mean)))$y,
    c(NA, NA, 1.5, 1.5)
  )
})
