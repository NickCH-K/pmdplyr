left <- data.frame(
  i = c(1, 1, 2, 2),
  t = c(1, 2, 1, 2),
  a = 1:4
)
right <- data.frame(
  i = c(1, 2),
  t = c(1, 1),
  b = 1:2
)
joined <- data.frame(
  i = c(1, 1, 2, 2),
  t = c(1, 2, 1, 2),
  a = c(1, 2, 3, 4),
  b = c(1, NA, 2, NA)
)

test_that("safe_join works", {
  expect_error(safe_join(left, right, expect = c("x", "y"), by = "i", join = left_join))
  expect_equal(safe_join(left, right, expect = "y", join = left_join), joined)
  expect_equal(safe_join(left, right, expect = "y"), TRUE)
})

test_that("inexact_left_join works", {
  expect_equal(
    inexact_left_join(left, right %>% dplyr::rename(t2 = t),
      var = t, jvar = t2, method = "last"
    ),
    joined %>%
      dplyr::mutate(b = c(1, 1, 2, 2), t2 = 1) %>%
      dplyr::select(i, t, a, t2, b)
  )
})
