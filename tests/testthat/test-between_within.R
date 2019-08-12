library(rlang)
library(pmdplyr)
library(tibble)
library(dplyr)

df <- data.frame(
  x = 1:6,
  i = c(rep(1, 3), rep(2, 3))
)

test_that("within_i and between_i calculations work", {
  expect_equal(
    df %>%
      dplyr::mutate(w = within_i(x, .i = i)) %>%
      dplyr::pull(w),
    c(-1, 0, 1, -1, 0, 1)
  )
  expect_equal(
    df %>%
      dplyr::mutate(b = between_i(x, .i = i)) %>%
      dplyr::pull(b),
    c(-1.5, -1.5, -1.5, 1.5, 1.5, 1.5)
  )
  expect_equal(
    df %>%
      dplyr::group_by(i) %>%
      dplyr::mutate(w = within_i(x, .i = i)) %>%
      dplyr::pull(w),
    c(-1, 0, 1, -1, 0, 1)
  )
  expect_equal(
    df %>%
      dplyr::group_by(i) %>%
      dplyr::mutate(b = between_i(x, .i = i)) %>%
      dplyr::pull(b),
    c(0, 0, 0, 0, 0, 0)
  )
})
