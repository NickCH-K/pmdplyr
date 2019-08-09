library(rlang)
library(pmdplyr)
library(tibble)
library(dplyr)

df <- data.frame(
  x = 1:6,
  i = c(rep(1, 3), rep(2, 3))
) %>%
  dplyr::mutate(
    w = within_i(x, .i = i),
    b = between_i(x, .i = i)
  )

test_that("within_i and between_i calculations work", {
  expect_equal(df %>% dplyr::pull(w), c(-1, 0, 1, -1, 0, 1))
  expect_equal(df %>% dplyr::pull(b), c(-1.5, -1.5, -1.5, 1.5, 1.5, 1.5))
})
