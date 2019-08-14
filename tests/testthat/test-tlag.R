df <- pibble(
  t = c(1, 1, 2, 2),
  x = 1:4,
  .t = t
)

df2 <- pibble(
  t = c(1, 2, 3, 1, 2, 3),
  i = c(1, 1, 1, 2, 2, 2),
  x = 1:6,
  .i = i,
  .t = t
)

df_to_resolve <- pibble(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(1, 2, 2, 1, 2, 2),
  x = c(1, 2, 2, 1, 2, 2),
  .i = i,
  .t = t
)

test_that("tlag works", {
  expect_equal(df %>%
      dplyr::mutate(y = tlag(x, .resolve = mean)) %>%
      dplyr::pull(y),
    c(NA, NA, 1.5, 1.5))
  expect_equal(df %>%
                 as_pibble(.t = t, .d = 0) %>%
                 dplyr::mutate(y = tlag(x, .resolve = mean, .default = 1)) %>%
                 dplyr::pull(y),
               c(1.0, 1.0, 1.5, 1.5))
  expect_equal(tlag(df$x, df, .t = t, .group_i = FALSE, .resolve = mean), c(NA, NA, 1.5, 1.5))
  expect_equal(df2 %>%
                 dplyr::group_by(i) %>%
                 dplyr::mutate(y = tlag(x, .quick = TRUE)) %>%
                 dplyr::pull(y),
               c(NA, 1, 2, NA, 4, 5))
  expect_equal(df2 %>%
                 dplyr::mutate(y = tlag(x, .quick = TRUE)) %>%
                 dplyr::pull(y),
               c(NA, 1, 2, NA, 4, 5))
  expect_equal(df2 %>%
                 as_pibble(.i = i, .t = t, .d = 0) %>%
                 dplyr::mutate(y = tlag(x, .quick = TRUE)) %>%
                 dplyr::pull(y),
               c(NA, 1, 2, NA, 4, 5))
  expect_equal(df_to_resolve %>%
                 dplyr::mutate(y = tlag(x)) %>%
                 dplyr::pull(y),
               c(NA, 1, 1, NA, 1, 1))
})
