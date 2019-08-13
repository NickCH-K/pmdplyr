df <- pibble(
  i = c(1, 1, 1, 2, 2, 2, 2),
  t = c(1, 3, 4, 2, 4, 6, 7),
  x = c(1, 2, NA, 4, NA, NA, 7),
  .i = i,
  .t = t
)

nonpib <- data.frame(
  i = c(1, 1, 1, 2, 2, 2, 2),
  t = c(1, 3, 4, 2, 4, 6, 7),
  x = c(1, 2, NA, 4, NA, NA, 7)
)

proper_fill <- pibble(
  i = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
  t = c(1, 2, 3, 4, 2, 3, 4, 5, 6, 7),
  x = c(1, 1, 2, NA, 4, 4, NA, NA, NA, 7),
  .i = i,
  .t = t
)

nonpib_proper_fill <- data.frame(
  i = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
  t = c(1, 2, 3, 4, 2, 3, 4, 5, 6, 7),
  x = c(1, 1, 2, NA, 4, 4, NA, NA, NA, 7)
)

backwards_fill <- pibble(
  i = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
  t = c(1, 2, 3, 4, 2, 3, 4, 5, 6, 7),
  x = c(1, 2, 2, NA, 4, NA, NA, NA, NA, 7),
  .i = i,
  .t = t
)

ungroup_fill <- pibble(
  i = c(1, 1, 1, 1, 2, 2, 2, 2, 2),
  t = c(1, 3, 4, 5, 2, 4, 5, 6, 7),
  x = c(1, 2, NA, NA, 4, NA, NA, NA, 7),
  .i = i,
  .t = t
)

na_fill <- pibble(
  i = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
  t = c(1, 2, 3, 4, 2, 3, 4, 5, 6, 7),
  x = c(1, NA, 2, NA, 4, NA, NA, NA, NA, 7),
  flag = c(F, T, F, F, F, T, F, T, F, F),
  .i = i,
  .t = t
)

balance_fill <- pibble(
  i = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2),
  t = as.numeric(rep(1:7, 2)),
  x = c(1, 1, 2, NA, NA, NA, NA, 4, 4, 4, NA, NA, NA, 7),
  .i = i,
  .t = t
)

balance_fill_na <- pibble(
  i = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2),
  t = as.numeric(rep(1:7, 2)),
  x = c(1, NA, 2, NA, NA, NA, NA, NA, 4, NA, NA, NA, NA, 7),
  .i = i,
  .t = t
)

non_fixed <- pibble(
  i = c(1, 1, 2, 2, 1, 2, 2),
  t = c(1, 3, 2, 7, 4, 4, 6),
  x = c(1, 2, 4, 7, NA, NA, NA),
  .i = i,
  .t = t
)


test_that("panel_fill works", {
  expect_equal(panel_fill(df), proper_fill)
  expect_equal(
    panel_fill(df %>% as_pibble(.i = NULL, .t = NULL),
      .i = i, .t = t
    ),
    proper_fill %>% as_pibble(.i = NULL, .t = NULL)
  )
  expect_equal(panel_fill(df, .group_i = FALSE), ungroup_fill)
  expect_equal(panel_fill(df,
    .i = i,
    .t = t,
    .set_NA = TRUE,
    .flag = "flag",
    .setpanel = FALSE
  ), na_fill)
  expect_equal(panel_fill(df,
    .i = i,
    .t = t,
    .set_NA = "x",
    .flag = "flag",
    .setpanel = FALSE
  ), na_fill)
  expect_equal(
    panel_fill(df %>% group_by(x), .backwards = TRUE),
    backwards_fill %>% group_by(x)
  )
  expect_equal(panel_fill(df, .min = 1, .max = 7), balance_fill)
  expect_equal(panel_fill(df, .min = 1, .max = 7, .set_NA = TRUE), balance_fill_na)
  expect_equal(panel_fill(df, .min = 1, .max = 7, .set_NA = "x"), balance_fill_na)
  expect_equal(panel_fill(nonpib, .i = i, .t = t, .setpanel = FALSE), nonpib_proper_fill)
})

test_that("panel_locf works", {
  expect_equal(panel_locf(df$x, df), c(1, 2, 2, 4, 4, 4, 7))
  expect_equal(panel_locf(df$x, df, .backwards = TRUE), c(1, 2, NA, 4, 7, 7, 7))
  expect_equal(panel_locf(df$x,
    df %>%
      dplyr::mutate(t = as.character(t)),
    .backwards = TRUE
  ), c(1, 2, NA, 4, 7, 7, 7))
  expect_equal(panel_locf(df$x, df, .group_i = FALSE), c(1, 2, 2, 4, 2, 2, 7))
})

test_that("fixed_check works", {
  expect_equal(fixed_check(df, .var = x, .within = i)[["x"]], non_fixed)
  expect_equal(fixed_check(df, .var = x, .within = c(i, t)), TRUE)
  expect_equal(fixed_check(df, .within = c(i, t)), TRUE)
})

test_that("fixed_force works", {
  expect_equal(
    fixed_force(df, .var = x, .within = i) %>% dplyr::pull(x),
    c(1, 1, 1, NA, NA, NA, NA)
  )
  expect_equal(
    fixed_force(df, .var = x, .within = i, .flag = "changed") %>% dplyr::pull(changed),
    c(F, T, T, T, F, F, T)
  )
  expect_equal(nrow(
    fixed_force(df, .var = x, .within = i, .resolve = "drop")
  ), 0)
  expect_equal(nrow(
    fixed_force(df, .within = c(i, t), .resolve = "drop")
  ), 4)
  expect_identical(names(
    fixed_force(
      df %>% group_by(x),
      .var = x,
      .within = i
    ) %@% "groups"
  )[1], "x")
})


test_that("mode_order works", {
  expect_equal(mode_order(c(1, 2, 2, NA, 5, 3, 4)), 2)
  expect_equal(mode_order(c(2, 2, 1, 1)), 2)
  expect_equal(mode_order(c("a", "a", "b")), "a")
})
