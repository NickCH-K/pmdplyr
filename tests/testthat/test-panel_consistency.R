df <- pibble(
  i = c(1, 1, 1, 2, 2, 2, 2),
  t = c(1, 3, 4, 2, 4, 6, 7),
  x = c(1, 2, NA, 4, NA, NA, 7),
  .i = i,
  .t = t
)

proper_fill <- pibble(
  i = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
  t = c(1, 2, 3, 4, 2, 3, 4, 5, 6, 7),
  x = c(1, 1, 2, NA, 4, 4, NA, NA, NA, 7),
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
non_fixed <- pibble(
  i = c(1, 1, 2, 2, 1, 2, 2),
  t = c(1, 3, 2, 7, 4, 4, 6),
  x = c(1, 2, 4, 7, NA, NA, NA),
  .i = i,
  .t = t
)


test_that("panel_fill works", {
  expect_equal(panel_fill(df), proper_fill)
  expect_equal(panel_fill(df, .min = 1, .max = 7), balance_fill)
})

test_that("panel_locf works", {
  expect_equal(panel_locf(df$x, df), c(1, 2, 2, 4, 4, 4, 7))
  expect_equal(panel_locf(df$x, df, .backwards = TRUE), c(1, 2, NA, 4, 7, 7, 7))
})

test_that("fixed_check works", {
  expect_equal(fixed_check(df, .var = x, .within = i)[["x"]], non_fixed)
  expect_equal(fixed_check(df, .var = x, .within = c(i, t)), TRUE)
})

test_that("fixed_force works", {
  expect_equal(
    fixed_force(df, .var = x, .within = i) %>% dplyr::pull(x),
    c(1, 1, 1, NA, NA, NA, NA)
  )
})


test_that("mode_order works", {
  expect_equal(mode_order(c(1, 2, 2, NA, 5, 3, 4)), 2)
  expect_equal(mode_order(c(2, 2, 1, 1)), 2)
  expect_equal(mode_order(c("a", "a", "b")), "a")
})
