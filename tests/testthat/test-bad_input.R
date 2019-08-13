# This tests errors for incorrect inputs in all files
# Note that the data.table warning is not tested so as to avoid needing the package

### BETWEEN_WITHIN
df <- pibble(
  i = 1:3,
  x = 1:3,
  .i = i
)

test_that("between_i input failstates", {
  expect_error(df %>% dplyr::mutate(y = between_i(.)))
  expect_error(pibble(a = 1, .t = a) %>% dplyr::mutate(y = between_i(a)))
  expect_error(df %>% dplyr::mutate(y = between_i(x, .fcn = 2)))
  expect_error(df %>% dplyr::mutate(y = between_i(1:2)))
})

test_that("within_i input failstates", {
  expect_error(df %>% dplyr::mutate(y = within_i(.)))
  expect_error(pibble(a = 1, .t = a) %>% dplyr::mutate(y = within_i(a)))
  expect_error(df %>% dplyr::mutate(y = within_i(x, .fcn = 2)))
  expect_error(df %>% dplyr::mutate(y = within_i(1:2)))
})

### ID_VARIABLE
test_that("id_variable input failstates", {
  expect_error(id_variable(a = 1:3, .method = "foo"))
  expect_error(id_variable(a = 1:3, .method = c("character", "random")))
  expect_error(id_variable(a = 1:3, .method = "character", .minwidth = 3))
})

### INEXACT_JOIN
left <- data.frame(i = 1:2, x = 1:2)
right <- data.frame(i = 1:2, y = 3:4, z = 5:6, a = 7:8)

test_that("safe_join input failstates", {
  expect_error(safe_join(left, right, expect = 2))
  expect_error(safe_join(left, right, expect = "foo"))
  expect_error(safe_join(left, right, expect = "1:1", join = 2))
})

test_that("inexact_join input failstates", {
  expect_error(pmdplyr:::inexact_join_prep(left, right, var = left$x, jvar = y, method = "last"))
  expect_error(pmdplyr:::inexact_join_prep(left, right, var = "x", jvar = right$y, method = "last"))
  expect_error(inexact_left_join(left, right, var = x, jvar = c(y, z, a), method = "last"))
  expect_error(inexact_left_join(left, right, var = x, jvar = y, method = 2))
  expect_error(inexact_left_join(left, right, var = x, jvar = y, method = "last", exact = 2))
  expect_error(inexact_left_join(left, right, var = i, jvar = i, method = "last"))
  expect_error(inexact_left_join(left, right, var = x, jvar = i, method = "last"))
  expect_error(inexact_left_join(left %>% dplyr::mutate(x = c("hey", "ho")),
    right,
    var = x, jvar = y, method = "closest"
  ))
  expect_error(inexact_left_join(left, right %>%
    mutate(y = c("hey", "ho")), var = x, jvar = y, method = "closest"))
  expect_warning(inexact_left_join(left, right, var = x, jvar = y, method = "closest", exact = FALSE))
  expect_error(inexact_left_join(left, right, var = x, jvar = y, method = "foo"))
  expect_error(inexact_left_join(left, right, var = x, jvar = c(y, z), method = "between", exact = FALSE))
})

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

### PANEL_CONSISTENCY

df <- pibble(
  i = c(1, 1, 1, 2, 2, 2, 2),
  t = c(1, 3, 4, 2, 4, 6, 7),
  x = c(1, 2, NA, 4, NA, NA, 7),
  .i = i,
  .t = t
)

test_that("panel_fill input failstates", {
  expect_error(panel_fill(df, .backwards = 2))
  expect_error(panel_fill(df, .set_NA = 2))
  expect_error(panel_fill(df, .group_i = 2))
  expect_error(panel_fill(df, .max = "foo"))
  expect_error(panel_fill(df, .min = "foo"))
  expect_error(panel_fill(df, .flag = 2))
  expect_error(panel_fill(df %>% as_pibble(.i = i)))
  expect_error(panel_fill(df %>% as_pibble(.i = i, .t = t, .d = 0)))
  expect_error(panel_fill(df, .set_NA = "i"))
})

inconsistent_df <- pibble(
  i = c(1, 1, 1, 1),
  t = c(1, 1, 2, 2),
  x = c(1, 2, 1, NA),
  .i = i,
  .t = t
)

test_that("panel_locf input failstates", {
  expect_error(panel_locf(as.matrix(df$x), df))
  expect_error(panel_locf(df$x, df, .resolve = 2))
  expect_error(panel_locf(df$x, df, .group_i = 2))
  expect_error(panel_locf(df$x, df, .backwards = 2))
  expect_error(panel_locf(df$x, df %>% as_pibble(.i = i)))
  expect_error(panel_locf(inconsistent_df$x, inconsistent_df))
})

test_that("fixed_check input failstates", {
  expect_error(fixed_check(as.matrix(df), .var = x, .within = i))
  expect_error(fixed_check(df, .var = x))
})

test_that("fixed_force input failstates", {
  expect_error(fixed_force(as.matrix(df), .var = x, .within = i))
  expect_error(fixed_force(df, .var = x, .within = i, .resolve = 2))
  expect_error(fixed_force(df, .var = x, .within = i, .flag = 2))
  expect_error(fixed_force(df, .var = x))
})

### UNEXPORTED_SHARED_FUNCTIONS
df <- data.frame(
  i = 1:3,
  t = 1:3
)

test_that("declare_in_fcn_check input failstates", {
  expect_error(declare_in_fcn_check(df,
    .i = "i",
    .t = "t",
    .d = 1,
    .uniqcheck = 2,
    .setpanel = TRUE,
    .noneed = FALSE
  ))
  expect_error(declare_in_fcn_check(df,
    .i = NA,
    .t = NA,
    .d = 1,
    .uniqcheck = TRUE,
    .setpanel = TRUE
  ))
  expect_error(declare_in_fcn_check(df,
    .i = "i",
    .t = "t",
    .d = 1,
    .uniqcheck = FALSE,
    .setpanel = 2,
    .noneed = FALSE
  ))
  expect_error(declare_in_fcn_check(df,
    .i = NA,
    .t = NA,
    .d = 1,
    .uniqcheck = FALSE,
    .setpanel = 2,
    .noneed = FALSE
  ))
})
