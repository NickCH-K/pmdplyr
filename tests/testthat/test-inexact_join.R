left <- pibble(
  i = c(1, 1, 2, 2),
  t = c(1, 2, 1, 2),
  a = c(1, 2, 3, 4),
  .i = i,
  .t = t
)
right <- pibble(
  i = c(1, 2),
  t2 = c(1, 1),
  b = c(1, 2),
  .i = i,
  .t = t2
)
joined <- pibble(
  i = c(1, 1, 2, 2),
  t = c(1, 2, 1, 2),
  a = c(1, 2, 3, 4),
  b = c(1, NA, 2, NA),
  .i = i,
  .t = t
)
last_join <- joined %>%
  dplyr::mutate(
    t2 = 1,
    b = c(1, 1, 2, 2)
  ) %>%
  dplyr::select(i, t, a, t2, b)

test_that("safe_join works", {
  expect_error(safe_join(left, right %>%
    dplyr::rename(t = t2) %>%
    as_pibble(.i = i, .t = t),
  expect = c("x", "y"), by = "i", join = dplyr::left_join
  ))
  expect_equal(safe_join(left, right %>%
    dplyr::rename(t = t2) %>%
    as_pibble(.i = i, .t = t),
  expect = c("x", "y"), join = dplyr::left_join
  ), joined)
  expect_error(safe_join(left, right %>%
    dplyr::rename(t = t2) %>%
    as_pibble(.i = i, .t = t),
  expect = "y"
  ))
  expect_error(safe_join(left, right %>%
    dplyr::rename(t = t2) %>%
    as_pibble(.i = i, .t = t),
  expect = "1:1", by = "i", join = dplyr::left_join
  ))
  expect_error(safe_join(left, right %>%
    dplyr::rename(t = t2) %>%
    as_pibble(.i = i, .t = t),
  expect = "1:m", by = "i", join = dplyr::left_join
  ))
  expect_error(safe_join(right %>%
    dplyr::rename(t = t2) %>%
    as_pibble(.i = i, .t = t), left,
  expect = "m:1", by = "i", join = dplyr::left_join
  ))
  expect_equal(
    safe_join(left, right %>%
      dplyr::select(-t2),
    expect = "m:1", by = "i", join = dplyr::left_join
    ),
    joined %>%
      dplyr::mutate(b = c(1, 1, 2, 2))
  )
  expect_equal(safe_join(left, right %>%
    dplyr::rename(t = t2) %>%
    as_pibble(.i = i, .t = t),
  expect = "no m:m"
  ), TRUE)
  expect_equal(
    safe_join(left, right,
      var = "t", jvar = "t2",
      expect = "1:1", method = "last",
      join = inexact_left_join
    ),
    inexact_left_join(left, right,
      var = t, jvar = t2, method = "last"
    )
  )
  expect_error(safe_join(left, left, by = "i", expect = "no m:m"))
})

test_that("inexact join methods work", {
  expect_equal(
    inexact_left_join(left, right,
      var = t, jvar = t2, by = "i", method = "last"
    ),
    last_join
  )
  expect_equal(
    inexact_left_join(left, right,
      var = t, jvar = t2, method = "next"
    ),
    joined %>%
      dplyr::mutate(t2 = c(1, NA, 1, NA)) %>%
      dplyr::select(i, t, a, t2, b)
  )
  expect_equal(
    inexact_left_join(left, right,
      var = t, jvar = t2, method = "closest"
    ),
    last_join
  )
  expect_equal(
    inexact_left_join(left, right %>%
      dplyr::mutate(
        t0 = 0,
        t2 = 2
      ),
    var = t, jvar = c(t0, t2),
    method = "between"
    ),
    joined %>%
      dplyr::mutate(
        t0 = c(0, NA, 0, NA),
        t2 = c(2, NA, 2, NA)
      ) %>%
      dplyr::select(i, t, a, t0, t2, b)
  )
  expect_equal(
    inexact_left_join(
      data.frame(t = c(2, 3, 4)),
      data.frame(t2 = c(3, 4, 5)),
      var = t, jvar = t2, method = "next", exact = FALSE
    ),
    data.frame(t = c(2, 3, 4), t2 = c(3, 4, 5))
  )
})

# Left is already well-covered
test_that("Different inexact joins work", {
  expect_equal(
    inexact_right_join(left, right,
      var = t, jvar = t2, by = "i", method = "last"
    ),
    last_join
  )
  expect_equal(
    inexact_inner_join(left, right,
      var = t, jvar = t2, by = "i", method = "last"
    ),
    last_join
  )
  expect_equal(
    inexact_full_join(left, right,
      var = t, jvar = t2, by = "i", method = "last"
    ),
    last_join
  )
  expect_equal(
    inexact_semi_join(left, right,
      var = t, jvar = t2, by = "i", method = "last"
    ),
    last_join %>%
      dplyr::select(-b)
  )
  expect_equal(
    inexact_anti_join(left, right,
      var = t, jvar = t2, by = "i", method = "last"
    ),
    last_join %>%
      dplyr::select(-b) %>%
      dplyr::filter(FALSE)
  )
  expect_equal(
    inexact_nest_join(left, right,
      var = t, jvar = t2, by = "i", method = "last"
    )[["y"]],
    list(
      pibble(b = 1),
      pibble(b = 1),
      pibble(b = 2),
      pibble(b = 2)
    )
  )
  expect_equal(
    inexact_right_join(left, right,
      var = t, jvar = t2, method = "last"
    ),
    last_join
  )
  expect_equal(
    inexact_inner_join(left, right,
      var = t, jvar = t2, method = "last"
    ),
    last_join
  )
  expect_equal(
    inexact_full_join(left, right,
      var = t, jvar = t2, method = "last"
    ),
    last_join
  )
  expect_equal(
    inexact_semi_join(left, right,
      var = t, jvar = t2, method = "last"
    ),
    last_join %>%
      dplyr::select(-b)
  )
  expect_equal(
    inexact_anti_join(left, right,
      var = t, jvar = t2, method = "last"
    ),
    last_join %>%
      dplyr::select(-b) %>%
      dplyr::filter(FALSE)
  )
  expect_equal(
    inexact_nest_join(left, right,
      var = t, jvar = t2, method = "last"
    )[["y"]],
    list(
      pibble(b = 1),
      pibble(b = 1),
      pibble(b = 2),
      pibble(b = 2)
    )
  )
})
