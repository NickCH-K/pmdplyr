cascade_data <- pibble(
  t = 1:3,
  x = 1:3,
  .t = t
)

test_that("mutate_cascade with tlag works", {
  # Basic
  expect_equal(
    cascade_data %>%
      mutate_cascade(x = x + tlag(x, .quick = TRUE)) %>%
      dplyr::pull(x),
    c(1, 3, 6)
  )
  # Declare pibble in function
  expect_equal(
    cascade_data %>%
      mutate_cascade(
        x = x + tlag(x, .quick = TRUE),
        .t = t
      ) %>%
      dplyr::pull(x),
    c(1, 3, 6)
  )
  # Alternate options
  expect_equal(
    cascade_data %>%
      mutate_cascade(
        x = x + tlag(x, .quick = TRUE),
        .backwards = TRUE,
        .skip = FALSE,
        .setpanel = FALSE
      ) %>%
      dplyr::pull(x),
    c(NA, 3, 5)
  )
})

test_that("mutate_subset works", {
  # Basic
  expect_equal(
    cascade_data %>%
      mutate_subset(y = mean(x), .filter = t <= 2) %>%
      dplyr::pull(y),
    c(1.5, 1.5, 1.5)
  )
  # Setpanel option
  expect_identical(
    cascade_data %>%
      mutate_subset(
        y = mean(x), .filter = t <= 2,
        .t = x,
        .setpanel = FALSE
      ) %@% ".t" %>% unname(),
    "t"
  )
})
