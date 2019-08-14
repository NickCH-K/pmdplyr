cascade_data <- pibble(
  t = 1:3,
  x = 1:3,
  .t = t
)

cascade2 <- pibble(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(1, 2, 3, 1, 2, 3),
  x = 1:6,
  .i = i,
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
    c(1, 3, 6))
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
    c(NA, 3, 5))
  expect_equal(
    cascade2 %>%
      dplyr::group_by(i) %>%
      mutate_cascade(x = x + tlag(x, .n = -1, .quick = TRUE), .backwards = TRUE) %>%
      dplyr::pull(x),
    c(6, 5, 3, 15, 11, 6))
  expect_equal(
    (cascade2 %>%
       as_pibble() %>%
       mutate_cascade(x = x +
                        tlag(x, .df = cascade2, .n = -1, .quick = TRUE),
                      .backwards = TRUE, .i = i, .t = t, .setpanel = FALSE)) %@% ".i", NA_character_)
  expect_false(
    "tbl_pb" %in% class(cascade2 %>%
       as_tibble() %>%
       mutate_cascade(x = x +
                        tlag(x, .df = cascade2, .n = -1, .quick = TRUE),
                      .backwards = TRUE, .i = i, .t = t, .setpanel = FALSE)))
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
  expect_equal(
    cascade2 %>%
      mutate_subset(y = mean(x), .filter = t <= 2) %>%
      dplyr::pull(y),
    c(1.5, 1.5, 1.5, 4.5, 4.5, 4.5)
  )
  expect_equal(
    cascade2 %>% dplyr::group_by(i) %>%
      mutate_subset(y = mean(x), .filter = t <= 2) %>%
      dplyr::pull(y),
    c(1.5, 1.5, 1.5, 4.5, 4.5, 4.5)
  )
  expect_identical(
    (cascade2 %>%
      as_pibble() %>%
      mutate_subset(y = mean(x), .filter = t <= 2, .setpanel = FALSE)) %@% ".i", NA_character_)
  expect_false(
    "tbl_pb" %in% class(cascade2 %>%
      as_tibble() %>%
      mutate_subset(y = mean(x),
                    .filter = t <= 2,
                    .i = i,
                    .t = t,
                    .setpanel = FALSE)))
})
