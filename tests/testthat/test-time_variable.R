td <- data.frame(
  year = 2008:2006,
  month = 1:3,
  date = lubridate::ymd(c("100101", "100302", "100604"))
)

present_t <- td %>%
  dplyr::mutate(t = time_variable(year, month,
                                  .method = "present")) %>%
  dplyr::pull(4)

year_t <- td %>%
  dplyr::mutate(t = time_variable(date,
                                  .method = "year")) %>%
  dplyr::pull(4)

month_t <- td %>%
  dplyr::mutate(t = time_variable(date,
                                  .method = "month")) %>%
  dplyr::pull(4)

week_t <- td %>%
  dplyr::mutate(t = time_variable(date,
                                  .method = "week")) %>%
  dplyr::pull(4)

day_t <- td %>%
  dplyr::mutate(t = time_variable(date,
                                  .method = "day")) %>%
  dplyr::pull(4)

turnover_t <- td %>%
  dplyr::mutate(t = time_variable(year, month,
                                  .method = "turnover",
                                  .turnover = c(NA, 12),
                                  .turnover_start = c(NA, 1))) %>%
  dplyr::pull(4)

test_that("time_variable works", {
  expect_equal(present_t, 3:1)
  expect_type(present_t, "integer")
  expect_equal(year_t, rep(2010L, 3))
  expect_equal(month_t, c(1L, 3L, 6L))
  expect_equal(week_t, c(1L, 10L, 23L))
  expect_equal(day_t, c(1L, 61L, 155L))
  expect_equal(turnover_t, c(23L, 12L, 1L))
})
