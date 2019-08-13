td <- data.frame(
  year = 2008:2006,
  month = 1:3,
  date = lubridate::ymd(c("100101", "100302", "100604"))
)

td_multiyear <- data.frame(
  date = lubridate::ymd(c("100101", "110201", "120301", "130401"))
)

present_t <- td %>%
  dplyr::mutate(t = time_variable(year, month,
    .method = "present"
  )) %>%
  dplyr::pull(t)

year_t <- td %>%
  dplyr::mutate(t = time_variable(date,
    .method = "year"
  )) %>%
  dplyr::pull(t)

month_t <- td %>%
  dplyr::mutate(t = time_variable(date,
    .method = "month"
  )) %>%
  dplyr::pull(t)


turnover_t <- td %>%
  dplyr::mutate(t = time_variable(year, month,
    .method = "turnover",
    .turnover = c(NA, 12),
    .turnover_start = c(NA, 1)
  )) %>%
  dplyr::pull(t)

test_that("time_variable works", {
  expect_equal(present_t, 3:1)
  expect_type(present_t, "integer")
  expect_equal(year_t, rep(2010L, 3))
  expect_equal(month_t, c(1L, 3L, 6L))
  expect_equal(td %>%
                 dplyr::mutate(t = time_variable(date,
                                                 .method = "week",
                                                 .breaks = 1
                 )) %>%
                 dplyr::pull(t), c(1L, 10L, 23L))
  expect_equal(td %>%
                 dplyr::mutate(t = time_variable(date,
                                                 .method = "day",
                                                 .breaks = 2
                 )) %>%
                 dplyr::pull(t),
               c(1L, 61L, 155L))
  expect_equal(turnover_t, c(23L, 12L, 1L))
  expect_equal(td %>%
                 dplyr::mutate(t = time_variable(year,
                                                 .method = "present")) %>%
                 dplyr::pull(t), c(3L, 2L, 1L))
  expect_equal(td_multiyear %>% dplyr::mutate(t = time_variable(date,
                                                      .method = "year",
                                                      .breaks = c(2011, 2013))) %>%
                 dplyr::pull(t), c(NA, 1L, 1L, 2L))
  expect_equal(td_multiyear %>% dplyr::mutate(t = time_variable(date,
                                                                .method = "month",
                                                                .breaks = c(2, 5))) %>%
                 dplyr::pull(t), c(NA, 1L, 3L, 5L))
  expect_equal(td_multiyear %>% dplyr::mutate(t = time_variable(date,
                                                                .method = "year",
                                                                .skip = 2012)) %>%
                 dplyr::pull(t), c(1L, 2L, NA, 3L))
  expect_equal(td_multiyear %>% dplyr::mutate(t = time_variable(date,
                                                                .method = "year",
                                                                .breaks = c(2010, 2013),
                                                                .skip = 2012)) %>%
                 dplyr::pull(t), c(NA, NA, NA, 1L))
})
