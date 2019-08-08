td <- data.frame(year = 2006:2008,
                 month = 3:1,
                 date = lubridate::ymd(c('100101','100302','100604')))

test_that("time_variable works", {
  expect_equal((td %>% dplyr::mutate(t = time_variable(year, month,
                                                  .method = 'present')))$t,
               1:3)
  expect_equal((td %>%
                  dplyr::mutate(t = time_variable(date,
                                                  .method='year')))$t,
               rep(2010,3))
  expect_equal((td %>%
                  dplyr::mutate(t = time_variable(date,
                                                  .method='month')))$t,
               c(1, 3, 6))
  expect_equal((td %>%
                  dplyr::mutate(t = time_variable(date,
                                                  .method='week')))$t,
               c(1, 10, 23))
  expect_equal((td %>%
                  dplyr::mutate(t = time_variable(date,
                                                  .method='day')))$t,
               c(1, 61, 155))
  expect_equal((td %>% dplyr::mutate(t = time_variable(year, month,
                                                       .method = 'turnover',
                                                       .turnover = c(NA, 12),
                                                       .turnover_start = c(NA,1))))$t,
               c(1, 12, 23))
})
