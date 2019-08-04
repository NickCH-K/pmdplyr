library(rlang)
library(pmdplyr)
library(tibble)
library(dplyr)

select_dummy_wrapper <- function(.data, ...) {
  .df <- select(.data, ...)
  vec_restore(.data, .df)
}
tibble(
  a = 1:4,
  b = LETTERS[1:4],
  c = 5,
  d = 7
) %>%
  as_pibble(.i = "b", .t = "a") %>%
  select_dummy_wrapper(a:c) -> select_cols

test_that("vec_restore returns tbl_pb", {
  expect_s3_class(select_cols, "tbl_pb")
})
