library(pmdplyr)

test_that("id_variable works", {
  expect_equal(id_variable(
    a = c(1, 2, 2), b = c("a", "b", "b"),
    .method = "character", .minwidth = TRUE
  ), c("1a", "2b", "2b"))
  expect_equal(id_variable(
    a = c(1, 2, 2), b = c("a", "b", "b"),
    .method = "character"
  ), c("|1||a|", "|2||b|", "|2||b|"))
  expect_equal(id_variable(
    a = c(1, 2, 2), b = c("a", "b", "b"),
    .method = "number"
  ), c(1, 2, 2))
  randid <- id_variable(a = c(1, 2, 2), b = c("a", "b", "b"), .method = "random")
  expect_equal(randid[2], randid[3])
  expect_false(randid[1] == randid[2])
})
