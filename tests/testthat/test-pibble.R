library(pmdplyr)
library(testthat)

grades <- pibble(
  student_id = c(0001, 0001, 0002, 0002, 0002),
  module_id = c("ECON100", "ACCY101", "ECON100", "MARK105", "FIN223"),
  semester = c(1, 2, 2, 1, 1),
  grade = c(79, 85, 96, 68, 86),
  .i = c(student_id, module_id),
  .t = semester
)

test_that("students' grades over time", {
  expect_s3_class(grades, "tbl_pb")
  expect_identical(attr(grades, ".i") %>% unname(), c("student_id", "module_id"))
  expect_identical(attr(grades, ".t") %>% unname(), "semester")
  expect_output(print(grades), regexp = "# A pibble:\\s+5 x 4")
})
