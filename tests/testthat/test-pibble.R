grades <- pibble(
  student_id = c(0001, 0001, 0002, 0002, 0002),
  module_id = c("ECON100", "ACCY101", "ECON100", "MARK105", "FIN223"),
  semester = c(1, 2, 2, 1, 1),
  grade = c(79, 85, 96, 68, 86),
  .i = c(student_id, module_id),
  .t = semester
)

df <- pibble(
  i = 1:3,
  t = 1:3,
  .i = i,
  .t = t
)

test_that("students' grades over time", {
  expect_s3_class(grades, "tbl_pb")
  expect_identical(attr(grades, ".i") %>% unname(), c("student_id", "module_id"))
  expect_identical(attr(grades, ".t") %>% unname(), "semester")
  expect_output(print(grades), regexp = "# A pibble:\\s+5 ")
  expect_output(print(grades), regexp = "identifiers ")
  expect_output(print(df), regexp = "identifier ")
  expect_identical(type_sum(df), "pibble")
  expect_output(print(df %>% dplyr::group_by(i)),
    regexp = "# Groups:\\s+i "
  )
})

test_that("is_pibble", {
  expect_message(is_pibble(grades),
    regexp = ".i = student_id, module_id; .t = semester; .d = 1."
  )
  expect_true(is_pibble(grades))
  expect_silent(is_pibble(grades, .silent = TRUE))
  expect_error(is_pibble(LETTERS), "Requires data to be a data frame or tibble")
  expect_error(is_pibble(grades, .silent = 5), "silent must be TRUE or FALSE")
})

test_that("as_pibble", {
  expect_error(as_pibble(NULL), "A pibble must not be NULL")
})
