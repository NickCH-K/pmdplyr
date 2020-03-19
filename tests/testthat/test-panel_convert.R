library(pmdplyr)

pb <- pibble(
  i = c(1, 1, 2, 2),
  t = c(1, 2, 1, 2),
  x = 1:4,
  .i = i,
  .t = t
)

pkgs <- utils::installed.packages()

if ("tsibble" %in% pkgs) {
  ts <- tsibble::tsibble(
    i = c(1, 1, 2, 2),
    t = c(1, 2, 1, 2),
    x = 1:4,
    key = i,
    index = t
  )
  test_that("pibble tsibble conversion", {
    expect_equal(panel_convert(pb, to = "tsibble"), ts)
    expect_equal(panel_convert(ts, to = "pibble"), pb)
    expect_equal(
      tsibble::tsibble(
        i = c(1, 1, 1),
        t = 1:3,
        key = i,
        index = t
      ) %>%
        panel_convert("pibble"),
      pibble(
        i = c(1, 1, 1),
        t = 1:3,
        .i = i, .t = t
      )
    )
    expect_equal(
      tsibble::tsibble(
        i = c(1, 1, 1),
        t = 1:3,
        key = i,
        index = t,
        regular = FALSE
      ) %>%
        panel_convert("pibble"),
      pibble(
        i = c(1, 1, 1),
        t = 1:3,
        .i = i, .t = t, .d = 0
      )
    )
    expect_equal(
      tsibble::tsibble(
        i = c(1, 1, 1),
        t = c(1, 3, 5),
        key = i,
        index = t
      ) %>%
        panel_convert("pibble"),
      pibble(
        i = c(1, 1, 1),
        t = c(1, 3, 5),
        .i = i, .t = t, .d = 2
      )
    )
  })
}

if ("plm" %in% pkgs & "tsibble" %in% pkgs) {
  test_that("pibble to plm to tsibble", {
    expect_equal(pb %>% panel_convert("plm") %>% panel_convert("tsibble"), ts)
  })
}

if ("plm" %in% pkgs) {
  pd <- plm::pdata.frame(data.frame(
    i = c(1, 1, 2, 2),
    t = c(1, 2, 1, 2),
    x = 1:4
  ),
  index = c("i", "t")
  )
  test_that("pibble pdata.frame conversion", {
    expect_equal(panel_convert(pb, to = "plm"), pd)
    expect_equal(panel_convert(pd, to = "pibble"), pb)
  })
}

# THIS IS THE ONE WITH THE GROUP BY ADD WARNING
if ("panelr" %in% pkgs) {
  pb <- pibble(
    i = as.factor(c(1, 1, 2, 2)),
    t = c(1, 2, 1, 2),
    x = 1:4,
    .i = i,
    .t = t
  )
  pr <- panelr::panel_data(data.frame(
    i = c(1, 1, 2, 2),
    t = c(1, 2, 1, 2),
    x = 1:4
  ),
  id = i,
  wave = t
  )
  test_that("pibble panel_data conversion", {
    expect_equal(panel_convert(pb, to = "panelr"), pr)
    expect_equal(panel_convert(pr, to = "pibble"), pb)
  })
}
