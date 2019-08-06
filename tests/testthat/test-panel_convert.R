library(pmdplyr)

pb <- pibble(i = c(1,1,2,2),
             t = c(1,2,1,2),
             x = 1:4,
             .i = i,
             .t = t)

if ('tsibble' %in% utils::installed.packages()) {
  ts <- tsibble::tsibble(i = c(1,1,2,2),
                t = c(1,2,1,2),
                x = 1:4,
                key = i,
                index = t)
  test_that("pibble tsibble conversion", {
    expect_equal(panel_convert(pb, to = 'tsibble'), ts)
    expect_equal(panel_convert(ts, to = 'pibble'), pb)
  })
}

if ('plm' %in% utils::installed.packages()) {
  pd <- plm::pdata.frame(data.frame(i = c(1,1,2,2),
                t = c(1,2,1,2),
                x = 1:4),
                index = c("i", "t"))
  test_that("pibble pdata.frame conversion", {
    expect_equal(panel_convert(pb, to = 'plm'), pd)
    expect_equal(panel_convert(pd, to = 'pibble'), pb)
  })
}

if ('panelr' %in% utils::installed.packages()) {
  pb <- pibble(i = as.factor(c(1,1,2,2)),
               t = c(1,2,1,2),
               x = 1:4,
               .i = i,
               .t = t)
  pr <- panelr::panel_data(data.frame(i = c(1,1,2,2),
                               t = c(1,2,1,2),
                               x = 1:4),
                    id = i,
                    wave = t)
  test_that("pibble panel_data conversion", {
    expect_equal(panel_convert(pb, to = 'panelr'), pr)
    expect_equal(panel_convert(pr, to = 'pibble'), pb)
  })
}
