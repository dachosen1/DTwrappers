context("dt.define.variable")

age.name = "Age"
income.name = "Income"
region.name = "Region"

library(formulaic)

dat = formulaic::snack.dat

dt.define.variable1 <- dt.define.variable(dat = dat, variable.name = "Age Decade", the.values = dat[, floor(get(age.name) / 10)])
dt.define.variable2 <- dt.define.variable(dat = dat, variable.name = "Income in Thousands", the.values = expression(floor(Income / 10^3)), specification = "by.expression")
dt.define.variable3 <- dt.define.variable(dat = dat, variable.name = "Income in Thousands", the.values = "floor( Income/ 10^3)", specification = "by.expression")
dt.define.variable4 <- dt.define.variable(dat = dat, variable.name = "Region and Country", the.values = expression(sprintf('%s, USA', Region)), specification = "by.expression")

test_that("dt.define.variable works", {
  expect_equal(dt.define.variable1[`Age Decade`!=floor(get(age.name) / 10),.N], 0)
  expect_equal(dt.define.variable2[`Income in Thousands`!=floor(get(income.name) / 10^3),.N], 0)
  expect_equal(dt.define.variable3[`Income in Thousands`!=floor(get(income.name) / 10^3),.N], 0)
  expect_equal(dt.define.variable4[`Region and Country`!=sprintf('%s, USA', get(region.name)),.N], 0)
})
