context("dt.count")

age.name = "Age"
income.name = "Income"
region.name = "Region"
gender.name = "Gender"

library(formulaic)

dat = formulaic::snack.dat

dt.count0 = dt.count(dat = dat)

dt.count1 = dt.count(dat = dat, grouping.variables = c(region.name, gender.name))

dt.count2 = dt.count(dat = dat, the.filter = "Age > 65", grouping.variables = c(region.name, gender.name), count.name = "Records with Age > 65") 

test_that("dt.count works", {
  expect_equal(dt.count0$N, 23000)
  
  expect_equal(dt.count1$Region, c("Midwest","Midwest","Northeast","Northeast","South","South","West","West"))
  expect_equal(dt.count1$Gender, c("Female", "Male","Female", "Male","Female", "Male","Female", "Male"))
  expect_equal(dt.count1$N, c(2139,2093,3588,3335,1679,1541,4278,4347))
  
  expect_equal(dt.count2$Region, c("Midwest","Midwest","Northeast","Northeast","South","South","West","West"))
  expect_equal(dt.count2$Gender, c("Female", "Male","Female", "Male","Female", "Male","Female", "Male"))
  expect_equal(dt.count2$`Records with Age > 65`, c(736,897,1403,1265,575,483,1564,1518))
  
})
