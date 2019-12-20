context("dt.last.k.rows")

library(formulaic)

id.name = "User ID"
age.name = "Age"
product.name = "Product"
gender.name  = "Gender"
region.name = "Region"


dat = formulaic::snack.dat

dt.last.k.rows1 = dt.last.k.rows(dat = formulaic::snack.dat, 
k = 2, 
the.variables = c(id.name, age.name, product.name), 
grouping.variables = gender.name, 
grouping.type = "by")

dt.last.k.rows2 = dt.last.k.rows(dat = formulaic::snack.dat, 
k = 1, 
the.variables = c(id.name, age.name, product.name), 
grouping.variables = c(gender.name, region.name), 
grouping.type = "keyby")


test_that("dt.count works", {
  expect_equal(sum(dt.last.k.rows1$Gender != c("Male","Male","Female","Female")), 0)
  expect_equal(sum(dt.last.k.rows1$`User ID` != c("id_999", "id_1000","id_994", "id_998")), 0)
  expect_equal(sum(dt.last.k.rows1$Age != c(89,36,42,36)), 0)
  expect_equal(sum(dt.last.k.rows1$Product != c("Chippy_Cheese","Chippy_Cheese","Chippy_Cheese","Chippy_Cheese")), 0)
  expect_equal(sum(dt.last.k.rows2$Gender != c("Female","Female","Female","Female","Male","Male","Male","Male")), 0)
  expect_equal(sum(dt.last.k.rows2$Region != c("Midwest","Northeast","South","West","Midwest","Northeast","South","West")), 0)
  expect_equal(sum(dt.last.k.rows2$`User ID` != c("id_992", "id_998","id_980", "id_991","id_997", "id_995","id_952", "id_1000")), 0)
  expect_equal(sum(dt.last.k.rows2$Age != c(40,36,60,37,54,81,73,36)), 0)
  expect_equal(sum(dt.last.k.rows2$Product != c("Chippy_Cheese","Chippy_Cheese","Chippy_Cheese","Chippy_Cheese","Chippy_Cheese","Chippy_Cheese","Chippy_Cheese","Chippy_Cheese")), 0)
})
