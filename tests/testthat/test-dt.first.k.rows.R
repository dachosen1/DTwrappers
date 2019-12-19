context("dt.first.k.rows")

library(formulaic)

id.name = "User ID"
age.name = "Age"
product.name = "Product"
gender.name  = "Gender"
region.name = "Region"


dat = formulaic::snack.dat

dt.first.k.rows1 = dt.first.k.rows(dat = formulaic::snack.dat, 
k = 2, 
the.variables = c(id.name, age.name, product.name), 
grouping.variables = gender.name, 
grouping.type = "by")

dt.first.k.rows2 = dt.first.k.rows(dat = formulaic::snack.dat, 
k = 1, 
the.variables = c(id.name, age.name, product.name), 
grouping.variables = c(gender.name, region.name), 
grouping.type = "keyby")


test_that("dt.count works", {
  expect_equal(sum(dt.first.k.rows1$Gender != c("Male","Male","Female","Female")), 0)
  expect_equal(sum(dt.first.k.rows1$`User ID` != c("id_1", "id_2","id_4", "id_6")), 0)
  expect_equal(sum(dt.first.k.rows1$Age != c(49,65,54,64)), 0)
  expect_equal(sum(dt.first.k.rows1$Product != c("Cookie_Crumble","Cookie_Crumble","Cookie_Crumble","Cookie_Crumble")), 0)
  expect_equal(sum(dt.first.k.rows2$Gender != c("Female","Female","Female","Female","Male","Male","Male","Male")), 0)
  expect_equal(sum(dt.first.k.rows2$Region != c("Midwest","Northeast","South","West","Midwest","Northeast","South","West")), 0)
  expect_equal(sum(dt.first.k.rows2$`User ID` != c("id_8", "id_14","id_10", "id_4","id_21", "id_18","id_7", "id_1")), 0)
  expect_equal(sum(dt.first.k.rows2$Age != c(66,32,81,54,57,19,49,49)), 0)
  expect_equal(sum(dt.first.k.rows2$Product != c("Cookie_Crumble","Cookie_Crumble","Cookie_Crumble","Cookie_Crumble","Cookie_Crumble","Cookie_Crumble","Cookie_Crumble","Cookie_Crumble")), 0)
})
