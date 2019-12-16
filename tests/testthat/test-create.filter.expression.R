context("create.filter.expression")

age.name = "Age"
region.name = "Region"

test_that("create.filter.expression works", {
  expect_equal(create.filter.expression(the.filter = NULL), TRUE)
  expect_equal(create.filter.expression(the.filter = c(age.name, region.name)), expression(Age, Region))
  expect_equal(create.filter.expression(the.filter = age.name == region.name), FALSE)
  expect_equal(create.filter.expression(the.filter = "get(region.name) == 'South'"), expression(get(region.name) == 'South'))
  expect_equal(create.filter.expression(the.filter = "get(age.name) > 20"), expression(get(age.name) > 20))
  expect_equal(create.filter.expression(the.filter = "get(region.name) == 'South' & get(age.name) > 20"), expression(get(region.name) == 'South' & get(age.name) > 20))
})
