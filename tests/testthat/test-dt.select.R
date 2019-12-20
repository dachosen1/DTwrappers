context("dt.select")

library(formulaic)

id.name = 'User ID'
awareness.name = 'Awareness'
consideration.name = 'Consideration'
consumption.name = 'Consumption'
satisfaction.name = 'Satisfaction'
advocacy.name = 'Advocacy'
gender.name = 'Gender'


dat = formulaic::snack.dat

dt.select0 = dt.select(dat = dat, 
                      the.variables = c(id.name, awareness.name))

dt.select1 = dt.select(dat = dat, 
                       the.filter = "Age > 65 & Region == 'Northeast' & Product == 'Tiramisoup' & Awareness == 1",
                       the.variables = c(consideration.name, consumption.name, satisfaction.name, advocacy.name),
                       grouping.variables = c(gender.name))

dt.select2 = dt.select(dat = dat,
                       the.filter = "Age > 65 & Region == 'Northeast' & Product == 'Tiramisoup' & Awareness == 1",
                       the.variables = c(consideration.name, consumption.name, satisfaction.name, advocacy.name),
                       grouping.variables = c(gender.name), 
                       first.k = 2)

dt.select3 = dt.select(dat = dat,
                       the.filter = "Age > 65 & Region == 'Northeast' & Product == 'Tiramisoup' & Awareness == 1",
                       the.variables = c(consideration.name, consumption.name, satisfaction.name, advocacy.name),
                       grouping.variables = c(gender.name), 
                       last.k = 2)

dt.select4 = dt.select(dat = dat,
                       the.filter = "Age > 65 & Region == 'Northeast' & Product == 'Tiramisoup' & Awareness == 1",
                       the.variables = c(consideration.name, consumption.name, satisfaction.name, advocacy.name),
                       grouping.variables = c(gender.name), 
                       first.k = 2,
                       last.k = 2)

dt.select5 = dt.select(dat = dat,
                       the.filter = "Age > 65 & Region == 'Northeast' & Product == 'Tiramisoup' & Awareness == 1",
                       the.variables = c(consideration.name, consumption.name, satisfaction.name, advocacy.name),
                       grouping.variables = c(gender.name), 
                       row.indices = 7:9)

test_that("dt.select works", {
  expect_equal(sum(!names(dt.select0)%in%c(id.name, awareness.name)),0)
  expect_equal(sum(!names(dt.select1)%in%c(gender.name,consideration.name, consumption.name, satisfaction.name, advocacy.name)), 0)
  expect_equal(sum(!names(dt.select2)%in%c(gender.name,consideration.name, consumption.name, satisfaction.name, advocacy.name)), 0)
  expect_equal(nrow(dt.select2), 4)
  expect_equal(sum(!names(dt.select3)%in%c(gender.name,consideration.name, consumption.name, satisfaction.name, advocacy.name)), 0)
  expect_equal(nrow(dt.select3), 4)
  expect_equal(sum(!names(dt.select4)%in%c(gender.name,consideration.name, consumption.name, satisfaction.name, advocacy.name)), 0)
  expect_equal(nrow(dt.select4), 4)
  expect_equal(sum(!names(dt.select5)%in%c(gender.name,consideration.name, consumption.name, satisfaction.name, advocacy.name)), 0)
  expect_equal(nrow(dt.select5), 4)
  
})
