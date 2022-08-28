test_that("addivtive models work", {
  data("mtcars")
  m <- lm(mpg ~ wt + cyl, data = mtcars)
  testthat::expect_s3_class(regression_plane(m), "plotly")
})


test_that("interaction models work", {
  data("mtcars")
  m <- lm(mpg ~ wt * cyl, data = mtcars)
   testthat::expect_s3_class(regression_plane(m), "plotly")
})

test_that("polynomial models work", {
  data("mtcars")
  m <- lm(mpg ~ wt * cyl + I(wt^2) + I(cyl^2), data = mtcars)
  testthat::expect_s3_class(regression_plane(m), "plotly")
})

test_that("categorical explanatory variables don't work", {
  data("mtcars")
  mtcars$am <- factor(mtcars$am)
  m <- lm(mpg ~ wt * am, data = mtcars)
  testthat::expect_error(regression_plane(m))
})

