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

test_that("categorical factor variables work", {
  data("mtcars")
  mtcars$am <- factor(mtcars$am)
  m <- lm(mpg ~ wt * cyl * am, data = mtcars)
  testthat::expect_s3_class(regression_plane(m), "plotly")
})

test_that("categorical logical variables work", {
  data("mtcars")
  mtcars$am <- as.logical(mtcars$am)
  m <- lm(mpg ~ wt * cyl * am, data = mtcars)
  testthat::expect_s3_class(regression_plane(m), "plotly")
})

test_that("categorical character variables work", {
  data("mtcars")
  mtcars$am <- as.character(mtcars$am)
  m <- lm(mpg ~ wt * cyl * am, data = mtcars)
  testthat::expect_s3_class(regression_plane(m), "plotly")
})

test_that("Multiple mesh_steps work", {
  data("mtcars")
  mtcars$am <- as.logical(mtcars$am)
  m <- lm(mpg ~ wt * cyl * am, data = mtcars)
  testthat::expect_s3_class(regression_plane(m, mesh = TRUE, mesh_step = c(2, 1)),
                            "plotly"
                            )
})

test_that("more than 2 numeric variables fails", {
  data("mtcars")
  m <- lm(mpg ~ wt + cyl + hp, data = mtcars)
  testthat::expect_error(regression_plane(m),
                         "Only models with two numeric variables should be visualized using a plane."
                         )
})

