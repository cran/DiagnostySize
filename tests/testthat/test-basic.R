test_that("package loads", {
  expect_true(requireNamespace("DiagnostySize", quietly = TRUE))
})
test_that("no return value",{
  expect_null(run_app())
})
test_that("no parameters accepted",{
  expect_error(run_app(2))
})
test_that("no parameters accepted",{
  expect_error(run_app("test"))
})
