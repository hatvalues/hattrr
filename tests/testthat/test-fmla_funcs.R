nims <- c("x1", "x2", "x3")
dframe <- data.frame(c(1, 1, -1), c("a", "b", "c"), runif(3), rnorm(3))
names(dframe) <- c("y", nims)

fmla <- fmla_from_names("y", nims)
test_that("fmla is valid fmla", {
  expect_s3_class(fmla, "formula")
})
test_that("fmla is valid fmla", {
  expect_s3_class(lm(fmla, dframe), "lm")
})


fmla <- fmla_from_dframe("y", dframe)
test_that("fmla is valid fmla", {
  expect_s3_class(fmla, "formula")
})
test_that("fmla is valid fmla", {
  expect_s3_class(lm(fmla, dframe), "lm")
})





