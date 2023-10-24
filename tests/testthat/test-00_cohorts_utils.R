test_that("Basic Test", {
  served <- data.frame(
    PersonalID = c(1, 2, 3, 4, 5, 6),
    ProjectName = c("A", "A", "B", "B", "C", "C")
  )
  result <- chrt_hoh_count(served)
  expect_s3_class(result, "data.frame")
  expect_identical(result$ProjectName, c("A", "B", "C"))
  expect_identical(result$served, c(2L, 2L, 2L))
})

test_that("No Duplicates in Input", {
  served <- data.frame(
    PersonalID = c(1, 2, 3),
    ProjectName = c("A", "B", "C")
  )
  result <- chrt_hoh_count(served)
  expect_s3_class(result, "data.frame")
  expect_identical(result$ProjectName, c("A", "B", "C"))
  expect_identical(result$served, c(1L, 1L, 1L))
})

