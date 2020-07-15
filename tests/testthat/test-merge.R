test_that("merge_behavr works", {
  original <- toy_activity_data(data.frame(id = 1), duration = days(1))
  x <- copy(original)[, c("id", "t", "asleep")]
  y <- copy(original)[, c("id", "t", "moving")]
  derived <- merge_behavr(x , y)
  expect_true(all(derived[, colnames(original), with = F] == original))

})


test_that("merge_behavr_all works", {
  original <- toy_activity_data(data.frame(id = 1:3), duration = days(1))
  x <- copy(original)[, c("id", "t", "asleep")]
  y <- copy(original)[, c("id", "t", "moving")]
  derived <- merge_behavr_all(x, y)

  expect_true(all(derived[, colnames(original), with = F] == original))
})
