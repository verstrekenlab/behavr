test_that("merge_behavr works", {
  original <- toy_activity_data(data.frame(id = 1), duration = days(1))[1:10,]
  x <- copy(original)[, c("id", "t", "moving")]
  y <- copy(original)[, c("id", "t", "asleep")]
  derived <- merge_behavr(x , y)
  expect_true(identical(
    derived,
    original
  ))

})


test_that("merge_behavr_all works", {
  original <- toy_activity_data(data.frame(id = 1:3), duration = days(1))[1:20]
  metadata <- data.table(id = 1:2)

  original[11:20, id:=2]
  data.table::setkey(original, id)
  data.table::setkey(metadata, id)
  setmeta(original, metadata)

  x <- copy(original)[, c("id", "t", "moving")]
  y <- copy(original)[, c("id", "t", "asleep")]
  derived <- merge_behavr_all(x, y)

  expect_true(identical(derived, original))
})
