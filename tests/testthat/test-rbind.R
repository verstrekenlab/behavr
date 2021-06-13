test_that("rbind works as expected", {

  data1 <- behavr:::toy_ethoscope_data()[t < 6,]
  data2 <- behavr:::toy_ethoscope_data()[t < 6,]
  data2$id <- "toy_data_2"
  metadata2 <- meta(data2)
  metadata2$id <- "toy_data_2"
  data.table::setkey(data2, id)
  data.table::setkey(metadata2, id)
  behavr::setmeta(data2, metadata2)

  data <- behavr::rbind_behavr(data1, data2)

  expect_equal(nrow(data), 24)
  expect_true(all(unique(data$id) == c("toy_data", "toy_data_2")))
})


test_that("rbind fails as expected", {

  data1 <- behavr:::toy_ethoscope_data()[t < 6,]
  data2 <- behavr:::toy_ethoscope_data()[t < 6,]
  data2$id <- "toy_data_2"
  data2$missing_column <- TRUE
  metadata2 <- meta(data2)
  metadata2$id <- "toy_data_2"
  data.table::setkey(data2, id)
  data.table::setkey(metadata2, id)
  behavr::setmeta(data2, metadata2)

  # this test is not  really catching my custom error message,
  # but another one produced by R anyway
  expect_condition(data <- behavr::rbind_behavr(data1, data2))
})
