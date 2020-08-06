#' Merge several behavr tables into a single dataframe
#'
#' This function merges two behavr tables,
#' represented by x and y, in such a way
#' that x gains all the columns in y not present in x
#' The merge is performed using the t column
#' @export
merge_behavr <- function(x, y) {
  columns_x <- colnames(x)
  stopifnot("t" %in% colnames(x))
  stopifnot("t" %in% colnames(y))
  data <- merge(x, y[, c("t", setdiff(colnames(y), columns_x)), with = F], by = "t")
  meta_x <- x[, meta = T]
  meta_y <- y[, meta = T]
  metadata <- data.table::as.data.table(dplyr::full_join(meta_x, meta_y))
  setkey(data, id)
  setkey(metadata, id)

  setmeta(data, metadata)

  return(data)
}



#' @export
merge_behavr_all <- function(x, y) {

  data <- purrr::map(x[, unique(id)], function(i) {
    merged <- merge_behavr(x[id == i], y[id == i])

    metadata_x <- x[, meta = T][id == i]
    metadata_y <- y[, meta = T][id == i]
    metadata <- merge(
      metadata_x,
      metadata_y[, c("id", setdiff(colnames(metadata_y), colnames(metadata_x))), with = F],
      by = "id"
    )
    if (!"id" %in% colnames(merged)) {
      merged$id <- i
    }
    setkey(merged, id)
    setmeta(merged, metadata)
  })

  bind_behavr_list(data)

}

#' Convenience wrapper around bin_apply_all and merge_behavr_all
#'
#' Bin variable `y` along variable `x` on bins of length `x_bin_length`
#' using function `summary_FUN`.
#'
#' @importFrom purrr map
#' @export
bin_all <- function(data, x="t", y, x_bin_length = 30*60, FUN = mean, keep_columns=NULL) {


  # TODO Figure out how to do it with ...
  data <- purrr::map(
    y,
    ~bin_apply_all(
      data = data,
      y = .,
      x = x,
      x_bin_length = x_bin_length,
      FUN = FUN,
      keep_columns = keep_columns
    )
  )

  sapply(data, function(x) "t" %in% colnames(x))
  # if more than one variable was passed,
  # merge the results
  # all together
  if (all(sapply(data, function(x) inherits(x, "behavr"))))
    data <- Reduce(x = data, f = merge_behavr_all)
  else if (any(sapply(data, function(x) inherits(x, "behavr"))))
    stop("Some entries are of class behavr and some others are not")
  else if (length(data) > 1)
    data <- do.call(dplyr::full_join, data)
  else
    data <- data[[1]]

  return(data)
}

# fslbehavr::bin_apply(laptop_test.csv, "asleep", x_bin_length = 2419200, FUN = mean)
