#' Merge several behavr tables into a single dataframe
#'
#' This function merges two behavr tables,
#' represented by x and y, in such a way
#' that x gains all the columns in y not present in x
#' The merge is performed using the t column
#' Both tables should refer to the same individual
#' since this function is not id-aware
#' @export
merge_behavr <- function(x, y) {
  columns_x <- colnames(x)
  stopifnot("t" %in% colnames(x))
  stopifnot("t" %in% colnames(y))

  # merge the data
  data <- merge(x, y[, c("t", setdiff(colnames(y), columns_x)), with = F], by = "t")
  data <- data[, c("id", "t", setdiff(colnames(data), c("id", "t"))), with=F]

  # merge the metadata
  meta_x <- x[, meta = T]
  meta_y <- y[, meta = T]
  metadata <- data.table::as.data.table(dplyr::full_join(meta_x, meta_y))

  # set keys
  setkey(data, id)
  setkey(metadata, id)

  # set the right order

  # restore the behavr table
  setmeta(data, metadata)
  return(data)
}

#' @export
#' @rdname merge_behavr
#' @details merge_behavr with multi individual tables
#' @export

merge_behavr_all <- function(x, y) {

  x_id_column <- data.table::key(x)
  y_id_column <- data.table::key(y)
  x$id_ <- x[, x_id_column, with=F]
  y$id_ <- y[, y_id_column, with=F]

  unique_ids <- unique(x$id_[x$id_ %in% y$id_])

  data <- purrr::map(unique_ids, function(id_value) {
     res <- merge_behavr(
       x[id_ == id_value, ],
       y[id_ == id_value, ]
     )
     res[, id_ := NULL]

  })

  merged <- bind_behavr_list(data)
  return(merged)
}

#' Convenience wrapper around bin_apply_all and merge_behavr_all
#'
#' Bin variable `y` along variable `x` on bins of length `x_bin_length`
#' using function `summary_FUN`.
#'
#' @importFrom purrr map
#' @param data A behavr table
#' @param x Name of column to make bins of, normally "t" for time
#' @param y Columns in data to compute aggregates for. Can be more than 1!
#' @export
bin_all <- function(data, x="t", y) {

  data <- purrr::map(
    y,
    ~bin_apply_all(
      data = data,
      y = .,
      x = x,
      ...
    )
  )

  # if more than one variable was passed, merge the results
  if (all(sapply(data, function(x) inherits(x, "behavr"))))
    data <- Reduce(x = data, f = merge_behavr_all)
  else if (any(sapply(data, function(x) inherits(x, "behavr"))))
    stop("Some entries are of class behavr and some others are not")
  # else if (length(data) > 1)
  #   data <- do.call(dplyr::full_join, data)
  # else
  #   data <- data[[1]]

  return(data)
}

