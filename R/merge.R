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


