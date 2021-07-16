
#' Make a writable behavr object
#' @param data A rejoined behavr or any data.table object
#' @param meta If TRUE, repeat also for the metadata
#' @return A behavr table with no columns of type list
#' TODO Possibly this should be part of a fwrite_behavr function in behavr
#' @importFrom dplyr pull
#' @export
simplify_behavr <- function(data, meta = FALSE) {

  if(meta) {
    metadata <- data[, meta = T]
    metadata <- simplify_behavr(metadata, FALSE)
    behavr::setmeta(data, metadata)
  }

  types <- sapply(1:ncol(data), function(i) {
    column_name <- colnames(data)[i]
    res <- c(is.list(data[[i]]))
    names(res) <- column_name
    res
  })

  if (sum(types) != 0) {
    for (column in which(types)) {

      single_column <- dplyr::pull(data[, column, with = F])
      single_column <- sapply(single_column, function(x) x[[1]])
      cmd <- sprintf("%s := single_column", names(types)[column])
      data[, eval(parse(text = cmd))]
    }
  }
  return(data)
}
