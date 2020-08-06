#' @importFrom data.table as.data.table setkey
#' @export
make_data_table <- function(data) {
  # data is a named list of length 1 with a single tibble
  data_name <- names(data)
  data <- data.table::as.data.table(data[[1]])
  data.table::setkey(data, id)
  data <- list(data)
  names(data) <- data_name
  return(data)
}
