#' Rbind two behavrs with same columns in data and metadata
#' @param x First behavr table
#' @param y Second behavr table
#' @return Rbinded behavr
#' @export
rbind_behavr <- function(x, y, rbind_meta=FALSE) {

  data1 <- x
  data2 <- y

  metadata1 <- meta(data1)
  metadata2 <- meta(data2)

  keys <- data.table::key(data1)

  if(! all(sort(colnames(data1)) == sort(colnames(data2)))) {
    message("Please ensure both behavrs have identical column names and order in data field")
    stop("")
  }
  if(! all(sort(colnames(metadata1)) == sort(colnames(metadata2)))) {
   stop("Please ensure both behavrs have identical column names and order in metadata field")
  }

  data <- rbind(data1, data2)
  data.table::setkey(data, id)

  if (rbind_meta) {
    metadata <- rbind(metadata1, metadata2)
    data.table::setkey(metadata, id)
    behavr::setmeta(data, metadata)
  } else {
    behavr::setmeta(data, metadata1)
  }

  data.table::setkeyv(data, keys)

  return(data)
}
