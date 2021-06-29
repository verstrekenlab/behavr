
#' Recreate a behavr table that has been rejoined (dt) based on the column names in b
#' @rdname rejoin
#' @param b Reference behavr dataset
#' @param dt data.table resulting from a previously rejoined behavr
#' @details This function is useful if dt has undergone some processing / filtering
#' in its data.table form but we want to recover again its behavr original format
#' @export
#' @examples
#' # add some meta variable to the metadata
#' # to showcase that the metadata columns are also recovered
#' dt <- behavr::toy_ethoscope_data()
#' metadata <- behavr::meta(dt)
#' metadata[, metavar := "C"]
#' behavr::setmeta(dt, metadata)
#' dt_rejoined <- behavr::rejoin(dt)
#' dt_unjoin  <- behavr::split(dt_rejoined)
#' @export
unjoin <- function(b, dt) {

  metadata_colnames <- colnames(behavr::meta(b))
  data_colnames <- colnames(b)[colnames(b) %in% colnames(dt)]
  data <- dt[, data_colnames, with=F]
  metadata <- dt[, metadata_colnames, with=F]
  metadata <- metadata[!duplicated(id), ]

  data.table::setkeyv(data, data.table::key(b))
  data.table::setkeyv(metadata, data.table::key(behavr::meta(b)))
  behavr::setmeta(data, metadata)
  return(data)
}
