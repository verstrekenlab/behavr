#' Join data and metadata
#'
#' This function joins the data of a [behavr] table to its own metadata.
#' When dealing with large data sets, it is preferable to keep metadata and data separate
#' until a summary of data is computed.
#' Indeed, joining many metavariables to
#' very long time series may result in unnecessary -- and prohibitively -- large memory footprint.
#' @inheritParams meta
#' @return a [data.table]
#' @examples
#' set.seed(1)
#' met <- data.table::data.table(id = 1:5,
#'                               condition = letters[1:5],
#'                               sex = c("M", "M", "M", "F", "F"),
#'                               key = "id")
#' data <- met[,
#'              list(t = 1L:100L,
#'                   x = rnorm(100),
#'                   y = rnorm(100),
#'                   eating = runif(100) > .5 ),
#'              by = "id"]
#'
#' d <- behavr(data, met)
#' summary_d <- d[, .(test = mean(x)), by = id]
#' rejoin(summary_d)
#' @seealso
#' * [behavr] -- to formally create a behavr object
#' @export
rejoin <- function(x){
 if(!is.behavr(x))
   stop("x is not a behavr table")
 check_conform(x)
 data.table::as.data.table(meta(x)[x])
}


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
  data_colnames <- colnames(b)
  data <- dt[, data_colnames, with=F]
  metadata <- dt[, metadata_colnames, with=F]
  metadata <- metadata[!duplicated(id), ]

  data.table::setkeyv(data, data.table::key(b))
  data.table::setkeyv(metadata, data.table::key(behavr::meta(b)))
  behavr::setmeta(data, metadata)
  return(data)
}

