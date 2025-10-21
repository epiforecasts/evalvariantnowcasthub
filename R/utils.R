#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' result <- quiet(message("This message should be suppressed"))
#' print(result)
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}

#' Extract contexts from a zip file
#' @param url Link to zip file.
#' @return data in file
open_zip <- function(url) {
  tmp_file <- tempfile()
  utils::download.file(url, tmp_file)
  latest_data <- readr::read_tsv(gzfile(tmp_file))
  rm(tmp_file)
  return(latest_data)
}
