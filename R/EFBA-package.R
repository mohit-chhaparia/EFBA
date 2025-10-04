#' EFBA: Empirical Frequency Band Analysis
#'
#' Tools for empirical frequency-band analysis of non-stationary time series.
#' These imports make base plotting and IO functions visible to the package,
#' removing "no visible global function definition" NOTES.
#'
#' @keywords internal
#' @useDynLib EFBA, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom graphics abline par plot.new
#' @importFrom grDevices pdf dev.off gray rgb
#' @importFrom utils read.csv
"_PACKAGE"
