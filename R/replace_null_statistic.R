#' Replace NULL Statistics with Specified Value
#'
#' When a statistical summary function errors, the `"stat"` column will be
#' `NULL`. It is, however, sometimes useful to replace these values with a
#' non-`NULL` value, e.g. `NA`.
#'
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param value (usually a `scalar`)\cr
#'   The value to replace `NULL` values with. Default is `NA`.
#' @param rows ([`data-masking`][rlang::args_data_masking])\cr
#'   Expression that return a logical value, and are defined in terms of the variables in `.data`.
#'   Only rows for which the condition evaluates to `TRUE` are replaced.
#'   Default is `TRUE`, which applies to all rows.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' # the quantile functions error because the input is character, while the median function returns NA
#' data.frame(x = rep_len(NA_character_, 10)) |>
#'   ard_summary(
#'     variables = x,
#'     statistic = ~ continuous_summary_fns(c("median", "p25", "p75"))
#'   ) |>
#'   replace_null_statistic(rows = !is.null(error))
replace_null_statistic <- function(x, value = NA, rows = TRUE) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(x, "card")

  # replace NULL values --------------------------------------------------------
  # base R lapply instead of dplyr::rowwise + dplyr::mutate
  # (rowwise mutate creates a DataMask per row, which is very expensive)
  rows_mask <- rlang::eval_tidy(rlang::enquo(rows), data = x)
  if (is.logical(rows_mask) && length(rows_mask) == 1L) {
    rows_mask <- rep(rows_mask, nrow(x))
  }
  stat_col <- x[["stat"]]
  for (i in which(rows_mask)) {
    if (is.null(stat_col[[i]])) {
      stat_col[[i]] <- value
    }
  }
  x[["stat"]] <- stat_col

  # restore previous grouping structure and original class of x
  x |>
    dplyr::group_by(dplyr::pick(dplyr::group_vars(x))) |>
    structure(class = class(x))
}
