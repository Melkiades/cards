#' Add Unobserved Levels to Hierarchical ARDs
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' A stacked hierarchical ARD keeps only the levels seen in the data, so a
#' category that never occurs (an SOC with no events, a preferred term absent
#' under an observed SOC, an unused grade) simply drops out instead of showing
#' up with a count of zero.
#'
#' `add_hierarchical_unobserved_levels()` puts those rows back. Name the
#' hierarchical variable(s) to complete and the missing levels are added with a
#' count of zero; proportions are left as `NaN`, since a never-observed level has
#' no one at risk (`0 / 0` is undefined) and should be recoded for display rather
#' than asserted as zero here. The expected levels are taken from the variable's
#' factor `levels()`, which the ARD already stores, so no reference data is
#' needed.
#'
#' @param x (`card`)\cr
#'   a stacked hierarchical ARD created with [ard_stack_hierarchical()].
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   hierarchical variable(s) to complete, in hierarchy order. Use a single
#'   variable (e.g. `variables = AESOC`) to complete the top level, or the full
#'   set (e.g. `variables = c(AESOC, AEDECOD)`) to also fill missing children
#'   under each observed parent.
#' @param mapping (named `list` or `data.frame`)\cr
#'   optional. Only needed to add children under a parent that is itself
#'   unobserved -- factor levels cannot say which children belong there. Supply
#'   either a named list, `list("SOC A" = c("PT1", "PT2"))`, or a two-column data
#'   frame whose columns are named after the parent and child variables.
#'
#' @return a stacked hierarchical ARD
#' @seealso [gtsummary::tbl_hierarchical()], [ard_stack_hierarchical()], [sort_ard_hierarchical()]
#' @name add_hierarchical_unobserved_levels
#'
#' @examples
#' set.seed(1)
#' adae <- data.frame(
#'   USUBJID = sprintf("S%03d", 1:20),
#'   AESOC = factor(sample(c("Cardiac", "GI"), 20, TRUE), levels = c("Cardiac", "GI", "Vascular")),
#'   AEDECOD = factor(sample(c("PT1", "PT2"), 20, TRUE), levels = c("PT1", "PT2", "PT3"))
#' )
#'
#' ard <- ard_stack_hierarchical(
#'   adae,
#'   variables = c(AESOC, AEDECOD),
#'   id = USUBJID,
#'   denominator = data.frame(USUBJID = sprintf("S%03d", 1:30))
#' )
#'
#' # complete the top level: the unobserved SOC "Vascular" is added as a zero-row
#' ard |>
#'   add_hierarchical_unobserved_levels(variables = AESOC)
#'
#' # complete both levels: also fill the missing PT ("PT3") under each observed SOC
#' ard |>
#'   add_hierarchical_unobserved_levels(variables = c(AESOC, AEDECOD))
#'
#' # `mapping` names the children to add under the unobserved parent "Vascular"
#' ard |>
#'   add_hierarchical_unobserved_levels(
#'     variables = c(AESOC, AEDECOD),
#'     mapping = list(Vascular = c("PT1", "PT2"))
#'   )
NULL

# count statistics set to zero on an added level. Proportions are left as `NaN`
# (a never-observed level has no one at risk, so `0 / 0` is undefined) and are
# recoded for display downstream rather than being asserted as zero here
.hierarchical_zero_stats <- c("n", "n_cum")
.hierarchical_nan_stats <- c("p", "p_cum")

#' @rdname add_hierarchical_unobserved_levels
#' @export
add_hierarchical_unobserved_levels <- function(x, variables, mapping = NULL) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(variables)
  check_class(x, "card")
  check_class(x, "ard_stack_hierarchical")

  # `variables` is tidy-selected against the ARD's own variable column so the
  # helper accepts the same style of input as ard_stack_hierarchical()
  var_universe <- unique(x[["variable"]])
  scaffold <- as.data.frame(
    stats::setNames(rep(list(logical(0)), length(var_universe)), var_universe)
  )
  process_selectors(scaffold, variables = {{ variables }})

  if (!is.null(mapping) && !is.list(mapping) && !is.data.frame(mapping)) {
    cli::cli_abort(
      "The {.arg mapping} argument must be {.code NULL}, a named {.cls list}, or a {.cls data.frame}.",
      call = get_cli_abort_call()
    )
  }

  top_var <- variables[1L]
  child_var <- if (length(variables) >= 2L) variables[2L] else NA_character_

  # helper: first level value from a list-column (`variable_level`, `groupN_level`)
  level_chr <- function(col) {
    vapply(
      col,
      function(z) {
        z <- as.character(z)
        if (length(z)) z[[1L]] else NA_character_
      },
      character(1L)
    )
  }

  # helper: the factor levels stored in a list-column, if any. The ARD keeps the
  # full factor (including unobserved levels) inside each list element, so the
  # expected universe can be recovered without the original data.
  level_universe <- function(col) {
    for (z in col) {
      if (is.factor(z)) {
        return(levels(z))
      }
    }
    NULL
  }

  # the hierarchical parent of a nested variable is stored in the last populated
  # `groupN` column: without a `by` the top variable has no group columns and the
  # child's parent is `group1`; with a `by` the arm occupies `group1` and the
  # parent shifts to `group2`. Detect the child's parent group column from data.
  child_rows <- if (!is.na(child_var)) x[x[["variable"]] == child_var, ] else x[0, ]
  parent_group_col <- NA_character_
  if (nrow(child_rows) > 0L) {
    group_cols <- grep("^group[0-9]+$", names(x), value = TRUE)
    for (gc in group_cols) {
      if (any(as.character(child_rows[[gc]]) == top_var, na.rm = TRUE)) {
        parent_group_col <- gc
        break
      }
    }
  }
  parent_level_col <- if (!is.na(parent_group_col)) paste0(parent_group_col, "_level") else NA_character_

  # build a zero-row block from an observed template, overriding the variable and
  # its level, optionally setting the hierarchical parent, and zeroing counts
  build_block <- function(template, parent_level, variable, level) {
    if (nrow(template) == 0L) {
      return(template)
    }
    template[["variable"]] <- variable
    template[["variable_level"]] <- rep(list(level), nrow(template))
    if (!is.null(parent_level) && !is.na(parent_group_col)) {
      template[[parent_group_col]] <- top_var
      template[[parent_level_col]] <- rep(list(parent_level), nrow(template))
    }
    is_zero <- template[["stat_name"]] %in% .hierarchical_zero_stats
    template[["stat"]][is_zero] <- as.list(rep(0, sum(is_zero)))
    is_nan <- template[["stat_name"]] %in% .hierarchical_nan_stats
    template[["stat"]][is_nan] <- as.list(rep(NaN, sum(is_nan)))
    if ("warning" %in% names(template)) template[["warning"]] <- rep(list(NULL), nrow(template))
    if ("error" %in% names(template)) template[["error"]] <- rep(list(NULL), nrow(template))
    template
  }

  # observed top-level values and the expected universe. Without a `mapping` the
  # universe is the top variable's factor levels stored in the ARD; a `mapping`
  # overrides that (and can introduce parents the factor levels do not contain).
  observed_top <- unique(level_chr(x[["variable_level"]][x[["variable"]] == top_var]))
  top_factor_levels <- level_universe(x[["variable_level"]][x[["variable"]] == top_var])
  expected_top <- if (is.null(mapping)) {
    top_factor_levels %||% observed_top
  } else {
    union(.zero_rows_expected_top(mapping, top_var), observed_top)
  }
  missing_top <- setdiff(expected_top, observed_top)

  # child factor levels stored in the ARD, used when `mapping` is NULL
  child_factor_levels <- if (!is.na(child_var)) {
    level_universe(child_rows[["variable_level"]])
  } else {
    NULL
  }

  # blueprint rows carry the correct stat structure (n/N/p, by-groups, fmt_fun).
  # one blueprint per `by`-group is preserved by taking all rows of one level.
  blueprint_top <- x[x[["variable"]] == top_var & level_chr(x[["variable_level"]]) == observed_top[1L], ]
  # a child blueprint spans one child level under one parent, across all
  # `by`-groups; the parent level is overwritten per added row
  blueprint_child <- if (nrow(child_rows) > 0L) {
    first_child <- level_chr(child_rows[["variable_level"]])[1L]
    child_one <- child_rows[level_chr(child_rows[["variable_level"]]) == first_child, ]
    if (!is.na(parent_level_col)) {
      first_parent <- level_chr(child_one[[parent_level_col]])[1L]
      child_one[level_chr(child_one[[parent_level_col]]) == first_parent, ]
    } else {
      child_one
    }
  } else {
    x[0, ]
  }

  new_blocks <- list()

  # top-level completion plus the children of any missing parent. Without a
  # `mapping`, factor levels cannot say which children belong under an unobserved
  # parent, so such a parent is added at the top level only.
  for (lvl in missing_top) {
    new_blocks <- c(new_blocks, list(build_block(blueprint_top, NULL, top_var, lvl)))
    if (!is.na(child_var) && !is.null(mapping)) {
      for (kid in .zero_rows_children(mapping, lvl, top_var, child_var)) {
        new_blocks <- c(new_blocks, list(build_block(blueprint_child, lvl, child_var, kid)))
      }
    }
  }

  # nested completion: observed parent, unobserved child. Expected children come
  # from `mapping` when supplied, otherwise from the child's factor levels.
  if (!is.na(child_var) && !is.na(parent_level_col)) {
    for (parent in observed_top) {
      expected_kids <- if (is.null(mapping)) {
        child_factor_levels %||% character(0L)
      } else {
        .zero_rows_children(mapping, parent, top_var, child_var)
      }
      observed_kids <- unique(level_chr(
        child_rows[["variable_level"]][level_chr(child_rows[[parent_level_col]]) == parent]
      ))
      for (kid in setdiff(expected_kids, observed_kids)) {
        new_blocks <- c(new_blocks, list(build_block(blueprint_child, parent, child_var, kid)))
      }
    }
  }

  if (length(new_blocks) == 0L) {
    return(x)
  }

  out <- dplyr::bind_rows(x, dplyr::bind_rows(new_blocks))
  class(out) <- class(x)
  out
}

# expected top-level values from a list (its names) or data.frame (first column)
.zero_rows_expected_top <- function(mapping, top_var) {
  if (is.data.frame(mapping)) {
    if (!top_var %in% names(mapping)) {
      cli::cli_abort(
        "A {.cls data.frame} {.arg mapping} must contain a column named {.val {top_var}}.",
        call = get_cli_abort_call()
      )
    }
    unique(as.character(mapping[[top_var]]))
  } else {
    names(mapping)
  }
}

# expected child levels for a parent from a list or data.frame mapping
.zero_rows_children <- function(mapping, parent, top_var, child_var) {
  if (is.data.frame(mapping)) {
    if (!child_var %in% names(mapping)) {
      cli::cli_abort(
        "A {.cls data.frame} {.arg mapping} must contain a column named {.val {child_var}}.",
        call = get_cli_abort_call()
      )
    }
    as.character(unique(mapping[[child_var]][as.character(mapping[[top_var]]) == parent]))
  } else {
    as.character(mapping[[parent]] %||% character(0L))
  }
}
