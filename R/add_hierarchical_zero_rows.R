#' Add Zero-Count Rows for Unobserved Hierarchical Levels
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' Stacked hierarchical ARDs created with [ard_stack_hierarchical()] include only
#' the levels observed in the data, because the underlying tabulation routes
#' through the `strata` (observed-only) branch of the engine. Predefined
#' categories -- SMQ/CQ baskets, SOCs, preferred terms, grade scales -- therefore
#' disappear from the ARD instead of appearing with a count of zero.
#'
#' `add_hierarchical_zero_rows()` restores those rows. A single `mapping`
#' argument describes the expected universe of levels and covers both scenarios
#' that arise in practice:
#'
#' - a **top-level** category that is never observed (e.g. an SOC with no events),
#' - a **nested** child that is never observed under an otherwise present parent
#'   (e.g. a preferred term with no events within an observed SOC).
#'
#' A nested child has no unambiguous parent when it is absent, so the expected
#' parent/child structure must be supplied explicitly through `mapping` rather
#' than inferred from factor levels.
#'
#' @param x (`card`)\cr
#'   a stacked hierarchical ARD of class `'card'` created with
#'   [ard_stack_hierarchical()] or [ard_stack_hierarchical_count()].
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   the hierarchical variables used to create `x`, in the same order. The first
#'   variable is the top level; the second, when present, is the nested child.
#' @param mapping (named `list` or `data.frame`)\cr
#'   the expected universe of levels. Either
#'   - a **named list** mapping each parent level to a character vector of its
#'     expected child levels, e.g.
#'     `list("SOC A" = c("PT1", "PT2"), "SOC B" = "PT3")`, or
#'   - a **two-column data frame** whose columns are named after the first two
#'     `variables`, e.g. `data.frame(AESOC = ..., AEDECOD = ...)`, listing every
#'     valid parent/child combination.
#'
#'   Parents present in `mapping` but absent from `x` are added as top-level
#'   zero-rows together with their mapped children. Children present in `mapping`
#'   but absent under an observed parent are added as nested zero-rows.
#' @param statistic (`character`)\cr
#'   the statistics to set to zero on the added rows. Statistics not listed are
#'   carried over from a matching observed row (so denominators such as `N`
#'   remain correct). Defaults to `c("n", "p", "n_cum", "p_cum")`.
#'
#' @return an ARD data frame of class 'card'
#' @seealso [ard_stack_hierarchical()], [sort_ard_hierarchical()]
#' @name add_hierarchical_zero_rows
#'
#' @examples
#' set.seed(1)
#' adae <- data.frame(
#'   USUBJID = sprintf("S%03d", 1:20),
#'   SOC = factor(sample(c("Cardiac", "GI"), 20, TRUE), levels = c("Cardiac", "GI")),
#'   PT = sample(c("PT1", "PT2"), 20, TRUE)
#' )
#'
#' ard <- ard_stack_hierarchical(
#'   adae,
#'   variables = c(SOC, PT),
#'   id = USUBJID,
#'   denominator = data.frame(USUBJID = sprintf("S%03d", 1:30))
#' )
#'
#' # add an unobserved SOC ("Vascular") and an unobserved PT under "Cardiac"
#' ard |>
#'   add_hierarchical_zero_rows(
#'     variables = c(SOC, PT),
#'     mapping = list(
#'       Cardiac = c("PT1", "PT2", "PT3"),
#'       GI = c("PT1", "PT2"),
#'       Vascular = "PTX"
#'     )
#'   )
NULL

#' @rdname add_hierarchical_zero_rows
#' @export
add_hierarchical_zero_rows <- function(x,
                                       variables,
                                       mapping,
                                       statistic = c("n", "p", "n_cum", "p_cum")) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(variables)
  check_not_missing(mapping)
  check_class(x, "card")
  check_class(x, "ard_stack_hierarchical")
  if (!is.character(statistic)) {
    cli::cli_abort(
      "The {.arg statistic} argument must be a {.cls character} vector.",
      call = get_cli_abort_call()
    )
  }

  # `variables` is tidy-selected against the ARD's own variable column so the
  # helper accepts the same style of input as ard_stack_hierarchical()
  var_universe <- unique(x[["variable"]])
  scaffold <- as.data.frame(
    stats::setNames(rep(list(logical(0)), length(var_universe)), var_universe)
  )
  process_selectors(scaffold, variables = {{ variables }})

  if (!is.list(mapping) && !is.data.frame(mapping)) {
    cli::cli_abort(
      "The {.arg mapping} argument must be a named {.cls list} or a {.cls data.frame}.",
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
  # its level, optionally setting the hierarchical parent, and zeroing statistics
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
    is_zero <- template[["stat_name"]] %in% statistic
    template[["stat"]][is_zero] <- as.list(rep(0, sum(is_zero)))
    if ("warning" %in% names(template)) template[["warning"]] <- rep(list(NULL), nrow(template))
    if ("error" %in% names(template)) template[["error"]] <- rep(list(NULL), nrow(template))
    template
  }

  # observed top-level values and the expected universe from `mapping`
  observed_top <- unique(level_chr(x[["variable_level"]][x[["variable"]] == top_var]))
  expected_top <- .zero_rows_expected_top(mapping, top_var)
  missing_top <- setdiff(expected_top, observed_top)

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

  # top-level completion plus the children of any missing parent
  for (lvl in missing_top) {
    new_blocks <- c(new_blocks, list(build_block(blueprint_top, NULL, top_var, lvl)))
    if (!is.na(child_var)) {
      for (kid in .zero_rows_children(mapping, lvl, top_var, child_var)) {
        new_blocks <- c(new_blocks, list(build_block(blueprint_child, lvl, child_var, kid)))
      }
    }
  }

  # nested completion: observed parent, unobserved child
  if (!is.na(child_var) && !is.na(parent_level_col)) {
    for (parent in observed_top) {
      expected_kids <- .zero_rows_children(mapping, parent, top_var, child_var)
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
