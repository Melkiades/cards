skip_on_cran()

# a small hierarchical ARD where "Vascular" is a declared but unobserved SOC and
# "PT3" is a declared but unobserved preferred term. Both are carried as factor
# levels so the expected universe is recoverable from the ARD alone.
make_ard <- function(by = FALSE) {
  set.seed(1)
  adae <- data.frame(
    USUBJID = sprintf("S%03d", 1:20),
    SOC = factor(sample(c("Cardiac", "GI"), 20, TRUE), levels = c("Cardiac", "GI", "Vascular")),
    PT = factor(sample(c("PT1", "PT2"), 20, TRUE), levels = c("PT1", "PT2", "PT3")),
    TRT = factor(rep(c("A", "B"), 10))
  )
  denom <- data.frame(USUBJID = sprintf("S%03d", 1:30), TRT = factor(rep(c("A", "B"), 15)))
  if (by) {
    ard_stack_hierarchical(adae, variables = c(SOC, PT), by = TRT, id = USUBJID, denominator = denom)
  } else {
    ard_stack_hierarchical(adae, variables = c(SOC, PT), id = USUBJID, denominator = denom)
  }
}

# first level value from a list-column
lvl1 <- function(col) {
  vapply(col, function(z) {
    z <- as.character(z)
    if (length(z)) z[[1L]] else NA_character_
  }, character(1L))
}

test_that("add_hierarchical_unobserved_levels(variables = SOC) completes the top level from factor levels", {
  ard <- make_ard()
  out <- add_hierarchical_unobserved_levels(ard, variables = SOC)

  expect_s3_class(out, "ard_stack_hierarchical")
  expect_setequal(
    unique(lvl1(out$variable_level[out$variable == "SOC"])),
    c("Cardiac", "GI", "Vascular")
  )
  # top-level only: no PT rows are invented under the unobserved parent
  expect_false(any(lvl1(out$group1_level[out$variable == "PT"]) == "Vascular"))
  # the added row has n = 0 and carries a real denominator N
  expect_equal(
    out$stat[out$variable == "SOC" & lvl1(out$variable_level) == "Vascular" & out$stat_name == "n"][[1L]],
    0
  )
  expect_equal(
    out$stat[out$variable == "SOC" & lvl1(out$variable_level) == "Vascular" & out$stat_name == "N"][[1L]],
    30
  )
})

test_that("add_hierarchical_unobserved_levels(variables = c(SOC, PT)) completes nested levels from factor levels", {
  ard <- make_ard()
  out <- add_hierarchical_unobserved_levels(ard, variables = c(SOC, PT))

  # top level recovered
  expect_true("Vascular" %in% lvl1(out$variable_level[out$variable == "SOC"]))
  # the unobserved PT3 is filled under each observed parent from PT's factor levels
  for (parent in c("Cardiac", "GI")) {
    expect_true(
      "PT3" %in% lvl1(out$variable_level[out$variable == "PT" & lvl1(out$group1_level) == parent])
    )
  }
  # no children invented under the unobserved parent without a mapping
  expect_length(
    unique(lvl1(out$variable_level[out$variable == "PT" & lvl1(out$group1_level) == "Vascular"])),
    0L
  )
})

test_that("add_hierarchical_unobserved_levels() adds children of a missing parent", {
  ard <- make_ard()
  out <- add_hierarchical_unobserved_levels(
    ard,
    variables = c(SOC, PT),
    mapping = list(Vascular = c("PTX", "PTY"))
  )

  kids <- out$variable_level[out$variable == "PT" & lvl1(out$group1_level) == "Vascular"]
  expect_setequal(unique(lvl1(kids)), c("PTX", "PTY"))
  expect_true(all(
    unlist(out$stat[out$variable == "PT" & lvl1(out$group1_level) == "Vascular" & out$stat_name == "n"]) == 0
  ))
})

test_that("add_hierarchical_unobserved_levels() adds a missing child of an observed parent", {
  ard <- make_ard()
  out <- add_hierarchical_unobserved_levels(
    ard,
    variables = c(SOC, PT),
    mapping = list(Cardiac = c("PT1", "PT2", "PT3"))
  )

  expect_true("PT3" %in% lvl1(out$variable_level[out$variable == "PT" & lvl1(out$group1_level) == "Cardiac"]))
  expect_equal(
    out$stat[out$variable == "PT" & lvl1(out$variable_level) == "PT3" &
      lvl1(out$group1_level) == "Cardiac" & out$stat_name == "n"][[1L]],
    0
  )
})

test_that("add_hierarchical_unobserved_levels() accepts a data.frame mapping", {
  ard <- make_ard()
  mapping <- data.frame(
    SOC = c("Vascular", "Vascular", "Cardiac"),
    PT = c("PTX", "PTY", "PT3")
  )
  out <- add_hierarchical_unobserved_levels(ard, variables = c(SOC, PT), mapping = mapping)

  expect_true("Vascular" %in% lvl1(out$variable_level[out$variable == "SOC"]))
  expect_setequal(
    unique(lvl1(out$variable_level[out$variable == "PT" & lvl1(out$group1_level) == "Vascular"])),
    c("PTX", "PTY")
  )
  expect_true("PT3" %in% lvl1(out$variable_level[out$variable == "PT" & lvl1(out$group1_level) == "Cardiac"]))
})

test_that("add_hierarchical_unobserved_levels() preserves the by structure", {
  ard <- make_ard(by = TRUE)
  out <- add_hierarchical_unobserved_levels(ard, variables = c(SOC, PT), mapping = list(Vascular = "PTX"))

  # one Vascular SOC row per by-group, with the arm retained in group1
  vasc_soc <- out[out$variable == "SOC" & lvl1(out$variable_level) == "Vascular" & out$stat_name == "n", ]
  expect_equal(nrow(vasc_soc), 2L)
  expect_setequal(lvl1(vasc_soc$group1_level), c("A", "B"))

  # one Vascular > PTX row per by-group, with the parent SOC in group2
  vasc_pt <- out[out$variable == "PT" & lvl1(out$variable_level) == "PTX" & out$stat_name == "n", ]
  expect_equal(nrow(vasc_pt), 2L)
  expect_setequal(lvl1(vasc_pt$group2_level), c("Vascular"))
})

test_that("add_hierarchical_unobserved_levels() is a no-op when nothing is missing", {
  # an ARD whose factor levels are all observed leaves the input untouched
  set.seed(1)
  adae <- data.frame(
    USUBJID = sprintf("S%03d", 1:20),
    SOC = factor(sample(c("Cardiac", "GI"), 20, TRUE), levels = c("Cardiac", "GI")),
    PT = factor(sample(c("PT1", "PT2"), 20, TRUE), levels = c("PT1", "PT2"))
  )
  ard <- ard_stack_hierarchical(
    adae,
    variables = c(SOC, PT), id = USUBJID,
    denominator = data.frame(USUBJID = sprintf("S%03d", 1:30))
  )
  out <- add_hierarchical_unobserved_levels(ard, variables = c(SOC, PT))
  expect_equal(nrow(out), nrow(ard))
})

test_that("add_hierarchical_unobserved_levels() input checks", {
  ard <- make_ard()
  expect_error(
    add_hierarchical_unobserved_levels(data.frame(a = 1), variables = a),
    class = "check_class"
  )
  expect_error(
    add_hierarchical_unobserved_levels(ard, variables = c(SOC, PT), mapping = "not a mapping"),
    "must be"
  )
})

test_that("add_hierarchical_unobserved_levels() output remains a valid ARD", {
  ard <- make_ard()
  out <- add_hierarchical_unobserved_levels(ard, variables = c(SOC, PT), mapping = list(Vascular = "PTX"))
  expect_no_error(sort_ard_hierarchical(out))
})
