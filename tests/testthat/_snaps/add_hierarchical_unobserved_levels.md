# add_hierarchical_unobserved_levels() errors with incorrect inputs

    Code
      add_hierarchical_unobserved_levels(ard_tabulate(cards::ADSL, variables = AGEGR1),
      levels = levels_all)
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! The `x` argument must be class <ard_stack_hierarchical/ard_stack_hierarchical_count>, not a <card> object.

---

    Code
      add_hierarchical_unobserved_levels(ard, levels = "EYE DISORDERS")
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! The `levels` argument must be class <data.frame>, not a string.

---

    Code
      add_hierarchical_unobserved_levels(ard, levels = data.frame(TRTA = "Placebo"))
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! The columns of `levels` must be hierarchy variables of `x`, starting from the top.
      i The hierarchy variables of `x`, from the top, are "AESOC" and "AEDECOD".

---

    Code
      add_hierarchical_unobserved_levels(ard, levels = dplyr::distinct(levels_all,
        AEDECOD))
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! The columns of `levels` must be hierarchy variables of `x`, starting from the top.
      i The hierarchy variables of `x`, from the top, are "AESOC" and "AEDECOD".

---

    Code
      add_hierarchical_unobserved_levels(ard, levels = data.frame(AESOC = NA_character_))
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! The `levels` argument cannot contain missing values.

---

    Code
      add_hierarchical_unobserved_levels(ard, levels = data.frame(AESOC = 1))
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! Can't convert `levels$AESOC` <double> to <character>.

---

    Code
      add_hierarchical_unobserved_levels(ard_cum, levels = levels_all)
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! Cannot add unobserved levels to an ARD with the "n_cum" statistic.
      i Only the "n", "N", and "p" statistics can be set for an unobserved level.

---

    Code
      add_hierarchical_unobserved_levels(dplyr::filter(ard, variable != "AEDECOD"),
      levels = levels_all)
    Condition
      Error in `add_hierarchical_unobserved_levels()`:
      ! Cannot add levels for "AEDECOD": `x` has no rows for this variable to copy.

