# Run from project root: source("tests/test_publication_check_helper.R")
source("scripts/00_shared.R")
checks <- list(
  mf_report_check(setNames(0.113223, "Delta_H"), 0.113, "named observed", 0.0006),
  mf_report_check(0.129515, 0.130, "unnamed observed", 0.0006),
  mf_report_check(setNames(500.077836, NA_character_), 500.08,
                  "invalid inherited row name", 0.01)
)
stopifnot(all(vapply(checks, function(x) isTRUE(x$pass), logical(1))))
stopifnot(all(vapply(checks, function(x) is.character(rownames(x)), logical(1))))
missing_lookup <- tryCatch(mf_report_check(c(Delta_H = 0.1)["missing"],
                                           0.1, "missing metric"), error = identity)
stopifnot(inherits(missing_lookup, "error"))
stopifnot(grepl("missing/non-finite estimate", conditionMessage(missing_lookup),
               fixed = TRUE))
message("Publication reporting helper regression checks PASSED.")
