#
# filepath <- file.choose() # stkhldr_data
#
# sheet_names <- readxl::excel_sheets(filepath)
# stkhldrs <- sheet_names[sheet_names != "COORDS"]
# n_removed <- 5
# randomized_stkhldr_indexes <- sample(seq_along(stkhldrs), length(stkhldrs) - n_removed, replace = FALSE)
# randomized_stkhldrs <- stkhldrs[randomized_stkhldr_indexes]
#
#
# updated_salinization_conventional_fcms <- lapply(
#   randomized_stkhldrs, function(random_stkhldr) readxl::read_excel(filepath, sheet = random_stkhldr)
# )
# # usethis::use_data(old_salinization_conventional_fcms, overwrite = TRUE)
# salinization_conventional_fcms <- updated_salinization_conventional_fcms
# usethis::use_data(salinization_conventional_fcms, overwrite = TRUE)
#
#
# ivfn_and_tfn_buffer <- 0.2
#
# create_sample_ivfn_fcm_from_fcm <- function(fcm, buffer) {
#   only_positive_values <- ifelse(all(fcm >= 0), TRUE, FALSE)
#   as.data.frame(apply(
#     fcm, c(1, 2),
#     function(element) {
#       if (element == 0) {
#         return(ivfn(0, 0))
#       }
#
#       try_lower <- element - buffer
#       if (only_positive_values & try_lower < 0) {
#         use_lower <- 0
#       } else if (!only_positive_values & try_lower < -1) {
#         use_lower <- -1
#       } else {
#         use_lower <- try_lower
#       }
#
#       try_upper <- element + buffer
#       if (try_upper > 1) {
#         use_upper <- 1
#       } else {
#         use_upper <- try_upper
#       }
#
#       ivfn(use_lower, use_upper)
#     }
#   ))
# }
# updated_salinization_ivfn_fcms <- lapply(updated_salinization_conventional_fcms, create_sample_ivfn_fcm_from_fcm, buffer = ivfn_and_tfn_buffer)
# # usethis::use_data(old_salinization_ivfn_fcms, overwrite = TRUE)
# salinization_ivfn_fcms <- updated_salinization_ivfn_fcms
# usethis::use_data(salinization_ivfn_fcms, overwrite = TRUE)
#
# create_sample_tfn_fcm_from_fcm <- function(fcm, buffer) {
#   only_positive_values <- ifelse(all(fcm >= 0), TRUE, FALSE)
#   as.data.frame(apply(
#     fcm, c(1, 2),
#     function(element) {
#       if (element == 0) {
#         return(tfn(0, 0, 0))
#       }
#
#       try_lower <- element - buffer
#       if (only_positive_values & try_lower < 0) {
#         use_lower <- 0
#       } else if (!only_positive_values & try_lower < -1) {
#         use_lower <- -1
#       } else {
#         use_lower <- try_lower
#       }
#
#       try_upper <- element + buffer
#       if (try_upper > 1) {
#         use_upper <- 1
#       } else {
#         use_upper <- try_upper
#       }
#
#       tfn(use_lower, element, use_upper)
#     }
#   ))
# }
# updated_salinization_tfn_fcms <- lapply(updated_salinization_conventional_fcms, create_sample_tfn_fcm_from_fcm, buffer = ivfn_and_tfn_buffer)
# # usethis::use_data(old_salinization_tfn_fcms, overwrite = TRUE)
# salinization_tfn_fcms <- updated_salinization_tfn_fcms
# usethis::use_data(salinization_tfn_fcms, overwrite = TRUE)
#
