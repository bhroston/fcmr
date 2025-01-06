# ## code to prepare `DATASET` dataset goes here
#
# vat_fgcm_examples <- list(
#   fcm_as_fgcm = list(
#     grey_adj_matrix = list(),
#     initial_state_vector = list()
#   ),
#   fgcm = list(
#     grey_adj_matrix = list(),
#     initial_state_vector = list()
#   )
# )
#
# # Derived from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
# # Use to compare results with FCM (Exact as fcm)
# w5_2 <- grey_number(0.6, 0.6)
# w2_5 <- grey_number(-0.42, -0.42)
# w4_2 <- grey_number(0.8, 0.8)
# w2_4 <- grey_number(0.7, 0.7)
# w4_1 <- grey_number(-0.8, -0.8)
# w1_4 <- grey_number(0.38, 0.38)
# w3_1 <- grey_number(0.755, 0.755)
# #w1_3 <- grey_number(0.13, 0.43) They say greyness is 0.2, but can't be
# w1_3 <- grey_number(0.28, 0.28) # Might be this?
# w4_7 <- grey_number(0.09, 0.09)
# w7_4 <- grey_number(0.3, 0.3)
# w6_3 <- grey_number(0.4, 0.4)
# w6_8 <- grey_number(0.53, 0.53)
# w8_6 <- grey_number(0.6, 0.6)
#
# fgcm_as_fcm_grey_adj_matrix_edge_weights <- c(w2_5, w5_2, w4_2, w2_4, w4_1, w1_4,
#                                   w3_1, w1_3, w4_7, w7_4, w6_3, w6_8,
#                                   w8_6)
# fgcm_as_fcm_grey_adj_matrix_edge_indeces <- c("2-5", "5-2", "4-2", "2-4", "4-1", "1-4",
#                                   "3-1", "1-3", "4-7", "7-4", "6-3", "6-8",
#                                   "8-6")
# vat_fgcm_examples$fcm_as_fgcm$grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
#   values = fgcm_as_fcm_grey_adj_matrix_edge_weights,
#   locs = fgcm_as_fcm_grey_adj_matrix_edge_indeces,
#   size = 8
# )
#
# C1 <- grey_number(0.48, 0.48)
# C2 <- grey_number(0.57, 0.57)
# C3 <- grey_number(0.58, 0.58)
# C4 <- grey_number(0.68, 0.68)
# C5 <- grey_number(0.58, 0.58)
# C6 <- grey_number(0.59, 0.59)
# C7 <- grey_number(0.52, 0.52)
# C8 <- grey_number(0.58, 0.58)
# vat_fgcm_examples$fcm_as_fgcm$initial_state_vector <- c(C1, C2, C3, C4, C5, C6, C7, C8)
#
#
# # From Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
# # Representative of FGCM (Incorporates proper grey numbers)
# # Does not replicate paper results
# w5_2 <- grey_number(0.5, 0.7)
# w2_5 <- grey_number(-0.52, -0.32)
# w4_2 <- grey_number(0.7, 0.9)
# w2_4 <- grey_number(0.6, 0.8)
# w4_1 <- grey_number(-0.9, -0.7)
# w1_4 <- grey_number(0.28, 0.48)
# w3_1 <- grey_number(0.51, 1.0)
# #w1_3 <- grey_number(0.13, 0.43)
# #w1_3 <- grey_number(0.13, 0.53)
# w1_3 <- grey_number(0.03, 0.43)
# w4_7 <- grey_number(-0.16, 0.34)
# w7_4 <- grey_number(0.2, 0.4)
# w6_3 <- grey_number(0.3, 0.5)
# w6_8 <- grey_number(0.43, 0.63)
# w8_6 <- grey_number(0.35, 0.85)
#
# fgcm_adj_matrix_edge_weights <- c(w2_5, w5_2, w4_2, w2_4, w4_1, w1_4,
#                                   w3_1, w1_3, w4_7, w7_4, w6_3, w6_8,
#                                   w8_6)
# fgcm_adj_matrix_edge_indeces <- c("2-5", "5-2", "4-2", "2-4", "4-1", "1-4",
#                                   "3-1", "1-3", "4-7", "7-4", "6-3", "6-8",
#                                   "8-6")
# vat_fgcm_examples$fgcm$grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
#   values = fgcm_adj_matrix_edge_weights,
#   locs = fgcm_adj_matrix_edge_indeces,
#   size = 8
# )
#
# C1 <- grey_number(0.48, 0.48)
# C2 <- grey_number(0.57, 0.57)
# C3 <- grey_number(0.58, 0.58)
# C4 <- grey_number(0.68, 0.68)
# C5 <- grey_number(0.58, 0.58)
# C6 <- grey_number(0.59, 0.59)
# C7 <- grey_number(0.52, 0.52)
# C8 <- grey_number(0.58, 0.58)
# vat_fgcm_examples$fgcm$initial_state_vector = c(C1, C2, C3, C4, C5, C6, C7, C8)
#
#
# usethis::use_data(vat_fgcm_examples, overwrite = TRUE)
