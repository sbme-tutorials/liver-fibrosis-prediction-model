hcv_data_dis <- data.table::copy(hcv_data)
discretize <- function(data, column_name, A, B) {
  column_sym <- rlang::sym(column_name)
  hcv_data_dis <<- dplyr::mutate(hcv_data_dis,!!column_sym :=
                                             cut(data[[column_name]],
                                                 breaks = c(A),
                                                 labels = c(B)))
}