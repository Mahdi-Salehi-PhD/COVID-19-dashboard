# data wrangling
library(dplyr)
library(data.table)
library(tidyr)

compute_cum_ranks <- function(data) {
  f_rank <- function(data) {
    f1 <- function(x) {
      frankv(x, order = , ties.method = "first")
    }

    df <- data[which(sapply(data, is.numeric))]
    # df
    df_name <- names(df)
    #
    new_df <- sapply(df, f1)
    colnames(new_df) <- paste0("rank_", df_name)
    data.frame(data, new_df)
  }

  data %>%
    group_by(DateRep) %>%
    tidyr::nest() -> data2

  data2$data <- lapply(data2$data, f_rank)
  data2 <- tidyr::unnest(data2)
  data2 <- ungroup(data2)
  return(data2)
}
