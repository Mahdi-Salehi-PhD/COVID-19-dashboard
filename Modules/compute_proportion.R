compute_proportion <- function(data, pop, n = 1e6) {
  f1 <- function(x, pop, n) {
    round(x / (pop / 1e6), 2)
  }

  df <- data[which(sapply(data, is.numeric))]
  df2 <- data[-which(sapply(data, is.numeric))]
  dff <- df %>% select(-starts_with("Pop"), -starts_with("Rec"))
  res <- sapply(dff, f1, pop = data$Population, n = n)
  colnames(res) <- paste0(names(dff))
  res <- as.data.frame(res)
  res$Recovered <- ifelse(df$Cases != 0, df$Recovered / df$Cases, 0)
  res2 <- cbind(df2, population = pop, res)
  res2$cum_recovered <- ifelse(data$cum_cases == 0, 0, data$cum_recovered / data$cum_cases)
  res2
}
