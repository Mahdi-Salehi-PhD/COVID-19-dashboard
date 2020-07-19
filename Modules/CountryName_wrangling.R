library(stringr)

f_Country_wrangling <- Vectorize(function(x) {
  f <- function(x) {
    x_spl <- unlist(strsplit(x, ""))
    if ("*" %in% x_spl) {
      nx <- which(x_spl %in% "*")
      paste0(x_spl[-nx], collapse = "")
    } else {
      return(x)
    }
  }
  res <- f(x) %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("-", "_")
  res[which(res == "US")] <- "USA"
  res[which(res == "United_Stats_of_America")] <- "USA"
  res[which(res == "U.S.")] <- "USA"
  res[which(res == "United_States_of_America")] <- "USA"
  res[which(res == "United_States")] <- "USA"
  res[which(res == "U.K.")] <- "UK"
  res[which(res == "United_Kingdom")] <- "UK"
  res[which(res == "Korea,_South")] <- "South_Korea"
  res[which(res == "Czechia")] <- "Czech_Republic"
  res[which(res == "Czech_Republic_(Czechia)")] <- "Czech_Republic"
  res[which(res == "Viet_Nam")] <- "Vietnam"
  res[which(res == "St._Vincent_&_Grenadines")] <- "Saint_Vincent_and_the_Grenadines"
  res[which(res == "Saint_Vincent")] <- "Saint_Vincent_and_the_Grenadines"
  res[which(res == "Saint_Kitts")] <- "Saint_Kitts_and_Nevis"
  res[which(res == "Saint_Kitts_&_Nevis")] <- "Saint_Kitts_and_Nevis"
  res[which(res == "Côte_d'Ivoire")] <- "Ivory_Coast"
  res[which(res == "Cote_d'Ivoire")] <- "Ivory_Coast"
  res[which(res == "Cأ´te_d'Ivoire")] <- "Ivory_Coast"
  res[which(res == "Côte_d'Ivoire")] <- "Ivory_Coast"
  res[which(res == "Antigua_and_Barbuda")] <- "Barbuda"
  res[which(res == "Cabo_Verde")] <- "Cape_Verde"
  res[which(res == "Trinidad_and_Tobago")] <- "Tobago"
  res[which(res == "Democratic_Republic_of_the_Congo")] <- "DR_Congo"
  res[which(res == "Congo_(Kinshasa)")] <- "DR_Congo"
  res[which(res == "Congo_(Brazzaville)")] <- "Congo"
  res[which(res == "TFYR_Macedonia")] <- "North_Macedonia"
  res[which(res == "West_Bank_and_Gaza Kosovo")] <- "State_of_Palestine"
  res[which(res == "Burma")] <- "Myanmar"
  res[which(res == "Eswatini")] <- "Swaziland"
  res[which(res == "Iran_(Islamic_Republic_of)")] <- "Iran"
  res[which(res == "Bolivia_(Plurinational_State_of)")] <- "Bolivia"
  res[which(res == "Côte_d'Ivoire")] <- "Ivory_Coast"
  res[which(res == "Brunei_Darussalam")] <- "Brunei"
  res[which(res == "Russian_Federation")] <- "Russia"
  res[which(res == "Taiwan,_Province_of_China")] <- "Taiwan"
  res[which(res == "Moldova_(Republic_of)")] <- "Moldova"
  res[which(res == "United_Kingdom_of_Great_Britain_and_Northern_Ireland")] <- "UK"
  res[which(res == "Venezuela_(Bolivarian_Republic_of)")] <- "Venezuela"
  res[which(res == "Syrian_Arab_Republic")] <- "Syria"
  res[which(res == "Lao_People's_Democratic_Republic")] <- "Laos"
  res[which(res == "Korea_(Republic_of)")] <- "South_Korea"
  res[which(res == "Congo_(Democratic_Republic_of_the)")] <- "DR_Congo"
  res[which(res == "Congo")] <- "Congo"
  res[which(res == "Macedonia_(the_former_Yugoslav_Republic_of)")] <- "North_Macedonia"
  res[which(res == "Tanzania,_United_Republic_of")] <- "Tanzania"
  res[which(res == "Sao_Tome_&_Principe")] <- "Sao_Tome_and_Principe"
  return(res)
})
