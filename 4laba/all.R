install.packages("rvest")
install.packages(c("httr", "xml2", "rvest"), repos = "https://cloud.r-project.org")
library(rvest)
years <- 2014:2021
get_data <- function(year) {
  url <- paste0('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=', year)
  page <- read_html(url)
  nodes <- html_nodes(page, 'table#t2')
  df <- html_table(nodes) %>% as.data.frame()
  rownames(df) <- df[, 2]
  df <- df[, 3:11]
  return(df)
}

data_list <- lapply(years, get_data)
names(data_list) <- years

countries <- c('Portugal','Czech Republic','Croatia','Russia','United States')

extract_index <- function(df, country, column_name ) {
  sapply(country, function(ctry) {
    if (ctry %in% rownames(df)) {
      value <- df[ctry, column_name]
      if (value == "-") {
        return(NA)
      } else {
        return(as.numeric(value))
      }
    } else {
      return(NA)
    }
  })
}
colors = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F") 

index_names <- colnames(data_list[[1]])
index_names_rus <- c(
  "Quality.of.Life.Index" = "Индекс качества жизни",
  "Purchasing.Power.Index" = "Индекс покупательной способности",
  "Safety.Index" = "Индекс безопасности",
  "Health.Care.Index" = "Индекс здравоохранения",
  "Cost.of.Living.Index" = "Индекс стоимости жизни",
  "Property.Price.to.Income.Ratio" = "Соотношение цен на жилье и доходов",
  "Traffic.Commute.Time.Index" = "Индекс времени в пути",
  "Pollution.Index" = "Индекс загрязнения",
  "Climate.Index" = "Индекс климата"
)
index_names_rus <- index_names_rus[index_names]

par(mar = c(4, 4, 4, 11), xpd = TRUE) 
for (i in 1:length(index_names)) {
  index <- index_names[i]
  matrix_data <- t(sapply(data_list, extract_index, countries, index))
  
  y_min <- min(matrix_data, na.rm = TRUE) - 5
  y_max <- max(matrix_data, na.rm = TRUE) + 5
  
  plot(NA, xlim = range(years), ylim = c(y_min, y_max),xlab = 'Года', ylab = index_names_rus[i],
       main = paste('Динамика', index_names_rus[i], 'по странам'))
  
  for (j in 1:length(countries)) {
    lines(years, matrix_data[, j], type = 'b', col = colors[j], lty = 1, pch = 16, lwd = 2)
  }
  legend("topright", inset = c(-0.45, 0.25), legend = countries, fill = colors, bty = 'n')
}
# 4
library(dplyr)
library(purrr)

url <- "https://ru.wikipedia.org/wiki/Список_музеев_Москвы"
webpage <- read_html(url)

process_table <- function(table_node) {
  table_df <- html_table(table_node, fill = TRUE)
  if (nrow(table_df) == 0) return(NULL)
  col_names <- names(table_df)
  if (length(col_names) == 0 || all(col_names == "")) {
    col_names <- as.character(table_df[1, ])
    table_df <- table_df[-1, ]
  }
  col_names <- tolower(col_names)
  
  name_col <- which(grepl("Название|name", col_names, ignore.case = TRUE))[1]
  
  address_col <- which(grepl("Адрес|address", col_names, ignore.case = TRUE))[1]
  if (is.na(name_col)) name_col <- 2
  if (is.na(address_col)) address_col <- 3
  museum_links <- table_node %>%
    html_nodes(xpath = paste0(".//tr[position()>1]/td[", name_col, "]//a[not(@class='new')][1]")) %>%
    html_attr("href") %>%
    na.omit() %>%
    paste0("https://ru.wikipedia.org", .)
  external_links <- table_node %>%
    html_nodes(xpath = ".//tr[position()>1]//a[contains(@href,'http')][1]/@href") %>%
    html_text()
  
  result <- data.frame(
    Название = if (!is.na(name_col)) table_df[[name_col]] else NA,
    Адрес = if (!is.na(address_col)) table_df[[address_col]] else NA,
    Ссылка = NA_character_
  )
  
  for (i in 1:nrow(result)) {
    if (length(museum_links) >= i && !is.na(museum_links[i])) {
      result$Ссылка[i] <- museum_links[i]
    } else if (length(external_links) >= i && !is.na(external_links[i])) {
      result$Ссылка[i] <- external_links[i]
    }
  }
  
  result <- result %>%
    filter(!is.na(Название), Название != "",
           !grepl("название|name", Название, ignore.case = TRUE)) %>%
    mutate(across(everything(), ~gsub("\\[.*?\\]", "", .))) %>%
    mutate(across(everything(), trimws)) %>%
    distinct(Название, Адрес, .keep_all = TRUE)
  
  return(result)
}

all_tables <- webpage %>% html_nodes("table")
museum_data <- map_dfr(all_tables, process_table)
