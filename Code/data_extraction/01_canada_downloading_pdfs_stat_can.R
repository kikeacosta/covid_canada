Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
library(wayback)
library(tidyverse)
library(rvest)
library(lubridate)

setwd("U:/nextcloud/Projects/COVID_19/COVerAge-DB/canada/statcan_pdf/")
get_date <- function(x){
  x %>%
    str_split(pattern=" ") %>%
    unlist() %>%
    '['(2:4) %>%
    paste(collapse = " ") %>%
    dmy() %>%
    as.character()
}

# looking for all captures in the wayback machine
archive_check_url <- "https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html"
res <- get_mementos(archive_check_url)
timestamps <- get_timemap(res$link[2]) %>% 
  filter(!is.na(datetime))

# timestamps$link <- paste0(timestamps$link,"/covid-19-current-situation/covid-19-current-cases#age")

timestamps$datetime <- lapply(timestamps$datetime, get_date) %>% unlist()
timestamps <- timestamps %>% 
  group_by(datetime) %>% 
  slice(1) %>% 
  ungroup()

dim(timestamps)[1]
i <- 1
for (i in 1 : dim(timestamps)[1]){
  print(i)
  m_url <- timestamps$link[i]
  all <- read_html(m_url)
  if (i <= 50) {
    nodos <- html_nodes(all, xpath = '//*[@id="main"]/p[2]/a')
  } else {
    nodos <- html_nodes(all, xpath = '//*[@id="main"]/h1/a')
  }
  url_pdf <- html_attr(nodos, "href")
  pdf_file <- paste0(timestamps$datetime[i], "_can.pdf")
  download.file(url_pdf, pdf_file, mode = "wb", method = "libcurl")
}




