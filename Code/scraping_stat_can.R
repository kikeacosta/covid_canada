Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
library(wayback)
library(tidyverse)
library(rvest)
library(lubridate)
library(pdftools)

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






# downloading pdfs
# m_url <- "https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html"
# all <- read_html(m_url)
# nodos <- html_nodes(all, xpath = '//*[@id="main"]/h1/a')
# url_pdf <- html_attr(nodos, "href")

# reading pdf
txt <- pdf_text(pdf_file)
txt

txt[15]
capture.output(cat(txt[9]))
capture.output(cat(txt[13]))






# tabulizer... progress
install.packages("Rtools")
install.packages("ghit")
# on 64-bit Windows
ghit::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere
ghit::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"))
library(tabulizer)
f2 <- pdf_file
extract_tables(f2, pages = 9, method = "data.frame")





# webscrabing other options
library(readxl)
library(googlesheets4)
library(googledrive)
library(rio)
library(lubridate)
library(XML)
library(RCurl)
library(httr)



























m_url <- "https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html"
m_url2 <- getURL(m_url)

tables <- readHTMLTable(m_url2) 
date_f <- str_split(names(tables[1]), ",")[[1]][2] %>% dmy()



x <- GET("https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html")











test2 <- html_nodes(tables, xpath = '//*[@id="main"]/details[5]/table/tbody')




test <- html_nodes(tables, xpath = '//*[@id="main"]/details[6]/div[4]/table')

minimal_html()

test <- html_nodes(tables, xpath = '//*[@id="main"]/details[6]/div[4]/table/tbody')
test2 <- html_table(test)

test <- html_nodes(tables, xpath = '//*[@id="main"]/details[6]/div[4]/table/tbody/tr[1]/td[1]')
test2 <- html_text(test)

test <- html_text(tables, xpath = '//*[@id="main"]/details[6]/div[4]/table/tbody/tr[1]/td[1]')


test <- html_nodes(tables, xpath = '//*[@id="main"]/details[6]/div[4]/table/tbody')








library(rvest)

webpage <- read_html("https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html")

tbls <- html_nodes(webpage, "tbody")

head(tbls)


tbls_ls <- webpage %>%
  html_nodes("td") %>%
  .[10] %>%
  html_table(fill = TRUE)


library(XML)
x <- GET("https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html")
tbls_xml <- readHTMLTable(x)


library(RCurl)
url <- "https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html"
url_parsed <- htmlParse(getURL(url), asText = TRUE)
tableNodes <- getNodeSet(url_parsed, '//*[@id="main"]/details[6]/div[4]/table/tbody')

original_page <- GET(url) %>% content("text") 


sample1 <- minimal_html('<table class="table table-bordered table-striped">
                <caption class="text-left">
					Age and sex<sup id="fn3-8-rf"><a class="fn-lnk" href="#fn3"><span class="wb-invisible">Footnote </span>3</a></sup> distribution of COVID-19 cases deceased in Canada as of <span class="updateTime">October 18, 2020, 7 pm EDT</span> (n=<span class="fig3-d-n">9,654</span><sup><a class="fn-lnk" href="#fn1"><span class="wb-invisible">Footnote </span>1</a></sup>)                </caption>
                        <thead>
                          <tr class="bg-primary">
                          <th scope="col">Age group (years)</th>
                          <th scope="col">Number of cases with case reports (proportion)</th>
                          <th scope="col">Number of male cases (proportion)</th>
                          <th scope="col">Number of female cases (proportion)</th>
                          <th scope="col">Number of other cases (proportion)</th>
                          </tr>
                          </thead>
                          <tbody class="deaths-data">
                          <tr><td>0-19</td><td>2 (0.0%)</td><td>1 (0.0%)</td><td>1 (0.0%)</td><td>0 (0.0%)</td></tr><tr><td>20-29</td><td>9 (0.1%)</td><td>6 (0.1%)</td><td>3 (0.0%)</td><td>0 (0.0%)</td></tr><tr><td>30-39</td><td>16 (0.2%)</td><td>11 (0.1%)</td><td>5 (0.1%)</td><td>0 (0.0%)</td></tr><tr><td>40-49</td><td>54 (0.6%)</td><td>38 (0.4%)</td><td>16 (0.2%)</td><td>0 (0.0%)</td></tr><tr><td>50-59</td><td>232 (2.4%)</td><td>135 (1.4%)</td><td>97 (1.0%)</td><td>0 (0.0%)</td></tr><tr><td>60-69</td><td>704 (7.3%)</td><td>436 (4.5%)</td><td>268 (2.8%)</td><td>0 (0.0%)</td></tr><tr><td>70-79</td><td>1,751 (18.1%)</td><td>1,020 (10.6%)</td><td>731 (7.6%)</td><td>0 (0.0%)</td></tr><tr><td>80+</td><td>6,886 (71.3%)</td><td>2,807 (29.1%)</td><td>4,079 (42.3%)</td><td>0 (0.0%)</td></tr></tbody>
                          </table>')




test <- sample1 %>%
  html_table()


test[1]

test[20]

test[39]



html_text(tables, '//*[@id="main"]/details[5]/table/tbody')



tbls_ls <- webpage %>%
  html_nodes("tbody")

tbls_ls[10] %>%
  html_text()

library("rvest")
url <- "https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html"
population <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="main"]/details[6]/div[4]/table') %>%
  html_table()

population <- population[[1]]

head(population)



require(rvest)
doc <- read_html("https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html")
doc %>% 
  html_node(xpath = "/html/body/table[2]//tr[1]/td[2]/table//tr/td/text()[6]") %>% 
  html_text





