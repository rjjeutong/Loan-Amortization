
library(tidyverse)
library(rvest)
library(stringr)
library(DT)

## what's your zipcode ###
my_zip <- 68507

u <- paste0("https://www.zillow.com/homes/for_sale/",my_zip,"_rb/")

#### SCRAPE THE FIRST PAGE ####
page <- read_html(u)

address <- page %>% 
  html_nodes(".list-card-addr") %>% 
  html_text(trim = T)

type <- page %>% 
  html_nodes(".list-card-type") %>% 
  html_text(trim = T)

price <- page %>% 
  html_nodes(".list-card-price") %>% 
  html_text(trim = T)

details <- page %>% 
  html_nodes(".list-card-details") %>% 
  html_text(trim = T)

houses <- tibble(address, type, price, details)

View(houses)

#### GET THE TOTAL NUMBER OF PAGE.. WILL LOOP OVER THEM ####
pgs <- page %>% 
  html_nodes("#mobile-pagination-root a") %>% 
  html_text(trim = T) %>% 
  as.numeric() %>% 
  max(na.rm=T)

### LOOP THOUGH ALL THE PAGE AND SCRAPE.. APPEND THE DATA AFTER EACH ITERATION ###
for(i in 2:pgs) {
  
  u_i <- paste0(u,i,"_p/")
  
  page_i <- read_html(u_i)
  
  address_i <- page_i %>% 
    html_nodes(".list-card-addr") %>% 
    html_text(trim = T)
  
  type_i <- page_i %>% 
    html_nodes(".list-card-type") %>% 
    html_text(trim = T)
  
  price_i <- page_i %>% 
    html_nodes(".list-card-price") %>% 
    html_text(trim = T)
  
  details_i <- page_i %>% 
    html_nodes(".list-card-details") %>% 
    html_text(trim = T)
  
  houses <- rbind(houses, tibble(address=address_i, type=type_i, price=price_i, details=details_i))
  Sys.sleep(sample(x = 11:16, size = 1))
}

### FORMAT THE price COLUMN ###
my_houses <- houses %>% #tail(15) %>% 
  na_if("$--") %>% 
  mutate(price = str_replace_all(price,"Est. ", ""))

### SOME SUMMARY STATISTICS.. THIS MAY BE INTERESTING ####
my_houses %>% 
  mutate(price = parse_number(price)) %>% 
  group_by(type) %>% 
  summarize(max = max(price, na.rm=T),
            median = median(price, na.rm=T),
            mean = mean(price, na.rm=T),
            min = min(price, na.rm=T),
            n = n()) %>%
  datatable() %>% 
  formatCurrency(columns=c('max','median','mean','min'), digits=0)

options(scipen = 1000)

#### A BOX PLOT.. MAY NOT BE VERY REPRESENTATIVE DUE TO THE NUMBER OF OBSERVATION FOR EACH TYPE ###
my_houses %>% 
  na.omit() %>% 
  mutate(price = parse_number(price),
         type = fct_reorder(type, price, median)) %>% 
  #filter(price < 5000000) %>% 
  ggplot(aes(type, price)) +
  geom_boxplot() +
  coord_flip()

### A HISTOGRAM OF PRICE BY PROPERTY TYPE ####
my_houses %>% 
  na.omit() %>% 
  mutate(price = parse_number(price)) %>% 
  #filter(price < 5000000) %>% 
  ggplot(aes(price)) +
  geom_histogram(bins = 30) +
  facet_wrap(~type)
