
library(tidyverse)
library(DT)

#i <- 15.8698;
i <- 3.5; i <- i/100/12

n <- 30; n <- n*12
v <- (1+i)^-1
l <- 100000
pmt <- i*l/(1-v^n)
pmt

period <- 1:n

intpmt <- c()
prcpmt <- c()
pv_dv_new <- l
extra_pmt <- 0
for(k in period){
  int_k <- pv_dv_new[k]*i    # INTEREST PAYMENT
  intpmt <- c(intpmt, int_k)
  
  prc_k <- pmt -  int_k + extra_pmt #PMT - INTEREST PMT + EXTRA PMT. EQUITY
  prcpmt <- c(prcpmt, prc_k)
  
  pv_dv_k <- pv_dv_new[k] - prc_k # BALANCE ON THE LOAN
  pv_dv_new <- c(pv_dv_new, pv_dv_k)
}

yr <- rep(1:(n/12), 12) %>% sort()


amort <- tibble(Year = yr,
                payment = pmt,
                extraPmt = extra_pmt,
                TotalPmt = payment + extra_pmt,
                `Cumm Pmt` = TotalPmt*period,
                interest = intpmt,
                `Cum Interest` = cumsum(intpmt),
                principal = TotalPmt - interest,
                `Cum Principal` = cumsum(principal),
                balance = pv_dv_new[-1],
                ind = 1:n)
amort <- amort %>% 
  filter(interest >= 0)

amort %>%
  filter(interest >= 0) %>% 
  datatable(options = list(pageLength = 100)) %>% 
  formatCurrency(columns = names(amort[!names(amort) %in% "Year"]))

amort %>% 
  pivot_longer(cols = c('principal','interest'),
               names_to = 'break out', values_to = 'value') %>% 
  ggplot(data = ., mapping = aes(x = ind, y = value, color=`break out`)) +
  geom_line(size = 2)
