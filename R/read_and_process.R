library(tidyverse)
library(magrittr)
library(bayestestR)
library(pspline)

library(conflicted)
conflicts_prefer(dplyr::filter())

conflict_scout()

# functions ---------------------------------------------------------------

label_tetanus <- function(df){
  
  # df <- my_data %>%
  #    filter(status %in% c(1, 41))
  
  df %<>% 
    filter(status %in% c(1, 41))
  
  peak = 1
  my_period = NULL
  
  for(i in 1:(nrow(df)-3)){
    
    my_period[1] <- 1
    status = df$status[i]
    next_status = df$status[i+1]
    status_diff = next_status - status
    if (abs(status_diff) == 40) {
      peak = peak+1
    }
    my_period[i+1] <- peak
    
  }
  my_period[nrow(df) -1 ] <- max(my_period)
  my_period[nrow(df)  ] <- max(my_period)
  
  df$period <- my_period
  
  df %<>% 
    group_by(status, period) %>% 
    mutate(id = cur_group_id(),
           period_id = case_when(id > ceiling(max(df$period)/2) ~ id - floor(max(df$period)/2),
                                 .default = id),
           period_id = period_id - 1) %>% 
    select(-id) %>% 
    ungroup()
  
  return(df)
}

make_torque_positive <- function(df){
  
  df %>% 
    mutate(torque_newton_meters = -torque_newton_meters,
           speed_d_s = - speed_d_s)
}

splineFN_new3 <- function(x, n_order = 0, spar = 0.001, method = 1) {
  x <- x %>% select(time_seconds, torque_newton_meters)
  colnames(x) <- c("timescale", "param")
  # y <- as.vector(predict(smooth.Pspline(x$timescale, x$param,
  #                                       spar = spar,
  #                                       method = method),
  #                        x$timescale,
  #                        n_order))
  y <- as.vector(predict(pspline::sm.spline(x$timescale, x$param),
                         x$timescale,
                         n_order))
  df <- tibble(timescale = x$timescale, parameter = y)
  return(df)
}

# input -------------------------------------------------------------------

file_path <- here::here("inst", "extdata", "Test005_HAPMVC40_ 16_06_2021 10_31_10 60 10 Right 0.CSV")
file_path <- here::here("inst", "extdata", "Test005_HAPMVC_ 16_06_2021 10_21_11 60 5 Right 0.CSV")
file_path <- here::here("inst", "extdata", "test005_HAPFFR_ 16_06_2021 10_42_48 60 1 Right 1.CSV")
file_path <- here::here("inst", "extdata", "test005_HAPfatigue_ 16_06_2021 10_53_49 60 1 Right 0.CSV")

# read and preprocess -----------------------------------------------------

my_data <- file_path %>%
  readr::read_csv(locale = readr::locale(encoding = "UTF-8"),
                  col_names = TRUE,
                  show_col_types = FALSE,
                  skip = 0) %>% 
  janitor::clean_names(case = "snake") %>% 
  make_torque_positive() %>% 
  label_tetanus() %>% 
  `attr<-`("date", Sys.Date()) %>% 
  `attr<-`("time", Sys.Date()) %>% 
  `attr<-`("filename", basename(file_path)) 


# explore -----------------------------------------------------------------

my_data %>% 
  select(time_seconds, period, status, period_id) %>% 
  print(n = 500)

my_data$period_id %>%  max()


