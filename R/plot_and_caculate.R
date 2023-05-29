
# plot --------------------------------------------------------------------

my_data  %T>% 
  {my_title <<- attr(., "filename")} %>% 
  #filter(status != "41") %>% 
  #filter(torque_newton_meters != 0) %>% 
  #filter(time_seconds > 0 & time_seconds < 24) %>% 
  #filter(period_id %in% c (1:10)) %>% 
  #filter(period_id == 1) %>% 
  ggplot(aes(x = time_seconds, y = torque_newton_meters))+
  geom_point(size = 0.4, color = "grey")+
  geom_point(data = . %>% filter(status != "41"),
             size = 0.2, color = "red")+
  labs(subtitle = my_title,
       y = "Torque (N/m)",
       x = "time (sec)")+
  theme_bw(base_size = 20)

#ggsave(paste0(my_title, ".png"))


# calculate ---------------------------------------------------------------

#plot the peak torque
my_data %>% 
  filter(period_id != 0) %>% 
  summarize(peak_torque = max(torque_newton_meters), .by = period_id) %>% 
  ggplot(aes(x = period_id, y = peak_torque))+
  geom_point()

#plot the first_order derivation
my_data %>% 
  filter(period_id != 0) %>% 
  nest(.by = period_id) %>% 
  mutate(spline_0 = map(.x = data,
                        .f = ~splineFN_new3(.x) %>%
                          select(zero_order = parameter))) %>% 
  mutate(spline_1 = map(.x = data,
                        .f = ~splineFN_new3(.x,
                                            n_order = 1) %>%
                          select(timescale, first_order = parameter))) %>% 
  mutate(max_slope = map_dbl(.x = spline_1,
                             .f = ~.x$timescale[which.max(.x$first_order)])) %>% 
  unnest(c(spline_0, spline_1, data)) %>% 
  select(-timescale) %>% 
  filter(period_id %in% c(1)) %>% 
  ggplot(aes(x = time_seconds, y = first_order))+
  geom_point()

# model a fit for one tetanus
x <- my_data %>% filter(period_id == 2)

model <- x %>% 
  select(time_seconds, torque_newton_meters) %>% 
  with(pspline::sm.spline(.$time_seconds, .$torque_newton_meters))

givenY <- 11.8
estX <- approx(x=model$ysmth, y=model$x, xout=givenY)$y


# In the pipe below, the required physiological parameters are calculated from 
# each tetanus
# 
# The file: "test005_HAPfatigue_ 16_06_2021 10_53_49 60 1 Right 0.CSV" contains 
# the 60 contractions measurring muscle fatigue
#
# mfr = maximal rate of force rise
# max_torque = peak force 
# early_rt = early relaxation time
# half_rt = half relaxation time

my_data %>% 
  filter(period_id != 0) %>% 
  nest(.by = period_id) %>% 
  mutate(spline_0 = map(.x = data,
                        .f = ~splineFN_new3(.x) %>%
                          select(zero_order = parameter))) %>% 
  mutate(spline_1 = map(.x = data,
                        .f = ~splineFN_new3(.x,
                                            n_order = 1) %>%
                          select(timescale, first_order = parameter) %>% 
                          left_join(.x %>%  #here I need to get the status back into the processed tibble!
                                      select(status, timescale = time_seconds),
                                    by = c("timescale")))) %>% 
  mutate(max_torque = map_dbl(.x = spline_0,
                              .f = ~.x$zero_order %>%  max())) %>% #pluck("spline_1", 1)
  mutate(max_derivative = map_dbl(.x = spline_1,
                                  .f = ~ .x %>% 
                                    filter(status == 41) %>%  #THIS GOES WRONG HERE
                                    pull(first_order) %>% 
                                    max())) %>% 
  mutate(model = map(.x = data ,
                     .f = ~ .x %>% 
                       filter(status != 41) %>% 
                       with(pspline::sm.spline(.$time_seconds, 
                                               .$torque_newton_meters)))) %>% 
  mutate(max_75_time = map2_dbl(.x = model,
                                .y = max_torque*0.75,
                                .f = ~ approx(x=.x$ysmth, y=.x$x, xout=.y)$y)) %>% 
  mutate(max_50_time = map2_dbl(.x = model,
                                .y = max_torque*0.5,
                                .f = ~ approx(x=.x$ysmth, y=.x$x, xout=.y)$y)) %>% 
  mutate(max_25_time = map2_dbl(.x = model,
                                .y = max_torque*0.25,
                                .f = ~ approx(x=.x$ysmth, y=.x$x, xout=.y)$y)) %>% 
  mutate(mfr = max_derivative/max_torque,
         early_rt = abs(max_75_time - max_50_time),
         half_rt = abs(max_50_time - max_25_time)) %>% 
  select(period_id, max_torque,max_50_time, max_75_time, max_25_time) %>% 
  ggplot(aes(x = period_id, y = abs(max_50_time - max_25_time)))+
  geom_point()
  

