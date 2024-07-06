library(dplyr)
library(lubridate)
library(fpp3)
library(urca)

setwd("SETUP_YOUR_WORKING_DIRECTORY_HERE")

temp = readr::read_csv('twitch.csv')
filtered_temp <- temp[temp$language == "English", ]

twitch <- temp %>%
  group_by(year, month) %>%
  summarize(
    watch_time_min = sum(watch_time_min),
    stream_time_min = sum(stream_time_min),
    .groups = 'drop'
  )

# Define month levels in the correct order
month_levels <- tolower(month.name)
# Convert month column to a factor with the correct levels
twitch$month <- factor(twitch$month, levels = month_levels)
# Create a date column
twitch <- twitch %>%
  mutate(date = make_date(year, match(month, month_levels), 1))
# Tell R that date column is a time series date index by converting it to a tsibble
twitch <- twitch %>% 
  mutate(date = yearmonth(ymd(date))) %>% 
  as_tsibble(index=date)

# Download the file. 
temp2 = readr::read_csv('fredgraph.csv')

temp2 = temp2 %>% 
  mutate(date = yearmonth(ymd(DATE))) %>% 
  as_tsibble(index=date) %>% 
  filter_index("2016 Jan" ~ "2023 Dec")

twitch$PPIintnet = temp2$WPS3721
twitch$unemp = temp2$UNRATE
twitch$ffrate = temp2$FEDFUNDS
twitch$mltrvl = temp2$TRFVOLUSM227NFWA

final_data = twitch

final_data <- final_data %>%
  mutate(unemp = as.numeric(unemp),
         PPIintnet = as.numeric(PPIintnet),
         ffrate = as.numeric(ffrate),
         mltrvl = as.numeric(mltrvl))

# create a hold out period for model training and out-sample forecasting comparison.
finalHOLD = final_data %>% filter_index('2016 Jan' ~ '2022 Dec')

finalHOLD %>% autoplot(log(stream_time_min))
finalHOLD %>% gg_season(log(stream_time_min))

# Test stationarity without differencing.
finalHOLD %>%features(log(stream_time_min),unitroot_kpss) 
# Test stationarity with first differencing. 
finalHOLD %>%features(difference(log(stream_time_min)),unitroot_kpss) 
finalHOLD %>% autoplot(difference(log(stream_time_min)))

finalHOLD %>% 
  gg_tsdisplay(difference(log(stream_time_min)), lag_max=36, plot_type = 'partial')

finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(0,1,0) + PDQ(1,0,0))) %>% report()

finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(1,1,0) + PDQ(1,0,0))) %>% report()

finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(0,1,0) + PDQ(1,0,1))) %>% report()

finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(1,1,0) + PDQ(1,0,1))) %>% report()

candidate = finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(1,1,0) + PDQ(1,0,1)))

candidate %>% residuals() %>% gg_tsdisplay(.resid, lag_max = 30, plot_type = 'partial')

candidate %>% augment() %>% features(.innov,ljung_box,lag=60,dof=6)

candidate %>% forecast(h=12) %>% 
  autoplot(filter_index(final_data,"2019 Dec"~ "2023 Dec"), level = 50)

finalHOLD %>% model(STL(log(stream_time_min)~season(window=7)+
                          trend(window = 13))) %>% components() %>% autoplot()

candidate2 = finalHOLD %>% model(ETS(log(stream_time_min)~error("A")+trend("A")+
                                       season("A")))
candidate2%>%forecast(h=12)%>%
  autoplot(filter_index(final_data,"2019 Dec"~ "2023 Dec"), level = 50)

candidate3 = finalHOLD %>% model(NNETAR(log(stream_time_min)~AR(P=1,p=11),n_networks=100))

candidate3 %>% forecast(h=12,times=100,bootstrap=TRUE)%>%
  autoplot(filter_index(final_data,"2019 Dec"~ "2023 Dec"), level = 50)

lastmodel = finalHOLD %>% 
  model(candidate1 = ETS(log(stream_time_min)~error("A")+trend("A")+season("A")),
        candidate2 = ARIMA(log(stream_time_min) ~ 0 + pdq(1,1,0) + PDQ(1,0,1)),
        candidate3 = NNETAR(log(stream_time_min)~AR(P=1,p=11),n_networks=100))

fcst_12mnt = lastmodel %>% forecast(h=12, times = 100, bootstrap = TRUE)

fcst_12mnt %>%
  autoplot(filter_index(final_data,"2021 Dec"~ "2023 Dec"), level = 0)

comb = lastmodel %>% mutate(COMBINED = (candidate1+candidate2+candidate3)/3) 

fcst_12mnt = comb %>% forecast(h=12)

fcst_12mnt %>% autoplot(filter_index(final_data,"2021 Dec"~ "2023 Dec"), level = 0)

residuals <- fcst_12mnt %>% 
  as_tibble() %>%
  left_join(final_data%>%filter_index("2023 Jan" ~ "2023 Dec"), by = "date") %>%
  mutate(
    .resid = (.mean - stream_time_min.y)^2
  )

# Calculate MSE
mse <- residuals %>%
  group_by(.model) %>%
  summarise(MSE = mean(.resid, na.rm = TRUE))

print(mse)

finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(1,1,0) + PDQ(1,0,1) +
                unemp + log(PPIintnet))
  ) %>% report()


finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(1,1,0) + PDQ(1,0,1) +
                unemp + log(PPIintnet) + lag(log(PPIintnet)) + lag(unemp)
              + log(mltrvl))
  ) %>% report()

dynmc_arima1 <- finalHOLD %>% 
  model(ARIMA(log(stream_time_min) ~ 0 + pdq(1,1,0) + PDQ(1,0,1) +
                log(unemp) + log(PPIintnet)))

dynmc_arima1 %>% augment() %>% features(.innov,ljung_box,lag=60,dof=6)

ftr_unemp = 
  final_data %>% filter_index("2023 Jan" ~ "2023 Dec") %>% pull(unemp)
ftr_PPIintnet = 
  final_data %>% filter_index("2023 Jan" ~ "2023 Dec") %>% pull(PPIintnet)
ftr_ffrate = 
  final_data %>% filter_index("2023 Jan" ~ "2023 Dec") %>% pull(ffrate)

future1 = new_data(finalHOLD, 12) %>% 
  mutate(unemp = ftr_unemp, PPIintnet = ftr_PPIintnet, ffrate = ftr_ffrate)

future2 = new_data(finalHOLD, 12) %>% 
  mutate(unemp = c(2.3,3,3,3,3,3,3,3,3,3,3,3), PPIintnet = c(65,67,69,67,68,65,67,66,66,66,66,66),
         ffrate = c(4.5,4.5,4.5,4.5,4,4,4,4,4,4,4,4))

final_data%>%filter_index("2019 Jan"~"2023 Dec")%>%autoplot(stream_time_min)+
  autolayer(dynmc_arima1%>%forecast(future1),colour="RED",level=NULL)+
  autolayer(dynmc_arima1%>%forecast(future2),colour="BLUE",level=NULL)+
  ggtitle("Future1 in Red; Future2 in Blue")+
  labs(y="Twitch Stream Time")

finalHOLD %>% model(STL(log(stream_time_min))) %>% components() %>% autoplot()

deSEASON = finalHOLD %>% model(STL(log(stream_time_min))) %>% components() %>% 
  select(season_adjust)

deSEASON %>% gg_tsdisplay(difference(season_adjust), lag_max=30, plot_type = 'partial')

STL_SARIMA = decomposition_model(
  STL(log(stream_time_min)), 
  ARIMA(season_adjust~0+pdq(1,1,0)+PDQ(0,0,0)))

finalHOLD %>% model(STL_SARIMA) %>% report()

finalHOLD %>% model(STL_SARIMA) %>% forecast(h=12) %>%
  autoplot(filter_index(final_data,"2019 Dec"~ "2023 Dec"), level = 0)