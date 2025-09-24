#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# defining season based on streamflow #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)
library(dataRetrieval)

GL4discharge <- read.csv('Data/gl_network_discharge_raw.csv') |> select(-X) |> 
  filter(local_site=='GL4') |>
  mutate(Date=as.Date(date)) |> 
  drop_na(discharge_vol_cm)  |>
  addWaterYear() |>
  filter(waterYear >= 2009)

ggplot(GL4discharge, aes(Date, discharge_vol_cm)) +
  geom_line() +
  geom_point(alpha=0.25,size=1)

# some years with low data that might need to be removed but overall looks decent

percentile_days<- GL4discharge |>
  group_by(waterYear) |>
  arrange(Date) |>
  mutate(cumulative_dis = cumsum(discharge_vol_cm),
         total_flow = sum(discharge_vol_cm)) |>
  summarise(
    day_20th = pick(Date)[[1]][which(pick(cumulative_dis)[[1]] >= 0.2 * pick(total_flow)[[1]])[1]],
    day_50th = pick(Date)[[1]][which(pick(cumulative_dis)[[1]] >= 0.5 * pick(total_flow)[[1]])[1]],
    day_80th = pick(Date)[[1]][which(pick(cumulative_dis)[[1]] >= 0.8 * pick(total_flow)[[1]])[1]]
  ) |> 
  # summarise(
  #   day_20th = cur_data()$Date[which(cumulative_dis >= 0.2 * total_flow)[1]],
  #   day_50th = cur_data()$Date[which(cumulative_dis >= 0.5 * total_flow)[1]],
  #   day_80th = cur_data()$Date[which(cumulative_dis >= 0.8 * total_flow)[1]]
  # ) |>
  mutate(day_20th_doy = yday(day_20th),
         day_50th_doy = yday(day_50th),
         day_80th_doy = yday(day_80th)) |>
  ungroup()





# look at water year hydrograph, cumulative discharge graph
checks <- GL4discharge |>
  mutate(
    wy_doy = yday(Date) - yday(as.Date(paste0(year(Date), "-10-01"))) + 1,
    wy_doy = ifelse(wy_doy <= 0, wy_doy + 365, wy_doy) 
  ) |>
  group_by(waterYear) |>
  arrange(Date) |>
  mutate(cumulative_dis = cumsum(discharge_vol_cm),
         total_flow = sum(discharge_vol_cm))


# hydrograph
ggplot(checks, aes(wy_doy, discharge_vol_cm, color = as.factor(waterYear))) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(1, 92, 183, 274, 366),
                     labels = c("Oct", "Jan", "Apr", "Jul", "Oct"))


# cumulative sum
ggplot(checks, aes(wy_doy, cumulative_dis, color = as.factor(waterYear))) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(1, 92, 183, 274, 366),
                     labels = c("Oct", "Jan", "Apr", "Jul", "Oct"))

