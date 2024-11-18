# defining season based on streamflow

library(tidyverse)
library(dataRetrieval)

GL4discharge <- read.csv('Data/discharge_outliers_removed.csv') |>
  mutate(Date=as.Date(date)) |>
  filter(site=='GL4_OUTLET',
         year(Date)>2008) |>
  addWaterYear()

ggplot(GL4discharge, aes(Date,result)) +
  geom_point()  +
  geom_line()
#there's a lot of missing data still. But let's see what we can find


percentile_days <- GL4discharge %>%
  group_by(waterYear) %>%
  arrange(Date) %>%
  mutate(cumulative_dis = cumsum(result),
         total_flow = sum(result)) %>%
  summarise(
    day_20th = cur_data()$Date[which(cumulative_dis >= 0.2 * total_flow)[1]],
    day_50th = cur_data()$Date[which(cumulative_dis >= 0.5 * total_flow)[1]],
    day_80th = cur_data()$Date[which(cumulative_dis >= 0.8 * total_flow)[1]]
  ) %>%
  mutate(day_20th_doy = yday(day_20th),
         day_50th_doy = yday(day_50th),
         day_80th_doy = yday(day_80th))
# things look reasonable except 2015, 2018, 2021 - which really don't make sense


library(ggdark)
percentile_days %>%
  filter(!waterYear %in% c(2015, 2018, 2021)) |>
  #filter(site_no == 401733105392404) %>%
  pivot_longer(day_20th_doy:day_80th_doy) %>%
  mutate(name=factor(name,
                     labels=c("20th",
                              "50th",
                              "80th"))) %>%
  ggplot(aes(x=waterYear, y=value, color=name))+#,
  #shape=site_no,
  # linetype=site_no))+
  geom_point()+
  # scale_color_manual(values=c("cornflowerblue","white"),
  #                    name="Cumul. Q percentiles:")+
  scale_color_manual(values=c("yellow","cyan","purple"),
                     name="Cumul. Q percentiles:")+
  scale_shape_manual(values=c(21,2))+
  geom_smooth(method="lm", se=TRUE)+
  labs(
    # title="The Loch outlet & Michigan River",
    # subtitle="USGS Site no. 06614800",
    x="Year",
    y="Day of year")+
  dark_theme_bw(base_size=18)+
  theme(legend.position="bottom") +
  facet_wrap(name~., scales="free_y")+
  # scale_y_continuous(
  #       breaks = c(106,136,167,197,228,251),
  #                    labels = c("Apr 15", "May 15", "June 15", "July 15","Aug 15","Sept 15")) +
  scale_x_continuous(breaks = c(1990, 2005, 2020))+
  theme(legend.position="none")



# exclude those years and find average doys to use in remainder of dataset
ave_percentile_days <- percentile_days|>
  filter(!waterYear %in% c(2015, 2018, 2021)) |>
  summarise(average20_doy = mean(day_20th_doy),
            average50_doy = mean(day_50th_doy),
            average80_doy = mean(day_80th_doy))

percentile_days_ex <- percentile_days|>
  filter(!waterYear %in% c(2015, 2018, 2021))

write.csv(ave_percentile_days, 'Data/szn_streamflow/ave_percentile_days.csv')
write.csv(percentile_days_ex, 'Data/szn_streamflow/percentile_days.csv')


