# defining season based on streamflow

library(tidyverse)
library(dataRetrieval)

gl_nutrients <- read.csv('Data/gl_network_nuts_raw.csv')|> select(-X) |> mutate(date=as.Date(date))
gl_discharge <- read.csv('Data/gl_network_discharge_raw.csv')  |> select(-X) |> mutate(date=as.Date(date)) |> drop_na(discharge_vol_cm)

ggplot(gl_discharge, aes(date,discharge_vol_cm,color=local_site)) +
  geom_line()



# ALB has good data 1994 on, with a few years we'll likely need to remove (looks like 2011-2013)
# GL4 has good data 2009 on, with a few years we'll likely need to remove (looks like 2014-2015)


#GL4discharge <- left_join(gl_nutrients, gl_discharge) |>
GL4discharge <- gl_discharge |>
  drop_na(discharge_vol_cm) |>
  filter(local_site=='GL4',
         year(date)>2008) |>
  mutate(Date=date) |>
  addWaterYear()

ggplot() +
  geom_point(GL4discharge, mapping=aes(date, discharge_vol_cm)) +
  geom_line(GL4discharge, mapping=aes(date, discharge_vol_cm)) 
 

ALBdischarge <- gl_discharge |>
  filter(local_site=='ALB',
         year(date)>1993) |>
  mutate(Date=date) |>
  addWaterYear()

ggplot(ALBdischarge, aes(date, discharge_vol_cm)) +
  geom_point()  +
  geom_line()

# we are just going to use GL4 though because it will be more representative of the network due to proximity
percentile_days<- GL4discharge |>
  group_by(waterYear) |>
  arrange(Date) |>
  mutate(cumulative_dis = cumsum(discharge_vol_cm),
         total_flow = sum(discharge_vol_cm)) |>
  summarise(
    day_20th = cur_data()$Date[which(cumulative_dis >= 0.2 * total_flow)[1]],
    day_50th = cur_data()$Date[which(cumulative_dis >= 0.5 * total_flow)[1]],
    day_80th = cur_data()$Date[which(cumulative_dis >= 0.8 * total_flow)[1]]
  ) |>
  mutate(day_20th_doy = yday(day_20th),
         day_50th_doy = yday(day_50th),
         day_80th_doy = yday(day_80th)) |>
  ungroup()
# things look reasonable except 2015, 2018 - which really don't make sense (GL4)



checks <- GL4discharge |>
  mutate(fakedate = as.Date(paste('1990',month(date), day(date), sep='-'), format='%Y-%m-%d'))

ggplot(checks, aes(fakedate, discharge_vol_cm, color=waterYear)) +
  geom_point() +

  scale_color_viridis_c()



percentile_days |>
  filter(!waterYear %in% c(2015, 2018, 2021)) |>
  #filter(site_no == 401733105392404) |>
  pivot_longer(day_20th_doy:day_80th_doy) |>
  mutate(name=factor(name,
                     labels=c("20th",
                              "50th",
                              "80th"))) |>
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
  theme_bw(base_size=18)+
  theme(legend.position="bottom") +
  facet_wrap(name~., scales="free_y")+
  # scale_y_continuous(
  #       breaks = c(106,136,167,197,228,251),
  #                    labels = c("Apr 15", "May 15", "June 15", "July 15","Aug 15","Sept 15")) +
 # scale_x_continuous(breaks = c(1990, 2005, 2020))+
  theme(legend.position="none")



# exclude those years and find average doys to use in remainder of dataset
ave_percentile_days <- percentile_days|>
  filter(!waterYear %in% c(2015, 2018, 2021)) |>
  summarise(average20_doy = mean(day_20th_doy),
            std20 = sd(day_20th_doy),
            average50_doy = mean(day_50th_doy),
            std50 = sd(day_50th_doy),
            average80_doy = mean(day_80th_doy),
            std80 = sd(day_80th_doy),)

percentile_days_ex <- percentile_days|>
  filter(!waterYear %in% c(2015, 2018, 2021))

write.csv(ave_percentile_days, 'Data/szn_streamflow/ave_percentile_days.csv')
write.csv(percentile_days_ex, 'Data/szn_streamflow/percentile_days.csv')


