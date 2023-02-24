#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to make a discharge TS figure 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


source('Data/CALL_DATA_PACKAGES.R') 


# timeseries ####
ggplot(discharge, aes(date, result, color = as.factor(network_position))) +
  geom_line() +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('Network\nposition',direction = -1) +
  labs(x = '', y = 'Daily discharge '~m^3)
ggsave('Figures/discharge_timeseries.png', width = 6.5, height = 4.5, units = 'in', dpi = 1200)



# average timeseries - water year ####
wateryear_ave <- discharge |>
  mutate(x = round((day(date)/5))*5,
         x = ifelse(x == 0, 1, x), 
         date2 = paste(year(date), month(date), x, sep = "-")) |>
  mutate(Date = as.Date(date2)) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date)))) |>
  # mutate(decade = ifelse(year(date) <= 1990, 1, NA),
  #        decade = ifelse(between(year(date), 1990, 2000), "1990-1999", decade),
  #        decade = ifelse(between(year(date), 2000, 2009), "2000-2009", decade),
  #        decade = ifelse(between(year(date), 2010, 2019), "2010-2019", decade)) |>
  # mutate(decade = as.factor(decade)) |>
  group_by(site, CDate) |>
  mutate(ave_weekly_dis = mean(result)) |>
  ungroup() |>
  addWaterYear() |>
  select(site, network_position, CDate, ave_weekly_dis) |>
  distinct() |>
  # mutate(date = CDate) |>
  # mutate(date = ifelse(is.na(date), as.Date(as.character('1901-03-01')), date)) |> # this date rounded to Feb. 30 and then became NA.
  mutate(ave_weekly_dis = ifelse(network_position %in% c('1','4') &
                                   year(CDate) == '1900', NA, ave_weekly_dis))

ggplot(wateryear_ave, aes(CDate, ave_weekly_dis, color = as.factor(network_position))) +
 geom_line() +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('',end=0.9,direction = -1) +
  labs(x = '', y = 'Daily discharge '~m^3) +
  scale_x_date(labels = date_format('%b'))
ggsave('Figures/ave_weekly_dis.png', width = 6.5, height = 4.5, units = 'in', dpi = 1200)

