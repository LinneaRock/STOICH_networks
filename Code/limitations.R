#::::::::::::::::::::
# Nutrient limitation
#::::::::::::::::::::   


source('Data/CALL_DATA_PACKAGES.R')

# set up data ####
medians <- nuts |>
  pivot_wider(names_from = param, values_from = result) |>
  group_by(site, year(date), season) |>
  mutate(yearlymedTP_umolL = median(TP_umolL, na.rm = TRUE),
         yearlymedIN_umolL = median(IN_umolL, na.rm = TRUE),
         yearlymedTN_umolL = median(TN_umolL, na.rm = TRUE),
         yearlymedIP_umolL = median(IP_umolL, na.rm = TRUE)) |>
  ungroup() |>
  select(site, network_position, season, `year(date)`,eco_type, yearlymedTP_umolL, yearlymedIP_umolL, yearlymedIN_umolL, yearlymedTN_umolL) |>
  distinct() |>
  group_by(site, season) |>
  mutate(MEDTP_umolL = median(yearlymedTP_umolL, na.rm = TRUE),
         MEDTN_umolL = median(yearlymedTN_umolL, na.rm = TRUE),
         MEDIN_umolL = median(yearlymedIN_umolL, na.rm = TRUE),
         MEDIP_umolL = median(yearlymedIP_umolL, na.rm = TRUE)) |>
  ungroup() |>
  select(site, network_position, season, eco_type, MEDTP_umolL, MEDTN_umolL, MEDIP_umolL, MEDIN_umolL) |>
  distinct()

nuts_wide <- nuts |>
  pivot_wider(names_from = param, values_from = result) 
  
  


# DIN:TP limitation ####
ggplot() +
   geom_point(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
                                        fill = season), shape = 21, alpha = 0.2) +
  geom_point(medians, mapping = aes(log10(MEDTP_umolL), log10(MEDIN_umolL/MEDTP_umolL), 
                                    color = season, shape = eco_type), size = 3) +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  theme_minimal() +
  labs(y = "Log DIN:TP", x = "Log TP") +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Bergström, 2010)', 
           x = -1, y = 0.1, hjust = 0, size = 4) +
  annotate('text', label = 'Predicted P limitation above dashed line \n (Bergström, 2010)', 
           x = -1.25, y = 0.75, hjust = 0, size = 4) 
  



## color by network position ####
ggplot() +
  geom_point(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
                                      fill = network_position), shape = 21, alpha = 0.5) +
  geom_point(medians, mapping = aes(log10(MEDTP_umolL), log10(MEDIN_umolL/MEDTP_umolL), 
                                    color = network_position), size = 3) +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  theme_classic() +
  scale_color_viridis_d('Network Position') +
  scale_fill_viridis_d('Network Position') +
  labs(y = "Log DIN:TP", x = "Log TP") +
  guides(fill=guide_legend(ncol=2),
         color=guide_legend(ncol=2)) +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Bergström, 2010)', 
           x = -1, y = 0.1, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation above dashed line \n (Bergström, 2010)', 
           x = -1.25, y = 0.65, hjust = 0, size = 2)

ggsave('Figures/Limitation/nutrient_limitation.png',width=6.25, height=4.25, units='in')


### dark-theme color by network position ####
#plot without data
ggplot() +
  geom_point(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
                                      fill = network_position), shape = 21, alpha = 0.5, color='black') +
  geom_point(medians, mapping = aes(log10(MEDTP_umolL), log10(MEDIN_umolL/MEDTP_umolL), 
                                    color = as.numeric(network_position)), size = 3, color='black') +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  dark_theme_classic() +
  scale_color_viridis_c('Network position') +
  scale_fill_manual('Network position', values=c(rep('black', 14))) +
  labs(y = "Log IN:TP", x = "Log TP") +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Bergström, 2010)', 
           x = -1, y = 0.1, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation above dashed line \n (Bergström, 2010)', 
           x = -1.25, y = 0.65, hjust = 0, size = 2)


ggsave('Figures/DarkTheme/lim_blank.png',width=6.25, height=4.25, units='in')


# plot with data
ggplot() +
  geom_point(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
                                      fill = as.numeric(network_position)), shape = 21, alpha = 0.5) +
  geom_point(medians, mapping = aes(log10(MEDTP_umolL), log10(MEDIN_umolL/MEDTP_umolL), 
                                    color = as.numeric(network_position)), size = 3) +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  dark_theme_classic() +
  scale_color_viridis_c('Network position') +
  scale_fill_viridis_c('Network position') +
  labs(y = "Log IN:TP", x = "Log TP") +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Bergström, 2010)', 
           x = -1, y = 0.1, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation above dashed line \n (Bergström, 2010)', 
           x = -1.25, y = 0.65, hjust = 0, size = 2) 

ggsave('Figures/DarkTheme/nutrient_limitation.png',width=6.25, height=4.25, units='in')


## test for differences in limitation ####
### boxplot and kruskal wallis test ####
#get significance to add to figure using geom_text 

tmp_lim <- nuts_wide |>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim))

  h <- aov(lim~network_position, tmp_lim)
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$network_position$Letters) 
  cld2$network_position <- rownames(cld2)
  sig.letters <- cld2


sig.letters <- sig.letters |>
  drop_na(letters) 

means <- left_join(tmp_lim, sig.letters) |>
  group_by(letters, network_position) |>
  summarise(max.result = max(lim, na.rm = TRUE)) |>
  distinct()


ggplot(nuts_wide, aes(network_position, log10(IN_umolL/TP_umolL), group = network_position)) +
  geom_jitter(aes(color=eco_type), shape = 16, size =2, alpha = 0.2,
              position=position_jitter(0.3)) +
  geom_boxplot(alpha = 0.2) +
  # stat_summary(geom = 'point', fun = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  labs(x = 'Network Position', y = '') +
  geom_text(means, mapping=aes(network_position, 
                               max.result, label = letters), 
            color='red4', size=4) +
  # ^^ Kruskal-Wallis test comparing lakes along network 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))

ggsave('Figures/Limitation/limitation_KWtest.png',width=6.25, height=4.25, units='in')


### Mann-Kendall/Thiel-Sens (non-parametric) test to see if there is a significant trend along the network ####
mk_lim_dat <- tmp_lim |>
  select(network_position, lim) |>
  arrange(network_position)

sens.slope(mk_lim_dat$lim) # this one line provides all the same information as below

#### Notes from test!! ####
# The Mann-Kendall test and Thiel-Sen slope tell us that DIN:TP ratio that there is a significant decrease along the network in DIN:TP ratio. So while the entire network is P-limited, it moves closer toward exiting P-limitation as we move further down the network. 


# mk_lim <- mk_lim_dat |>
#   summarise(z.stat = glance(mk.test(lim))$statistic,
#             p.value = glance(mk.test(lim))$p.value,
#             n = glance(mk.test(lim))$parameter,
#             ### Calculate Sen's slope and add to dataframe ####
#             slope = as.numeric(sens.slope(lim)[[1]])) 




## Check out DIN:TP inlet vs outlet
inout <- tmp_lim |>
  select(site, network_position, lim, date ,eco_type) |>
  mutate(position = ifelse(grepl("INLET", site), 'inlet', NA)) |>
  mutate(position = ifelse(grepl("OUTLET", site), 'outlet', position)) |>
  mutate(position = ifelse(eco_type=='lake', 'in-lake', position)) |>
  mutate(position = ifelse(site=='GL1_LAKE', 'inlet2', position)) |>
  left_join(sites) |>
  left_join(greenlakes_LC) |>
  select(-Layer_1, -LandCoverArea_km2,- arik_flow_site, - notes, - elevation_m, - drainage_area_ha, - geometry) |>
  distinct() |>
  group_by(site, date, position, WS_Group) |>
  summarise(lim=mean(lim)) |> # take mean of depth 0, 3m measures in hte lakes
  ungroup() |>
  pivot_wider(id_cols = c('WS_Group', 'date'),names_from = 'position', values_from = 'lim')


### inlet vs outlet limitation ####
ggplot(inout) +
  geom_point(aes(inlet, outlet)) +
  geom_point(aes(inlet2, outlet)) +
  geom_abline(slope=1, intercept=0) +
  labs(x='Inlet DIN:TP', y='Outlet DIN:TP') +
  theme_classic()
ggsave('Figures/Limitation/limitation_inletoutlet.png',width=6.25, height=4.25, units='in')


### in-lake vs outlet limitation ####
ggplot(inout) +
  geom_point(aes(`in-lake`, outlet)) +
  geom_abline(slope=1, intercept=0) +
  labs(x='In-lake DIN:TP', y='Outlet DIN:TP') +
  theme_classic()
ggsave('Figures/Limitation/limitation_in-lake_outlet.png',width=6.25, height=4.25, units='in')


### inlet vs in-lake limitation ####
ggplot(inout) +
  geom_point(aes(inlet, `in-lake`)) +
  geom_point(aes(inlet2, `in-lake`)) +
  geom_abline(slope=1, intercept=0, color='black') +
  labs(x='Inlet DIN:TP', y='In-lake DIN:TP') +
  theme_classic()
ggsave('Figures/Limitation/limitation_inlet_in-lake.png',width=6.25, height=4.25, units='in')

#### dark-theme comparisons limitation ####
ggplot(inout) +
  geom_point(aes(inlet, `in-lake`, color='x:inlet, y:in-lake')) +
  geom_point(aes(inlet2, `in-lake`, color='x:inlet, y:in-lake')) +
  geom_point(aes(`in-lake`, outlet, color='x:in-lake, y:outlet')) +
  geom_point(aes(inlet, outlet, color='x:inlet, y:outlet')) +
  geom_point(aes(inlet2, outlet, color='x:inlet, y:outlet')) +
  geom_abline(slope=1, intercept=0) +
  scale_color_manual('', values=c('#762A83','#F7F7F7','#1B7837')) +
  labs(x='X IN:TP', y='Y IN:TP') +
  dark_theme_classic()
ggsave('Figures/DarkTheme/limitation_inlet_in-lake.png',width=6.25, height=4.25, units='in')


# are there annual trends in limitation? ####
limitation_ts <- nuts_wide |>
  mutate(np = IN_umolL/TP_umolL) |>
  drop_na(np) |>
  mutate(year = year(date)) |>
  mutate(CDate=as.Date(paste0(ifelse(month(date) < 10, "1901", "1900"),
                              "-", month(date), "-", day(date)))) 

ggplot(limitation_ts) +
  geom_line(aes(CDate, np, color = as.character(year))) +
  facet_wrap(~site, scales = 'free') +
  scale_color_viridis_d()

ggplot(limitation_ts) +
  geom_line(aes(CDate, np, color = as.character(year))) +
  facet_wrap(~site, scales = 'free') +
  scale_color_viridis_d()

alb_inlet_ts <- limitation_ts |>
  filter(site=='ALB_INLET') |>
  select(date, np) |>
  mutate(month = month(date),
         year = year(date)) |>
  group_by(year, month) |>
  summarise(mean_NP = mean(np)) |>
  ungroup()

ggplot(alb_inlet_ts) +
  geom_line(aes(date, np))

ts <- ts(alb_inlet_ts, frequency=9)

plot(decompose(ts))

# Other Explorations of nutrient limitation ####


## TN:TP limitation ####
ggplot() +
  geom_point(nuts_wide, mapping=aes(log10(TP_umolL), log10(TN_umolL/TP_umolL), fill = season), 
             size = 2.5, shape = 21, alpha = 0.2) +
  geom_point(medians, mapping = aes(log10(MEDTP_umolL), log10(MEDTN_umolL/MEDTP_umolL), 
                                    color = season, shape = eco_type), size = 3) +
  geom_abline(slope = 0, intercept = log10(41), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(19), linetype = "dashed") +  # bergstrom N limitation line
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  geom_vline(xintercept = log(30, base = 10)) + # dodds mccauley N limitation P > 30
  geom_abline(slope = 0, intercept = log(32, base = 10)) + # dodds mccauley N limitation TN:TP < 14
  geom_abline(slope = 0, intercept = log(38, base = 10), color = '#336a98') + # Sakamoto, 1966; Smith 1982; Rhee 1980, Forsberg 1980  
  geom_abline(slope = 0, intercept = log(22, base = 10), color = '#336a98') + # Sakamoto, 1966; Smith 1982; Rhee 1980, Forsberg 1980
  geom_abline(slope = 0, intercept = log(53, base = 10), color = "#ffc857") + # Ptacnik, 2010
  theme_minimal() +
  labs(y = "Log TN:TP", x = "Log TP") +
  annotate('text', label = 'Redfield 16:1 line', x = 0.5, y = 1.1, hjust = 0, size = 2.5, color = "red4") +
  annotate('text', label = 'Predicted N limitation\n below dashed line \n (Bergström, 2010)', 
           x = -0.75, y = 1.0, hjust = 0, size = 2.5) +
  annotate('text', label = 'Predicted P limitation\n above dashed line \n (Bergström, 2010)', 
           x = 0.25, y = 1.75, hjust = 0, size = 2.5) +
  annotate('text', label = 'Predicted N limitation \n below blue line \n (Forsberg, 1980; Rhee, 1980; \n Sakamoto, 1966; Smith, 1982)', x = -1.5, y = 1, hjust = 0, size = 2.5, color = '#336a98') +
  annotate('text', label = 'Predicted P limitation \n above blue line \n (Forsberg, 1980; Rhee, 1980; \n Sakamoto, 1966; Smith, 1982)', x = -1.5, y = 3, hjust = 0, size = 2.5, color = '#336a98') + 
  annotate('text', label = 'Predicted N limitation \n (Dodds & McCauley, 1992)', 
           x = 1.5, y = 1, hjust = 0, size = 2.5) +
  annotate('text', label = 'Predicted P limitation \n (Ptacnik et al., 2010)',
           x = 1, y = 1.76, hjust = 0, size = 2.5, color = "#ffc857")




## DIN:DIP limitation ####
ggplot(nuts_wide) +
  geom_point(aes(log10(IP_umolL), log10((IN_umolL/IP_umolL)), fill = season), size = 2.5, shape = 21, alpha = 0.2) +
  geom_point(medians, mapping = aes(log10(MEDIP_umolL), log10(MEDIN_umolL/MEDIP_umolL), 
                                    color = season, shape = eco_type), size = 3) +
  geom_abline(slope = 0, intercept = log10(100), linetype = "dashed") + # Keck and Lepori P limitation line
  geom_abline(slope = 0, intercept = log10(1), linetype = "dashed") +  # keck and lepori N limitation line
  #geom_abline(slope = 0, intercept = log10(16), color = "red4") + # redfield
  theme_minimal() +
  labs(y = "Log DIN:DIP", x = "Log IP") +
  #annotate('text', label = 'Redfield 16:1 line', x = -0.5, y = 1.1, hjust = 0, size = 2, color = "red4") +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Keck & Lepori, 2012)', x = -2, y = -0.1,
           hjust = 0, size = 4) +
  annotate('text', label = 'Predicted P limitation\n above dashed line \n (Keck & Lepori, 2012)', 
           x = -2.5, y = 2, hjust = 0, size = 4)

