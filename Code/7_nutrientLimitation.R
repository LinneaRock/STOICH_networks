#~~~~~~~~~~~~~~~~~~~~#
# Nutrient limitation
#~~~~~~~~~~~~~~~~~~~~#

# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R')

# 2. Format data ####

nuts_wide <- nuts |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  left_join(distances_Km |>
              filter(site1=='Arikaree_GLACIER') |>
              select(-site1) |>
              rename(site=site2,
                     distancefromglacier_km=distance_Km) |>
              mutate(distancefromglacier_km = as.numeric(distancefromglacier_km))
  ) |>
   distinct() |>
  mutate(distancefromglacier_km = ifelse(is.na(distancefromglacier_km), 
                                         0, distancefromglacier_km)) |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  # group_by(network_position, param) |>
  # mutate(mean = mean(result),
  #        median = median(result),
  #        min = min(result),
  #        max = max(result),
  #        SE = std.error(result),
  #        n = n()) |>
  # ungroup() |>
  # distinct() |>
  select(-network_position) |>
  pivot_wider(names_from = param, values_from = result)
# there were no seasonal trends - getting rid of these data
  # pivot_wider(names_from = param, values_from = result) |>
  # group_by(site, year(date), season) |>
  # mutate(yearlymedTP_umolL = median(TP_umolL, na.rm = TRUE),
  #        yearlymedIN_umolL = median(IN_umolL, na.rm = TRUE),
  #        yearlymedTN_umolL = median(TN_umolL, na.rm = TRUE),
  #        yearlymedIP_umolL = median(IP_umolL, na.rm = TRUE)) |>
  # ungroup() |>
  # select(site, network_position, season, `year(date)`,eco_type, yearlymedTP_umolL, yearlymedIP_umolL, yearlymedIN_umolL, yearlymedTN_umolL) |>
  # distinct() #|>
  # group_by(site, season) |>
  # mutate(MEDTP_umolL = median(yearlymedTP_umolL, na.rm = TRUE),
  #        MEDTN_umolL = median(yearlymedTN_umolL, na.rm = TRUE),
  #        MEDIN_umolL = median(yearlymedIN_umolL, na.rm = TRUE),
  #        MEDIP_umolL = median(yearlymedIP_umolL, na.rm = TRUE)) |>
  # ungroup() |>
  # select(site, network_position, season, eco_type, MEDTP_umolL, MEDTN_umolL, MEDIP_umolL, MEDIN_umolL) |>
  # distinct()

nuts_wide <- nuts |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  pivot_wider(names_from = param, values_from = result)

# 3. Nutrient limitation using DIN:TP from Bergstrom ####
IN <- ggplot() +
  geom_jitter(nuts_wide, mapping = aes(log10(IN_umolL), log10(IN_umolL/TP_umolL), 
                                      fill = distancefromglacier_km), shape = 21, alpha = 0.5) +
  # geom_jitter(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
  #                                      fill = distancefromglacier_km), shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  theme_classic() +
  scale_color_viridis_c('Distance from  glacier (km)') +
  scale_fill_viridis_c('Distance from \nglacier (km)') +
  labs(y = "log(IN:TP)", x = "log(IN)") +
  annotate('text', label = 'Predicted N limitation below \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.05, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation above \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.65, hjust = 0, size = 3) +
  theme(legend.position='left') 

IN

TP <- ggplot() +
  geom_jitter(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
                                       fill = distancefromglacier_km), shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  theme_classic() +
  scale_color_viridis_c('Distance from glacier (km)') +
  scale_fill_viridis_c('Distance from glacier (km)') +
  labs(y = "log(IN:TP)", x = "log(TP)") +
  annotate('text', label = 'Predicted N limitation below \ndashed line (Bergström, 2010)', 
           x = -1.25, y = 0.1, hjust = 0, size = 5) +
  annotate('text', label = 'Predicted P limitation above \mdashed line (Bergström, 2010)', 
           x = -1.25, y = 0.65, hjust = 0, size = 5)


violin <- nuts_wide|>
  mutate(position = ifelse(grepl('INLET', site), 'inlets', NA),
         position = ifelse(grepl('OUTLET', site), 'outlets', position),
         position = ifelse(eco_type == 'lake', 'lakes', position),
         position = ifelse(eco_type == 'glacier', 'glacier', position)) |>
  mutate(WS_Group=factor(WS_Group, levels=c('GL5','GL4','GL3','ALB')))

TP

# 4. Compare inlets, lakes, outlets #### 


#get significance to add to figure using geom_text 


sig.letters <- data.frame(NA)

  h <- aov(lim~position, violin|>mutate(lim = log10(IN_umolL/TP_umolL)) |>
             drop_na(lim) |>
             filter(is.finite(lim)) |> 
             filter(position!='glacier') |>
             mutate(position = factor(position, levels=c('inlets','lakes','outlets'))))
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$position$Letters)
  cld2$position <- rownames(cld2)


means <- left_join(violin |> filter(position!='glacier'), cld2) |>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  group_by(letters, position) |>
  summarise(max.result = max(lim, na.rm = TRUE),
            mean = mean(lim, na.rm=TRUE)) |>
  distinct() |>
  # changing the order of letters (which were reversed) for attractiveness of figure
  mutate(letters=ifelse(position=='inlets','a',
                        ifelse(position=='lakes', 'b', 
                               ifelse(position=='outlets','c', letters))))



compare <- ggplot(violin |> filter(position!='glacier')) +
  geom_violin(aes(position, log10(IN_umolL/TP_umolL))) +
  geom_jitter(aes(position, log10(IN_umolL/TP_umolL), color=WS_Group)) +
  geom_point(means, mapping=aes(position, mean)) +
  scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  geom_text(means, mapping=aes(position, 
                               max.result+0.5, label = letters), 
            color='red4', size=4) +
  theme_classic() +
  labs(x='', y='log(IN:TP)') 
  
  # ^^ Kruskal-Wallis test comparing lakeout along network 


IN + compare +
  plot_layout(guides='collect') &
  theme(legend.position='none') &
  ylim(min(0), max(3.5)) 

ggsave('Figures/nutrient_limitation.png',width=8.5, height=5.5, units='in')


compare_lims <- violin |>
  mutate(lim=IN_umolL/TP_umolL) |>
  select(-eco_type) |>
  group_by(date, position) |>
  summarise(lim=mean(lim)) |> # take mean of depth 0, 3m measures in hte lakes
  ungroup() |>
  pivot_wider(names_from = 'position', values_from = 'lim')

  
ggplot(compare_lims) +
  geom_point(aes(inlets, outlets)) +
  geom_abline(slope=1, intercept = 0)

ggplot(compare_lims) +
  geom_point(aes(inlets, lakes)) +
  geom_abline(slope=1, intercept = 0)

ggplot(compare_lims) +
  geom_point(aes(lakes,outlets)) +
  geom_abline(slope=1, intercept = 0)


# 5. Are there trends in IN:TP ratio? ####
# Mann-Kendall/Thiel-Sens (non-parametric) test to see if there is a significant trend along the network 

mk_lim_dat <- nuts_wide|>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  select(distancefromglacier_km, lim) |>
  arrange(distancefromglacier_km)

sens.slope(mk_lim_dat$lim) # this one line provides all the same information as below

#### Notes from test ####
# The Mann-Kendall test and Thiel-Sen slope tell us that there is a significant decrease along the network in the logged IN:TP ratio. So while the entire network is P-limited, it moves closer toward exiting P-limitation as we move further down the network. 


# data:  mk_lim_dat$lim
# z = -5.7516, n = 987, p-value = 8.84e-09
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
#   -0.0002873674 -0.0001406540
# sample estimates:
#   Sen's slope 
# -0.0002126189 

## N:P vs TP ####
mk_lim_dat <- nuts_wide|>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  select(TP_umolL, lim) |>
  arrange(TP_umolL)

sens.slope(mk_lim_dat$lim) 

# data:  mk_lim_dat$lim
# z = -18.269, n = 987, p-value < 2.2e-16
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
#   -0.0007070226 -0.0005844776
# sample estimates:
#   Sen's slope 
# -0.0006442713 


## N:P vs IN ####
mk_lim_dat <- nuts_wide|>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  select(IN_umolL, lim) |>
  arrange(IN_umolL)

sens.slope(mk_lim_dat$lim) 

# data:  mk_lim_dat$lim
# z = 27.265, n = 987, p-value < 2.2e-16
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
#   0.0008424348 0.0009437187
# sample estimates:
#   Sen's slope 
# 0.0008929035 
