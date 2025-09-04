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
  select(-network_position) |>
  pivot_wider(names_from = param, values_from = result)


# 3. Nutrient limitation using DIN:TP from Bergstrom ####
IN <- ggplot() +
  geom_jitter(nuts_wide |> filter(is.finite(log10(IN_umolL/TP_umolL))), mapping = aes(log10(IN_umolL), log10(IN_umolL/TP_umolL), 
                                      fill = distancefromglacier_km), shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log10(3.4*2.211353), linetype = "dashed") + # bergstrom P limitation line, multiply by constant to get molar ratio (assuming DIN is as N in DIN:TP)
  geom_abline(slope = 0, intercept = log10(1.5*2.211353), linetype = "dashed") +  # bergstrom N limitation line, multiply by constant to get molar ratio
  theme_classic() +
  scale_color_viridis_c('Distance from  glacier (km)') +
  scale_fill_viridis_c('Distance from \nglacier (km)') +
  labs(y = "log(IN:TP)", x = "log(IN)") +
  annotate('text', label = 'Predicted N limitation below \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.15, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation above \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.85, hjust = 0, size = 3) +
  theme(legend.position='left') 

IN

TP <- ggplot() +
  geom_jitter(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
                                       fill = distancefromglacier_km), shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log10(3.4*2.211353), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5*2.211353), linetype = "dashed") +  # bergstrom N limitation line
  theme_classic() +
  scale_color_viridis_c('Distance from glacier (km)') +
  scale_fill_viridis_c('Distance from glacier (km)') +
  labs(y = "log(IN:TP)", x = "log(TP)") +
  annotate('text', label = 'Predicted N limitation below \ndashed line (Bergström, 2010)', 
           x = -1.25, y = 0.15, hjust = 0, size = 5) +
  annotate('text', label = 'Predicted P limitation above \ndashed line (Bergström, 2010)', 
           x = -1.25, y = 0.85, hjust = 0, size = 5)


violin <- nuts_wide|>
  mutate(position = ifelse(grepl('INLET', site), 'inlets', NA),
         position = ifelse(grepl('OUTLET', site), 'outlets', position),
         position = ifelse(eco_type == 'lake', 'lakes', position),
         position = ifelse(eco_type == 'glacier', 'glacier', position)) |>
  mutate(WS_Group=factor(WS_Group, levels=c('GL5','GL4','GL3','ALB')))

TP


ggplot() +
  geom_jitter(nuts_wide, mapping = aes(distancefromglacier_km, log10(IN_umolL/TP_umolL), 
                                       fill = distancefromglacier_km), shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log10(3.4*2.211353), linetype = "dashed") + # bergstrom P limitation line, multiply by constant to get molar ratio (assuming DIN is as N in DIN:TP)
  geom_abline(slope = 0, intercept = log10(1.5*2.211353), linetype = "dashed") +  # bergstrom N limitation line, multiply by constant to get molar ratio
  theme_classic() +
  scale_color_viridis_c('Distance from  glacier (km)') +
  scale_fill_viridis_c('Distance from \nglacier (km)') +
  labs(y = "log(IN:TP)", x = "log(IN)") +
  annotate('text', label = 'Predicted N limitation below \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.15, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation above \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.85, hjust = 0, size = 3) +
  theme(legend.position='left') 

# 4. compare szns ####
sig.letters <- data.frame(NA)

h <- aov(lim~szn, violin|>mutate(lim = log10(IN_umolL/TP_umolL)) |>
           drop_na(lim) |>
           filter(is.finite(lim)) |> 
           #filter(szn!='glacier') |>
           mutate(szn = factor(szn, levels=c('baseflow','spring snowmelt','falling limb'))))
tukey <- TukeyHSD(h)
cld <- multcompLetters4(h, tukey)
cld2 <- data.frame(letters = cld$szn$Letters)
cld2$szn <- rownames(cld2)


means <- left_join(violin, cld2) |>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  group_by(letters, szn) |>
  summarise(max.result = max(lim, na.rm = TRUE),
            mean = mean(lim, na.rm=TRUE)) |>
  distinct() |>
  # changing the order of letters (which were reversed) for attractiveness of figure
  mutate(letters=ifelse(szn=='baseflow','a',
                        ifelse(szn=='falling limb', 'b', 
                               ifelse(szn=='spring snowmelt','c', letters))))



compare<-ggplot(violin |> filter(is.finite(log10(IN_umolL/TP_umolL)))) +
  geom_violin(aes(szn, log10(IN_umolL/TP_umolL))) +
  geom_jitter(aes(szn, log10(IN_umolL/TP_umolL), fill=WS_Group, color=WS_Group),shape=21, alpha=0.65) +
  geom_point(means, mapping=aes(szn, mean)) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  geom_text(means, mapping=aes(szn, 
                               max.result+0.5, label = letters), 
            color='red4', size=4) +
  theme_classic() +
  labs(x='', y='log(IN:TP)') 


IN + compare +
  plot_layout(guides='collect') &
  theme(legend.position='none') &
  ylim(min(0), max(3.5)) 

ggsave('Figures/nutrient_limitation.png',width=8.5, height=5.5, units='in')


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
# z = -5.6906, n = 986, p-value = 1.266e-08
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
#   -0.0002851751 -0.0001382792
# sample estimates:
#   Sen's slope 
# -0.0002105964  

## N:P vs TP ####
mk_lim_dat <- nuts_wide|>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  select(TP_umolL, lim) |>
  arrange(TP_umolL)

sens.slope(mk_lim_dat$lim) 

# data:  mk_lim_dat$lim
# z = -18.245, n = 986, p-value < 2.2e-16
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
#   -0.0007068016 -0.0005840990
# sample estimates:
#   Sen's slope 
# -0.00064405 


## N:P vs IN ####
mk_lim_dat <- nuts_wide|>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  select(IN_umolL, lim) |>
  arrange(IN_umolL)

sens.slope(mk_lim_dat$lim) 

# data:  mk_lim_dat$lim
# z = 27.229, n = 986, p-value < 2.2e-16
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
#   0.0008423834 0.0009439332
# sample estimates:
#   Sen's slope 
# 0.0008928972 








# XX. Compare inlets, lakes, outlets #### 


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


# dark theme fig for presentation ####
library(ggdark)

ggplot() +
  geom_jitter(nuts_wide |> filter(is.finite(log10(IN_umolL/TP_umolL))), mapping = aes(log10(IN_umolL), log10(IN_umolL/TP_umolL), 
                                                                                      fill = distancefromglacier_km), shape = 21, alpha = 0.5) + # alpha=0)+ 
  geom_abline(slope = 0, intercept = log10(3.4*2.211353), linetype = "dashed") + # bergstrom P limitation line, multiply by constant to get molar ratio (assuming DIN is as N in DIN:TP)
  geom_abline(slope = 0, intercept = log10(1.5*2.211353), linetype = "dashed") +  # bergstrom N limitation line, multiply by constant to get molar ratio
  dark_theme_classic() +
  scale_color_viridis_c('Distance from  glacier (km)') +
  scale_fill_viridis_c('Distance from \nglacier (km)') +
  labs(y = "log(IN:TP)", x = "log(IN)") +
  annotate('text', label = 'Predicted N limitation below \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.15, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation above \ndashed line (Bergström, 2010)', 
           x = 0.45, y = 0.85, hjust = 0, size = 3) +
  theme(legend.position='left') 
ggsave('Figures/darkTheme/limitation.png',height=4.5,width=6.5,units='in',dpi=1200)
#ggsave('Figures/darkTheme/limitation_nopoints.png',height=4.5,width=6.5,units='in',dpi=1200)
