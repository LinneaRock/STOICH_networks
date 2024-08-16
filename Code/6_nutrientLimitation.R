#~~~~~~~~~~~~~~~~~~~~#
# Nutrient limitation
#~~~~~~~~~~~~~~~~~~~~#

# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R')

# 2. Format data ####

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
  distinct() |>
  mutate(network_position=ifelse(network_position=='12a','12.5',network_position),
         network_position=as.numeric(network_position))

nuts_wide <- nuts |>
  pivot_wider(names_from = param, values_from = result) |>
  mutate(network_position=ifelse(network_position=='12a','12.5',network_position),
         network_position=as.numeric(network_position))


# 3. Nutrient limitation using DIN:TP from Bergstrom ####
ggplot() +
  geom_point(nuts_wide, mapping = aes(log10(TP_umolL), log10(IN_umolL/TP_umolL), 
                                      fill = network_position), shape = 21, alpha = 0.5) +
  geom_point(medians, mapping = aes(log10(MEDTP_umolL), log10(MEDIN_umolL/MEDTP_umolL), 
                                    color = network_position), size = 3) +
  geom_abline(slope = 0, intercept = log10(3.4), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log10(1.5), linetype = "dashed") +  # bergstrom N limitation line
  theme_classic() +
  scale_color_viridis_c('Network position') +
  scale_fill_viridis_c('Network position') +
  labs(y = "log(IN:TP)", x = "log(TP)") +
  # guides(fill=guide_legend(ncol=2),
  #        color=guide_legend(ncol=2)) +
  annotate('text', label = 'Predicted N limitation below dashed line \n (Bergström, 2010)', 
           x = -1, y = 0.1, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation above dashed line \n (Bergström, 2010)', 
           x = -1.25, y = 0.65, hjust = 0, size = 2)

ggsave('Figures/Limitation/nutrient_limitation.png',width=6.25, height=4.25, units='in')



# 4. Is there any trend in IN:TP ratio? ####
# Mann-Kendall/Thiel-Sens (non-parametric) test to see if there is a significant trend along the network 

mk_lim_dat <- nuts_wide|>
  mutate(lim = log10(IN_umolL/TP_umolL)) |>
  drop_na(lim) |>
  filter(is.finite(lim)) |>
  select(network_position, lim) |>
  arrange(network_position)

sens.slope(mk_lim_dat$lim) # this one line provides all the same information as below

#### Notes from test ####
# The Mann-Kendall test and Thiel-Sen slope tell us that there is a significant decrease along the network in the logged IN:TP ratio. So while the entire network is P-limited, it moves closer toward exiting P-limitation as we move further down the network. 


# data:  mk_lim_dat$lim
# z = -3.9451, n = 804, p-value = 7.977e-05
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
#   -0.0003189885 -0.0001070293
# sample estimates:
#   Sen's slope 
#-0.000211746 

