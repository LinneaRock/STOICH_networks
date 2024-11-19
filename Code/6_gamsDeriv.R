#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Use gams to assess trends along network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 
library(colorblindr)
library(scales)



# 2. Format data ####
gams <- nuts |>
  mutate(nutrient = case_when(grepl('N', param)~'Nitrogen',
                              grepl('P', param)~'Phosphorus')) |>
  rbind(stoich |>
          mutate(nutrient='Ratio')) |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  left_join(distances_Km |>
              filter(site1=='Arikaree_GLACIER') |>
              select(-site1) |>
              rename(site=site2,
                     distancefromglacier_Km=distance_Km)
  ) |>
  mutate(distancefromglacier_Km = ifelse(is.na(distancefromglacier_Km), 
                                         0, distancefromglacier_Km)) |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  mutate(distancefromglacier_Km=as.numeric(distancefromglacier_Km)) |>
  mutate(param = sub('_.*','',param),
         param=ifelse(param=='tn.tp', 'TN:TP',
                      ifelse(param=='tdn.tdp','TDN:TDP',
                             ifelse(param=='pn.pp','PN:PP',
                                    ifelse(param=='in.ip','IN:IP',
                                           ifelse(param=='don.dop','DON:DOP', param)))))) |>
  mutate(nut_type = case_when(param %in% c('TN','TP','TN:TP')~'total',
                              param %in% c('TDN','TDP','TDN:TDP')~'total dissolved',
                              param %in% c('PN','PP','PN:PP')~'particulate',
                              param %in% c('IN','IP','IN:IP')~'inorganic',
                              param %in% c('DON','DOP','DON:DOP')~'dissolved organic'),
         nut_type = factor(nut_type, levels = c('dissolved organic', 'inorganic', 'particulate', 'total dissolved', 'total')))


# for pretty labels
gams$nutrient <- factor(gams$nutrient, labels=c(expression('N concentrations'~(mu*mol~L^-1)), expression('P concentrations'~(mu*mol~L^-1)), expression('Molar~ratio')))



# 3. Plot GAMs ####  

# Get the full Okabe-Ito palette
show_col(colorblindr::palette_OkabeIto_black)
okabe_ito_colors <- palette_OkabeIto_black

ggplot(gams, aes(distancefromglacier_Km, result, color=nut_type)) +
  facet_wrap(~nutrient, scales='free_y', labeller=label_parsed) +
  geom_jitter(aes(shape=eco_type))+
  geom_smooth(method = "gam", se = TRUE) + #fit a gam
  labs(y='', x="Distance from glacier (km)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_color_manual('',values=c(okabe_ito_colors[1:5])) +
  scale_shape_manual('', values=c(21,22,23)) 

ggsave('Figures/F_GAMS.png', width=8.5, height=4.5, units='in', dpi=1200)




# 4. Identifying and plotting periods of change ####
## Where along the network is rate of change in the parameters increasing or decreasing? 
## call in functions
source('Functions/derivative_analyses.R')

# set up
plotting_deriv <- data.frame()
params <- as.vector(unique(gams$param))
Term <- 'distancefromglacier_Km'

for(p in seq_along(params)) {
  
  # Filter the data for the current parameter
  data <- gams |> filter(param == params[p])
  
gam0 <- gam(
  result ~  s(distancefromglacier_Km,k=4), #formula, where k is the basis dimension
  data = data, 
  method = "REML" #The smoothing parameter estimation method, REML is default
)
summary(gam0)

# # From the broom package
# glance(gam0)
# tidy(gam0)



# Extract network_positions 
networkPos <- with(gams, data.frame(distancefromglacier_Km=seq(min(distancefromglacier_Km), max(distancefromglacier_Km), length.out=20)))

# Create a dataframe with predicted ("fitted") values from the GAM and year, on the response scale 
Pred <- cbind(networkPos,
              data.frame(predict(
                gam0, networkPos,
                type = "response",
                se.fit = TRUE
              )))
head(Pred)

# Calculate upper and lower bounds 
Pred <- transform(Pred,
                  upper = fit + (2 * se.fit),
                  lower = fit - (2 * se.fit))

#head(Pred)


# Extract first derivative of the trend
m1.d <- Deriv(gam0) 

# Calculate confidence intervals around the first derivative
m1.dci <- confint(m1.d, term = "distancefromglacier_Km")

# Extract periods of increasing or decreasing trends
m1.dsig <- signifD(Pred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

# Plot the first derivative 
plot.Deriv(m1.d)


# Append the results to the plotting_deriv dataframe
tmp <- data.frame(m1.d[["eval"]], m1.d[["distancefromglacier_Km"]][["deriv"]]) |>
  rename(deriv=2) |>
  mutate(upper = m1.dci[["distancefromglacier_Km"]][["upper"]],
         lower = m1.dci[["distancefromglacier_Km"]][["lower"]]) |>
  mutate(param=params[p])

# Append the results to the plotting_deriv dataframe
plotting_deriv <- plotting_deriv |>
  bind_rows(tmp)

}



# # this madness is just making pretty labels
# plotting_deriv$param <- factor(plotting_deriv$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 
# 
# ggplot(plotting_deriv, aes(network_position, deriv)) +
#   geom_line() +
#   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.25) +
#   geom_hline(yintercept = 0) +
#   theme_bw() +
#   labs(x='Network position',
#        y='First derivative of GAM') +
#   facet_wrap(~param, scales = 'free_y', ncol=3, labeller = label_parsed)
# # To interpret these first derivative plots– if the confidence intervals DO NOT overlap 0, it means that the trend is either increasing or decreasing.
# 
# ggsave('Figures/GAMs/derivative_plot.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)

plotting_deriv_vars <- plotting_deriv |>
  left_join(gams |> select(param, nutrient) |> distinct()) 

ggplot(plotting_deriv_vars |> filter(nutrient!='Ratio'), 
       aes(distancefromglacier_Km, deriv, color=param)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=param), alpha=0.15) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x='Distance from glacier (km)',
       y='First derivative of GAM') +
  facet_wrap(~param, scales = 'free_y',ncol=2) +
  theme(legend.title = element_blank())

ggsave('Figures/GAMs/GAMS_deriv.png', width=6.5, height=4.5, units='in', dpi=1200)


ggplot(plotting_deriv_vars |> filter(nutrient=='Ratio'), 
       aes(distancefromglacier_Km, deriv, color=param)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=param), alpha=0.15) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x='Distance from glacier (km)',
       y='First derivative of GAM') +
  facet_wrap(~param, scales = 'free_y',ncol=1) +
  theme(legend.title = element_blank())

ggsave('Figures/GAMs/GAMS_deriv_stoich.png', width=3.5, height=4.5, units='in', dpi=1200)

