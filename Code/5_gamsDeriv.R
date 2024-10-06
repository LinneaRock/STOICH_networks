#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Use gams to assess trends along network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# 1. Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 
## call in functions
source('Functions/derivative_analyses.R')


# 2. Format data ####
gams <- rbind(nuts, stoich) |>
  select(-network_position) |>
  left_join(sites |>
              as.data.frame() |>
              select(site, network_position, eco_type, elevation_m, drainage_area_ha, upstream_network_lakes, WS_Group)) |>
  group_by(network_position, param) |>
  mutate(mean = mean(result),
         median = median(result),
         min = min(result),
         max = max(result),
         SE = std.error(result),
         n = n()) |>
  ungroup() |>
  distinct() |>
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) |>
  # remove GL1 from these analyses
  filter(site != 'GL1_LAKE') |>
  mutate(network_position = network_position+1)

## 2a. Quick plots of all the variables of interest ####  


ggplot(gams, aes(network_position, result, group=param)) +
  facet_wrap(~param, scales='free_y',ncol=3) +
  geom_point(shape=21)+
  geom_smooth(method = "gam", se = TRUE, color="black") + #fit a gam
  labs(y='', x="Network position") +
  theme_pubr() 
ggsave('Figures/GAMS.png', width=10.5, height=8.5, units='in', dpi=1200)


# 3. Identifying and plotting periods of change ####
## Where along the network is rate of change in the parameters increasing or decreasing? 

# set up
plotting_deriv <- data.frame()
params <- as.vector(unique(gams$param))
Term <- 'network_position'

for(p in seq_along(params)) {
  
  # Filter the data for the current parameter
  data <- gams |> filter(param == params[p])
  
gam0 <- gam(
  result ~  s(network_position,k=4), #formula, where k is the basis dimension
  data = data, 
  method = "REML" #The smoothing parameter estimation method, REML is default
)
summary(gam0)

# # From the broom package
# glance(gam0)
# tidy(gam0)



# Extract network_positions 
networkPos <- with(gams, data.frame(network_position=seq(min(network_position), max(network_position), length.out=20)))

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
m1.dci <- confint(m1.d, term = "network_position")

# Extract periods of increasing or decreasing trends
m1.dsig <- signifD(Pred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

# Plot the first derivative 
plot.Deriv(m1.d)


# Append the results to the plotting_deriv dataframe
tmp <- data.frame(m1.d[["eval"]], m1.d[["network_position"]][["deriv"]]) |>
  rename(deriv=2) |>
  mutate(upper = m1.dci[["network_position"]][["upper"]],
         lower = m1.dci[["network_position"]][["lower"]]) |>
  mutate(param=params[p])

# Append the results to the plotting_deriv dataframe
plotting_deriv <- plotting_deriv |>
  bind_rows(tmp)

}



# this madness is just making pretty labels
plotting_deriv$param <- factor(plotting_deriv$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 

ggplot(plotting_deriv, aes(network_position, deriv)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.25) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x='Network position',
       y='First derivative of GAM') +
  facet_wrap(~param, scales = 'free_y', ncol=3, labeller = label_parsed)
# To interpret these first derivative plots– if the confidence intervals DO NOT overlap 0, it means that the trend is either increasing or decreasing.

ggsave('Figures/derivative_plot.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)












