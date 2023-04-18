#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Theil-Sens slope for timeseries trends and network trends ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

## Prepare data ####
nuts.lm <- nuts |>
 filter(!param %in% c('NH4_ueqL', 'NO3_ueqL', 'DOC_mgL')) |> # I don't want to look at these at the moment
  filter(eco_type != 'glacier') |>   # let's skip the glacier becuase I am more interested in the lake-stream part of the network
  mutate(year=year(date)) |>
  group_by(network_position, param) |>
  add_count() |>
  ungroup() |>
  filter(n>3)

stoich.lm <- stoich |>
  filter(eco_type != 'glacier') |>   # let's skip the glacier becuase I am more interested in the lake-stream part of the network
  mutate(year=year(date)) |>
  group_by(network_position, param) |>
  add_count() |>
  ungroup() |>
  filter(n>3)

## Quick plots of all the variables of interest ####  
ggplot(nuts.lm, aes(year, result, group=network_position, color=param)) +
  geom_point(shape=21, alpha=0.5) +
  #scale_y_log10() +
  facet_grid(param~network_position, scales="free") +
  geom_smooth(method="lm", color="black")

ggplot(stoich.lm, aes(year, result, group=network_position, color=param)) +
  geom_point(shape=21, alpha=0.5) +
  #scale_y_log10() +
  facet_grid(param~network_position, scales="free") +
  geom_smooth(method="lm", color="black")

## Mann-Kendall (non-parametric) test to see if significant trends exist in our data ####
mk_df <- nuts.lm |>
  group_by(network_position, param) |>
  summarise(z.stat = glance(mk.test(result))$statistic,
         p.value = glance(mk.test(result))$p.value,
         n = glance(mk.test(result))$parameter,
## Calculate Sen's slope and add to dataframe ####
         slope = as.numeric(sens.slope(result)[[1]])) |>
  ungroup() |>
  rbind((stoich.lm |>
           group_by(network_position, param) |>
           summarise(z.stat = glance(mk.test(result))$statistic,
                     p.value = glance(mk.test(result))$p.value,
                     n = glance(mk.test(result))$parameter,
                     slope = as.numeric(sens.slope(result)[[1]])) |>
           ungroup()))


ggplot(mk_df |> filter(p.value<=0.05,
                       n>=10), aes(slope, network_position,color=param)) +
  geom_jitter() +
  theme_bw() +
  geom_vline(xintercept = 0)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Use gams to assess trends along network ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
source('Functions/derivative_analyses.R')

nuts.gam <- nuts |>
  mutate(network_position = ifelse(network_position=='12a', '12.5', network_position)) |>
  mutate(network_position = as.numeric(network_position))

stoich.gam <- stoich |>
  mutate(network_position = ifelse(network_position=='12a', '12.5', network_position)) |>
  mutate(network_position = as.numeric(network_position))

## Quick plots of all the variables of interest ####  
ggplot(nuts.gam |>
         filter(!param %in% c('NH4_ueqL', 'NO3_ueqL', 'DOC_mgL')), 
       aes(network_position, result, group=param)) +
  facet_wrap(~param, scales='free_y') +
  geom_point(shape=21)+
  geom_smooth(method = "gam", se = TRUE, color="black") + #fit a gam
  labs(y="Concentration", x="Network position") +
  theme_pubr()

ggplot(stoich.gam, aes(network_position, result, group=param)) +
  facet_wrap(~param, scales='free_y') +
  geom_point(shape=21)+
  geom_smooth(method = "gam", se = TRUE, color="black") + #fit a gam
  labs(y="Molar ratio", x="Network position") +
  theme_pubr() +
  scale_y_log10()

## Identifying and plotting periods of change ####
### Where along the tnetwork is rate of change in the parameters increasing or decreasing? ####
gam0 <- gam(
  result ~  s(network_position,k=4), #formula, where k is the basis dimension
  data = stoich.gam |> filter(param=='don.dop'), #dataset 
  method = "REML" #The smoothing parameter estimation method, REML is default
)
summary(gam0)

# From the broom package
glance(gam0)
tidy(gam0)
# Create nested list using list()
my_nested_list1 <- list(list_1,                  
                        list_2,
                        list_3)

#### Extract network_positions ####
networkPos <- with(stoich.gam, data.frame(network_position=seq(min(network_position), max(network_position), length.out=20)))

#### Create a dataframe with predicted ("fitted") values from the GAM and year, on the response scale ####
Pred <- cbind(networkPos,
              data.frame(predict(
                gam0, networkPos,
                type = "response",
                se.fit = TRUE
              )))
head(Pred)

#### Calculate upper and lower bounds ####
Pred <- transform(Pred,
                  upper = fit + (2 * se.fit),
                  lower = fit - (2 * se.fit))

head(Pred)


#### Extract first derivative of the trend ####
Term = "network_position"
m1.d <- Deriv(gam0) 

#### Calculate confidence intervals around the first derivative ####
m1.dci <- confint(m1.d, term = "network_position")

# Extract periods of increasing or decreasing trends
m1.dsig <- signifD(Pred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

# Plot the first derivative 
plot.Deriv(m1.d)
# To interpret these first derivative plots– if the confidence intervals DO NOT overlap 0, it means that the trend is either increasing or decreasing.





# Add a column for periods of time when the trend is accelerating
Pred <- cbind(Pred, data.frame(incr=unlist(m1.dsig$incr)))


Pred %>%
  ggplot(aes(x=network_position,y=fit))+
  geom_point(data=stoich.gam |> filter(param=='don.dop'), aes(x=network_position, y=result),
             shape=21,fill="grey50", alpha=0.5)+ #Plot raw data
  geom_line(size=1, alpha=0.8)+ #Plot fitted trend
  geom_line(aes(x=network_position, y=incr), color="red", size=1, alpha=0.8)+ #Highlight period of increasing trend
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = network_position), alpha = 0.5, inherit.aes = FALSE) + #Plot CI around fitted trend
  labs(x="Network position",y="Molar ratio")+
  # coord_cartesian(xlim=c(1930,2020),
  #                 ylim=c(22,30))+
  # scale_x_continuous(breaks=seq(1930, 2020, 15))+
  # scale_y_continuous(breaks=seq(22,30,2))+
  theme_pubr(base_size=8, border=TRUE)

ggsave(plot=last_plot(), "Day2/figures/GAM_example.png",
       dpi=600, width = 6, height = 5, units = 'in')
