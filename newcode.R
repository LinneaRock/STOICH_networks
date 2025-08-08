#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nutrient variations across site types
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



lakes_streams_comparisons <- nuts |> 
  filter(site != 'FLUME',
         eco_type != 'glacier',
         site != 'ALB_CAMP',
         site != 'GL1_LAKE') |> 
  mutate(position = ifelse(grepl("INLET", site), 'inlet', NA)) |>
  mutate(position = ifelse(grepl("OUTLET", site), 'outlet', position)) |>
  mutate(position = ifelse(eco_type=='lake', 'lake', position)) |>
  mutate(lake_group = sub("_.*", "", site))  |>
  pivot_wider(id_cols=c('lake_group', 'date', 'param'), names_from='position',values_from='result', values_fn = mean)

# this madness is just making pretty labels
lakes_streams_comparisons$param <- factor(lakes_streams_comparisons$param, labels = c(expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'),  expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


ggplot(lakes_streams_comparisons, aes(lake, inlet)) +
  geom_point(color='grey50') +
  geom_smooth(method='lm', se=FALSE, color = 'aquamarine4') +
  geom_abline(slope=1, intercept=0, color='magenta4', linetype='dashed') +
  facet_wrap(~param, scales='free') +
  theme_bw()

ggplot(lakes_streams_comparisons, aes(lake, outlet)) +
  geom_point(color='grey50') +
  geom_smooth(method='lm', se=FALSE, color = 'aquamarine4') +
  geom_abline(slope=1, intercept=0, color='magenta4', linetype='dashed') +
  facet_wrap(~param, scales='free') +
  theme_bw()

