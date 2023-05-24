#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nutrient and stoichiometry scatterplots in vs out, in vs lake, lake vs out ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Call data and packages ####
source('Data/CALL_DATA_PACKAGES.R') 

## Prepare data ####

setup <- rbind(nuts, stoich) |>
  mutate(position = ifelse(grepl("INLET", site), 'inlet', NA)) |>
  mutate(position = ifelse(grepl("OUTLET", site), 'outlet', position)) |>
  mutate(position = ifelse(eco_type=='lake', 'in-lake', position)) |>
  mutate(position = ifelse(site=='GL1_LAKE', 'inlet2', position)) |>
  left_join(sites) |>
  left_join(greenlakes_LC) |>
  select(-Layer_1, -LandCoverArea_km2, -depth_m, - arik_flow_site, - notes, - elevation_m, - drainage_area_ha, - geometry) |>
  distinct()

## Write loop to create plots and dataframe ####
params <- unique(setup$param) # list of params
comp_data <- data.frame() # create empty dataframe

for(p in 1:length(params)) {
  tmp <- setup |>
    filter(param==params[p]) |>
    group_by(site,date,param,position,WS_Group,season) |>
    summarise(result=mean(result)) |>
    ungroup() |>
    pivot_wider(id_cols=c('WS_Group', 'date'), names_from='position',values_from='result') |>
    mutate(param=params[p])
  
  comp_data <- bind_rows(comp_data, tmp) 
}

# this madness is just making pretty labels
comp_data$param <- factor(comp_data$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')')))   

## Plot inlet vs outlet ####
ggplot(comp_data) +
  geom_point(aes(inlet, outlet, color=)) +
  theme_bw(base_size = 15) +
  geom_abline(slope=1,intercept=0, color= 'red4') +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5)
ggsave('Figures/Comparisons/inlets_outlets.png', width=10.5, height=8.5, units='in', dpi=1200)

## Plot inlet vs lake ####
ggplot(comp_data) +
  geom_point(aes(inlet, `in-lake`)) +
  geom_point(aes(inlet2, `in-lake`)) + # add the Green Lake 1 here (it serves as inlet to Albion Lake in this case)
  theme_bw(base_size = 15) +
  geom_abline(slope=1,intercept=0, color= 'red4') +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5)
ggsave('Figures/Comparisons/inlets_in-lake.png', width=10.5, height=8.5, units='in', dpi=1200)


## Plot in-lake vs outlet ####
ggplot(comp_data) +
  geom_point(aes(`in-lake`, outlet)) +
  theme_bw(base_size = 15) +
  geom_abline(slope=1,intercept=0, color= 'red4') +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5)
ggsave('Figures/Comparisons/in-lake_outlets.png', width=10.5, height=8.5, units='in', dpi=1200)



# Now, let's create a ladder plot of inlet, in-lake, outlet concentrations ####

ladder <- setup |>
  filter(site != 'GL2_LAKE') |>
  mutate(position = ifelse(position=='inlet2','inlet',position))  |>
  group_by(WS_Group, position, param) |>
  mutate(median_result = median(result))|>
  ungroup()

factor(unique(ladder$position)) # checking factor levels
ladder$position <- factor(ladder$position, levels = c('inlet','in-lake','outlet')) # changing factor levels
factor(unique(ladder$position)) # checking new factor levels 
ladder$WS_Group <- factor(ladder$WS_Group, levels = c('GL5','GL4','GL3','ALB'))

# this madness is just making pretty labels
ladder$param <- factor(ladder$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


# note, I am cutting the y-axis to be less than 500. This changes the appearance of IN:IP, which has inlet data extending to 1000 and outlet to >3000; TDN:TDP, which has outlet results extending beyond 9000 and inlet results extending beyond 500; and TN:TP, which has outlet results extending beyond 600
ggplot(ladder |>filter(result<500)) +
  geom_violin(aes(position, result)) +
  geom_jitter(aes(position, result, color=WS_Group), alpha=0.4) +
  geom_point(aes(position, median_result, fill=WS_Group),color='black', size=4, shape=21) +
  geom_line(aes(position, median_result, group=WS_Group)) +
  scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  labs(x='',y='') +
  facet_wrap(~param, scales='free',ncol=3,labeller=label_parsed) +
  theme_classic()

ggsave('Figures/Comparisons/ladder.png', height=8.5, width=10.5, units='in',dpi=1200)





### ladder fig for SFS ####
sfs_ladder <- setup |>
  filter(site != 'GL2_LAKE',
         param %in% c('in.ip','IN_umolL','IP_umolL', 'tn.tp','TN_umolL', 'TP_umolL')) |>
  mutate(position = ifelse(position=='inlet2','inlet',position))  |>
  group_by(WS_Group, position, param) |>
  mutate(median_result = median(result))|>
  ungroup() |>
  mutate(position = gsub('in-lake','lake',position))


factor(unique(sfs_ladder$position)) # checking new factor levels 
sfs_ladder$WS_Group <- factor(sfs_ladder$WS_Group, levels = c('GL5','GL4','GL3','ALB'))

# this madness is just making pretty labels
sfs_ladder$param <- factor(sfs_ladder$param, labels = c(expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 




#get significance to add to figure using geom_text 
list_params <- unique(sfs_ladder$param)

sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~position, sfs_ladder|>filter(param==i))
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$position$Letters) |>
    mutate(param = i)
  cld2$position <- rownames(cld2)
  sig.letters <- sig.letters |> bind_rows(cld2)
}

sig.letters <- sig.letters |>
  drop_na(letters) |>
  select(-NA.)

means <- left_join(sfs_ladder |>filter(result < 600), sig.letters) |>
  group_by(letters, param, position) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct() |> mutate(position = gsub('lake', 'in-lake', position))
means$position <- factor(means$position, levels = c('inlet', 'in-lake','outlet')) 
means$param <- factor(means$param, levels = c(expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 

sfs_ladder <- sfs_ladder |> mutate(position = gsub('lake', 'in-lake', position))
sfs_ladder$position <- factor(sfs_ladder$position, levels = c('inlet', 'in-lake','outlet')) 

ggplot(sfs_ladder |>filter(result<500)) +
  geom_violin(aes(position, result)) +
  geom_jitter(aes(position, result, color=WS_Group), alpha=0.4) +
  geom_point(aes(position, median_result, fill=WS_Group),color='black', size=4, shape=21) +
  geom_line(aes(position, median_result, group=WS_Group)) +
  scale_color_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  scale_fill_manual('Subwatershed', values=c('#906388','#9398D2','#81C4E7','#B5DDD8')) +
  labs(x='',y='') +
  facet_wrap(~param, scales='free',ncol=3,labeller=label_parsed) +
  dark_theme_bw()  +
  geom_text(means, mapping=aes(position, 
                               max.result, label = letters), 
           size=4) +
  # ^^ Kruskal-Wallis test comparing streams along network 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1),
        legend.position = 'bottom')

ggsave('Figures/DarkTheme/ladder.png',height=4.5, width=6.5, units='in',dpi=1200)

invert_geom_defaults()

