#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to make parameter boxplots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Call in data and libraries ####
library(tidyverse)
library(ggpubr)
library(multcompView)


sites <- read.csv("Data/sites.csv") |>
  mutate(network_position = factor(network_position, levels = c('1','2','3','4', '5', '6', '7','8','9','10','11','12','12a','13','14','15','16')))
gl_network <- read.csv("Data/greenlakes_network.csv") |>
  left_join(sites) |>
  mutate(date = as.Date(date, format = '%m/%d/%Y'))  |>
  mutate(season = factor(season, levels = c('Jan-Mar','Apr-Jun','Jul-Sep','Oct-Dec')))

# subset and format datasets for plotting ####
ions <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, 21:31) |>
  pivot_longer(7:17, names_to = 'param', values_to = 'result') |>
  drop_na(network_position) |>
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m)) # just look at photic zone

nuts <- gl_network |>
  select(site, network_position, eco_type, date, season, depth_m, 2:7, 10:14) |>
  pivot_longer(7:17, names_to = 'param', values_to = 'result') |>
  drop_na(network_position)  |>
  drop_na(result) |>
  filter(depth_m <=3 | is.na(depth_m)) # just look at photic zone








# Plotting by eco type ####
## plot non-nutrients by eco type ####
ggplot(ions, aes(eco_type, result)) +
  geom_jitter(aes(color = network_position), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_violin(alpha = 0.2) +
  stat_summary(geom = 'point', fun.y = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('Network position',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '') +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('glacier','lake'), c('lake','stream'), c('glacier','stream'))) +
  # ^^ Kruskal-Wallis test --- y.label.npc does not work ???!!!!!! 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  guides(color = guide_legend(ncol = 3)) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggsave('Figures/boxplots/eco_params.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)


## plot nutrients by eco type ####
ggplot(nuts, aes(eco_type, result)) +
  geom_jitter(aes(color = network_position), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_violin(alpha = 0.2) +
  stat_summary(geom = 'point', fun.y = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('Network position',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '') +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('glacier','lake'), c('lake','stream'), c('glacier','stream'))) +
  # ^^ Kruskal-Wallis test --- y.label.npc does not work ???!!!!!! 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  guides(color = guide_legend(ncol = 3)) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggsave('Figures/boxplots/eco_nuts.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)








# plotting by network position ####
## plot non-nutrients by network position ####
ggplot(ions, mapping = aes(network_position, result, group = network_position)) +
  geom_jitter(aes(color = eco_type), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_boxplot(alpha = 0.2) +
  stat_summary(geom = 'point', fun.y = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = 'Network position', y = '') +
 # geom_text(means, mapping = aes(network_position, max.result + 5, label = letters), hjust = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10),  #change legend text font size
        axis.text.x = element_text(angle=45, vjust=1,hjust=1))
ggsave('Figures/boxplots/position_params.png', height = 8.5, width = 10.5, units = 'in', dpi = 1200)

# get significance to add to figure using geom_text -- actually way too much on these plots
list_params <- as.vector(nuts |> select(param) |> distinct())[['param']]
sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~network_position, nuts|>filter(param==i))
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$network_position$Letters) |>
    mutate(param = i)
  cld2$network_position <- rownames(cld2)
  sig.letters <- sig.letters |> bind_rows(cld2)
}

sig.letters <- sig.letters |>
  drop_na(letters) |>
  select(-NA.) 

means <- left_join(nuts, sig.letters) |>
  group_by(letters, param, network_position) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()



## plot nutrients by network position ####
ggplot(nuts, aes(network_position, result, group = network_position)) +
  geom_jitter(aes(color = eco_type, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_boxplot(alpha = 0.2) +
  stat_summary(geom = 'point', fun = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black')) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = 'Network position', y = '') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10),  #change legend text font size
        axis.text.x = element_text(angle=45, vjust=1,hjust=1))
ggsave('Figures/boxplots/position_nuts.png', height =8.5, width = 10.5, units = 'in', dpi = 1200)

# get significance to add to figure using geom_text -- actually way too much on these plots
list_params <- as.vector(ions |> select(param) |> distinct())[['param']]
sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~network_position, ions|>filter(param==i))
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$network_position$Letters) |>
    mutate(param = i)
  cld2$network_position <- rownames(cld2)
  sig.letters <- sig.letters |> bind_rows(cld2)
}

sig.letters <- sig.letters |>
  drop_na(letters) |>
  select(-NA.) 

means <- left_join(ions, sig.letters) |>
  group_by(letters, param, network_position) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()





# plotting by season ####
## plot non-nutrients by season ####
ggplot(ions, aes(season, result, group = season)) +
  geom_jitter(aes(color = eco_type, shape = depth_m), 
              shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_violin(alpha = 0.2) +
  stat_summary(geom = 'point', fun.y = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black'),
        axis.text.x = element_text(angle=45, vjust=1,hjust=1)) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '') +
  geom_text(means, mapping = aes(season, max.result + 1, label = letters), hjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size
ggsave('Figures/boxplots/season_params.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)

# get significance to add to figure using geom_text 
list_params <- as.vector(ions |> select(param) |> distinct())[['param']]
sig.letters <- data.frame(NA)
for(i in list_params) {
  data <- ions |>
    mutate(season = season |> str_replace_all('-', '_'))
  h <- aov(result~as.factor(season), data |>filter(param==i))
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$`as.factor(season)`$Letters) |>
    mutate(param = i)
  cld2$season <- rownames(cld2)
  sig.letters <- sig.letters |> bind_rows(cld2)
}

sig.letters <- sig.letters |>
  drop_na(letters) |>
  select(-NA.)

means <- left_join(ions, sig.letters|>
                     mutate(season = season |> str_replace_all('_', '-'))) |>
  group_by(letters, param, season) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()

TukeyHSD(aov(result~as.factor(season), ions |>filter(param=='H_ueqL') |>
               mutate(season = season |> str_replace_all('-', '_'))))


## plot nutrients by season ####
ggplot(nuts, aes(season, result, group = season)) +
  geom_jitter(aes(color = eco_type, shape = depth_m), shape = 16, size =2, alpha = 0.2, position=position_jitter(0.3)) +
  geom_violin(alpha = 0.2) +
  stat_summary(geom = 'point', fun.y = 'mean', fill = 'black', color = 'white', size = 1.5, shape = 24) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(face='bold', family='serif',
                                  size=rel(1.2), hjust=0.5),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        text=element_text(family='serif'),
        axis.text=element_text(color='black'),
        axis.text.x = element_text(angle=45, vjust=1,hjust=1)) +
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales = 'free_y') +
  labs(x = '', y = '') +
  geom_text(means, mapping = aes(season, max.result + 1, label = letters), hjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size
ggsave('Figures/boxplots/season_nuts.png', height = 6.5, width = 8.5, units = 'in', dpi = 1200)

# get significance to add to figure using geom_text
list_params <- as.vector(nuts |> select(param) |> distinct())[['param']]
sig.letters <- data.frame(NA)
for(i in list_params) {
  data <- nuts |>
    mutate(season = season |> str_replace_all('-', '_'))
  h <- aov(result~as.factor(season), data |>filter(param==i))
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$`as.factor(season)`$Letters) |>
    mutate(param = i)
  cld2$season <- rownames(cld2)
  sig.letters <- sig.letters |> bind_rows(cld2)
}

sig.letters <- sig.letters |>
  drop_na(letters) |>
  select(-NA.)

means <- left_join(nuts, sig.letters|>
                     mutate(season = season |> str_replace_all('_', '-'))) |>
  group_by(letters, param, season) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()
