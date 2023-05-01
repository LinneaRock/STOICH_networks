#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to make parameter boxplots and perform the non-parametric :) 
# Kruskal-Wallis tests -- let's explore seasonaly variation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
source('Data/CALL_DATA_PACKAGES.R') 
seas <- c('Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Dec')


# Lakes along network position ####

lakes <- nuts |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  rbind(stoich) |>
  filter(eco_type=='lake')

# this madness is just making pretty labels
lakes$param <- factor(lakes$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


#get significance to add to figure using geom_text 
list_params <- as.vector(lakes |> select(param) |> distinct())[['param']]

sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~network_position, lakes|>filter(param==i))
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

means <- left_join(lakes, sig.letters) |>
  group_by(letters, param, network_position) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()


### plot lakes all time ####
ggplot(lakes, aes(network_position, result, group = network_position)) +
  geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Lake position along network', y = '') +
  geom_text(means, mapping=aes(network_position, 
                               max.result, label = letters), 
           color='red4', size=4) +
  # ^^ Kruskal-Wallis test comparing lakes along network 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))


ggsave('Figures/boxplots/LAKE_POSITION.png', height =8.5, width = 10.5, units = 'in', dpi = 1200)


tryCatch({

}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


### seasonal lake position plots ####

for(s in 1:length(seas)) { # open season loop
  tryCatch({ # open tryCatch to prevent errors from stopping loop
  sig.letters <- data.frame(NA)
  for(i in list_params) { # open parameter loop
    h <- aov(result~network_position, lakes|>filter(param==i,
                                                    season==seas[s]))
    tukey <- TukeyHSD(h)
    cld <- multcompLetters4(h, tukey)
    cld2 <- data.frame(letters = cld$network_position$Letters) |>
      mutate(param = i)
    cld2$network_position <- rownames(cld2)
    sig.letters <- sig.letters |> bind_rows(cld2)
  } # close parameter loop
  
  sig.letters <- sig.letters |>
    drop_na(letters) |>
    select(-NA.)
  
  means <- left_join(lakes, sig.letters) |>
    group_by(letters, param, network_position) |>
    summarise(max.result = max(result, na.rm = TRUE)) |>
    distinct()
  

  ### plot lakes all time ####
  ggplot(lakes |> filter(season == seas[s]), aes(network_position, result, group = network_position)) +
    geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
    scale_color_viridis_d('',direction = -1) +
    facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
    labs(x = 'Lake position along network', y = '', 
         title=season[s]) +
    geom_text(means, mapping=aes(network_position, 
                                 max.result, label = letters), 
              color='red4', size=4) +
    # ^^ Kruskal-Wallis test comparing lakes along network 
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))
  
  
  ggsave(paste0('Figures/boxplots/LAKE_POSITION',season[s],'.png'), height =8.5, width = 10.5, units = 'in', dpi = 1200)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
  } # close season loop





# streams along network position ####

streams <- nuts |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  rbind(stoich) |>
  filter(eco_type=='stream')


# this madness is just making pretty labels
streams$param <- factor(streams$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


#get significance to add to figure using geom_text 
list_params <- as.vector(streams |> select(param) |> distinct())[['param']]

sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~network_position, streams|>filter(param==i))
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

means <- left_join(streams, sig.letters) |>
  group_by(letters, param, network_position) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()



### plot streams all time ####
ggplot(streams, aes(network_position, result, group = network_position)) +
  geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Stream position along network', y = '') +
  geom_text(means, mapping=aes(network_position, 
                               max.result, label = letters), 
            color='red4', size=4) +
  # ^^ Kruskal-Wallis test comparing streams along network 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))


ggsave('Figures/boxplots/STREAM_POSITION.png', height =8.5, width = 10.5, units = 'in', dpi = 1200)


tryCatch({
  
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

### seasonal stream position plots ####

for(s in 1:length(seas)) { # open season loop
  tryCatch({ # open tryCatch to prevent errors from stopping loop
    sig.letters <- data.frame(NA)
    for(i in list_params) { # open parameter loop
      h <- aov(result~network_position, streams|>filter(param==i,
                                                      season==seas[s]))
      tukey <- TukeyHSD(h)
      cld <- multcompLetters4(h, tukey)
      cld2 <- data.frame(letters = cld$network_position$Letters) |>
        mutate(param = i)
      cld2$network_position <- rownames(cld2)
      sig.letters <- sig.letters |> bind_rows(cld2)
    } # close parameter loop
    
    sig.letters <- sig.letters |>
      drop_na(letters) |>
      select(-NA.)
    
    means <- left_join(streams, sig.letters) |>
      group_by(letters, param, network_position) |>
      summarise(max.result = max(result, na.rm = TRUE)) |>
      distinct()
    
    
    ### plot streams all time ####
    ggplot(streams |> filter(season == seas[s]), aes(network_position, result, group = network_position)) +
      geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
      scale_color_viridis_d('',direction = -1) +
      facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
      labs(x = 'Stream position along network', y = '', 
           title=season[s]) +
      geom_text(means, mapping=aes(network_position, 
                                   max.result, label = letters), 
                color='red4', size=4) +
      # ^^ Kruskal-Wallis test comparing streams along network 
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))
    
    
    ggsave(paste0('Figures/boxplots/STREAM_POSITION',season[s],'.png'), height =8.5, width = 10.5, units = 'in', dpi = 1200)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
} # close season loop











# Lakes vs streams ####

lakestream <- nuts |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  rbind(stoich) 


# this madness is just making pretty labels
lakestream$param <- factor(lakestream$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


#get significance to add to figure using geom_text 
list_params <- as.vector(lakestream |> select(param) |> distinct())[['param']]

sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~eco_type, lakestream|>filter(param==i))
  tukey <- TukeyHSD(h)
  cld <- multcompLetters4(h, tukey)
  cld2 <- data.frame(letters = cld$eco_type$Letters) |>
    mutate(param = i)
  cld2$eco_type <- rownames(cld2)
  sig.letters <- sig.letters |> bind_rows(cld2)
}

sig.letters <- sig.letters |>
  drop_na(letters) |>
  select(-NA.)

means <- left_join(lakestream, sig.letters) |>
  group_by(letters, param, eco_type) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()



### plot lakes vs streams all time ####
ggplot(lakestream, aes(eco_type, result, group = eco_type)) +
  geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Stream position along network', y = '') +
  geom_text(means, mapping=aes(eco_type, 
                               max.result, label = letters), 
            color='red4', size=4) +
  # ^^ Kruskal-Wallis test comparing lakestream along network 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))


ggsave('Figures/boxplots/LAVEvsSTREAM_POSITION.png', height =8.5, width = 10.5, units = 'in', dpi = 1200)



### seasonal lakes vs streams plots ####

for(s in 1:length(seas)) { # open season loop
  tryCatch({ # open tryCatch to prevent errors from stopping loop
    sig.letters <- data.frame(NA)
    for(i in list_params) { # open parameter loop
      h <- aov(result~eco_type, lakestream|>filter(param==i,
                                                        season==seas[s]))
      tukey <- TukeyHSD(h)
      cld <- multcompLetters4(h, tukey)
      cld2 <- data.frame(letters = cld$eco_type$Letters) |>
        mutate(param = i)
      cld2$eco_type <- rownames(cld2)
      sig.letters <- sig.letters |> bind_rows(cld2)
    } # close parameter loop
    
    sig.letters <- sig.letters |>
      drop_na(letters) |>
      select(-NA.)
    
    means <- left_join(lakestream, sig.letters) |>
      group_by(letters, param, eco_type) |>
      summarise(max.result = max(result, na.rm = TRUE)) |>
      distinct()
    
    
    ### plot lakes vs streams all time ####
    ggplot(lakestream |> filter(season == seas[s]), aes(eco_type, result, group = eco_type)) +
      geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
      scale_color_viridis_d('',direction = -1) +
      facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
      labs(x = 'Stream position along network', y = '', 
           title=season[s]) +
      geom_text(means, mapping=aes(eco_type, 
                                   max.result, label = letters), 
                color='red4', size=4) +
      # ^^ Kruskal-Wallis test comparing lakes vs streams  
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))
    
    
    ggsave(paste0('Figures/boxplots/LAKEvsSTREAM_POSITION',season[s],'.png'), height =8.5, width = 10.5, units = 'in', dpi = 1200)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
} # close season loop











# Inlets vs outlets ####

inout <- nuts |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  rbind(stoich) |>
  filter(eco_type == 'stream') |>
  mutate(position = ifelse(grepl('INLET', site), 'inlet', 'outlet'))


# this madness is just making pretty labels
inout$param <- factor(inout$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


#get significance to add to figure using geom_text 
list_params <- as.vector(inout |> select(param) |> distinct())[['param']]

sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~position, inout|>filter(param==i))
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

means <- left_join(inout, sig.letters) |>
  group_by(letters, param, position) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()



### plot lakes vs inout all time ####
ggplot(inout, aes(position, result, group = position)) +
  geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Stream position along network', y = '') +
  geom_text(means, mapping=aes(position, 
                               max.result, label = letters), 
            color='red4', size=4) +
  # ^^ Kruskal-Wallis test comparing ins vs outs along network 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))


ggsave('Figures/boxplots/INLETvsOUTLET_.png', height =8.5, width = 10.5, units = 'in', dpi = 1200)



### seasonal inlets vs outlets plots ####

for(s in 1:length(seas)) { # open season loop
  tryCatch({ # open tryCatch to prevent errors from stopping loop
    sig.letters <- data.frame(NA)
    for(i in list_params) { # open parameter loop
      h <- aov(result~position, inout|>filter(param==i,
                                                   season==seas[s]))
      tukey <- TukeyHSD(h)
      cld <- multcompLetters4(h, tukey)
      cld2 <- data.frame(letters = cld$position$Letters) |>
        mutate(param = i)
      cld2$position <- rownames(cld2)
      sig.letters <- sig.letters |> bind_rows(cld2)
    } # close parameter loop
    
    sig.letters <- sig.letters |>
      drop_na(letters) |>
      select(-NA.)
    
    means <- left_join(inout, sig.letters) |>
      group_by(letters, param, position) |>
      summarise(max.result = max(result, na.rm = TRUE)) |>
      distinct()
    
    
    ### plot ins v outs all time ####
    ggplot(inout |> filter(season == seas[s]), aes(position, result, group = position)) +
      geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
      scale_color_viridis_d('',direction = -1) +
      facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
      labs(x = 'Stream position along network', y = '', 
           title=season[s]) +
      geom_text(means, mapping=aes(position, 
                                   max.result, label = letters), 
                color='red4', size=4) +
      # ^^ Kruskal-Wallis test comparing inlets vs oultets along network 
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))
    
    
    ggsave(paste0('Figures/boxplots/INLETvsOUTLET_',season[s],'.png'), height =8.5, width = 10.5, units = 'in', dpi = 1200)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
} # close season loop













# lakes vs outlets ####

lakeout <- nuts |>
  filter(!param %in% c('DOC_mgL', 'NO3_ueqL', 'NH4_ueqL')) |>
  rbind(stoich) |>
  #filter(eco_type == 'stream') |>
  mutate(position = ifelse(grepl('OUTLET', site), 'outlet', 
                           ifelse(grepl('LAKE', site), 'lake', NA))) |>
  drop_na(position)


# this madness is just making pretty labels
lakeout$param <- factor(lakeout$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 


#get significance to add to figure using geom_text 
list_params <- as.vector(lakeout |> select(param) |> distinct())[['param']]

sig.letters <- data.frame(NA)
for(i in list_params) {
  h <- aov(result~position, lakeout|>filter(param==i))
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

means <- left_join(lakeout, sig.letters) |>
  group_by(letters, param, position) |>
  summarise(max.result = max(result, na.rm = TRUE)) |>
  distinct()



### plot lakes vs outlets all time ####
ggplot(lakeout, aes(position, result, group = position)) +
  geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
  scale_color_viridis_d('',direction = -1) +
  facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
  labs(x = 'Stream position along network', y = '') +
  geom_text(means, mapping=aes(position, 
                               max.result, label = letters), 
            color='red4', size=4) +
  # ^^ Kruskal-Wallis test comparing lakeout along network 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))


ggsave('Figures/boxplots/LAKESvsOUTLET_.png', height =8.5, width = 10.5, units = 'in', dpi = 1200)



### seasonal lake vs outlet plots ####

for(s in 1:length(seas)) { # open season loop
  tryCatch({ # open tryCatch to prevent errors from stopping loop
    sig.letters <- data.frame(NA)
    for(i in list_params) { # open parameter loop
      h <- aov(result~position, lakeout|>filter(param==i,
                                              season==seas[s]))
      tukey <- TukeyHSD(h)
      cld <- multcompLetters4(h, tukey)
      cld2 <- data.frame(letters = cld$position$Letters) |>
        mutate(param = i)
      cld2$position <- rownames(cld2)
      sig.letters <- sig.letters |> bind_rows(cld2)
    } # close parameter loop
    
    sig.letters <- sig.letters |>
      drop_na(letters) |>
      select(-NA.)
    
    means <- left_join(lakeout, sig.letters) |>
      group_by(letters, param, position) |>
      summarise(max.result = max(result, na.rm = TRUE)) |>
      distinct()
    
    
    ### plot lakes vs outlets all time ####
    ggplot(lakeout |> filter(season == seas[s]), aes(position, result, group = position)) +
      geom_jitter(aes(), shape = 16, size =2, alpha = 0.2,
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
      scale_color_viridis_d('',direction = -1) +
      facet_wrap(.~param, scales='free', labeller=label_parsed, nrow=5) +
      labs(x = 'Stream position along network', y = '', 
           title=season[s]) +
      geom_text(means, mapping=aes(position, 
                                   max.result, label = letters), 
                color='red4', size=4) +
      # ^^ Kruskal-Wallis test comparing inlets vs oultets along network 
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1))
    
    
    ggsave(paste0('Figures/boxplots/LAKESvsOUTLET_',season[s],'.png'), height =8.5, width = 10.5, units = 'in', dpi = 1200)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch
} # close season loop

