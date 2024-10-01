#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nutrients differ along network position
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('Data/CALL_DATA_PACKAGES.R')

alldata <- rbind(stoich, nuts) |>
  left_join(sites |> select(site,WS_Group)) |>
  mutate(WS_Group = ifelse(is.na(WS_Group), 'Glacial origin', WS_Group),
         WS_Group = ifelse(WS_Group=='GL2', 'ALB', WS_Group))

# this madness is just making pretty labels
#alldata$param <- alldata(lakes$param, labels = c(expression('(DON:DOP)'), expression('(DON'~mu*mol*L^-1*')'), expression('(DOP'~mu*mol*L^-1*')'), expression('(IN:IP)'), expression('(IN'~mu*mol*L^-1*')'), expression('(IP'~mu*mol*L^-1*')'), expression('(PN:PP)'), expression('(PN'~mu*mol*L^-1*')'), expression('(PP'~mu*mol*L^-1*')'), expression('(TDN:TDP)'), expression('(TDN'~mu*mol*L^-1*')'), expression('(TDP'~mu*mol*L^-1*')'),expression('(TN:TP)'), expression('(TN'~mu*mol*L^-1*')'), expression('(TP'~mu*mol*L^-1*')'))) 

ws_cols <- c('#D1DEFA','#906388','#9398D2','#81C4E7','#B5DDD8')

ggplot(alldata |> filter(result<3000)) +
  geom_boxplot(aes(network_position, result)) +
  geom_jitter(aes(network_position, result, color=WS_Group), alpha=0.5) +
  facet_wrap(~param, scales='free', ncol=3) +
  scale_color_manual(values = ws_cols) +
  theme_bw()
  