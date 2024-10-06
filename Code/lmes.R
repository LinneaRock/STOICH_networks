

library(lme4)

data <- rbind(nuts, stoich) |>
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
  mutate(WS_Group = ifelse(WS_Group == 'GL2', 'ALB', WS_Group)) 

m1 <- lmer(result~)