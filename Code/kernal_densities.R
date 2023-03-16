# Kernal density estimation (non-parametric) #
# https://cran.r-project.org/web/packages/kdensity/index.html
source('Data/CALL_DATA_PACKAGES.R') 


# get non-parametric kernal density estimation for each nutrient - streams ####
kde_IN_stream <- kdensity((nuts |> filter(param=='IN_umolL',
                                   eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_DON_stream <- kdensity((nuts |> filter(param=='DON_umolL',
                                    eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_TN_stream <- kdensity((nuts |> filter(param=='TN_umolL',
                                   eco_type == 'stream',
                                   eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_TDN_stream <- kdensity((nuts |> filter(param=='TDN_umolL',
                                    eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_PN_stream <- kdensity((nuts |> filter(param=='PN_umolL',
                                   eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')

kde_IP_stream <- kdensity((nuts |> filter(param=='IP_umolL',
                                   eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_DOP_stream <- kdensity((nuts |> filter(param=='DOP_umolL',
                                    eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_TP_stream <- kdensity((nuts |> filter(param=='TP_umolL',
                                   eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_TDP_stream <- kdensity((nuts |> filter(param=='TDP_umolL',
                                    eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')
kde_PP_stream <- kdensity((nuts |> filter(param=='PP_umolL',
                                   eco_type == 'stream'))$result, start='gumbel', kernel='gaussian')


ggplot() +
  geom_line(mapping = aes((nuts |> 
                             filter(param=='IN_umolL',
                                    eco_type=='stream'))$result, 
                          kde_IN_stream((nuts |> filter(param=='IN_umolL',
                                                        eco_type=='stream'))$result))) +
  geom_density(mapping = aes((nuts |> filter(param=='IN_umolL',
                                             eco_type=='stream'))$result), color = 'red') # some slight differences. let's use the kdensity function becuase we know it does what we want!


# get non-parametric kernal density estimation for each nutrient - lakes ####
kde_IN_lake <- kdensity((nuts |> filter(param=='IN_umolL',
                                          eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_DON_lake <- kdensity((nuts |> filter(param=='DON_umolL',
                                           eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_TN_lake <- kdensity((nuts |> filter(param=='TN_umolL',
                                          eco_type == 'lake',
                                          eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_TDN_lake <- kdensity((nuts |> filter(param=='TDN_umolL',
                                           eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_PN_lake <- kdensity((nuts |> filter(param=='PN_umolL',
                                          eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')

kde_IP_lake <- kdensity((nuts |> filter(param=='IP_umolL',
                                          eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_DOP_lake <- kdensity((nuts |> filter(param=='DOP_umolL',
                                           eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_TP_lake <- kdensity((nuts |> filter(param=='TP_umolL',
                                          eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_TDP_lake <- kdensity((nuts |> filter(param=='TDP_umolL',
                                           eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')
kde_PP_lake <- kdensity((nuts |> filter(param=='PP_umolL',
                                          eco_type == 'lake'))$result, start='gumbel', kernel='gaussian')


# plot 
ggplot() +
  geom_line(mapping = aes((nuts |> 
                             filter(param=='IN_umolL',
                                    eco_type=='lake'))$result, 
                          kde_IN_lake((nuts |> filter(param=='IN_umolL',
                                                        eco_type=='lake'))$result))) +
  geom_density(mapping = aes((nuts |> filter(param=='IN_umolL',
                                             eco_type=='lake'))$result), color = 'red') 



# create a kde df to work from ####

nuts_param <- as.vector(unique(nuts$param))
nuts_eco <- as.vector(c('lake', 'stream', 'glacier'))
inlets <- as.vector(unique((nuts |> filter(grepl('INLET', site)))$site))
outlets <- as.vector(unique((nuts |> filter(grepl('OUTLET', site)))$site))
k_densities <- data.frame()



for(e in 1:length(nuts_eco)) {
  for(p in 1:length(nuts_param)) {
    tmp_f <- kdensity((nuts |> filter(param==param[p],
                                    eco_type==nuts_eco[e]))$result, start='gumbel', kernel='gaussian')
    
    dens_df <- nuts |>
      filter(param==param[p],
             eco_type==nuts_eco[e]) |>
      mutate(total_type_density = tmp_f(result))
    
    k_densities <- rbind(k_densities,dens_df)
  }
}



