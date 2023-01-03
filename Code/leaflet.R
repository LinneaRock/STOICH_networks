
library(tidyverse)
library(leaflet)


network <- leaflet() |>
  addTiles() |>
  addMarkers(lng=-105.617178,lat=40.055807, popup='Green Lake 4 outlet') |>
  addMarkers(lng=-105.60542,lat=40.049773, popup='Green Lake 1') |>
  addMarkers(lng=-105.611345,lat=40.04934, popup='Green Lake 2') |>
  addMarkers(lng=-105.616125,lat=40.051527, popup='Green Lake 3') |>
  addMarkers(lng=-105.607123,lat=40.047178, popup='Lake Albion inlet') |>
  addMarkers(lng=-105.602094,lat=40.045151, popup='Lake Albion outlet') |>
  addMarkers(lng=-105.59229602595,lat=40.0428746797462, popup='Albion Camp - downstream') |>
  addMarkers(lng=-105.628126,lat=40.053672, popup='Green Lake 5 outlet') |>
  addMarkers(lng=-105.642890924836,lat=40.0505740259853, popup='Arikaree Cirque') |>
  addMarkers(lng=-105.6367,lat=40.05195303, popup='Navajo outlet') |>
  addMarkers(lng=-105.5578493,lat=40.01368597, popup='Outlfow of Saddle Stream')

network |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addProviderTiles(providers$Stamen.TopOSMFeatures) |>
  addGraticule()

