library(tidyverse)

u = "https://github.com/ITSLeeds/od/releases/download/0.2.1/od_iz_ed.Rds"
f = basename(u)
if(!file.exists(f)) download.file(u, f)
od = readRDS("od_iz_ed.Rds")
# head(od)

u = "https://github.com/ITSLeeds/od/releases/download/0.2.1/iz_zones11_ed.Rds"
f = basename(u)
if(!file.exists(f)) download.file(u, f)
zones = readRDS("iz_zones11_ed.Rds")
# head(zones)

odsf = od::od_to_sf(od, zones)
odsf_top3 = odsf %>%
  filter(geo_code1 != geo_code2) %>%
  top_n(n = 3, wt = all) %>%
  select(geo_code1, geo_code2, all, foot, bicycle, bus, car_driver) %>%
  arrange(desc(all))

plot(odsf_top3)

set.seed(1)
od_top3_jittered = od::od_jitter(odsf_top3, z = zones)
od_top3_disaggregated = od_disaggregate(odsf_top3, z = zones, max_per_od = 100)
plot(od_top3_disaggregated$geometry) # issue: loads of intrazonal flows
plot(zones$geometry, add = TRUE)
# why all the intra-zonal flows?
