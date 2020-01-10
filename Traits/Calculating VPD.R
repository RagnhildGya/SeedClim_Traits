### Calculating VPD ###

### Load library ###
library(ncdf4)

### Load datasets ###

load("Traits/Data/MetaBioclimAllCountries.RData")

metaNorway <- meta %>% 
  filter(Country == "NO")

long <- round(metaNorway$Longitude*2)+360
lat <- round(metaNorway$Latitude*2)+180

#https://crudata.uea.ac.uk/cru/data/hrg/

vap_nc <- fs::file_temp(ext = ".nc.gz")
url <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/cruts.1905011326.v4.03/vap/cru_ts4.03.1901.2018.vap.dat.nc.gz"

download.file(url, destfile = vap_nc, mode = "wb")

nc_path1 <- gunzip(vap_nc)

vap <- ncdf4::nc_open(nc_path1)

vap_time <- ncdf4::ncvar_get(vap, varid="vap", start=c(1, 1, 1000), count=c(720, 360, 365))

vap_time_avg <- rowMeans(vap_time, na.rm=TRUE, dims=2)

vaps <- vap_time_avg[long, lat]

rm(vap, vap_time, vap_time_avg)


#### Pet ###

pet_nc <- fs::file_temp(ext = ".nc.gz")
url <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/cruts.1905011326.v4.03/pet/cru_ts4.03.1901.2018.pet.dat.nc.gz"

download.file(url, destfile = pet_nc, mode = "wb")

isGzipped(pet_nc)

nc_path <- gunzip(pet_nc)

pet <- ncdf4::nc_open(nc_path)

pet_time <- ncdf4::ncvar_get(pet, varid="pet", start=c(1, 1, 1000), count=c(720, 360, 365))

pet_time_avg <- rowMeans(pet_time, na.rm=TRUE, dims=2)

pets <- pet_time_avg[long, lat]

vapor_air_pressure <- c()
potential_evapotranspiration <- c()
for(i in seq(1, nrow(metaNorway))){
  vapor_air_pressure <- c(vapor_air_pressure, vaps[i,i]) 
  potential_evapotranspiration <- c(potential_evapotranspiration, pets[i,i])
}

TWQ = metaNorway$bio10
SVP = 610.7*10^(7.5*TWQ/(237.3+TWQ))/1000

metaNorway$VPD <- SVP - (vapor_air_pressure/10)	#kPA	
metaNorway$PET <- potential_evapotranspiration
