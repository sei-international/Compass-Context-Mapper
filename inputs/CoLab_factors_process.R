library(yaml)

# Note that "Social protection" is already in the desired format, so it is entered directly into the input file list
cl_list <- c("AgriculturalLand", "EnvironmentalHealth", "Forests", "FossilFuelMinerals",
             "FreshWater", "GenderInequality", "GiniCoefficient", "Governance",
             "GrossNationalIncomePPP", "MaterialFootprint", "ParticipationGlobalEconInstitutions",
             "R_Dspending", "SocialProtection", "TransportationDomesticLogisticsPerfIndex",
             "UrbanPopulation", "Education", "EnergyAccess", "ICT", "MarineCoastal", "WaterSanitation")
cl_codes <- c("AG_LAND", "ENV_HLTH", "FOR", "FOS_MIN", "H2O", "GENDER", "GINI", "GOV", "GNI",
              "MAT_FOOT", "ECON_PART", "RnD", "SOC_PROT", "TRANSP", "URB", "EDUC", "ENER", "ICT",
              "MARINE", "WATSAN")
cl_prefix <- "CL_"
in_folder <- "inputs/CoLab data/"
out_folder <- "inputs/factor PCs/"
min_year <- 2005

iso3codes <- read_yaml("config_CoLab_factors.yml")$countries

options(warn=2)
for (metafactor in cl_list) {
  dat <- read.csv(paste0(in_folder, metafactor, ".csv"), header = T)
  
  idvar <- colnames(dat)[1]
  xyears <- colnames(dat)[2:ncol(dat)]
  years <- as.numeric(gsub("^X", "", xyears))
  
  dat.long <- reshape(dat, idvar = idvar, v.names = "X", sep = "", varying = xyears, times = years, direction = "long")
  dat.long <- dat.long[complete.cases(dat.long),]
  dat.long <- dat.long[c(2,1,3)]
  names(dat.long) <- c("Year","ISO_alpha3_code","PC1") # This is not actually a PC, but it works with existing scripts
  dat.long <- dat.long[dat.long$ISO_alpha3_code %in% iso3codes & dat.long$Year >= min_year,] # In case some non-country codes are used
  
  rownames(dat.long) <- 1:nrow(dat.long)
  
  write.csv(dat.long, paste0(out_folder, cl_prefix, metafactor))
}
options(warn=1)

# Generate YAML code for the factors -- uncomment and run if the list changes
factors <- list()
for (i in 1:length(cl_list)) {
  factors[[i]] <- list(paste0(cl_prefix, cl_list[i]), paste0(cl_prefix, cl_codes[i]), c(0.0,0.25,0.75,1.00))
  names(factors[[i]]) <- c("name","abbr","thresh")
}
write_yaml(factors, "CoLab_factors.yml")
