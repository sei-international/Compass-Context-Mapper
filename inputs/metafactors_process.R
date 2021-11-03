# Note that "Social protection" is already in the desired format, so it is entered directly into the input file list
mf_list <- c("GNI-PPP","Corruption","Stability","Voice",
             "Fisheries_Prod","Freshwater_res","Forest_Share",
             "Agric_Share","Nat_Res_Rent")
mf_prefix <- "MF_"
in_folder <- "inputs/metafactor data/"
out_folder <- "inputs/factor PCs/"

for (metafactor in mf_list) {
  dat <- read.csv(paste0(in_folder, metafactor, "_sel.csv"), header = T)
  
  idvar <- colnames(dat)[1]
  xyears <- colnames(dat)[2:ncol(dat)]
  years <- as.numeric(gsub("^X", "", xyears))
  
  dat.long <- reshape(dat, idvar = idvar, v.names = "X", sep = "", varying = xyears, times = years, direction = "long")
  dat.long <- dat.long[complete.cases(dat.long),]
  dat.long <- dat.long[c(2,1,3)]
  names(dat.long) <- c("Year","ISO_alpha3_code","PC1") # This is not actually a PC, but it works with existing scripts
  rownames(dat.long) <- 1:nrow(dat.long)
  
  write.csv(dat.long, paste0(out_folder, mf_prefix, metafactor))
}
