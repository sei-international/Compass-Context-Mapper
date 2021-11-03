#-----------------------------------------------------------------------------
#
# This file calculates trends from aggregate PCs for SDGs
#
#-----------------------------------------------------------------------------

library(Kendall)

folder <- "inputs/SDG PCs/"
ctry_codes <- read.csv("inputs/UN_area_codes.csv", header = T)

mk_test_threshold <- 0.1

for (SDG in 1:17) {
  sdg.pca <- read.csv(paste0(folder, SDG), row.names = 1)
  sdg.pca$TimePeriod <- as.numeric(sdg.pca$TimePeriod)
  # Replace UN M49 codes (as levels) with iso3 codes (as labels)
  sdg.pca$GeoAreaCode <- factor(sdg.pca$GeoAreaCode,
                              levels = ctry_codes$M49.code,
                              labels = ctry_codes$ISO.alpha3.code)
  # Get rid of regions, etc. Codes in ctry_codes are for countries only.
  sdg.pca <- sdg.pca[!is.na(sdg.pca$GeoAreaCode),]
  
  areas <- sort(unique(sdg.pca$GeoAreaCode))
  pcas <- names(sdg.pca)[3:ncol(sdg.pca)]
  
  out <- data.frame(matrix(nrow = length(areas), ncol = 1 + length(pcas)))
  names(out) <- c("GeoAreaCode", pcas)
  out$GeoAreaCode <- areas
  
  for (i in 1:length(areas)) {
    for (j in 1:length(pcas)) {
      sel <- sdg.pca[sdg.pca$GeoAreaCode == areas[i],]
      # Have to have enough observations
      if (length(sel[,2+j]) > 3) {
        # If all observations the same, then no trend
        if (length(unique(sel[,2+j])) > 1) {
          mk <- MannKendall(sel[,2+j]) # Offset by two to get past TimePeriod and GeoAreaCode columns
          sig <- mk$sl[1] < mk_test_threshold
        } else {
          sig <- FALSE
        }
        # If significant, check whether tendency is to increase or not
        if (sig) {
          dir <- sign(cor(sel[,2+j], sel$TimePeriod, use = "pairwise.complete.obs", method = "spearman"))
          if (dir > 0) {
            trend <- "p"
          } else {
            trend <- "n"
          }
        } else {
          trend <- "n"
        }
      } else {
        trend <- NA
      }
      out[i,1+j] <- trend # Only GeoAreaCode column
    }
  }
  write.csv(out, paste0("inputs/SDG trends/SDG_", SDG, "_trends.csv"), row.names = F)
}


