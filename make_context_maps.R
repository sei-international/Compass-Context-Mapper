
ctry.lvls <- read.csv("outputs/country_factor_levels.csv", header = T, stringsAsFactors = F)
global.prior <- read.csv("outputs/global_context_map.csv", header = T, row.names = 1)

# Run from here downward for different countries
for (ctry.code in countries) {
  context.map <- 0 * global.prior # Want that size, with NAs, but equal to zero otherwise
  
  lvls <- ctry.lvls[ctry.lvls$iso3 == ctry.code,][3:ncol(ctry.lvls)]
  n <- ncol(ctry.lvls) - 2
  n_inv <- 1.0/n
  
  if (nrow(lvls) > 0) {
    for (lvl in ctry.lvls[ctry.lvls$iso3 == ctry.code,][3:ncol(ctry.lvls)]) {
      factor.map <- read.csv(paste0("outputs/factor_map_",lvl,".csv"), header = T, row.names = 1)
      context.map <- context.map + n_inv * factor.map
    }
    
    for (i in 1:ncol(context.map)) {
      context.map[,i] <- pmin(pmax(context.map[,i],-3.0),3)
    }
    
    png(paste0("outputs/context_map_", ctry.code, ".png"), width = 780, height = 780)
    heatmap.2(as.matrix(context.map), na.rm = F, Rowv = NA, Colv = NA, scale = "none",
              breaks = seq(-3, 3, length.out=101), dendrogram = "none",
              density.info = "none", trace = "none", key = F, offsetRow = -55,
              lhei=c(1,5), lwid=c(1,5), margins = c(5,5),
              main = ctry.code,
              col = palette)
    dev.off()
    write.csv(context.map, paste0("outputs/context_map_", ctry.code, ".csv"))

  } else {
    print(paste("Skipping", ctry.code))
  }
  
}
