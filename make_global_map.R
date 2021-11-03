#-----------------------------------------------------------------------------
#
# This file takes SDG trends and calculates a global context map (or prior)
#
#-----------------------------------------------------------------------------

folder <- "inputs/SDG trends/"

context.map <- matrix(ncol = 17, nrow = 17)
rownames(context.map) <- paste0("SDG",1:17)
colnames(context.map) <- paste0("SDG",1:17)

# There is only a single PC for the SDGs
PCA <- "PC1"
PCB <- "PC1"

for (sdgA.SDG in 1:16) {
  for (sdgB.SDG in (sdgA.SDG+1):17) {
    sdgA <- read.csv(paste0(folder,"SDG_",sdgA.SDG,"_trends.csv"), header = T, stringsAsFactors = F)
    sdgB <- read.csv(paste0(folder,"SDG_",sdgB.SDG,"_trends.csv"), header = T, stringsAsFactors = F)
    
    sdgA <- sdgA[complete.cases(sdgA),]
    sdgB <- sdgB[complete.cases(sdgB),]
    
    # Ensure we're using a common set of area codes (ought to always be the same)
    areas <- intersect(sdgA$GeoAreaCode, sdgB$GeoAreaCode)
    
    sdg.table <- merge(sdgA[,c("GeoAreaCode",PCA)],sdgB[,c("GeoAreaCode",PCB)], by="GeoAreaCode")
    names(sdg.table) <- c("GeoAreaCode", paste0(PCA,"A"), paste0(PCB,"B"))
    
    freq.table <- table(sdg.table[,2:3])
    # Anything missing?
    if (ncol(freq.table) < 2) {
      current <- colnames(freq.table)
      missing <- setdiff(c("n","p"),current)
      newvals <- rep(0, nrow(freq.table))
      for (i in 1:length(missing)) {
        freq.table <- cbind(freq.table, newvals)
      }
      colnames(freq.table) <- c(current, missing)
    }
    if (nrow(freq.table) < 2) {
      current <- rownames(freq.table)
      missing <- setdiff(c("n","p"),rownames(freq.table))
      newvals <- rep(0, ncol(freq.table)) # This should be 2, but just to be sure
      for (i in 1:length(missing)) {
        freq.table <- rbind(freq.table, newvals)
      }
      rownames(freq.table) <- c(current, missing)
    }
    numAB <- freq.table["p","p"]/sum(freq.table[,"p"])
    denAB <- freq.table["p","n"]/sum(freq.table[,"n"])
    
    alpha_AB <- log(numAB + base^(-maxval), base) - log(denAB + base^(-maxval), base)
    
    numBA <- freq.table["p","p"]/sum(freq.table["p",])
    denBA <- freq.table["n","p"]/sum(freq.table["n",])
    
    alpha_BA <- log(numBA + base^(-maxval), base) - log(denBA + base^(-maxval), base)
    
    context.map[sdgA.SDG, sdgB.SDG] <- alpha_AB
    context.map[sdgB.SDG, sdgA.SDG] <- alpha_BA
  }
}

png(paste0("outputs/global_context_map.png"), width = 780, height = 780)
heatmap.2(as.matrix(context.map), na.rm = F, Rowv = NA, Colv = NA, scale = "none",
          breaks = seq(-3, 3, length.out=101), dendrogram = "none",
          density.info = "none", trace = "none", key = F, offsetRow = -55,
          lhei=c(1,5), lwid=c(1,5), margins = c(5,5),
          main = "global prior",
          col = palette)
dev.off()
write.csv(context.map, "outputs/global_context_map.csv")

