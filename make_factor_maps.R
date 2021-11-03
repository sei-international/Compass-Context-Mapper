#-----------------------------------------------------------------------------
#
# This file takes SDG trends and calculates factor maps
#
#-----------------------------------------------------------------------------

num_sdgs <- 17

# For SDGs, only one PC
PCA <- "PC1"
PCB <- "PC1"

folder1 <- "inputs/factor PCs/"
folder2 <- "inputs/SDG trends/"

ctry_codes <- read.csv("inputs/UN_area_codes.csv", header = T)

ctry.lvls <- data.frame(matrix(ncol = 2, nrow = nrow(ctry_codes)))
names(ctry.lvls) <- c("name","iso3")
ctry.lvls$name <- ctry_codes$Country.or.Area
ctry.lvls$iso3 <- ctry_codes$ISO.alpha3.code

ranges0 <- data.frame(matrix(ncol = 5, nrow = num_sdgs^2))
names(ranges0) <- c("SDG_in", "SDG_out", "factor", "min", "max")
for (i in 1:num_sdgs) {
  r <- num_sdgs * (i - 1) + 1
  ranges0[r:(r + num_sdgs - 1),"SDG_in"] <- i
  ranges0[r:(r + num_sdgs - 1),"SDG_out"] <- 1:num_sdgs
}
ranges <- data.frame(matrix(ncol = 5, nrow = 0))

for (f in factors) {
  fact.fname <- paste0(folder1, f$name)
  cat("Processing factor '", f$name, "'", sep = "")
  fact.code <- f$abbr
  fact.dat <- read.csv(fact.fname, header = T, row.names = 1)
  
  ranges_tmp <- ranges0
  ranges_tmp[,"factor"] <- fact.code
  
  nPCs <- length(grep("PC",names(fact.dat)))
  cat(" with ", nPCs, "PCs...")

  for (PCndx in 1:nPCs) {
    PC <- paste0("PC",PCndx)
    nlvls <- length(f$thresh) - 1 # Remove one for the endpoint
    vals <- fact.dat[,PC]
    fact.dat$discr <- cut(vals, labels = paste0(fact.code,PCndx,"_",1:nlvls),
                          include.lowest = T,
                          breaks = quantile(vals,
                                            probs = f$thresh))
    # Find the most frequent categorization over the available years
    fact.lvl <- aggregate(fact.dat$discr, by = list(fact.dat$ISO_alpha3_code), function(x) names(which.max(table(x))))
    names(fact.lvl) <- c("iso3",paste0(fact.code,"_",PC))
    fact.lvl[,2] <- factor(fact.lvl[,2])
    ctry.lvls <- merge(ctry.lvls, fact.lvl, by = "iso3", all.x = T)
    
    # Create context maps, with p-values
    context.map <- matrix(ncol = num_sdgs, nrow = num_sdgs)
    rownames(context.map) <- paste0("SDG",1:num_sdgs)
    colnames(context.map) <- paste0("SDG",1:num_sdgs)

    p.values <- matrix(ncol = num_sdgs, nrow = num_sdgs)
    rownames(p.values) <- paste0("SDG",1:num_sdgs)
    colnames(p.values) <- paste0("SDG",1:num_sdgs)
    
    nlevel <- length(levels(fact.lvl[,2]))
    
    for (lvl in levels(fact.lvl[,2])) {
      for (sdgA.SDG in 1:16) {
        for (sdgB.SDG in (sdgA.SDG+1):num_sdgs) {
          sdgA <- read.csv(paste0(folder2,"SDG_",sdgA.SDG,"_trends.csv"), header = T, stringsAsFactors = F)
          sdgB <- read.csv(paste0(folder2,"SDG_",sdgB.SDG,"_trends.csv"), header = T, stringsAsFactors = F)
          
          sdgA <- sdgA[complete.cases(sdgA),]
          sdgB <- sdgB[complete.cases(sdgB),]
          
          sdg.table <- merge(sdgA[,c("GeoAreaCode",PCA)],sdgB[,c("GeoAreaCode",PCB)], by="GeoAreaCode")
          names(sdg.table) <- c("iso3", paste0(PCA,"A"), paste0(PCB,"B"))
          sdg.table <- merge(sdg.table, fact.lvl)
          
          freq.table <- table(sdg.table[,2:4])
          
          if (sum(dim(freq.table)) == 2 + 2 + nlevel) {
            trunc_val <- base^(-maxval)
            
            # Add small number to avoid divide by zero errors
            NF_p <- sum(freq.table[,"p",lvl]) + 1e-9
            NF_n <- sum(freq.table[,"n",lvl]) + 1e-9
            NFp_ <- sum(freq.table["p",,lvl]) + 1e-9
            NFn_ <- sum(freq.table["n",,lvl]) + 1e-9
            
            # Set an upper/lower limit at the maximum value
            NFpp_AB <- freq.table["p","p",lvl] + trunc_val * NF_p
            NFpp_BA <- freq.table["p","p",lvl] + trunc_val * NFp_
            NFpn <- freq.table["p","n",lvl] + trunc_val * NF_n
            NFnp <- freq.table["n","p",lvl] + trunc_val * NFn_
            
            NF_p <- (1 + trunc_val) * NF_p
            NF_n <- (1 + trunc_val) * NF_n
            NFp_ <- (1 + trunc_val) * NFp_
            NFn_ <- (1 + trunc_val) * NFn_
            
            num_AB <- NFpp_AB/NF_p
            denom_AB <- NFpn/NF_n
            
            alpha_AB <- log(num_AB, base) - log(denom_AB, base)
            
            num_BA <- NFpp_BA/NFp_
            denom_BA <- NFnp/NFn_
            
            alpha_BA <- log(num_BA, base) - log(denom_BA, base)
            
            # Binomial test
            i_min_num_AB <- round(NFpp_AB/sqrt(base))
            i_max_num_AB <- min(round(NF_p),round(NFpp_AB * sqrt(base)))
            p_num_AB <- sum(dbinom(i_min_num_AB:i_max_num_AB,round(NF_p),num_AB))
            
            i_min_num_BA <- round(NFpp_BA/sqrt(base))
            i_max_num_BA <- min(round(NFp_),round(NFpp_BA * sqrt(base)))
            p_num_BA <- sum(dbinom(i_min_num_BA:i_max_num_BA,round(NFp_),num_BA))
            
            i_min_denom_AB <- round(NFpn/sqrt(base))
            i_max_denom_AB <- min(round(NF_n),round(NFpn * sqrt(base)))
            p_denom_AB <- sum(dbinom(i_min_denom_AB:i_max_denom_AB,round(NF_n),denom_AB))
            p_AB <- 1 - p_num_AB * p_denom_AB
            
            i_min_denom_BA <- round(NFnp/sqrt(base))
            i_max_denom_BA <- min(round(NFn_),round(NFnp * sqrt(base)))
            p_denom_BA <- sum(dbinom(i_min_denom_BA:i_max_denom_BA,round(NFn_),denom_BA))
            p_BA <- 1 - p_num_BA * p_denom_BA
            
          } else {
            alpha_AB <- NA
            alpha_BA <- NA
            p_AB <- NA
            p_BA <- NA
          }
          
          context.map[sdgA.SDG, sdgB.SDG] <- alpha_AB
          context.map[sdgB.SDG, sdgA.SDG] <- alpha_BA
          
          p.values[sdgA.SDG, sdgB.SDG] <- p_AB
          p.values[sdgB.SDG, sdgA.SDG] <- p_BA
          
          rAB <- num_sdgs * (sdgA.SDG - 1) + sdgB.SDG
          rBA <- num_sdgs * (sdgB.SDG - 1) + sdgA.SDG
          if (alpha_AB < ranges_tmp[rAB,"min"] | is.na(ranges_tmp[rAB,"min"])) {
            ranges_tmp[rAB,"min"] <- alpha_AB
          }
          if (alpha_AB > ranges_tmp[rAB,"min"] | is.na(ranges_tmp[rAB,"min"])) {
            ranges_tmp[rAB,"max"] <- alpha_AB
          }
          if (alpha_BA < ranges_tmp[rBA,"min"] | is.na(ranges_tmp[rBA,"min"])) {
            ranges_tmp[rBA,"min"] <- alpha_BA
          }
          if (alpha_BA > ranges_tmp[rBA,"min"] | is.na(ranges_tmp[rBA,"min"])) {
            ranges_tmp[rBA,"max"] <- alpha_BA
          }
        }
      }
      
      png(paste0("outputs/factor_map_", lvl, ".png"), width = 780, height = 780)
      heatmap.2(as.matrix(context.map), na.rm = F, Rowv = NA, Colv = NA, scale = "none",
                breaks = seq(-3, 3, length.out=101), dendrogram = "none",
                density.info = "none", trace = "none", key = F, offsetRow = -55,
                lhei=c(1,5), lwid=c(1,5), margins = c(5,5),
                main = lvl,
                col = palette)
      dev.off()
      write.csv(context.map, paste0("outputs/factor_map_", lvl,".csv"))
      write.csv(p.values, paste0("outputs/p_values_", lvl,".csv"))
      
    }
    cat(PCndx, " ")
  }
  cat("finished '", f$name, "'\n", sep = "")
  ranges <- rbind(ranges, ranges_tmp)
}

ctry.lvls <- ctry.lvls[complete.cases(ctry.lvls[,2:ncol(ctry.lvls)]),]
write.csv(ctry.lvls, "outputs/country_factor_levels.csv", row.names = F)
write.csv(ranges, "outputs/ranges.csv", row.names = F)

