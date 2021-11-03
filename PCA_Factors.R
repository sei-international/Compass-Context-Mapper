
library(readr)
library(rjson)
library(mice)
library(dplyr) 
library(plyr)
library(missMDA)
library(sjmisc)
library(readxl)

#--------------------------------

#
# Find local Dropbox folder
#
#--------------------------------

if (file.exists(paste(Sys.getenv("APPDATA"),"Dropbox","info.json",sep = "\\"))) {
  DropboxPath <- fromJSON(file = paste(Sys.getenv("APPDATA"),"Dropbox","info.json",sep = "\\"))$personal$path
} else if (file.exists(paste(Sys.getenv("LOCALAPPDATA"),"Dropbox","info.json",sep = "\\"))) {
  DropboxPath <- fromJSON(file = paste(Sys.getenv("LOCALAPPDATA"),"Dropbox","info.json",sep = "\\"))$personal$path
} else {
  stop("Dropbox path cannot be found")
}

#DropboxPath = '\Users\chave\Dropbox\'
DataPath <- paste(DropboxPath, "A2030Compass-data", sep = "\\")
factorfile <- paste(DataPath,"FactorIndicatorCodes.xlsx", sep = "\\")
factor_codes_data <- read_excel(factorfile, sheet = 1)
factor_codes_data <- factor_codes_data %>% select("Factor Theme","Indicator")

factor_themes <- unique(factor_codes_data %>% select("Factor Theme"))

for(row in 1:nrow(factor_themes)){
  
  name = factor_themes$`Factor Theme`[row]
  print(name)
  
  factor_num <-paste(name,".xlsx", sep = "", collapse = NULL)
  factorfile <- paste(DataPath,factor_num, sep = "\\")
  

  indicator_codes_temp <- factor_codes_data[which(factor_codes_data$`Factor Theme` == name),]
  indicator_codes <- indicator_codes_temp$"Indicator"

  PCAPath <- paste(DataPath, "PCA_By_Factor\\Factors", sep = "\\")
  dir.create(PCAPath)
  #--------------------------------
  #
  # UN SDG indicators: From FAO
  #
  #--------------------------------
  fao.getData <- function(codes, datafile) {
    retval <- read.csv(datafile, header = T)
   retval <- retval[retval$Indicator_Code %in% codes,]
  retval <- reshape(retval[,c("Country_Code","Year","Indicator_Code","Value")],
                   idvar = c("Country_Code","Year"), timevar = "Indicator_Code",
                  direction = "wide")
  vars <- gsub("^Value\\.","",names(retval)[3:ncol(retval)])
  if(ncol(retval) >= 3){
    names(retval)[3:ncol(retval)] <- vars
   return(retval)
  }
  }
  
  FAO.series <- indicator_codes
  
  datafile <- paste(DataPath, "UN SDG indicators", "FAO", "UN_SDG_FAO.csv", sep = "\\")
  
  sel.fao <- fao.getData(FAO.series, datafile)
  
  #--------------------------------
  #
  # WDI
  #
  #--------------------------------
  wdi.getData <- function(codes, datafile, countryfile, nbuff = 1000, bom = '\ufeff') {
    cntry_data <- read.csv(countryfile, header = T, stringsAsFactors = F)
    cntry_list <- cntry_data[trimws(cntry_data$Region) != "",1]
    fhndl <- file(datafile, "r")
    # Get rid of the byte order mark in the header
    header <- unlist(strsplit(readLines(fhndl, n = 1, encoding = "UTF-8"),","))
    header <- gsub("\"",'',header)
    header <- gsub(" ",'_',header)
    years <- header[5:length(header)]
    header[5:length(header)] <- paste0("X",years)
    retval <- data.frame(matrix(nrow = 0, ncol = length(header)))
    while ( TRUE ) {
      lines <- read.table(text = readLines(fhndl, n = nbuff, encoding = "UTF-8"), sep = ",")
      retval <- rbind(retval, lines[(lines[,2] %in% cntry_list) & (as.character(lines[,4]) %in% codes),])
      if (nrow(lines) < nbuff) {
        break
      }
    }
    names(retval) <- header
    # The WDI file has an extra column at the end (trailing comma)
    while (is.na(names(retval)[ncol(retval)])) {
      retval[,ncol(retval)] <- NULL
    }
    if(nrow(retval) == 0){
      return(NULL)
    }
    retval.long <- reshape(retval, varying = paste0("X",years), sep = "", timevar = "Year", direction = "long")
    retval <- reshape(retval.long[,c("Country_Code","Year","Indicator_Code","X")],
                      idvar = c("Country_Code","Year"), timevar = "Indicator_Code",
                      direction = "wide")
    vars <- gsub("^X\\.","",names(retval)[3:ncol(retval)])
    names(retval)[3:ncol(retval)] <- vars
    #retval <- retval[rowSums(is.na(retval[,vars])) < length(vars),]
    return(retval)
  }
  
  WDI.series <- indicator_codes

  datafile <- paste(DataPath, "WDI", "WDIData.csv", sep = "\\")
  countryfile <- paste(DataPath, "WDI", "WDICountry.csv", sep = "\\")
  

  
  sel.wdi <- wdi.getData(WDI.series, datafile, countryfile)
  
  
  #--------------------------------
  #
  # UN SDG indicators: From UN
  #
  #--------------------------------
  
  un.filterData <- function(codes, names) {
    i <- 3
    l <- c("Year","Area_Code")
    for (n in names){
      for (c in codes) {
        if(str_contains(n, c)){
          l[i] <- n
          i <- i + 1
        }
      }
    }
    return(l)
  }
  
  un.getData <- function(datafile, codes) {
    retval <- read.csv(datafile, header = T)
    cols <- un.filterData(sel.codes,colnames(retval))
    retval <- retval[,cols]
    if(ncol(retval) >= 3){
      return(retval)
    }
  }
  
  datafile <- paste(DataPath, "UN SDG indicators", "UN", "all_data_combined.csv", sep = "\\")
  
  sel.codes <- indicator_codes
  
  sel.un <- un.getData(datafile, sel.codes)
  
  #--------------------------------
  #--------------------------------
  #
  # Combined
  #
  #--------------------------------
  #--------------------------------
  
  if(!is.null(sel.wdi))
    sel.wdi <- rename(sel.wdi, c("Country_Code"="Area_Code"))
  if(!is.null(sel.fao))
    sel.fao <- rename(sel.fao, c("Country_Code"="Area_Code"))
  
  if(!is.null(sel.wdi) && !is.null(sel.un))
    sel.temp <- merge(x = sel.wdi, y = sel.un, by = c("Year","Area_Code"), all = TRUE)
  else if(!is.null(sel.wdi))
    sel.temp <- sel.wdi
  else
    sel.temp <- sel.un
  if(!is.null(sel.temp) && !is.null(sel.fao))
    sel.all <- merge(x = sel.temp, y = sel.fao, by = c("Year","Area_Code"), all = TRUE)
  else if(!is.null(sel.fao))
    sel.all <- sel.fao
  else
    sel.all <- sel.temp
  

  if(!is.null(sel.all)){

    imputed_Data <- mice(sel.all, m=5, maxit=5,  method = 'cart', print =  FALSE)

    completeDataAdd <- complete(imputed_Data)
    
    completeData <- completeDataAdd[, which(apply(completeDataAdd, 2, var) != 0)]
    
    completeData <- completeData[,!(names(completeData) %in% c("Area_Code","Year"))]
    
    if(!is.null(completeData) && ncol(completeData) > 1){
      sel.pr <- prcomp(na.omit(completeData[c(1:length(completeData))]), center = TRUE, scale = TRUE, tol = 0.3)
      
      
      summary(sel.pr)
      
      print(sel.pr$rotation)
      #pairs(completeData[c(3:length(completeData))])
      
      print(cor(completeData[c(1:length(completeData))]))
      # <- autoplot(pilots.pca.scaled, data = pilots, colour = 'Group')
      #pca.plot.scaled
      pred <- predict(sel.pr, newdata=completeData[1:length(completeData)])
      timeseries <- cbind(completeDataAdd[1:2], pred)
      timeseries <- timeseries[!(grepl("Total", timeseries$Year, fixed = TRUE)),]
      name = gsub('/', '_',name)
      file <- paste(PCAPath, name , sep="\\" )
      write.csv(timeseries,file)
      
      screeplot(sel.pr, type = "l", npcs = 15, main = "Screeplot of the  PCs")
      abline(h = 1, col="red", lty=5)
      legend("topright", legend=c("Eigenvalue = 1"),
            col=c("red"), lty=5, cex=0.6)
      #cumpro <- cumsum(sel.pr$sdev^2 / sum(sel.pr$sdev^2))
      #plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
      #abline(v = 8, col="blue", lty=5)
      #abline(h = 0.88759, col="blue", lty=5)
      #legend("topleft", legend=c("Cut-off @ PC8"),
      #      col=c("blue"), lty=5, cex=0.6)
    }
  }
}

