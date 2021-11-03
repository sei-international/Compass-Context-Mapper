
library(readr)
library(rjson)
library(mice)
library(dplyr) 
library(plyr)
library(missMDA)
library(sjmisc)
library(readxl)
library(reshape2)


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
factorfile_temp <- paste(DataPath, "UN SDG indicators", "UN", sep = "\\")



i <- 1;
country_codes <- c(4,
   248	,
   8	,
   12	,
   16	,
   20	,
   24	,
   660,
   10	,
   28	,
   32	,
   51	,
   533,
   36	,
   40	,
   31	,
   44	,
   48	,
   50	,
   52	,
   112,
   56	,
   84	,
   204,
   60	,
   64	,
   68	,
   535,
   70	,
   72	,
   74	,
   76	,
   86	,
   92	,
   96	,
   100	,
   854	,
   108	,
   132	,
   116	,
   120	,
   124	,
   136	,
   140	,
   148	,
   152	,
   156	,
   344	,
   446	,
   162	,
   166	,
   170	,
   174	,
   178	,
   184	,
   188	,
   384	,
   191	,
   192	,
   531	,
   196	,
   203	,
   408	,
   180	,
   208	,
   262	,
   212	,
   214	,
   218	,
   818	,
   222	,
   226	,
   232	,
   233	,
   748	,
   231	,
   238	,
   234	,
   242	,
   246	,
   250	,
   254	,
   258	,
   260	,
   266	,
   270	,
   268	,
   276	,
   288	,
   292	,
   300	,
   304	,
   308	,
   312	,
   316	,
   320	,
   831	,
   324	,
   624	,
   328	,
   332	,
   334	,
   336	,
   340	,
   348	,
   352	,
   356	,
   360	,
   364	,
   368	,
   372	,
   833	,
   376	,
   380	,
   388	,
   392	,
   832	,
   400	,
   398	,
   404	,
   296	,
   414	,
   417	,
   418	,
   428	,
   422	,
   426	,
   430	,
   434	,
   438	,
   440	,
   442	,
   450	,
   454	,
   458	,
   462	,
   466	,
   470	,
   584	,
   474	,
   478	,
   480	,
   175	,
   484	,
   583	,
   492	,
   496	,
   499	,
   500	,
   504	,
   508	,
   104	,
   516	,
   520	,
   524	,
   528	,
   540	,
   554	,
   558	,
   562	,
   566	,
   570	,
   574	,
   807	,
   580	,
   578	,
   512	,
   586	,
   585	,
   591	,
   598	,
   600	,
   604	,
   608	,
   612	,
   616	,
   620	,
   630	,
   634	,
   410	,
   498	,
   638	,
   642	,
   643	,
   646	,
   652	,
   654	,
   659	,
   662	,
   663	,
   666	,
   670	,
   882	,
   674	,
   678	,
   680	,
   682	,
   686	,
   688	,
   690	,
   694	,
   702	,
   534	,
   703	,
   705	,
   90	,
   706	,
   710	,
   239	,
   728	,
   724	,
   144	,
   275	,
   729	,
   740	,
   744	,
   752	,
   756	,
   760	,
   762	,
   764	,
   626	,
   768	,
   772	,
   776	,
   780	,
   788	,
   792	,
   795	,
   796	,
   798	,
   800	,
   804	,
   784	,
   826	,
   834	,
   581	,
   840	,
   850	,
   858	,
   860	,
   548	,
   862	,
   704	,
   876	,
   732	,
   887	,
   894	,
   716	)

years <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,
           2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)

country_year <- merge(country_codes, years, all=TRUE)


while (i < 18){
   sdg_output_file <-paste("SDG",i,".txt", sep = "", collapse = NULL)
   sink(file = sdg_output_file)
   sdg_num <-paste("SDG",i,".xlsx", sep = "", collapse = NULL)
   factorfile <- paste(factorfile_temp,sdg_num, sep = "\\")
   factor_codes_data <- read_excel(factorfile, sheet = 2)
   factor_codes_data <- factor_codes_data %>% select("SeriesCode","Value","GeoAreaCode","TimePeriod")
   factor_codes_data$Value <- as.numeric(as.character(factor_codes_data$Value))
   factor_codes_data$GeoAreaCode <- as.numeric(as.character(factor_codes_data$GeoAreaCode))
   factor_codes_data$TimePeriod <- as.numeric(as.character(factor_codes_data$TimePeriod))
   
   data <- dcast(data = factor_codes_data, formula = TimePeriod + GeoAreaCode ~ SeriesCode, fun.aggregate = sum, value.var = "Value")

   PCAPath <- paste(DataPath, "PCA_By_Factor", sep = "\\")
   dir.create(PCAPath)
   
   sel.all <- data
   sel.all = sel.all[sel.all$GeoAreaCode %in% country_codes,]
   sel.all = sel.all[sel.all$TimePeriod %in% years,]
   
   
   if(ncol(sel.all) > 3){
      sel.nonlin = sel.all[, which(apply(sel.all, 2, var) != 0)]
   
      imputed_Data <- mice(sel.nonlin, m=5, maxit=5,  method = 'pmm', print =  FALSE)
      
      completeDataAdd <- complete(imputed_Data)
      
      completeData <- completeDataAdd[, which(apply(completeDataAdd, 2, var) != 0)]
      #completeData <- sel.all
      
      completeData <- completeData[,!(names(completeData) %in% c("GeoAreaCode","TimePeriod"))]
      if(ncol(completeData) > 1){
         sdg_file = paste( "SDG" , i , ".xlsx", sep="" )
         valence_file <- paste(PCAPath, sdg_file, sep="\\" )
         valence_data <- read_excel(valence_file, sheet = 2)
         valence_data <- valence_data[,c(1,2)]
         
         valence_data = valence_data[complete.cases(valence_data), ]
         sel.pr <- prcomp(na.omit(completeData[c(1:length(completeData))]), center = TRUE, scale = TRUE)
         
         sel.ev = sel.pr$sdev^2
         j = 0
         k = 1
         while(k <= length(sel.ev)) {
            if(sel.ev[k] >= 1)
            {
               j = j + 1
            }
            k = k + 1
         }
         sel.pr$rotation = sel.pr$rotation[1:k-1,1:j,drop = FALSE]
         indicators = rownames(sel.pr$rotation)
         valence_data <- merge(indicators,valence_data,by.x = 1, by.y = 2)
         sdg_wj = matrix(1:j)
         valence_data = valence_data[,c(2)]
         for(col in 1:ncol(sel.pr$rotation)){
            num = 0
            den = 0
            for(row in 1:nrow(sel.pr$rotation)){
               num = num + sel.pr$rotation[row,col]*valence_data[row]
               den = num + abs(sel.pr$rotation[row,col])
            }
            sdg_wj[col] = num/den
         }
         sdg_l_agg = matrix(1:nrow(sel.pr$rotation))
         for(row in 1:nrow(sel.pr$rotation))
         {
            load = 0
            for(col in 1:ncol(sel.pr$rotation))
            {
             load = load + sdg_wj[col]*sel.pr$rotation[row,col]
            }
            sdg_l_agg[row] = load
         }
         
         sel.pr$rotation = sel.pr$rotation[1:length(sel.ev),1:1, drop=FALSE]
         sel.pr$rotation[1:length(sel.ev)] = sdg_l_agg
         print(sel.pr$rotation)
         pred <- predict(sel.pr, newdata=completeData[1:length(completeData)])
         timeseries <- cbind(sel.all[1:2], pred)
         timeseries <- merge(timeseries, country_year, by.x = c("TimePeriod","GeoAreaCode"), by.y = c("y","x"), all.y
=TRUE)
         imputed_Data <- mice(timeseries, m=5, maxit=5,  method = 'pmm', print =  FALSE)
         
         timeseries <- complete(imputed_Data)
         #timeseries <- timeseries[!(grepl("Total", timeseries$Year, fixed = TRUE)),]
         file <- paste(PCAPath, i , sep="\\" )
         write.csv(timeseries,file)
        
         
         screeplot(sel.pr, type = "l", npcs = 15, main = "Screeplot of the  PCs")
         abline(h = 1, col="red", lty=5)
         legend("topright", legend=c("Eigenvalue = 1"),
                col=c("red"), lty=5, cex=0.6)
         
      }
   }
   else{
      imputed_Data <- mice(sel.all, m=5, maxit=5,  method = 'pmm', print =  FALSE)
      timeseries <- complete(imputed_Data)
      names(sel.all)[3] <- "PC1"
      file <- paste(PCAPath, i , sep="\\" )
      write.csv(sel.all,file)
   }
   sink(file = NULL)
   i <- i + 1
   
}
