require(data.table)
require(Hmisc)
require (MASS)

assetgrowth.dt <- fread("C:/Users/vsheth/Desktop/asset_growth/crsp_compustat_merged_8_6.csv")

assetgrowth.dt <- assetgrowth.dt[!sic %between% c(6000,6999)]

assetgrowth.dt <- assetgrowth.dt[fic == "USA"] 

assetgrowth.dt <- unique(assetgrowth.dt, by=c("fyear","GVKEY"))

assetgrowth.dt <- assetgrowth.dt[,assetgrowth:=as.double(NA_integer_)]

for (gvkey in unique(assetgrowth.dt$GVKEY))
{
  tempassetgrowth.dt <- assetgrowth.dt[GVKEY==gvkey]
  
  yearsList <- c()
  
  yearsList <- c(yearsList,tempassetgrowth.dt$fyear)
  
  for (year in yearsList) 
  {
    if ((year-1) %in% yearsList)
    {
      growth <- (tempassetgrowth.dt[fyear==year]$at - tempassetgrowth.dt[fyear==(year-1)]$at)/tempassetgrowth.dt[fyear==(year-1)]$at
      
      assetgrowth.dt[(GVKEY==gvkey) & (fyear == year),assetgrowth:=growth]
    }
  }
  
  remove(yearsList)
}

positions.dt <- c()

yearsList <- NULL

for (year in unique(assetgrowth.dt$fyear)) 
{
  tempassetgrowth.dt <- assetgrowth.dt[fyear==year & !(is.na(assetgrowth))]
  
  if (nrow(tempassetgrowth.dt) != 0) 
  {
    yearsList <- c(yearsList, year)
    
    tempassetgrowth.dt <- head(tempassetgrowth.dt[order(tempassetgrowth.dt$mkvalt, decreasing = T)], n=500)
  
    tempassetgrowth.dt <- tempassetgrowth.dt[order(tempassetgrowth.dt$assetgrowth, increasing = T),]
  
    tempassetgrowth.dt$bin <- as.numeric(cut2(tempassetgrowth.dt$assetgrowth, g=10))
     
#    write.table(rbind(tempassetgrowth.dt[bin==1]$conm$fyear),file = "C:/Users/vsheth/Desktop/asset_growth/positions.csv", append = TRUE,col.names = 
#                  FALSE, row.names = FALSE, sep = ",")
  
    l = list(positions.dt, as.data.table(rbind(tempassetgrowth.dt[bin==1]$conm)))
             
    positions.dt <- rbindlist(l, fill=TRUE)
    
  }
}

positions.dt$year <- yearsList





