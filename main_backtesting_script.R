require(data.table)
require(Hmisc)
require (MASS)

#import the data
assetgrowth.dt <- fread("C:/Users/vsheth/Desktop/asset_growth/crsp_compustat_merged_8_6.csv")

#exclude companies with SIC code between 6000 and 6999
assetgrowth.dt <- assetgrowth.dt[!sic %between% c(6000,6999)]

#exclude non-US companies
assetgrowth.dt <- assetgrowth.dt[fic == "USA"] 

#exclude duplicates
assetgrowth.dt <- unique(assetgrowth.dt, by=c("fyear","GVKEY"))

#create an asset growth column and initialize with NA
assetgrowth.dt <- assetgrowth.dt[,assetgrowth:=as.double(NA_integer_)]

#loop through all companies identified by GVKEY
for (gvkey in unique(assetgrowth.dt$GVKEY))
{
  tempassetgrowth.dt <- assetgrowth.dt[GVKEY==gvkey]
  
  yearsList <- c()
  
  yearsList <- c(yearsList,tempassetgrowth.dt$fyear)
  
  #loop through all the years for which data is available. Calculate the asset growth for each year and store it.
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

# read the historical returns data
returns.dt <- fread("C:/Users/vsheth/Desktop/asset_growth/returns_2009_2015.csv", na.strings = c("NA","C","B"))

# rename PERMNO to LPERMN
names(returns.dt)[names(returns.dt) =="PERMNO"] <- "LPERMNO"

#exclude rows for which returns data is not available
returns.dt <- returns.dt[!is.na(RET), fyear := as.integer(date / 10000)]

positions.dt <- c()

top500.dt <- data.frame()

yearsList <- NULL

assetgrowth.dt <- assetgrowth.dt[!is.na(assetgrowth)] 

for (year in unique(assetgrowth.dt$fyear)) 
{
  tempassetgrowth.dt <- assetgrowth.dt[fyear==year]
  
  if (nrow(tempassetgrowth.dt) != 0) 
  {
    yearsList <- c(yearsList, year)
    
    #For each year sort the companies in descending order by market cap. Select the 500 largest companies
    tempassetgrowth.dt <- head(tempassetgrowth.dt[order(tempassetgrowth.dt$mkvalt, decreasing = T)], n=500)
  
    #Now sort these 500 companies in ascending order of their asset growth rate.
    tempassetgrowth.dt <- tempassetgrowth.dt[order(tempassetgrowth.dt$assetgrowth, increasing = T),]
  
    #Assign companies to 1st to 10th decile based on the bucket in which asset growth falls.
    tempassetgrowth.dt$bin <- as.numeric(cut2(tempassetgrowth.dt$assetgrowth, g=10))
    
    # fix this. only need to add companies in bin 1
    templ1 = list(top500.dt, tempassetgrowth.dt)

    top500.dt <-rbindlist(templ1, fill=TRUE)
    
    templ2 = list(positions.dt, as.data.table(rbind(tempassetgrowth.dt[bin==1]$LPERMNO)))
             
    #generate the lists of companies we will be investing in for the given year
    positions.dt <- rbindlist(templ2, fill=TRUE)

  }
}

positions.dt$fyear <- yearsList

#merge the data for the top 500 companies and historical returns data
returns.top500.dt <- merge(top500.dt, returns.dt, by = c("LPERMNO","fyear"))

returns.top500.dt[, `:=`(month = as.integer(date / 100), RET = as.double(RET))]

#returns.company.summary.dt <- returns.top500.dt[, list(ret_mean = unlist(lapply(.SD, mean, na.rm=TRUE)),
#                               ret_sd = unlist(lapply(.SD, sd, na.rm = TRUE))),
#                          by=c("LPERMNO","fyear"),
#                          .SDcols = c("RET")]

#calculate the monthly portfolio returns (equally weighted portfolio)
returns.portfolio.monthly.summary.dt <- returns.top500.dt[, list(monthly_ret = unlist(lapply(.SD, mean, na.rm=TRUE)),
                                                                 monthly_sd = unlist(lapply(.SD, sd, na.rm = TRUE))),
                                                                 by=c("fyear","month"),
                                                                .SDcols = c("RET")]
 

#calculate the average monthly returns for each year (equally weighted portfolio)
returns.portfolio.summary.dt <- returns.portfolio.monthly.summary.dt[, list(monthly_ret = unlist(lapply(.SD, mean, na.rm=TRUE)),
                                                                            monthly_sd = unlist(lapply(.SD, sd, na.rm = TRUE))),
                                                                            by=c("fyear"),
                                                                           .SDcols = c("monthly_ret")]

returns.portfolio.summary.dt$annual_ret <-returns.portfolio.summary.dt$monthly_ret * 12

returns.portfolio.summary.dt$annual_sd <-returns.portfolio.summary.dt$monthly_sd * sqrt(12)

#merge the yearly portfolio returns data with the companies we invested in.
returns.portfolio.summary.dt <- merge(returns.portfolio.summary.dt, positions.dt, by = c("fyear"))
