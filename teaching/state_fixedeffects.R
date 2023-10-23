projectpath<-"C:/Users/Len/Dropbox/Teaching/Data TA/R tutorial/sample project/"

#Start log file
sink(file=paste0(projectpath,'/log files/get_state_fes.txt'),split=TRUE, append=FALSE)

#get data
df <- read.csv(paste0(projectpath,"raw data/wages_bycounty_yr2000.csv"))
df2<-read.csv(paste0(projectpath,"raw data/fips-unemp-16.csv"))

#merge into single dataframe
df <- merge(df, df2, by="fips", all.x = TRUE, all.y=FALSE)
View(df)

#create a "state" variable
df$state<-floor(df$fips/1000)
table(df$state)

#Create a "factor variable"
df$state.f <- factor(df$state)

model_withstatefes<-lm(wage~employment+unemp+state.f, data=df)
summary(model_withstatefes)

#Use Excel spreadsheet of county names
library(readxl)
countydata <- read_excel("C:/Users/Len/Dropbox/Teaching/Data TA/R tutorial/all-geocodes-v2017.xlsx", skip = 3)

## Merging in county data to add dummy variable for counties that start with "W"
#---------------------------------------------------------------------------------------------------
#Lets rename some columns into names that are easier to work with:
colnames(countydata)
colnames(countydata)[colnames(countydata)=="County Code (FIPS)"] <- "countycode"
colnames(countydata)[colnames(countydata)=="State Code (FIPS)"] <- "statecode"
countydata$fips<-as.numeric(paste0(countydata$statecode,countydata$countycode))
colnames(countydata)[colnames(countydata)=="Area Name (including legal/statistical area description)"] <- "countyname"

#Let's just keep these three columns (we could also just do the rename here,,)
countydata<-countydata[ , c("fips","statecode","countyname")]

df <- merge(df, countydata, by="fips", all.x = TRUE, all.y=FALSE)
df$startswithW<-(tolower(substr(df$countyname,1,1))=="w")*1

model_withW<-lm(wage~startswithW+employment+unemp+state.f, data=df)
summary(model_withW)

#close log file
sink.reset <- function(){
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
}
