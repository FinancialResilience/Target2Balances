library(stargazer)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(lubridate)
#install.packages("tibble")
library(tibble)
library(readxl)
#install.packages("data.table")
library(data.table)

#install.packages("readstata13")
library(readstata13)

#install.packages("xtable")
library(xtable)

#install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("reshape2")
library(reshape2)

setwd("C:/Users/ben/Google Drive/phD/david projects/Target_2")

'-----------------------INSTRUCTIONS ON REFRESHING DATA-----------------------'

'1. Open the excel file (TGB_export) which comes from the ECB.
2. Go to the tab "pivot"
3. Click the "refresh" button
4. Re-run the below code.'


Import <- read_excel("TGB_export.xlsm", sheet = "data")

CountryFilter<- function(CountryName){
  Matching <- grep(CountryName, Import$`Reference area`)
  CountryData <- Import[Matching,]
  output <- select(CountryData, `Time period or range`, `Observation value`)
  colnames(output) <- c("Date", "Amount")
  return(output)
}

ECB <- CountryFilter("European Central Bank")
Austria <- CountryFilter("Austria")
Belgium <- CountryFilter("Belgium")
Cyprus <- CountryFilter("Cyprus")
Germany <- CountryFilter("Germany")
Estonia <- CountryFilter("Estonia")
Spain <- CountryFilter("Spain")
Finland <- CountryFilter("Finland")
France <- CountryFilter("France")
Greece <- CountryFilter("Greece")
Ireland <- CountryFilter("Ireland")
Italy <- CountryFilter("Italy")
Lithuania <- CountryFilter("Lithuania")
Luxembourg <- CountryFilter("Luxembourg")
Latvia <- CountryFilter("Latvia")
Malta <- CountryFilter("Malta")
Netherlands <- CountryFilter("Netherlands")
Portugal <- CountryFilter("Portugal")
Slovenia <- CountryFilter("Slovenia")
Slovakia <- CountryFilter("Slovakia")
ExtraEU <- CountryFilter("Extra Euro area")

CountryList <- list(ECB, Austria, Belgium, Cyprus, Germany, Estonia, Finland, France,
                     Greece, Ireland, Italy, Lithuania, Luxembourg, Latvia, Malta, Netherlands,
                     Portugal, Slovenia, Slovakia, Spain, ExtraEU)

Combined <- Reduce(function(x,y) merge(x=x, y=y, by = "Date"), 
                   CountryList)

colnames(Combined) <- c("Date", "ECB", "Austria", "Belgium", "Cyprus", "Germany", "Estonia", "Finland",
                        "France", "Greece", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia",
                        "Malta", "Netherlands", "Portugal", "Slovenia", "Slovakia", "Spain", "Extra EU")

write.csv(Combined, 'output.csv')


'--------------Plot data---------'

CountryFilter2<- function(CountryName){
  Matching <- grep(CountryName, Import$`Reference area`)
  CountryData <- Import[Matching,]
  output <- cbind(CountryName, select(CountryData, `Time period or range`, `Observation value`))
  colnames(output) <- c("Country", "Date", "Amount")
  return(output)
}

ECB2 <- CountryFilter2("European Central Bank")
Austria2 <- CountryFilter2("Austria")
Belgium2 <- CountryFilter2("Belgium")
Cyprus2 <- CountryFilter2("Cyprus")
Germany2 <- CountryFilter2("Germany")
Estonia2 <- CountryFilter2("Estonia")
Spain2 <- CountryFilter2("Spain")
Finland2 <- CountryFilter2("Finland")
France2 <- CountryFilter2("France")
Greece2 <- CountryFilter2("Greece")
Ireland2 <- CountryFilter2("Ireland")
Italy2 <- CountryFilter2("Italy")
Lithuania2 <- CountryFilter2("Lithuania")
Luxembourg2 <- CountryFilter2("Luxembourg")
Latvia2 <- CountryFilter2("Latvia")
Malta2 <- CountryFilter2("Malta")
Netherlands2 <- CountryFilter2("Netherlands")
Portugal2 <- CountryFilter2("Portugal")
Slovenia2 <- CountryFilter2("Slovenia")
Slovakia2 <- CountryFilter2("Slovakia")
ExtraEU2 <- CountryFilter2("Extra Euro area")

CombinedPlot <- rbind(ECB2, Austria2, Belgium2, Cyprus2, Germany2, Estonia2, Spain2, Finland2, France2,
                      Greece2, Ireland2, Italy2, Lithuania2, Luxembourg2, Latvia2, Malta2,
                      Netherlands2, Portugal2, Slovenia2, Slovakia2, ExtraEU2) 
colnames(CombinedPlot) <- c("Country", "Date", "Value")




'-------------Graph------------'


Lineplot <- ggplot(CombinedPlot, aes(x=Date, y=Value, colour=Country, group=Country))+
  geom_line()   

require(scales)

Lineplot + 
  scale_fill_brewer(palette="Spectral")+
  theme_light() + 
  theme(panel.border = element_blank(),
        axis.text.x = element_text(angle = 45)) +
  labs(x = "Year",
       title = "TARGET-2 Balance by Country",
       y = "Balance (EUR Millions)") +
  scale_y_continuous(labels = comma)

ggsave("Target2Plot.png", height = 8, width = 12)