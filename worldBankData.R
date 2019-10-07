#R-Script by Joseph Holler, 2019
#Query and visualize World Bank data in maps and graphs
#install a libarary to connect to the World Bank API
#easier, better than using https://data.worldbank.org ?
#note that the 'safely managed' variables for water and sanitation have not been thoroughly created yet
install.packages("wbstats")
library(wbstats)

#install a library to connect to the Natural Earth geographic data
install.packages("rnaturalearth")
library(rnaturalearth)

#install a library with good color gradients
install.packages("RColorBrewer")
library(RColorBrewer)

#activate libraries for spatial data, graphing, and data manipulation
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)

#get countries in the SF (simple geographic features) data format from Natural Earth
necountries <- ne_countries(returnclass = "sf")

#get a list of countries in Natural Earth
countrylist <- necountries$adm0_a3

#download metadata for World Bank databases
wbMetaData <- wbcache() 
wbCountryList <- wbcountries()

#search for indicators mentioning sanitation or water, using the wbMetaData cache downloaded above
#You may change the search pattern to any keyword, where the pattern is currently: sanitation|water
wbIndicators <- wbsearch (pattern= "sanitation|water", extra=TRUE, fields="indicator", cache=wbMetaData)

#download world bank data for one indicator to map. The gapfill option allows unknown data to be filled by latest information, limited by the number of years specified by mrv.
#you may change the indicator to a new indicatorID. Look this up in wbIndicators. You may also change the date to a different year. Every country does not have data for every year.
country_data <- left_join(necountries, wb(indicator = "SH.STA.BASS.ZS", country = countrylist, mrv=15, gapfill = TRUE) %>% filter(date == "2017"), by = c("adm0_a3" = "iso3c"))

#Map the data you just downloaded
#you may change the palette of colors, where possible colors include:
#Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd 
#BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#you may also change the map title, currently Population with Basic Sanitation, and legend title, currently Percentage.
ggplot() + 
  geom_sf(data = st_transform(country_data,54009), aes(fill = cut_interval(value,7)), color = "grey")+
  scale_fill_brewer(palette="RdBu")+
  guides(fill=guide_legend(title="Percentage", title.hjust=0.5)) +
  labs(title = "Population with Basic Sanitation") +
  theme(plot.title = element_text(hjust = 0.5))


#query ONE indicator over time for just a few countries
#you may change the indicator (currently SH.STA.ODFC.ZS) and change the list of country codes (currently Cameroon: CMR and Germany: DEU))
wbTimeGraphData <- wb(indicator = "SH.STA.ODFC.ZS", country = c("CMR","DEU")) %>% mutate(date = as.integer(date))

#plot ONE indicator over time for just a few countries, coded by color
#alternative qualitative color schemes include Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
#you mach change the Y-coordinate label (currently percentage), title (currently Practice of Open Defecation), and color legend label (currently "Country")
ggplot(data = wbTimeGraphData, aes(x=date, y=value, by = country)) +
  scale_color_brewer(palette="Set1") +
  geom_line(aes(color=country, alpha=0.5)) +
  geom_point(aes(shape=country, color=country)) +
  labs(x = "Year", y="Percentage", title = "Practice of Open Defecation", color="Country", shape="Country") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(alpha = FALSE)

#query TWO indicators for all countries in 2017
wbScatterGraphData <- wb(indicator = c("SH.STA.ODFC.ZS","SH.STA.BASS.ZS"), country = countrylist, mrv=15, gapfill = TRUE, return_wide=TRUE ) %>% filter(date == "2017")

#make a scatterplot of two variables for all countries. You must change the X and Y to match the variables you queried in line 70 above. You may also change the X axis label, y-axis label, and graph title.
ggplot(data = wbScatterGraphData, aes(x=SH.STA.ODFC.ZS, y=SH.STA.BASS.ZS)) +
  geom_point() +
  geom_smooth(method='lm',size=1.5) +
  labs(x = "Practice of Open Defecation (Percentage of Population)", y="Access to Basic Sanitation (Percentage of Population)", title = "Open Defecation and Access to Sanitation") +
  theme(plot.title = element_text(hjust = 0.5))
