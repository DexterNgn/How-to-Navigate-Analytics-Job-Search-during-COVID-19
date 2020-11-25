Jobdata<-read.csv("Job_DataSum.csv")
Joblocation<-read.csv("Job_Locations.csv")
Companyrating<-read.csv("Company_Rating.csv")

df<-merge(Jobdata,Companyrating,by="Company_Name",all.x = TRUE)
df<-merge(df,Joblocation,by=c("Location","State"),all.x = TRUE)
df$Description<-NULL
#df$JobID<-NULL
df$jobnum<-1
df$Date<-NULL
df$Function<-NULL
df$Type<-NULL
df$state<-df$State
df$State<-NULL


library(usmap)
library(ggplot2)
library(dplyr)
library(leaflet)

# US level 
dfrating<-df %>%
  group_by(state)%>%
  summarise(avgrating = mean(Rating))

plot_usmap(data = dfrating, labels = TRUE, values = "avgrating", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Rating", label = scales::comma
  ) + theme(legend.position = "right")

dfmap<-df %>%
  group_by(Location,state,Latitude,Longtitude) %>%
  summarise(sumjob = sum(jobnum))

leaflet_map <- leaflet(data = dfmap, options = leafletOptions(
  minZoom =0, maxZoom = 18)) %>% 
  addTiles() %>% 
  setView(lng = -98.5795, lat= 39.8283, zoom = 4) %>% 
  addCircleMarkers(lng = ~Longtitude, lat = ~Latitude,radius = 7,
                   color = 'grey', stroke=FALSE, fillOpacity = 0.6)

leaflet_map

leaflet(dfmap) %>%
  addTiles() %>%
  setView(lng = -98.5795, lat= 39.8283, zoom = 4) %>% 
  addCircles(lng = ~Longtitude, lat = ~Latitude, weight = 2,
             radius = ~sqrt(sumjob) * 10000, popup = ~Location)

dfmapstate<-df %>%
  group_by(state) %>%
  summarise(sumjob = sum(jobnum))

plot_usmap(data = dfmapstate, values = "sumjob", color = "pink") + 
  scale_fill_continuous(
    low = "white", high = "pink", name = "Total number of job", label = scales::comma
  ) + theme(legend.position = "right")

# region level 

east_coach<-c("ME","VT","NH","MA","RI","CT","NY",
              "NJ","PA","DE","MD","VA","NC","SC","GA","WV","AL","FL")
plot_usmap(data = dfmapstate, include = east_coach, labels = TRUE, values = "sumjob", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Total number of job", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = dfrating, include = east_coach,values = "avgrating", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "yellow", name = "Avg Rating", label = scales::comma
  ) + theme(legend.position = "right")

west_coach<-c("WA","MT","OR","ID","WY","CO","UT","NV",
              "CA","AK","HI","AZ","NM")
plot_usmap(data = dfmapstate, include = west_coach,labels = TRUE,values = "sumjob", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Total number of job", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = dfrating, include = west_coach,values = "avgrating", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "yellow", name = "Avg Rating", label = scales::comma
  ) + theme(legend.position = "right")

Midwest<-c("ND","SD","NE","KS","MN","IA","MO","WI",
              "IL","MI","IN","OH")
plot_usmap(data = dfmapstate, include = Midwest,labels = TRUE,values = "sumjob", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Total number of job", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = dfrating, include = Midwest,values = "avgrating", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "yellow", name = "Avg Rating", label = scales::comma
  ) + theme(legend.position = "right")

# City Level 
dfcity<-df %>%
  group_by(Location,state,Latitude,Longtitude,Company_Name) %>%
  summarise(sumjob = sum(jobnum),
            avgrating = mean(Rating))

leaflet(dfcity) %>%
  addTiles() %>%
  setView(lng = -122.3321, lat= 47.6062, zoom = 12) %>% 
  addCircles(lng = ~Longtitude, lat = ~Latitude, weight = 1,
             radius = ~sqrt(sumjob) * 100, popup = ~Location)


leaflet(dfcity) %>%
  addTiles() %>%
  setView(lng = -119.4179, lat= 36.7783, zoom = 10) %>% 
  addCircles(lng = ~Longtitude, lat = ~Latitude, weight = 1,
             radius = ~sqrt(sumjob) * 100, popup = ~Location)
