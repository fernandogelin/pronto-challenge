# set working directory
setwd("/Users/fernandogelin/Dropbox/pronto-challenge/data/")

# load required libraries
library(reshape2)
library(data.table)
library(dplyr)
library(rCharts)
library(ggvis)
library(ggplot2)
library(leaflet)
library(maptools)
library(rleafmap)
library(sp)
library(devtools)
library(jsonlite)
#library(ggmap)
#library(gdistance)
#library(geosphere)
#library(rMaps)
#library(maps)



# read files
list.files()
station <- read.csv("2015_station_data.csv", header=T)
#status <- read.csv("2015_status_data.csv", header=T)
trip <- read.csv("2015_trip_data.csv", header=T)
weather <- read.csv("2015_weather_data.csv", header=T)

station <- as.data.table(station)
#status <- as.data.table(status)
trip <- as.data.table(trip)
weather <- as.data.table(weather)

# transform weather and trip tables
weather[,Date := as.IDate(Date, format="%m/%d/%Y")]
trip[,"Date":= as.IDate(starttime, format="%m/%d/%Y %H:%M")]
trip[,"stopdate":= as.IDate(stoptime, format="%m/%d/%Y %H:%M")]
trip[,"starttime":= as.ITime(starttime, format="%m/%d/%Y %H:%M")]
trip[,"stoptime":= as.ITime(stoptime, format="%m/%d/%Y %H:%M")]
trip[,"trip":=as.factor(paste(from_station_id,to_station_id, sep="-"))]


trip_weather <- merge(weather, trip, by="Date")


#EDA
#plotting histogram and boxplot of trip duration reveals a skewd distribuition.
ggplot(trip) + aes(tripduration) + geom_histogram(binwidth=500, colour="black", fill="white")
ggplot(trip) + aes(1, tripduration) + geom_boxplot()

#split trip dataset: annual members, one day, 3-day
trip_annual <- filter(trip, usertype == "Annual Member")
trip_short_term <- filter(trip, usertype == "Short-Term Pass Holder")

ggplot(trip_short_term) + aes(tripduration) + geom_histogram(binwidth=500, colour="black", fill="white")
ggplot(trip_annual) + aes(tripduration) + geom_histogram(binwidth=500, colour="black", fill="white")

# number of trips per month
trip_weather %>% as.data.frame %>% ggvis(~yday(Date)) %>% layer_histograms(width=1)

# trips per month by gender
ggplot(trip) + aes(month(Date)) + geom_bar(aes(fill=factor(gender), alpha=0.5), binwidth=1)

#times used (station)
u = data.table()
for (i in 1:nrow(station)) {
     station_id =  as.character(station[i]$terminal)
     print(station_id)
     check_in_count = count(filter(trip, from_station_id == station_id))
     check_out_count = count(filter(trip, to_station_id == station_id))
     v = data.table("check_in_count"=check_in_count, "check_out_count"=check_out_count)
     u = rbind(u,v)
}

station_usage <- cbind(station,u)

# map station usage and range, density plot

m <- leaflet(data=station_usage)
m %>% addProviderTiles("CartoDB.Positron") %>% 
     addCircles(~long, ~lat,weight = 1, radius = ~sqrt(check_out_count.n)*3, color="blue") %>%
     addCircles(~long, ~lat,weight = 1, radius = ~sqrt(check_in_count.n)*3, color="red")










# Mapping the most popular route

# trips with return at the same station
trip_same = filter(trip,from_station_id == to_station_id)
# trips with return at a different station
trip_diff = filter(trip,from_station_id != to_station_id)

summary(trip_diff) 
most_popular_start1 = station[like(terminal,"WF-01")]
most_popular_end1 = station[like(terminal,"WF-04")]

most_popular_start2 = station[like(terminal,"CBD-13")]
most_popular_end2 = station[like(terminal,"BT-01")]

most_popular_start3 = station[like(terminal,"BT-03")]
most_popular_end3 = station[like(terminal,"CBD-13")]

most_popular_start4 = station[like(terminal,"BT-01")]
most_popular_end4 = station[like(terminal,"WF-01")]



map <- leaflet(data=station_usage)
map %>% addProviderTiles("HERE.normalNightGreyMobile") %>% 
  setView(-122.3329,47.60076, zoom=12)

map = Leaflet$new()
map$setView(c(47.60076, -122.3329), zoom=1)
map$tileLayer(provider = 'Acetate.terrain')

mywaypoints = list(c(most_popular_start1$lat, most_popular_start1$long), 
                   c(most_popular_end1$lat, most_popular_end1$long))
     
mywaypoints2 = list(c(most_popular_start2$lat, most_popular_start2$long), 
                   c(most_popular_end2$lat, most_popular_end2$long))



map$addAssets(
     css = "http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.css",
     jshead = "http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.min.js",
     jshead = "http://d3js.org/d3.v3.min.js",
     jshead = "http://rawgit.com/jashkenas/coffee-script/master/extras/coffee-script.js"
)

routingTemplate = "
<script>

var mywaypoints = %s
L.Routing.control({
waypoints: [
L.latLng.apply(null, mywaypoints[0]),
L.latLng.apply(null, mywaypoints[1]),
]
}).addTo(map);


</script>"

map$setTemplate(
     afterScript = sprintf(routingTemplate, RJSONIO::toJSON(mywaypoints))
)

map$set(width = 1450, height = 800)
map



############### trips over the year, weahter, pronto week
# number of trips along the day
trip[,"hour_start":=hour(starttime)]
trip[,"hour_stop":=hour(starttime)]
trip %>% ggvis(~hour_stop) %>% layer_densities(fill="blue")

#trips per day over the year
weather[,"yday":=yday(Date)]
weather[,"timestamp":=as.numeric(as.POSIXct(Date))]
# March 15 is unusually low, it turns out the precipitation was high
filter(weather, yday(Date) == 74)
weather %>% ggvis(~timestamp, ~Precipitation_In) %>% layer_lines()
# April 20th unusually high
filter(weather, yday(Date) == 110)



#trip[,"yday":=yday(Date)]
#trip[,"wday":=wday(Date)]
#trip[,"hour":=hour(starttime)]
#trip[,"month":=month(Date)]
#trip[hour == 0, hour := 24]
trip[,"timestamp" :=  as.numeric(as.POSIXct(Date))]
trip[,"timestamp_hour" :=  as.numeric(as.POSIXct(paste("2015-7-10", starttime, sep=" ")))]
trip_count_by_day <- plyr::count(trip, vars="timestamp")

k <- plyr::count(trip, vars=c("usertype","timestamp"))
write.csv(k, "timestamp.csv")

trip_count_by_day_member <- plyr::count(trip_annual, vars="timestamp")
trip_count_by_day_short <- plyr::count(trip_short_term, vars="timestamp")

trip_count_by_hour <- plyr::count(trip, vars="timestamp_hour")

trip_annual[,"hour":=hour(starttime)]
plyr::count(trip_annual, vars="hour")

trip_short_term[,"hour":=hour(starttime)]
plyr::count(trip_short_term, vars="hour")

trip_count_by_day %>% ggvis(~timestamp, ~freq) %>% layer_bars()

  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br />")
  }
  
  base <- trip %>% ggvis(~timestamp) %>% layer_bars(fill="blue",width=1)
  base #%>% add_tooltip(all_values, "hover")
  
#################################################
  
# plotting routes
# create data frame with routes
routes <- data.frame(from = trip$from_station_name, to = trip$to_station_name, from_id = trip$from_station_id, to_id = trip$to_station_id, stringsAsFactors = F)

routes %>% filter(from == to) %>% count # number of trips that end at the same station
routes %>% filter(from != to) %>% count # number of trips that end at a different station
route_diff <- routes %>% filter(from != to)

route_diff_count <- as.data.table(plyr::count(route_diff, vars=c("from","to", "from_id", "to_id")), stringsAsFactors = FALSE)
#route_diff_count[,from := sub("&", ",", from)]
#route_diff_count[,to := sub("&", ",", to)]
#route_diff_count[,from := paste(from, " seattle, wa", sep=",")]
#route_diff_count[,to := paste(to, " seattle, wa", sep=",")]
#route_diff_count = as.data.frame(route_diff_count)
route_diff_count =  filter(route_diff_count, to_id != "Pronto shop")
route_diff_count =  filter(route_diff_count, from_id != "Pronto shop")
#add coordinates to table with different routes count

trip_latlong = data.frame()
for (i in 1:nrow(route_diff_count)) {
     print(i)
     id_from <- as.character(route_diff_count[i,]$from_id)
     id_to <- as.character(route_diff_count[i,]$to_id)
     station_from <- filter(station, terminal == as.character(route_diff_count[i,]$from_id))
     station_to <- filter(station, terminal == as.character(route_diff_count[i,]$to_id))
     df = data.frame("from_lat"=station_from$lat, "from_long"=station_from$long, "to_lat"=station_to$lat, "to_long"=station_to$long)
     trip_latlong <- rbind(trip_latlong, df)
}

route_diff_count_latlong <- cbind(route_diff_count, trip_latlong)

write.csv(route_diff_count_latlong[order(-freq)][1:1000], "routes.csv")



#day_week_hour_freq <- plyr::count(trip, vars=c("wday", "hour"))
#month_day_freq <- plyr::count(trip, vars=c("month", "yday"))
timestamp <- plyr::count(trip, vars="timestamp")
timestamp_hour <- plyr::count(trip, vars="timestamp_hour")




#write.csv(timestamp, "timestamp.csv")
#write.csv(timestamp_hour, "timestamp_hour.csv")

# number of trips per day with precipitation data
weather <- weather[-366]
trip_prec <- as.data.table(cbind("timestamp"=timestamp$timestamp, "precip"=weather$Precipitation_In, "temp"=weather$Mean_Temperature_F, "freq"=trip_count_by_day$freq))
prec <- as.data.table(cbind("timestamp"=1000*timestamp$timestamp, "precip"=weather$Precipitation_In))
temp <- as.data.table(cbind("timestamp"=timestamp$timestamp, "temp"=weather$Mean_Temperature_F))
freq <- as.data.table(timestamp)[,"timestamp" := timestamp]
ggplot(trip_prec, aes(freq, precip)) + geom_point() + geom_smooth(method = "glm", family = gaussian(lin="log"), start=c(2,0))
ggplot(trip_prec, aes(freq, temp)) + geom_point() + geom_smooth(method="lm")


trip_prec %>% ggvis(~timestamp, ~freq) %>% layer_lines(~timestamp, ~precip)


write(toJSON(t(freq), dataframe = "column", matrix="columnmajor"), "timestamp.json")
write(toJSON(t(prec), dataframe = "column", matrix="columnmajor"), "prec.json")
write(toJSON(t(temp), dataframe = "column", matrix="columnmajor"), "temp.json")
write.csv(temp, "temp.csv")

p <- ggplot(temp, aes(timestapm, temp)) + geom_tile()



#coocurance matrix. for chord diagram, from-to trips
chord_matrix <- data.table("from"=trip$from_station_id, "to"=trip$to_station_id)
chord_matrix[,"from":= sub("-", "", from)]
chord_matrix[,"to":= sub("-", "", to)]

m <- data.table("fromto"=paste(chord_matrix$from, chord_matrix$to, sep="_"))
m_freq <- as.data.table(plyr::count(m, vars="fromto"))
m_split <- m_freq[,c("from","to") := tstrsplit(as.character(fromto), "_", fixed = T)]
m_split[,"fromto":=NULL]

write.csv(m_split, "stationMatrix.csv")

cooc_matrix <- dcast(m_split, to ~ from, value.var = "freq")
cooc_matrix <- as.data.frame(cooc_matrix)
cooc_matrix[is.na(cooc_matrix)] <- 0

#per neigborhood
chord_matrix_region <- chord_matrix[,"from":= gsub("\\d+","", from)]
chord_matrix_region <- chord_matrix[,"to":= gsub("\\d+","", to)]
k <- data.table("fromto"=paste(chord_matrix_region$from, chord_matrix_region$to, sep="_"))
k_freq <- as.data.table(plyr::count(k, vars="fromto"))
k_split <- k_freq[,c("from","to") := tstrsplit(as.character(fromto), "_", fixed = T)]
k_split[,"fromto":=NULL]
cooc_matrix_region <- dcast(k_split, to ~ from, value.var = "freq")
cooc_matrix_region <- as.data.frame(cooc_matrix_region)
cooc_matrix_region[is.na(cooc_matrix_region)] <- 0
k_freq <- data.table("from"=k_freq$from, "to"=k_freq$to, "freq"=k_freq$freq)
m_freq <- data.table("from"=m_freq$from, "to"=m_freq$to, "freq"=m_freq$freq)
write.csv(k_freq, "k_freq.csv")
write.csv(m_freq, "m_freq.csv")




### temp and precip scatterplot
weather <- weather[-366]
trip_weather_reduced <- data.table("date"=weather$Date, "temp"=weather$Mean_Temperature_F, "prec"=weather$Precipitation_In, "count_member"=trip_count_by_day_member$freq, "count_short"=trip_count_by_day_short$freq)

write.csv(trip_weather_reduced, "trip_weather_reduced.csv")

