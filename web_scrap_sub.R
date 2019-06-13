library(XML); library("rvest")
library("dplyr"); library("lubridate")
Sys.setlocale(category = "LC_ALL", locale = "C")

# Data Configuration
# City Url
uriCityWeather <-
  data.frame(
    city = c("Dublin","Cork","Shannon","carnmalin"),
    url = c(
      "http://www.weatherbase.com/weather/weatherhourly.php3?s=96930&cityname=Dublin-Dublin-Ireland&date=",
      "http://www.weatherbase.com/weather/weatherhourly.php3?s=55930&cityname=Cork-Cork-Ireland&date=",
      "http://www.weatherbase.com/weather/weatherhourly.php3?s=26930&cityname=Shannon-Shannon-Ireland&date=",
      "http://www.weatherbase.com/weather/weatherhourly.php3?s=39800&cityname=Carnmalin-Carnmalin-Ireland&date="))

str(url)
head(url)

#Plan for month of November & December
nbDay <- 61

listOfCityYear <- data.frame(
  year = c(
   
    "2018","2018","2018","2018"

    

  ),
  city = c(
    
    "Dublin","Cork","Shannon","carnmalin"

  )
)

str(listOfCityYear)
head(listOfCityYear)

# Dataframe for storing city weather
weather <- data.frame(matrix(nrow = 1, ncol = 15))
names(weather) =  c(
  "City",
  "date_obs",
  "date.time",
  "Local.Time",
  "Temperature",
  "Dewpoint",
  "Humidity",
  "Barometer",
  "Visibility",
  "Wind.Direction.code",
  "Wind.Speed",
  "Gust.Speed",
  "Precipitation",
  "Events",
  "Conditions"
)

weather$date.time <- as.POSIXct(weather$date.time)

head(weather)
str(weather)

for (j in 1:nrow(listOfCityYear)) {
  
  # Starting Date
  year <- listOfCityYear[j,]$year
  day <- paste(year,"-12-31", sep="")
  date <- strptime(day, "%Y-%m-%d")
  
  # LOOP nbDay days
  for (i in 1:nbDay) {
   
    day <- as.character(date)
    city <- listOfCityYear[j,]$city
    year <- listOfCityYear[j,]$year
    fileUrl <-paste(uriCityWeather[as.character(uriCityWeather$city) == city,]$url,day,"&units=metric",sep = "")
    
    
# Scraping script bellow

    DayWeather <- fileUrl %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="left-weather-content"]/table[3]') %>%
      html_table()

    # end of scraping script
 
    
    # Continue if table is not empty
    if (length(DayWeather) > 0) {
      DayWeather <- as.data.frame(DayWeather)
      names(DayWeather) =  c(
        "Local.Time",
        "Temperature",
        "Dewpoint",
        "Humidity",
        "Barometer",
        "Visibility",
        "Wind.Direction.code",
        "Wind.Speed",
        "Gust.Speed",
        "Precipitation",
        "Events",
        "Conditions"
      )
      
      
      # fill data for date and city
      DayWeather$date.time <- NA
      DayWeather$City <- city
      DayWeather$date_obs <- day
      
      DayWeather$date.time <-paste(DayWeather$date_obs,DayWeather$Local.Time)
      DayWeather$date.time <-strptime(DayWeather$date.time,format = "%Y-%m-%d %I:%M %p")
      
      # Bind with the past records
      weather <- rbind(weather, DayWeather)
      message(fileUrl)
    }
    # retract one day
    date <- date - 60 * 24 * 60
  }
}
str(weather)
head(weather)

# Humidity in %
weather$Humidity <- as.numeric(gsub(pattern = "(.*) %","\\1",weather$Humidity))
# Pressure in hPa
weather$Barometer <- as.numeric(gsub(pattern = "(.*) hPa","\\1",weather$Barometer))
# Visibility in km
weather$Visibility <-as.numeric(gsub(pattern = "(.*) km","\\1",weather$Visibility))
# Wind Speed
weather$Wind.Speed <- as.numeric( gsub(pattern = "(.*) km/h","\\1",weather$Wind.Speed))
# Gust speed
weather$Gust.Speed <- as.numeric( gsub(pattern = "(.*) km/h","\\1",weather$Gust.Speed))

weather_1 <- weather[!is.na(weather$City),]

str(weather_1)
head(weather_1)

