# Lat and longitude 
library(httr)
library(dplyr)
library(stringr)
library(leaflet)
library(ggplot2)

locations <- read.csv("stJoeLand.csv", stringsAsFactors = FALSE)
locations <- locations %>% 
  mutate(ID = rownames(locations)) 

google_Key <- "#######"

baseurl <- "https://maps.googleapis.com/maps/api/geocode/json?address="

locations <- locations %>% 
  mutate(address = gsub("[ ]{2,}", " ", address),
         city =gsub("[ ]{2,}", " ", city),
         state = gsub("[ ]{2,}", " ", state),
         ZIP_code = gsub("[ ]{2,}", " ", ZIP_code)) %>% 
  mutate(address_g = gsub(" ", "%20", address),
         city_g = gsub(" ", "%20", city),
         search_g = paste(baseurl, address_g, "%20",city_g, "%20", state, "&key=", google_Key, sep = ""))

get_addresses <- function(search) {
  out <- tryCatch(
    {
      Sys.sleep(1)
      initial_result <- GET(search)
      info <- jsonlite::fromJSON(content(initial_result, as = "text"))[[1]]
      final <- data.frame(Full_Address = info$formatted_address,
                          Latitude = info$geometry$location$lat,
                          Longitude = info$geometry$location$lng)
    }, error = function(e) {
      search
    }
  )
  return(out)
}

result <- lapply(locations$search_g[1:10], get_addresses)
result2 <- bind_rows(result)
result2 <- result2 %>% 
  mutate(ID = rownames(.)) 

full_data <- merge(locations, result2, by = "ID", all = FALSE)

# Color Bins

quantileNum <- 5
probs <- seq(0, 1, length.out = quantileNum + 1)
bins <- quantile(full_data$sale_price, probs, na.rm = TRUE, names = FALSE)
while (length(unique(bins)) != length(bins)) {
  quantileNum <- quantileNum - 1
  probs <- seq(0, 1, length.out = quantileNum + 1)
  bins <- quantile(full_data$sale_price, probs, na.rm = TRUE, names = FALSE)
}

pal <- colorBin("YlGn", bins = bins)
# LEAFLEET 

map <- leaflet(full_data) %>% 
  setView(lng = -86.2520, lat = 41.6764, zoom = 10) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(lng=~Longitude, lat=~Latitude,
             popup = paste("Address: ",
                           full_data$Full_Address,
                           "<br> Latitude: ",
                           full_data$Latitude, 
                           "<br> Longitude: ",
                           full_data$Longitude,
                           "<br> Price: $",
                           full_data$sale_price,
                           "<br> School District: ",
                           full_data$school_district, 
                           "<br> Acreage: ",
                           full_data$acreage, 
                           "<br> Year Built: ",
                           full_data$year_built, 
                           "<br> Floors: ",
                           full_data$floors, 
                           "<br> Bedrooms: ",
                           full_data$bedrooms, 
                           "<br> Bathrooms: ",
                           full_data$bathrooms,
                           sep = "")) %>% 
  addCircles(lng=~Longitude, lat=~Latitude,
            weight = 15,
            fillOpacity = 5,
            color = pal(full_data$sale_price),
            radius = 100) %>% 
  addLegend("bottomright", pal = pal, title = "Sale Price", values = full_data$sale_price)
map

# Hospital 

# Memorial Hospital: 615 North Michigan Street South Bend, IN 46601
# St. Joseph County Sheriff's office: 401 W Sample Street South Bend, IN 46601
basemapurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="

hospital <- gsub(" ", "%20", "615 North Michigan Street South Bend IN 46601")
sheriff <- gsub(" ", "%20", "401 W Sample Street South Bend IN 46601")
full_data <- full_data %>% 
  mutate(hospital_search = paste(basemapurl, 
                                 address_g, 
                                 "%20", 
                                 city_g, 
                                 "%20", 
                                 state, 
                                 "&destination=",
                                 hospital, 
                                 "&key=", 
                                 google_Key,
                                 sep = ""),
         sheriff_search = paste(basemapurl, 
                                address_g, 
                                "%20", 
                                city_g, 
                                "%20", 
                                state, 
                                "&destination=",
                                sheriff, 
                                "&key=", 
                                google_Key,
                                sep = ""))

get_distances <- function(search1, search2) {
  out <- tryCatch(
    {
      Sys.sleep(1)
      initial_result <- GET(search1)
      info <- jsonlite::fromJSON(content(initial_result, as = "text"))
      distance_h <- info[["routes"]][["legs"]][[1]][["distance"]][["text"]]
      Sys.sleep(.5)
      initial_result2 <- GET(search2)
      info2 <- jsonlite::fromJSON(content(initial_result2, as = "text"))
      distance_s <- info2[["routes"]][["legs"]][[1]][["distance"]][["text"]]
      
      final <- data.frame(Hospital_Distance = as.character(distance_h),
                          Sheriff_Distance = as.character(distance_s))
      scraperesult <- list(final)
      return(scraperesult)
    }, error = function(e) {
      search
    }
  )
  return(out)
}

result_d <- mapply(get_distances, full_data$hospital_search, full_data$sheriff_search)
result_d2 <- bind_rows(result_d)
result_d2 <- result_d2 %>% 
  mutate(ID = rownames(.),
         Hospital_Distance_Num = as.numeric(str_replace(as.character(Hospital_Distance), " mi", "")),
         Sheriff_Distance_Num = as.numeric(str_replace(as.character(Sheriff_Distance), " mi", ""))) 

full_data <- merge(full_data, result_d2, by = "ID", all = FALSE) %>% 
  select(-ID, -address_g, -city_g, -search_g, -hospital_search, -sheriff_search)

hospital <- lm(sale_price ~ Hospital_Distance_Num, data = full_data)
summary(hospital)

ggplot(full_data, aes(x = Hospital_Distance_Num, y = sale_price)) + 
  geom_point() + theme_classic() + ggtitle("Sale Price vs. Distance to Hospital") + geom_smooth(method = "lm")

sheriff <- lm(sale_price ~ Sheriff_Distance_Num, data = full_data)
summary(sheriff)

ggplot(full_data, aes(x = Sheriff_Distance_Num, y = sale_price)) + 
  geom_point() + theme_classic() + ggtitle("Sale Price vs. Distance to Sheriff's Office") + geom_smooth(method = "lm")
