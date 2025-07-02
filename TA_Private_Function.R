### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


### Purpose : This code creates a function for the private transport accessibility

compute_ta_priv <- function(sits_complete, travel_times, travel_distances, costs, emp,
                            fix_cost, variable_cost, adjustment,ipc,
                            varname, percap_varname, ruta, filename, year, distance) {
  
  ## -----------------------------------------------------
  ## FIXING DATA
  print("Fixing matrixes") 
  
  
  # Cleaning a little bit the SIT zones
  sits_complete <- sit_zones
  sits <- sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
  sits <- sits[!duplicated(sits@data[c("Name")]),] 
  
  sits_order <- data.frame(sits$Name,sits$matrix_order)
  colnames(sits_order) <- c("SIT","matrix_order")
  
  # Filtering the SIT zones for which I have employment
  sits_order <- sits_order[(sits_order$SIT %in% unique(emp$k)), ] 
  
  
  ## Filtering times
  colnames(travel_times) <- sit_zones$Name
  travel_times <- travel_times[c(sits_order$matrix_order)]
  travel_times$sit <- sit_zones$Name
  travel_times <- travel_times[c(sits_order$matrix_order),]
  

  
  print("Reshaping data wide to long") 
  ## Travel times from wide to long
  travel_times_long <- melt(travel_times)   
  colnames(travel_times_long) <- c('sito','sitd','time')
  
  
  ## Filtering distance matrix
  travel_distances <- travel_distances[c(sits_order$matrix_order)]
  travel_distances <- travel_distances[c(sits_order$matrix_order), ]
  colnames(travel_distances) <- sits_order$SIT
  travel_distances$sit <- sits_order$SIT
  
  
  # Distances from wide to long
  travel_dist_long <- melt(travel_distances)   
  colnames(travel_dist_long) <- c('sito','sitd','dist')
  
  
  print("Merging data sets") 
  ## Merging distances, costs and times
  colnames(costs) <- c("k","wmin")
  times_distances <- merge(travel_times_long, costs, by.x = 'sitd', by.y = 'k', all.x = T)
  times_distances <- merge(times_distances, travel_dist_long, 
                           by = c('sitd','sito'), all.x = T)
  
  print("Computing cost") 
  ## Assigning costs
  times_distances$transpcost = (fix_cost + (variable_cost * times_distances$dist))*adjustment
  
  ## Opportunity cost 
  times_distances$opcost = (((times_distances$time*times_distances$wmin) + times_distances$transpcost))*ipc
  
  ## merge with emploment
  times_distances = merge(times_distances, emp, by.x = 'sitd', by.y = 'k', all.x = T)
  
  print("Computing TA") 
  
  ## Accessibility measure
  times_distances$access = times_distances$emp/times_distances$opcost
  times_distances = times_distances[!is.infinite(times_distances$access),]
  
  print("Collapsing data") 
  
  ## Info at sit level
  ta_info = times_distances %>%
    group_by(sito) %>%
    summarise(ta = sum(access, na.rm = T))
  
  ta_info$ta_percap = ta_info$ta/nrow(ta_info)
  
  colnames(ta_info) = c('k',varname, percap_varname)
  
  write.csv(times_distances, row.names = FALSE, 
            file = paste0(ruta, paste0(varname, "_components_dist"), distance, "_", year, ".csv"))
  
  write.csv(ta_info, row.names = FALSE, file = paste0(ruta,filename))
  
  ###########Accessibility measure over time
  ## Accessibility measure just over time
  times_distances$access_time = times_distances$emp/(times_distances$time*times_distances$wmin)
  
  ta_time = times_distances %>%
    group_by(sito) %>%
    summarise(ta = sum(access_time, na.rm = T))
  
  
  ta_time$ta_percap = ta_time$ta/nrow(ta_time)
  
  colnames(ta_time) = c('k',varname, percap_varname)
  
  write.csv(ta_time, file = paste0(ruta, "TA_Private_OnlyTime_", year, ".csv"), row.names = FALSE)
  
  print("Process ready!!!!!") 
  return(ta_info)
}


