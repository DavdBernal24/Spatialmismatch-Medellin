### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


### Purpose : This code creates a function for the Accessibility maps


data_to_map <- function(ruta, filename, shape_data, var, palette, decimals,
                        shape_vias, shape_lineas_2012, shape_lineas_2017, shape_estaciones, breaks) {
  
  ## creating intervals
  #print("Computing breaks")
  brks <- classIntervals(breaks, n=9, style="quantile", dataPrecision = 2)
  
  ## color palette to fill the map
  colors <- c(brewer.pal(9, palette))
  
  ## creating map and saving in pdf
  print("Plotting data")
  pdf(file = paste(ruta,filename, sep = ""))
  par(mar = c(0, 0, 0, 0))
  plot(shape_data,
       col=colors[findInterval(var, brks$brks,
                               all.inside=TRUE)], axes=F)
  title(paste (""))
  addnortharrow(pos='topleft', scale = 0.5, padin = c(1.5, 0.3))
  addscalebar(widthhint = 0.2,unitcategory = "metric", padin = c(1.5, 0.3))
  legend("bottomright", # location of legend
         legend = leglabs(round(brks$brks, decimals)), # categories or elements in legend
         fill = colors, # color palette
         bty = "n", # turn off the legend BORDER
         cex = 0.9, # change the font size
         x.intersp = 0.5,
         y.intersp = 0.7,
         inset = c(0.0,0.18)
  )
  plot(shape_vias, col='gray', lwd = 2, add = T)
  plot(shape_lineas_2012, col='blue', lwd = 3, add = T)
  plot(shape_lineas_2017, col='purple', lwd = 3, add = T)
  plot(shape_estaciones, pch = 16, add = T)
  legend(x = "bottomright", 
         legend = c("Metro lines", "New Metro lines","Metro stations","Main roads"),
         col = c("blue", "purple", "black","gray"), 
         lwd = 3, 
         cex = 0.7,
         lty = c(1,1,NA,1),
         pch = c(NA,NA,16,NA),
         inset = c(0.02,0.09))
  dev.off()
  
  print("Map ready!!!!!")
}




## This is the same function, I just want to change the order of the colors 

data_to_map_reverse = function(ruta, filename, shape_data, var, palette, decimals,
                               shape_vias, shape_lineas_2012, shape_lineas_2017, shape_estaciones, breaks) {
  
  ## creating intervals
  #print("Computing breaks")
  brks <- classIntervals(breaks, n=9, style="quantile", dataPrecision = 2)
  
  ## color palette to fill the map
  colors <- c(rev(brewer.pal(9, palette)))
  
  
  ## creating map and saving in pdf
  print("Plotting data")
  pdf(file = paste(ruta,filename, sep = ""))
  par(mar = c(0, 0, 0, 0))
  plot(shape_data,
       col=colors[findInterval(var, brks$brks,
                               all.inside=TRUE)], axes=F)
  title(paste (""))
  addnortharrow(pos='topleft', scale = 0.5, padin = c(1.5, 0.3))
  addscalebar(widthhint = 0.2,unitcategory = "metric", padin = c(1.5, 0.3))
  legend("bottomright", # location of legend
         legend = leglabs(round(brks$brks, decimals)), # categories or elements in legend
         fill = colors, # color palette
         bty = "n", # turn off the legend BORDER
         cex = 0.9, # change the font size
         x.intersp = 0.5,
         y.intersp = 0.7,
         inset = c(0.0,0.18)
  )
  plot(shape_vias, col='gray', lwd = 2, add = T)
  plot(shape_lineas_2012, col='blue', lwd = 3, add = T)
  plot(shape_lineas_2017, col='purple', lwd = 3, add = T)
  plot(shape_estaciones, pch = 16, add = T)
  legend(x = "bottomright", 
         legend = c("Metro lines", "New Metro lines","Metro stations","Main roads"),
         col = c("blue", "purple", "black","gray"), 
         lwd = 3, 
         cex = 0.7,
         lty = c(1,1,NA,1),
         pch = c(NA,NA,16,NA),
         inset = c(0.02,0.09))
  dev.off()
  
  
  #print("Map ready!!!!!")
}