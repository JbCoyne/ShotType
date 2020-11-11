ShotType <- function(x,y) {
  locx <- x
  locy <- y
  
  ### For invalid coordinates
  ifelse((locx>1128 || locx<0 || locy>600 || locy<0), Location <- "ERROR, Please Enter a valid court coordinate (0 <= x <= 1128) and (0 <= y <= 600)",
  
  ### Corner Threes       
  ifelse((locx<=168 & locy < 36), Location <- "Corner 3", 
  ifelse((locy>564 & locx<=168), Location <- "Corner 3",
  ifelse((locx>=960 & locy < 36), Location <- "Corner 3",
  ifelse((locx>=960 & locy >564), Location <- "Corner 3",  
  
  ### Non-Corner Threes
  ifelse((locx>168 & locx<=564 & 285 < sqrt(((locx-63)*(locx-63))+((locy-300)*(locy-300)))), Location <- "Above the Break 3",
  ifelse((locx>564 & locx<960 & 285 < sqrt(((locx-1065)*(locx-1065))+((locy-300)*(locy-300)))), Location <- "Above the Break 3",
  
  ### Restricted Area
  ifelse((locx>=1065 & locx<=1080 & locy>=252 & locy<=348), Location <- "Restricted Area",
  ifelse((locx>=1017 & locx<1065 & 48 >= sqrt(((locx-1065)*(locx-1065))+((locy - 300)*(locy-300)))), Location <- "Restricted Area",
  ifelse((locx>=48 & locx<=63 & locy>=252 & locy<=348), Location <- "Restricted Area",
  ifelse((locx>63 & locx<=111 & 48 >= sqrt(((locx-63)*(locx-63))+((locy - 300)*(locy-300)))), Location <- "Restricted Area",
  
  ### Non-Restricted Area Paint
  ifelse((locx<=228 & locy>=228 & locy<=252), Location <- "Non-RA Paint",
  ifelse((locx<=252 & locy>=348 & locy<=372), Location <- "Non-RA Paint",       
  ifelse((locx<48 & locy>252 & locy<348), Location <- "Non-RA Paint",
  ifelse((locx>=63 & locx<=228 & locy>228 & locy<348 & 48 < sqrt(((locx-63)*(locx-63))+((locy-300)*(locy-300)))), Location <- "Non-RA Paint",
  ifelse((locx>=900 & locy>=228 & locy<=252), Location <- "Non-RA Paint",
  ifelse((locx<=228 & locy>=348 & locy<=372), Location <- "Non-RA Paint",       
  ifelse((locx>1080 & locy>252 & locy<348), Location <- "Non-RA Paint",
  ifelse((locx<=1065 & locx>=900 & locy>228 & locy<348 & 48 < sqrt(((locx-1065)*(locx-1065))+((locy-300)*(locy-300)))), Location <- "Non-RA Paint",
  
  ### Non-Paint 2's
  ifelse((locx<=168 & locy>=36 & locy<228), Location <- "Non-Paint 2",
  ifelse((locx<=168 & locy>=372 & locy<=564), Location <- "Non-Paint 2",
  ifelse((locx>168 & locx<=348 & 285 >= sqrt(((locx-63)*(locx-63))+((locy-300)*(locy-300)))), Location <- "Non-Paint 2",
  ifelse((locx>=960 & locy>=36 & locy<228), Location <- "Non-Paint 2",
  ifelse((locx>=960 & locy>=372 & locy<=564), Location <- "Non-Paint 2",
  ifelse((locx>=780 & locx<960 & 285 >= sqrt(((locx-1065)*(locx-1065))+((locy-300)*(locy-300)))), Location <- "Non-Paint 2",
  
  Location <- "Unknown")))))))))))))))))))))))))
print(Location, quote = FALSE)

  ### Uncomment this code if you want function to return shot type in a data frame with coordinates, and comment line above this comment
  
#new.frame <- cbind(x,y,Location)
#new.frame
}