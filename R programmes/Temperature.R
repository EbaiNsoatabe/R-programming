heat <- function(temperature, z = TRUE){
  if (z == TRUE){
    if(temperature <= 100 & temperature >= 60){
      print("True")
    } else{
      print("False")
    }
  } else{
    if(temperature <= 90 & temperature >= 60){
      print("True")
    }  else{
      print("False")
    } 
  }
}
