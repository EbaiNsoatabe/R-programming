mean_checker <- function(vec1, vec2){
  if(mean(vec1) > mean(vec2)){
    print("Quebec has higher average uptake")
  } else if(mean(vec2) > mean(vec1)){
    print("Mississippi has higher average uptake")
  } else{
    print("Equal Means")
  }
}
vec1 <- quebec_CO2$upt
vec2 <- mississippi_CO2$upt