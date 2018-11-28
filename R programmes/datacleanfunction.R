clean <- function(table){

  Unclean <- table
  
  slash <- "/"
  after = "/.+"
  between <- "/.+/"
  before <- ".+/"
  
  vandr <- Unclean$variety_and_region
  
  first <- regmatches(vandr, gregexpr(before, vandr))
  type <- gsub(between, "", first)
  Unclean$Variety <- type
  
  second <- regmatches(vandr, gregexpr(after, vandr))
  Unclean$Region <- second
  
  third <- regmatches(Unclean$Region, regexpr(between, Unclean$Region))
  Unclean$Location <- third
  
  area <- gsub(between, "", Unclean$Region)
  Unclean$Region <- area
  
  subregion <- gsub(slash, "", Unclean$Location)
  Unclean$Location <- subregion
  
  Unclean$variety_and_region <- NULL
  
  return(Unclean)
}

WineUnclean <- read.csv("C:\\Users\\Admin\\R programmes/wine.csv", stringsAsFactors = FALSE)
WineClean <- clean(WineUnclean)