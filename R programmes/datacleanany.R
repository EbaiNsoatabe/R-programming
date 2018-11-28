clean <- function(table, column){
  
  Unclean <- table
  col <- column
  
  slash <- "/"
  after = "/.+"
  between <- "/.+/"
  before <- ".+/"
  
  vandr <- col
  
  first <- regmatches(vandr, gregexpr(before, vandr))
  type <- gsub(between, "", first)
  Unclean$First <- type
  
  second <- regmatches(vandr, gregexpr(after, vandr))
  area <- gsub(between, "", second)
  Unclean$Second <- area
  
  third <- regmatches(second, regexpr(between, second))
  subregion <- gsub(slash, "", third)
  Unclean$Third <- subregion
  
  return(Unclean)
}

WineUnclean <- read.csv("C:\\Users\\Admin\\R programmes/wine.csv", stringsAsFactors = FALSE)
WineClean <- clean(WineUnclean, WineUnclean$variety_and_region)
WineClean$variety_and_region <- NULL