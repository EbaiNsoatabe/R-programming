clean <- function(){

WineUnclean <- read.csv("C:\\Users\\Admin\\R programmes/wine.csv", stringsAsFactors = FALSE)

slash <- "/"
after = "/.+"
between <- "/.+/"
before <- ".+/"

vandr <- WineUnclean$variety_and_region

first <- regmatches(vandr, gregexpr(after, vandr))
WineUnclean$Region <- first 

second <- regmatches(WineUnclean$Region, regexpr(between, WineUnclean$Region))
WineUnclean$Location <- second

third <- regmatches(vandr, gregexpr(before, vandr))
type <- gsub(between, "", third)
WineUnclean$Variety <- type

area <- gsub(between, "", WineUnclean$Region)
WineUnclean$Region <- area

subregion <- gsub(slash, "", WineUnclean$Location)
WineUnclean$Location <- subregion

WineUnclean$variety_and_region <- NULL

return(WineUnclean)
}

WineClean <- clean()