
### read in household articles, cars and body part data ###

#setwd("~/02_Projekte/03_FWD/Show-Case/FWD_test")

d.household <- read.csv("data/householdarticles.csv",header = TRUE, sep = ",")
d.cars <- read.csv("data/cars.csv",header = TRUE, sep = ",")
d.bodypart <- read.csv("data/bodyparts.csv",header = TRUE, sep = ",")

household <- as.character(d.household$household)
cars <- as.character(d.cars$car.names)
bodypart <- as.character(d.bodypart$bodypart)

# 20 signal words
signal.household <- c("household", "plant", "lock" ,"vandalism", "storm","hail", "burglary", "robbery", "fire", "water", "flood", "earthquake", "living", "dining", "kitchen",  "room", "hallway", "attic", "basement", "home",  "stole", "laptop", "pc" )
signal.car <- c("car", "tire", "tyre", "wheel", "front", "back", "parked", "light", "parking", "lot", "driving", "mirror", "crash", "SUV", "van", "cruiser", "auto", "limousine", "garage", "axles")
signal.bodypart <- c("health", "medicine", "body", "sprain", "sport", "brain", "injury", "trauma", "violation", "ill", "hospital", "medical", "ambulance","evacuation", "accident", "domestic", "surgery", "doctor", "therapy", "care")

household <- append(household,signal.household)
car <- append(cars, signal.car)
health <- append(bodypart, signal.bodypart)


library(tm)

# transform to lower case
household <- tolower(household)
car <- tolower(car)
health <- tolower(health)

# remove punctuation (.-)
household <- removePunctuation(household)
car <- removePunctuation(car)
health <- removePunctuation(health)

#Stem words
household <- tm::stemDocument(household)
health <- tm::stemDocument(health)

health <- append(health, c("injury", "therapy", "body"))


#for tetecting repeats in data
repeat.fct <- function(data){
    
    # if everywhere 1: good
    # if a 2 or higher the expression is repeated
    count <- numeric(length(data))
    for (i in 1:length(data)){
        for ( j in 1:length(data)){
            
            if (data[i] == data[j]){
                count[i] <- count[i] +1
            }
        }
    }
    return(count)
}

car <- car[-279] # motors
car <- car[-267] # motors

household <- household[-46] # iron
household <- household[-30] # bed




