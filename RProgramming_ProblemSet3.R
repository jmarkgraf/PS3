# Wustl: Statistical Computing
# Problem Set 3
# Author: Jonas Markgraf
# Date: Feb 16, 2017
###############################

# clear environment
rm(list=ls())

# 1) Define new S3 class "door". Values can take on numeric values 1, 2, or 3. -------------

door <- function(x) {
  if(!is.numeric(x)) stop("X must be numeric")
  if(x != 1 & x != 2 & x != 3) stop("X must be 1, 2, or 3")
  structure(list(x), class = "door")
}

# Examples:
door2 <- door(2)
class(door(2))
door(4)  # as expected: the following functions don't work
door(2.5)  
door("Test")
class(door(2))

# 2) Create method "PlayGame" ------------------

PlayGame <- function(choice) {
  choice_val <- choice[1]
  lottery <- sample(1:3, 1)
  ifelse(lottery == choice_val, "You won, congrats!", "This was the wrong number, next time!")
}

# Examples:
PlayGame(door2)

# 3) Create S4 method "PlayGame" -------------------

setClass(Class="Door",
         slots =
           c(x = "numeric")
         )

# controlling if input meets requirements
setValidity("Door", function(object){
  test1 <- object@x %in% c(1:3)
  test2 <- length(object@x) == 1
  if(!test1 | !test2){return("x is not a valid value")}
} )

# Examples:
DOOR2 <- new("Door", x = 2)
new("Door", x = 4)
new("Door", x = "Test")

# create "PlayGame" method for S4
## set generic
setGeneric("PlayGame",
           function(x)  {
             standardGeneric("PlayGame")
           })

# set method
setMethod("PlayGame", signature = "Door",
          function(x){
            choice_val <- x@x
            lottery <- sample(1:3, 1)
            ifelse(lottery == choice_val, 
                   "You won, congrats!", "This was the wrong number, next time!")
          }
)

PlayGame(DOOR2)
