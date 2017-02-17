# Wustl: Statistical Computing
# Problem Set 3
# Author: Jonas Markgraf
# Date: Feb 16, 2017
###############################

# clear environment
rm(list=ls())

# 1) Define new S3 class "door". Values can take on numeric values 1, 2, or 3. -------------

door <- function(x) {
  # Function Name: door()
  # Purpose: create numeric objects of class 'door'
  # Function:
  ## 1) does not allow non-numeric arguments
  ## 2) does only allow arguments that are 1, 2, or 3
  ## 3) assigns class 'door' to argument
  # Arguments:
  ## x: must be numeric, and either 1, 2, or 3.
  # Author: Jonas Markgraf
  if(!is.numeric(x)) stop("X must be numeric")
  if(x != 1 & x != 2 & x != 3) stop("X must be 1, 2, or 3")
  structure(list(x), class = "door")
}

# Examples:
door2 <- door(2)
class(door(2))

# examples that do not work
door(4)  # value > 3
door(2.5)  # value not 1, 2, or 3
door("Test")  # non-numeric

# 2) Create method "PlayGame" ------------------

PlayGame <- function(choice) {
  # Method name: PlayGame()
  # Purpose: compare vector of class 'door', created with 'door' function with lottery value
  # Function:
  ## 1) extracts numeric value from input list and assigns to 'choice_val'
  ## 2) draws random number (1,2, or 3) and assigns to 'lottery'
  ## 3) compares values and prints congratulations or expresses sympathies with loser
  # Arg:
  ## choice: object of class 'door'; can be created with 'door()' function
  # Author: Jonas Markgraf
  choice_val <- choice[1]
  lottery <- sample(1:3, 1)
  ifelse(lottery == choice_val, "You won, congrats!", "This was the wrong number, next time!")
}

# Examples:
PlayGame(door2)

# 3) Create S4 method "PlayGame" -------------------

# setting S4 class "Door"
setClass(Class="Door",
         # purpose: formally setting new S4 class "Door"
         # objects of the S4 class "Door" must be numeric
         slots =
           c(x = "numeric")
         )

# validating that object meets defined requirements
setValidity("Door", function(object){
  # Purpose: need to guarantee that input arguments for S4 method "PlayGame" meet requirements
  ## test1: element must be 1, 2, or 3
  ## test2: element must be of length 1
  test1 <- object@x %in% c(1:3)
  test2 <- length(object@x) == 1
  if(!test1 | !test2){return("x is not a valid value")}
} )

# Examples:
Player1 <- new("Door", x = 2)
Player2 <- new("Door", x = 1)

# examples that do not work:
new("Door", x = 4)  # value numeric, but not 1, 2, or 3
new("Door", x = "Test")  # value not numeric

# create "PlayGame" method for S4 ---------------

## set new generic "PlayGame"
setGeneric("PlayGame",
           # Purpose: no generic with this name exists, so we need to set it
           function(x)  {
             standardGeneric("PlayGame")
           })

# set method "PlayGame"
setMethod("PlayGame", signature = "Door",  # elements must be of S4 class "Door"
          function(x){
            # Method Name: PlayGame()
            # Purpose: compare vector of class 'door', created with 'door' function with lottery value
            # Function:
            ## 1) extracts numeric value from input list and assigns to 'choice_val'
            ## 2) draws random number (1, 2, or 3) and assigns to 'lottery'
            ## 3) compares values and prints congratulations or expresses sympathies with loser
            # Arg:
            ## x: object of S4 class 'Door'; needs to be formally created with "new("Door", val)", see above
            # Author: Jonas Markgraf 
            choice_val <- x@x
            lottery <- sample(1:3, 1)
            ifelse(lottery == choice_val, 
                   "You won, congrats!", "This was the wrong number, next time!")
          }
)

# Examples:
PlayGame(Player1)
PlayGame(Player2)
