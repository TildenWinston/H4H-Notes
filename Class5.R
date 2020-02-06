# Class 5

adowords <- scan("MuchAdo-justwordsnopunct.txt", what="char")
table(adowords) #best way to count words

table(tolower(adowords)) #Flattens capitalization

head(sort(table(tolower(adowords)), decreasing=T))
# i and the you   a  to 
# 689 588 571 492 473 424 

# These words are the most common in the language, but the frequencies of them is important and they are what stylometers care about

head(sort(table(tolower(adowords)), decreasing=T), 15)

# Comedy is a ply that ends in marriage
# Comedy is a play that avoids we for four acts "Comedy, computing, & Shakespeare: the Folger's Muchael Witmore in INT's ENLIGHTENMENT MINUTES

# Example from Henry the fourth

court <- c("KING", "PRINCE", "wESTMORELAND")
tavern <- c("PRINCE", "FALSTAFF", "POINS")

# %in% finds overlap of vectors
court %in% tavern # FALSE  TRUE FALSE
which(tavern %in% court)

#intersect works similarly, returns the value
intersect(court, tavern) #PRINCE

#Oposite
setdiff(court, tavern) # "KING"         "wESTMORELAND"
setdiff(tavern, court) # "FALSTAFF" "POINS" 

adolines <- scan("MuchAdo-nostagedirections.txt", what="char", sep="\n")
head(adolines)
length(adolines)

grep("^ACT [1-5]", adolines) #^ means start at the beginning of the line
actpositions <- grep("^ACT [1-5]", adolines) # Gives the positions of where ACT # is located

# Finds text between ACT and ENDs  (?:ACT [1-5])(.*?)(?=(ACT [1-5]|END))

adolines <- c(adolines, "END")
actpositions <- c(actpositions, length(adolines))

actfreqraw.l <- list() # We have to initialize the variable

#for(*Iteration set up here*){*What is actually done each loop*} # Basic look structure

for(i in 1:5){
  # actpositions is the vector of the position of act
  act <- adolines[actpositions[i]] #when i = 1, act = "ACT 1"
  start <- actpositions[i]+1 # One line after the act division 
  end <- actpositions[i + 1] - 1 # Looks at the next act and takes a look a the line before the division
  chunkolines <- adolines[start:end] # Text between the acts
  blob <- paste(chunkolines, collapse=" ")
  actwords <- tolower(unlist(strsplit(blob, "\\W")))
  blanks <- which(actwords=="") # Look for empty strings
  actwords <- actwords[-blanks] # Remove all of the empty strings
  actfreq.t <- table(actwords)
  actfreqraw.l[[act]] <- actfreq.t # List of tables of word frequency by act.
}

actfreqraw.l[[1]] # Returns table from act 1

#End class
