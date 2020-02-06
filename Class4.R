#Class 4
# Required Texts: 
# Mills.txt
# MuchAdo-nostagedirections.txt

# This is a commenting test Shift+CTRL+C

mill <- scan("MillSample.txt", what="char")

#These words still have some punctuation in their midst

strsplit("asdfasdf", split = "") #Splits each character

strsplit("y'know, this is a text", split="\\W") #Splits into words, removes some punctuation

mill.l <- strsplit(mill, split = "\\W")
mill <- unlist(mill.l)

#Still have wierd stuff such as lone "s" which should probably be removed
mill[-176] #Remove lone s
mill[175] <- "woman's"#manually add it back to the word

nchar(mill)
mill.wls <- nchar(mill)

mean(mill.wls) #Gives average word length

mill.t <- table(mill.wls)
barplot(mill.t, main="Word-Lengths in Mill", xlab = "Number of Letters", ylab = "Counts", ) #Creates a bar graph
#Press tab to get suggestions as to what other arguements we can add to the function
plot(adochar.t, type = "b")

median(mill.wls) #Gives median value


#Pattern Matching grep time
#Grep is based on regex
grep("x", letters)

grep("x", letters, value = T)

grep("woman", mill) #search for women in mill


#Work with much ado no stage directions
adolines <- scan("MuchAdo-nostagedirections.txt", what="character", sep="\n")
adolines[162:164] #Major Beatrice entry

grep("BENEDICK", adolines)
bene.index <- grep("BENEDICK", adolines)
beat.index <- grep("BEATRICE", adolines)

adolines[beat.index+1] #Returns lines after BEATRICE marks

ado.empty <- rep(NA, length(adolines)) #Creates an empty vector
class(NA) #Logical

#This vector can then be used
beat.there <- ado.empty
bene.there <- ado.empty

bene.there[bene.index] <- 1 #This represents all of the locations where benedick speaks as the number 1 in an otherwise empty array
beat.there[beat.index] <- 1

plot(bene.there, type="h", ylim = c(-.2, .2), col="blue", yaxt="n", ylab = "Character Speaks", xlab = "Line Number")
lines(beat.there, type="h", ylim = c(-.2, .2 ), col="red")

actpositions <- grep("ACT [1-5]", adolines)

#Getting started with Loops
#Will be continued next week

for(i in 1:10){
  print(i)
}

for(i in 1:length(actpositions)){
  print(actpositions[i])
}


#End ClassS