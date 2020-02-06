#FInd Waldo by subsetting charnames, for example:
charnames <- list("Benedick", "Leonato", "Claudio", "Waldo")
charnames [[4]]

#Combining vectors
wom <- c("Beatrice", "Hero")
men <- c("John", "Waldo", "Pedro")
charnames2 <- list(wom,men)

charnames2[2][[1]][2]
charnames2[[2]][2]

#Lists of lists
morewom <- c("Innogen", "Ursula", wom)
moremen <- list("Antonio", men)
charnames3 <- list(morewom, moremen)

charnames3[[2]][[2]][2]


#Class starts in ernest
#Time to read in the play
adowords <- scan("MuchAdo-justwordsnopunct.txt", what="char")

adowords[1:10]
adowords[2878:2991]

#nchar(adowords[1:10]) returns the number of letters in each word
nchar(adowords[1:10])

#The Jockers way to scan
ado2 <- scan("MuchAdo-justwordsnopunct.txt", what = "char", sep = " ")
ado2 <- unlist(strsplit(ado2, " "))

#Does a word appear and where?
which(adowords=="parrot-teacher")


table(adowords)[1:15]
adowords <- tolower(adowords)
table(adowords)[1:15]
ado.t <- table(adowords)

ado.t <- sort(ado.t, decreasing = T)
ado.t[1:20]

#Basic Stylometry of Much ado about nothing
adochar.t <- table(nchar(adowords))
plot(adochar.t, type = "b")



#New text MillSample

mill <- scan("MillSample.txt", what = "char")

#Time to remove punctuation
mill <- paste(mill, collapse=" ")
millsplit <- strsplit(mill, "\\W") #\\W means anything that is not a letter character
millsplit <- unlist(millsplit)
which(millsplit=="") #These are blanks
blanks <- which(millsplit == "")
mill <- millsplit[-blanks]
