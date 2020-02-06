# Class 6

# We start the class by looking at Visualization of MFWs from last weeks homework.
# Making shapes around words and trying to get insights.

# We want to see all of the words said by specific characters.
# What ever a character says can be terminated by either another character or by an act marker.

# Most frequent words by characters.

adolines <- scan("MuchAdo-nostagedirections.txt", what="char", sep="\n")
adolines <- gsub("^Scene[1-7]", "", adolines) # Use gsub to remove Scene markers
adolines <- gsub("=+", "", adolines) # Removes equal signs

adolines <- append(adolines, "END")
tail(adolines)

# Captial letter followed by a capital letter ^ start of line, + until end of line
speechmarkers <- grep("^[A-Z][A-Z]+", adolines) # This gets character names as well as ACT and END

ben.index <- grep("BENEDICK", adolines) # Gets where Benedick talks

# This is where bendick speaks
which(speechmarkers %in% ben.index) # Tells you if it ever happens.

beninspeech <- which(speechmarkers %in% ben.index) #The locations of where benedick speaks

adolines[ben.index[1]+1] # First line after BENEDICK character marker
speechmarkers[beninspeech]
speechmarkers
adolines[149]

ben.start <- ben.index[1]+1
ben.end <- speechmarkers[beninspeech[1]+1]-1
adolines[ben.start:ben.end]

ben.start <- ben.index[3]+1
ben.end <- speechmarkers[beninspeech[3]+1]-1
adolines[ben.start:ben.end]


benlines <- NULL
#length(beninspeech should be 137)
for (i in 1:length(beninspeech)){
  ben.start <- ben.index[i]+1
  ben.end <- speechmarkers[beninspeech[i]+1]-1
  benlines <- c(benlines, adolines[ben.start:ben.end])
}


which(benlines=="")
benlines <- benlines[-which(benlines=="")]

# Now we look at script

# Opensouceshakespear.org

# Saving to objects to file.

# saveRDS(speakerraw.l,"ado-speakerrawlist.RDS")
# rm(speakerraw.l)
# speakerraw.l<-readRDS("ado-speakerrawlist.RDS")

speakerraw.l[["benedick"]][1:15] # This gives a table
speakerraw.l[["beatrice"]][1:15]

setdiff(names(speakerraw.l[["benedick"]][1:15]), names(speakerraw.l[["beatrice"]][1:15]))

setdiff(names(speakerraw.l[["beatrice"]][1:15]), names(speakerraw.l[["benedick"]][1:15]))

        