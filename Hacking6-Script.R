#Hacking, Class 6 - 2/5/2020
# A Script

# setwd("~/projects/hack/muchado")

adolines<-scan("MuchAdo-nostagedirections.txt",what="char",sep="\n")
adolines[1:10]
adolines<-append(adolines,"END")
actpositions<-grep("^ACT [1-5]",adolines)
actpositions<-c(actpositions,length(adolines))

# Clean up?
adolines.clean<-gsub("=+","",adolines) # Remove text decorations
adolines.clean<-gsub("Scene [0-9]+","", adolines.clean) # Remove scene markers
adolines.clean<-gsub("[A-Z][A-Z]+","",adolines.clean) # Remove act markers and character prompts

actfreqraw.l<-list() #initialize a list
for(i in 1:5){
  act<-paste(c("act",i),collapse="")
  start<-actpositions[i]+1
  end<-actpositions[i+1]-1
  chunkoflines<-adolines.clean[start:end]
  bloboflines<-paste(chunkoflines,collapse=" ")
  actwords<-tolower(unlist(strsplit(bloboflines,"\\W")))
  actwords<-actwords[which(actwords != "")] #zaps strsplit ""s
  actfreq.t<-table(actwords)
  actfreqraw.l[[act]]<-actfreq.t
}

#Finding speeches
speechmarkers<-grep('^[A-Z][A-Z]+',adolines) # character prompts, note, also grabs "ACT" markers and "END"

ben.index<-grep("BENEDICK", adolines)
beat.index<-grep("BEATRICE", adolines) 
claud.index<-grep("CLAUDIO", adolines)
hero.index<-grep("HERO",adolines)
dogb.index<-grep("DOGBERRY",adolines)
donj.index<-grep("DON JOHN", adolines)
char.indices.l<-list(ben.index, beat.index, 
                    claud.index, hero.index, dogb.index,donj.index)

#Set speaker names
speakers<-c("benedick","beatrice","claudio","hero","dogberry","donjohn")

# Initialize List
speakerraw.l<-list()

# Loop de Loop
for(i in 1:6){
  speaklines<-NULL 
  speaker<-speakers[i]
  speakinchar<-which(speechmarkers %in% char.indices.l[[i]])
  speechstart<-char.indices.l[[i]]+1 #first lines of speeches
  speechend<-speechmarkers[speakinchar+1]-1 #last lines of speeches
  
  for(j in 1:length(speakinchar)) { #inner loop inside of the outer loop
    start<-speechstart[j]
    end<-speechend[j]
    speaklines<-c(speaklines,adolines[start:end])
  }
  
  print(paste(speaker,length(speaklines))) # report number of lines per speaker (includes ""s)
  speechlowered<-tolower(paste(speaklines,collapse=" "))
  speakerwords<-unlist(strsplit(speechlowered,"\\W"))
  speakerwords<-speakerwords[which(speakerwords!="")]
  speakfreqs.t<-table(speakerwords)
  speakfreqs.t<-sort(speakfreqs.t,decreasing=T)
  speakerraw.l[[speaker]]<-speakfreqs.t
}

speakerraw.l[[1]][1:10] # Gets back benedicks top 10 words


### Saving objects -----

#Save and recover data
saveRDS(speakerraw.l,"ado-speakerrawlist.RDS")
rm(speakerraw.l)
speakerraw.l<-readRDS("ado-speakerrawlist.RDS")

### More fun with setdiff and intersect-----

# Set Differences
setdiff(names(speakerraw.l[[1]][1:30]),names(speakerraw.l[[2]][1:30]))
setdiff(names(speakerraw.l[[2]][1:30]),names(speakerraw.l[[1]][1:30]))
setdiff(names(speakerraw.l[[3]][1:30]),names(speakerraw.l[[2]][1:30]))
setdiff(names(speakerraw.l[[2]][1:30]),names(speakerraw.l[[3]][1:30]))

# Intersections
intersect(names(speakerraw.l[[1]][1:30]),names(speakerraw.l[[2]][1:30]))

# Try also (more about this on Thursday)
speakers30.l<-lapply(speakerraw.l,'[',1:30)
speakwords30.l<-lapply(speakers30.l,names)
library("gplots")
venn(speakwords30.l[1:5])

### Stopwords ---

# Stopwords?
stopwords<-scan("stopwords.txt",what="char",sep="\n")

# Print out a list of frequent words for each speaker (subtracting out stopwords)
for(i in 1:length(speakers)){
  print(speakers[i])
  print(setdiff(names(speakerraw.l[[i]][1:100]),stopwords))
}
