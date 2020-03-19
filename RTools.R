
scanner<-function(filename){
  library(stringr)
  lines<-scan(filename,what="char",sep="\n")
  words<-tolower(unlist(str_split(lines,boundary("word"))))
  bagofwords<-words[which(words!="")]
  return(bagofwords) # This is an explicit return statement
}

