
scanner<-function(filename){
  library(stringr)
  lines<-scan(filename,what="char",sep="\n")
  words<-tolower(unlist(str_split(lines,boundary("word"))))
  bagofwords<-words[which(words!="")]
  return(bagofwords) # This is an explicit return statement
}


# From brad

scanner<-function(filename){
  library(stringr)
  lines<-scan(filename,what="char",sep="\n")
  words<-tolower(unlist(str_split(lines,boundary("word"))))
  bagofwords<-words[which(words!="")]
  return(bagofwords)
}

cosineDist <- function(x,y){1 - x%*%y/(sqrt(x%*%x * y%*%y))}

# Mark Algee-Hewitt's KWICker (tweaked and commented by Brad)
kwic<-function(target.kw, text.words, LRspan){
  #find places in text.words where target.kw can be found
  #target.index<-which(text.words==target.kw) #control for casing below
  target.index<-grep(tolower(target.kw),tolower(text.words))
  if(length(target.index)==0){return("No match")}
  #create empty variable
  target.kwics<-NULL
  #loop that repeats for all incidents of keyword in text
  for (i in 1:length(target.index)){
    #check to make sure, we're in bounds
    if (target.index[i]-LRspan<1){
      start<-1
    } else {
      start<-target.index[i]-LRspan
    }
    if (target.index[i]+LRspan>length(text.words)){
      end<-length(text.words)
    }else{
      end<-target.index[i]+LRspan
    }
    #grab chunk
    text.chunk<-text.words[start:end]
    #collapse chunk to a single variable, separated by spaces
    text.chunk<-paste(text.chunk, collapse=" ")
    #add chunk to list of chunks
    target.kwics<-c(target.kwics, text.chunk)
  }
  #return complete list of text chunks
  return(target.kwics)
}

#return collocates (fixed problem with indexing out of bounds)
colloc<-function(target.kw, text.words, LRspan){
  text<-tolower(text.words)
  target.index<-which(text==target.kw)
  sprintf("The word '%s' appears %s times in %s",target.kw,length(target.index),text.words)
  collocates<-NULL
  for(i in 1:length(target.index)){
    if(target.index[i]-LRspan<1){
      start<-1
    } else {
      start<-target.index[i]-LRspan}
    if(target.index[i]+LRspan>length(text.words)){
      end<-length(text.words)
    } else {
      end<-target.index[i]+LRspan 
    }
    Lcontext<-text.words[start:(target.index[i]-1)]
    Rcontext<-text.words[(target.index[i]+1):end]
    collocates<-c(collocates,Lcontext,Rcontext)
  }
  return(table((tolower(collocates))))
}
