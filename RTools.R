# R-tools
# Built in Hacking for Humanists
# Many of these tools are based on functions written by Matt Jockers or Mark Algee-Hewitt, two 
# computationally minded literary critics who have taught me much about R

#This function takes a single text and returns the text
#without punctuation or numbers or tabs or multiple spaces
text.clean<-function(text){
  #first, convert text to lower
  text.lower<-tolower(text)
  #remove tabs and tab-like spacings
  text.untabbed<-gsub("\\t+|\\s+", " ", text.lower)
  #pad newlines with spaces
  text.newline<-gsub("$"," ",text.untabbed)
  #then, split the text into characters
  text.letters<-unlist(strsplit(text.newline, ""))
  #then find which characters aren't letters
  bad.index<-which(!text.letters %in% letters)
  #then, find which characters are hyphens
  hyphen.index<-which(text.letters=="-")
  #then, convert hyphens into spaces
  text.letters[hyphen.index]<-" "
  #then, find the location of the spaces
  space.index<-which(text.letters==" ")
  #find which things in bad.index are spaces
  spaces.in.bad<-which(bad.index %in% space.index)
  #remove the spaces from bad.index
  bad.index<-bad.index[-spaces.in.bad]
  #remove all of the non letters from the character vector
  text.letters<-text.letters[-bad.index]
  #collapse the character vector back into the text
  text.all<-paste(text.letters, collapse="")
  #remove any remaining white space (leading/trailing)
  text.final<-gsub("\\s+", " ", text.all)
  text.final<-gsub("^\\s+|\\s+$", "", text.final)
  #finally, return the cleaned text
  return(text.final)
}

# This function extracts files from a directory (dir), atomizes thems as words, 
# and puts the bags of words in a list
getcorpus<-function(dir,type=".txt"){
  library(stringr)
  curr.folder<-getwd()
  setwd(dir)
  corpus<-list()
  files<-list.files(pattern=type)
  for(i in 1:length(files)){
    text<-scan(files[i],what="char",sep="\n")
    text<-paste(text,collapse=" ")
    lowertext<-tolower(text)
    text.words<-unlist(str_split(lowertext,boundary("word")))
    text.words<-text.words[which(text.words!="")]
    corpus[[files[i]]]<-text.words
  }
  setwd(curr.folder)
  return(corpus)
}

# Similarly, this function extracts files from a directory (dir), atomizes as words, 
# and puts frequency tables in a list
getfreqtables<-function(dir,type=".txt"){
  curr.folder<-getwd()
  setwd(dir)
  corpus<-list()
  files<-list.files(pattern=type)
  for(i in 1:length(files)){
    text<-scan(files[i],what="char",sep="\n")
    text<-paste(text,collapse=" ")
    lowertext<-tolower(text)
    text.words<-unlist(strsplit(lowertext,"\\W"))
    text.words<-text.words[which(text.words!="")]
    freqs.t<-table(text.words)
    relfreqs.t<-freqs.t/sum(freqs.t)
    corpus[[files[i]]]<-relfreqs.t
  }
  setwd(curr.folder)
  return(corpus)
}

# A quick tabling function, returns raw counts and relative frequencies
bradtable<-function(bagwords){
  for(i in 1:length(bagwords)){
  freqs.t<-table(bagwords)
  relfreqs.t<-freqs.t/sum(freqs.t)
  return(relfreqs.t)
  }
}

# This function finds words in a file (text.words) that are collocated with a target keyword 
# (target.kw) within the window specified (LRspan). Will throw an error if the word is not in the text
# Note LRspan is a +/-.
colloc<-function(target.kw,text.words,LRspan){
  target.index<-which(tolower(text.words)==tolower(target.kw))
  cat("The word",target.kw,"appears",length(target.index),"time(s)\n")
  collocates<-NULL
  for(i in 1:length(target.index)){
    if((target.index[i]-LRspan)<1){
      LHS<-text.words[1:target.index[i]]
    }else{
      LHS<-text.words[(target.index[i]-LRspan):target.index[i]]
    }
    if((target.index[i]+LRspan)>=length(text.words)){
      RHS<-text.words[target.index[i]:length(text.words)]
    }else{
      RHS<-text.words[target.index[i]:(target.index[i]+LRspan)]
    }
    collocates<-c(collocates,LHS[-length(LHS)],RHS[-1])
  }
  return(table(tolower(collocates)))
}

# This function goes through a single file (text.words) and matches
# target keywords (target.kw). It returns the keyword in its context,
# that is, its surrounding words. The size of the context is given by LRspan.
kwic<-function(target.kw,text.words,LRspan){
  text<-tolower(text.words)
  target.index<-grep(target.kw,tolower(text))
  if(length(target.index)==0){
    stop("No match")
  } else { 
    contexts<-NULL
    for(i in 1:length(target.index)){
      if(target.index[i]-LRspan<1){
        L<-1
      } else {
        L<-target.index[i]-LRspan
      }
      if(target.index[i]+LRspan>length(text)){
        R<-length(text)
      } else {
        R<-target.index[i]+LRspan
      }
      kwincontext<-text.words[L:R]
      kwincontext<-paste(kwincontext,collapse=" ")
      contexts<-c(contexts,kwincontext)
    }
  } 
  return(contexts)
}

# This is a second KWIC function for corpus objects made with  the getcorpus or makecorpus functions. Essentially it pulls keywords in context out of a well-structured list of bags of words (where each bag corresponds to a text in the corpus. It returns the KWICs in a list.
kwicorpus<-function(target.kw,corpus,LRspan){
  kwic.corpus.l<-list()
  for(i in 1:length(corpus)){
    target.index<-grep(target.kw,corpus[[i]])
    if(length(target.index)!=0){
      contexts<-NULL
      for(j in 1:length(target.index)){
        if(target.index[j]-LRspan<1){
          L<-1
        } else {
          L<-target.index[j]-LRspan
        }
        if(target.index[j]+LRspan>length(corpus[[i]])){
          R<-length(corpus[[i]])
        } else {
          R<-target.index[j]+LRspan
        }
        kwincontext<-corpus[[i]][L:R]
        kwincontext<-paste(kwincontext,collapse=" ")
        contexts<-c(contexts,kwincontext)
      }
      kwic.corpus.l[[names(corpus[i])]]<-contexts
    }
  }
  return(kwic.corpus.l)
}

# Michelle Claibourne's function for sentiment analysis
get_sentiment_afinn <- function(bagofwords){
  result <- sum(dict[which(dict$word %in% bagofwords), "weight"])
  return(result)
}
