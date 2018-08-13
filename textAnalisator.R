#This is a script file.

#read data from files
special.symb<- scan("symbols.txt", what = "character", sep= NULL)
special.symb<-c(special.symb, c("\"","'"))

myText<-scan("myText.txt", what="charcter", sep =NULL )
# To read big data remove comment

#myText<-scan("bigFile.txt", what="charcter", sep =NULL )

temp<-myText

# Splitting text
for( i in 1:length(special.symb)){
for( j in 1:length(temp)){
    if(grepl(special.symb[i], temp[j], fixed=TRUE)) {
     # print("====symb[i]=======")
     # print(special.symb[i]);
     # print(strsplit( temp[j], paste0("[",special.symb[i],"]")));
      tempList<-strsplit( temp[j], paste0("[",special.symb[i],"]"));
      temp[j]<-"";
      temp<-c(temp, tempList[[1]])
    #  print(temp)
    }
  }
}

# Clean temp. The  temp will not consist of ""
temp<-temp[temp!=""]
temp<-temp[(nchar(temp, type="chars")<=30)]
textData<-data.frame(temp)

#Read data for special task. Double consonants
taskCheck<- scan("task.txt",what= "Character", sep=NULL)

#Find all words which satisfy the task. Put them into the answer
answer<-numeric(length(temp))
for( j in 1: length(temp)){
for( i in 1: length(taskCheck)){
  if(grepl(taskCheck[i], temp[j], fixed=TRUE)){
    #print(i); print(taskCheck[i]); print(grepl(taskCheck[i], temp[j], fixed=TRUE))
    answer[j]<-temp[j]
  }
}
}

answer<-answer[answer!="0"]
 
deleteRepeat<- function(vector){
  repeating<-numeric(length(vector))
  for(j in 1:(length(vector)-1))
  for(i in 1: (length(vector)) ){
    if((vector[j]==vector[i]) & (i!=j)) {repeating[j]<-repeating[j]+1; 
    vector[j]="";
      }
    }
 vector
}

# Editing the answer
answer<-deleteRepeat(answer)
answer<- answer[answer!=""]
answer<-answer[answer!="NULL"]
answer

#Write answer as a data into the file
data.ans<-data.frame(words_with_repetitive_consonants=answer)
write.table(data.frame(words_with_repetitive_consonants=answer), file="answer.txt", row.names=TRUE, col.names=TRUE)

#Words
#textData

#Answer
data.ans
