#install packages
install.packages("stats")
install.packages("dplyr")
install.packages("randomForest")

#Load libaries
library(stats)
libary(dplyr)
libary(randomForest)

mydata<-iris
View(mydata)

#splitting the data
index = sample(2,nrow(mydata),replace=TRUE,prob=(c(0.7,0.3)))

#training data
Training = mydata[index==1,]
#testing data
Testing = mydata[index==2,]


#rfm

RFM = randomForest(Species~., data=Training)


#df <- data.frame(
 # row.names = c("Sepal.Length",
  #         "Sepal.Width",
   #        "Petal.Length",
        #   "Petal.Width"),
#  Value = as.character(c(5.5,
 #                        5.5,
  #                       5.5,
   #                      5.5)),
  #stringsAsFactors = FALSE)
newdf <- data.frame(Sepal.Length=5.5,
                    Sepal.Width=5.5,
                    Petal.Length=5.5,
                    Petal.Width=5.5)
#df <- rbind(df)
#testdata<-transpose(df)
#model accuracy

Species_Pred = predict(RFM, newdf)
Testing$Species_Pred = Species_Pred
View(Testing)


#Conf matrix
CFM = table(Testing$Species, Testing$Species_Pred)

