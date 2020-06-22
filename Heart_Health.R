# import libraries 
library(tree)
library(psych)

# read in my dataset
df = read.csv("HeartDataset.csv")
head(df)

#  Exploratory Analysis

dim(df)
str(df)
names(df)

# rename the coulmns for beter understande
names(df) <- c('Age','Sex','ChestPain','RestingBloodPressure', 'Cholestoral','FastingBloodSugar',
               'CardiographicResults','MaximumHeartRate','ExerciseInducedAngina', 'Depression','Slope','MajorVessels',
               'Thalassemia','Target')
attach(df)
summary(df)

#Check for missing values
sum(is.na(df))

#  Bar Plot for Target Class

table(df$Target)

barplot(table(df$Target),
        main=" heart-disease Count of 1025 patient",
        xlab="Heart Diseases",
        ylab="Count",
        border="red",
        col="blue",
        density=10
)


# Histogram
# Multiple histograms with density and normal fits on one page
multi.hist(df,dcol= c("blue","red"),dlty=c("dotted", "solid")) 

# dummy variables for the discrete categorical data

library(ade4)
library(data.table)
ohe_feats = c('Sex','ChestPain','FastingBloodSugar', 'CardiographicResults',
              'ExerciseInducedAngina', 'Slope', 'MajorVessels','Thalassemia')
for (f in ohe_feats){
        df_dummy = acm.disjonctif(df[f])
        df[f] = NULL
        df= cbind(df, df_dummy)
}
head(df)

HeartDiseases = ifelse(Target== 1,"Yes","No")

df= data.frame(df,HeartDiseases)
head(df)

# Decision Tree
tree.heart=tree(HeartDiseases~. -Target,df)
summary(tree.heart)

plot(tree.heart)
text(tree.heart, pretty=0)

tree.heart
set.seed(2)
train=sample(1:nrow(df), 200)
heart.test=df[-train,]
HeartDiseases.test=HeartDiseases[-train]
tree.heart=tree(HeartDiseases~.-Target,df,subset=train)

plot(tree.heart)
text(tree.heart, pretty=0)

tree.pred=predict(tree.heart,heart.test,type="class")
table(tree.pred,HeartDiseases.test)







