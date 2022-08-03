library(tidyverse)
library(hoopR)
library(caTools)
library(measures)
library(caret)
library(readxl)
library(zoo)

jt_19_20<-read_xlsx("./JaysonTatumBoxScores.xlsx",sheet = "JaysonTatum19-20RegSeason")
jt_20_21<-read_xlsx("./JaysonTatumBoxScores.xlsx",sheet= "JaysonTatum20-21RegSeason")
jt_21_22<-read_xlsx("./JaysonTatumBoxScores.xlsx",sheet= "JaysonTatum21-22RegSeason")



#############################################################################
#Cleaning and adding data

gmsc_var<-function(df){
  df%>%
    replace_na(list(Column1="H"))->df
  df$Column1<-str_replace(df$Column1,"@","A")
  
  df$`2`<-replace(df$`2`,grepl("W",df$`2`),"W")
  df$`2`<-replace(df$`2`,grepl("L",df$`2`),"L")
  df$`+/-`<-str_replace(df$`+/-`,"\\+","")
  df$MP<-substr(df$MP,1,unlist(gregexpr(":",df$MP))-1)
  
  colnames(df)[8]<-"W/L"
  colnames(df)[6]<-"H/A"
  
  i<-c(10:30)
  df[ , i] <- apply(df[ , i], 2,function(x) as.numeric(as.character(x)))
  
  df%>%
    na.omit()%>%
    mutate(cum_avg_gmsc=cummean(GmSc))->df
  iterate<-1
  abv_blw_vec<-c()
  for (i in df$GmSc){
    if(i>df$cum_avg_gmsc[iterate]){
      abv_blw_vec<-c(abv_blw_vec,"above")
    }
    if(i<df$cum_avg_gmsc[iterate]){
      abv_blw_vec<-c(abv_blw_vec,"below")
    }
    if(i==df$cum_avg_gmsc[iterate]){
      abv_blw_vec<-c(abv_blw_vec,NA)
    }
    iterate<-iterate+1
  }
  df%>%
    mutate(abv_bel=abv_blw_vec)%>%
    filter(!is.na(abv_bel))->df
  aawp<-c()
  bawp<-c()
  total_abv_wins<-0
  total_bel_wins<-0
  total_abv<-0
  total_bel<-0
  iterate<-1
  for (i in df$abv_bel){
    if (i=="above"&& df$`W/L`[iterate]=="W"){
      total_abv_wins<-total_abv_wins+1
    }
    if(i=="below"&&df$`W/L`[iterate]=="W"){
      total_bel_wins<-total_bel_wins+1
    }
    total_abv<-total_abv+1
    total_bel<-total_bel+1
    aawp<-c(aawp,total_abv_wins/total_abv)
    bawp<-c(bawp,total_bel_wins/total_bel)
    
    iterate<-iterate+1
  }
  
  df%>%
    mutate(abv_avg_wp=aawp,bel_avg_wp=bawp)%>%
    mutate(GmSc_Var=abv_avg_wp-bel_avg_wp)%>%
    select(-(31:34))->df
  df$GmSc_Var<-round(df$GmSc_Var,2)
  return(df)
}

gmsc_var(jt_21_22)->jt_21_22
gmsc_var(jt_20_21)->jt_20_21
gmsc_var(jt_19_20)->jt_19_20

jt_19_20%>%
  mutate(season="2019-2020")->jt_19_20
jt_20_21%>%
  mutate(season="2020-2021")->jt_20_21
jt_21_22%>%
  mutate(season="2021-2022")->jt_21_22

rbind(jt_19_20,jt_20_21,jt_21_22)->jayson_tatum
jayson_tatum$season<-as.factor(jayson_tatum$season)

##########################################################################
#ML
jayson_tatum%>%
  filter(!is.na(G))%>%
  select(`W/L`,PTS,AST,TRB,GmSc_Var)->ml_data
ml_data<-na.omit(ml_data)

ml_data$`W/L`<-replace(ml_data$`W/L`,ml_data$`W/L`=="L",0)
ml_data$`W/L`<-replace(ml_data$`W/L`,ml_data$`W/L`=="W",1)
ml_data$`W/L`<-as.integer(ml_data$`W/L`)
#Win is 1 Loss is 0
set.seed(10)
sample_split <- sample.split(Y = ml_data$`W/L`, SplitRatio = 0.8)
train_set <- subset(x = ml_data, sample_split == TRUE)
test_set <- subset(x = ml_data, sample_split == FALSE)
logistic <- glm(`W/L`~ ., data = train_set, family = "binomial")

as.data.frame(coef(summary(logistic)))->sumLogistic
sumLogistic%>%
  arrange(`Pr(>|z|)`)

probs <- predict(logistic, newdata = test_set, type = "response")
pred <- ifelse(probs > 0.5, 1, 0)

confusionMatrix(factor(pred), factor(test_set$`W/L`), positive = as.character(1))
#############################################################################
random_forest<-train(`W/L`~PTS+AST+TRB+GmSc_Var,data = ml_data,method = 'rf',
                      trControl = trainControl(method = 'cv', number = 5)) 
random_forest
