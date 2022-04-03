#Importing Libraries

library(caret)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)
library(rpart)
library(RColorBrewer)
library(e1071)
library(tidyr)
library(purrr)
###########################################################
# https://www.r-bloggers.com/2018/09/multiplot-with-ggplot/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots==1) {
    print(plots[[1]])}
  else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
    }
  }
}
###########################################################
# MAE <- function(actual, predicted) 
# {
#   mean(abs(actual - predicted))
# }

# RMSE <- function(actual, pred) 
# {
#   return(sqrt(sum((actual-pred)^2)/length(actual)))
# }
###########################################################
#Importing Dataset
data <- read.csv("data.csv")

#Exploring Dataset
head(data)
###########################################################
### Data Preprocessing and Analysis ###

#Types of the data
data %>% glimpse()


str(data)
data_new <- data
data_new <- as.data.frame(unclass(data),stringsAsFactors=TRUE)
#data_new <- lapply(data, function(x) as.factor(x))
#data_new$Age <- data$Age
str(data_new)
summary(data_new)
sapply(data_new, function(x) sum(is.na(x)))


#Plotting the distribution of the important features
g1 <- ggplot(data,aes(x=Age,fill="Steelblue"))+geom_histogram()+theme(legend.position = "none")
g2 <- ggplot(data,aes(x=Gender,fill=Gender))+geom_bar()+theme(legend.position = "none")
g3 <- ggplot(data,aes(x=self_employed,fill=self_employed))+geom_bar()+theme(legend.position = "none")
g4 <- ggplot(data,aes(x=family_history,fill=family_history))+geom_bar()+theme(legend.position = "none")
g5 <- ggplot(data,aes(x=treatment,fill=treatment))+geom_bar()+theme(legend.position = "none")
g6 <- ggplot(data,aes(x=work_interfere,fill=work_interfere))+geom_bar()+theme(legend.position = "none")
g7 <- ggplot(data,aes(x=no_employees,fill=no_employees))+geom_bar()+theme(legend.position = "none")
g8 <- ggplot(data,aes(x=remote_work,fill=remote_work))+geom_bar()+theme(legend.position = "none")
g9 <- ggplot(data,aes(x=tech_company,fill=tech_company))+geom_bar()+theme(legend.position = "none")
g10 <- ggplot(data,aes(x=benefits,fill=benefits))+geom_bar()+theme(legend.position = "none")
g11 <- ggplot(data,aes(x=care_options,fill=care_options))+geom_bar()+theme(legend.position = "none")
g12 <- ggplot(data,aes(x=wellness_program,fill=wellness_program))+geom_bar()+theme(legend.position = "none")
g13 <- ggplot(data,aes(x=seek_help,fill=seek_help))+geom_bar()+theme(legend.position = "none")
g14 <- ggplot(data,aes(x=anonymity,fill=anonymity))+geom_bar()+theme(legend.position = "none")
g15 <- ggplot(data,aes(x=leave,fill=leave))+geom_bar()+theme(legend.position = "none")
g16 <- ggplot(data,aes(x=mental_health_consequence,fill=mental_health_consequence))+geom_bar()+theme(legend.position = "none")
g17 <- ggplot(data,aes(x=phys_health_consequence,fill=phys_health_consequence))+geom_bar()+theme(legend.position = "none")
g18 <- ggplot(data,aes(x=coworkers,fill=coworkers))+geom_bar()+theme(legend.position = "none")
g19 <- ggplot(data,aes(x=supervisor,fill=supervisor))+geom_bar()+theme(legend.position = "none")
g20 <- ggplot(data,aes(x=mental_health_interview,fill=mental_health_interview))+geom_bar()+theme(legend.position = "none")
g21 <- ggplot(data,aes(x=phys_health_interview,fill=phys_health_interview))+geom_bar()+theme(legend.position = "none")
g22 <- ggplot(data,aes(x=mental_vs_physical,fill=mental_vs_physical))+geom_bar()+theme(legend.position = "none")
g23 <- ggplot(data,aes(x=obs_consequence,fill=obs_consequence))+geom_bar()+theme(legend.position = "none")

#Arranging the plots using grid.arrange function
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,nrow=3)
grid.arrange(g10,g11,g12,g13,g14,g15,g16,g17,g18,nrow=3)
grid.arrange(g19,g20,g21,g22,g23,nrow=3)

grid.arrange(g1)



Mode <- function(x)
{
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

summary(data$Age)

mean_data <- mean(data$Age)
sd_data <- sd(data$Age)
zscore <- abs((data$Age - mean_data)/sd_data)
#print(data[which((zscore>3)),2])


#Age
age1 <- ggplot(data, aes(Age))+
  geom_histogram()+xlim(0,75)+labs()
age1
sum(is.na(data_new$Age))
#SelfEmploed
data_new$self_employed[is.na(data_new$self_employed)] <- Mode(data_new$self_employed)
summary(data_new$self_employed)

#Sex
summary(data_new$Gender)
Gender_list <- unique(data_new$Gender)
Gender_list

data_new$sex <- as.factor(ifelse(data_new$Gender %in% c("Female ","Female","femail","woman","Female","Female (cis)","cis-female/femme", "Cis Female", "Trans woman","female","F","Woman","f","Femake", "Trans-female", "Female (trans)"), "Female" ,
                ifelse(data_new$Gender %in% c("Male ", "Mail", "maile","Cis Man", "Malr", "Man", "Male", "male", "M", "cis male", "m", "Male-ish", "Mal", "Male (CIS)", "Cis Male", "Make", "Male", "msle"), "Male", "Undecided")))
str(data_new$Gander)
table(data_new$Gander)
par(mfrow=c(1,2))
barplot(table(data$Gender),col = "#078AD7",main = "Orginal Gender Column")
barplot(table(data_new$sex),col = "#078AD7",main = "Cleaned Gender Column")

#work_interfere
summary(data_new$work_interfere)
data_new$work_interfere[is.na(data_new$work_interfere)] <- Mode(data_new$work_interfere)
summary(data_new$work_interfere)

par(mfrow=c(1,2))
barplot(table(data$work_interfere),col = "skyblue",main = "Orginal work_interfere")
barplot(table(data_new$work_interfere),col = "skyblue",main = "Cleaned work_interfere")

sapply(data_new, function(x) sum(is.na(x)))
###########################################################
#converting to numeric
str(data_new)
# remove timestamp, Gender, country, state and Comments.
data_new$Timestamp <- NULL
data_new$Gender <- NULL
data_new$Country <- NULL
data_new$state <- NULL
data_new$comments <- NULL
summary(data_new)


str(data_new)
sapply(data_new, function(x) sum(is.na(x)))
data_factors <- data_new
data_new[] <- lapply(data_new, function(x) as.numeric(as.factor(x)))
str(data_new)
###########################################################
#Correlation
cor_mat <- round(cor(data_new),2)
cor_mat
melted_cormat <- melt(cor_mat)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+xlab("")+ylab("")+ggtitle("Correlation plot")

correlations <- as.data.frame(round(cor(data_new,data_new$treatment),2))
names <- rownames(correlations)
rownames(correlations) <- NULL
correlations <- cbind(names,correlations)
correlations <- correlations[order(-correlations$V1),]
correlations$V1 <- abs(correlations$V1)
correlations

ggplot(data = correlations, aes(x = V1, y = names, color = names, group = names))+
  geom_segment(data = correlations,aes(x=0,xend = V1, y = names, yend = names),size = 1)+
  geom_point(size = 3)+ggtitle("Correlation with Treatment")+
  theme(legend.position = "none")+xlab("Correlation values")+ylab("Features")


data_new$Age <- as.factor(ifelse(data_new$Age < 30 , "< 30", ">=30"))

data_factors$Age <- data_new$Age
data_new$Age <- as.numeric(data_new$Age)
                              
#for (i in 1:ncol(data_new)) {
#  hist(data_new[,i],col="skyblue", xlab = colnames(data_new[i]), main = NULL)
#}

data_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key,  scales = "free") +
  geom_histogram()
###########################################################
#Normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
str(data_new)
data_norm <- data_new
data_norm[,-6] <- lapply(data_new[,-6], normalize)
data_norm[,6] <- as.factor(data_new[,6])
data_norm
###########################################################
#PCA
#https://rfunctions.blogspot.com/2015/01/pca-principal-component-analysis.html
data_PCA <- prcomp(data_new[,-6], center = T, scale = T)
print(data_PCA)
summary(data_PCA)
screeplot(data_PCA, type = "l", main = "PCA")
###########################################################
#Models
set.seed(101)

data_new$treatment <- as.factor(data_new$treatment)

#80:20 ratio
index <- createDataPartition(data_new$treatment, p=0.80, list = FALSE, times = 1)
training_data <- data_new[index, ]
testing_data <- data_new[-index, ]
training_data_factor <- data_factors[index, ]
testing_data_factor <- data_factors[-index, ]
###########################################################
# data_PCA$treatment <- as.factor(data_PCA$treatment)
# index_pca <- createDataPartition(data_PCA$treatment, p=0.75, list = FALSE)
# training_data_pca <- data_PCA[index_pca, ]
# testing_data_pca <- data_PCA[-index_pca, ]
# training_data_factor_pca <- data_new[index_pca, ]
# testing_data_factor_pca <- data_new[-index_pca, ]
###########################################################
#Logistic Regression

lm <- glm( treatment~., data = training_data_factor, family = "binomial" )
#lm_pca <- glm( treatment~., data = training_data_factor_pca, family = "binomial" )
summary(lm)
predict_prob <- predict(lm, testing_data_factor, type = "response")
pred_glm <- as.factor(ifelse(predict_prob < 0.5, "No", "Yes"))

confusionMatrix(pred_glm,testing_data_factor$treatment)
accuracy_glm <- accuracy(pred_glm,testing_data_factor$treatment)
RMSE_glm <- RMSE(as.numeric(testing_data_factor$treatment), as.numeric(pred_glm))
MAE_glm <- MAE(as.numeric(testing_data_factor$treatment), as.numeric(pred_glm))
roc_glm <- roc(as.numeric(testing_data_factor$treatment), as.numeric(pred_glm))
RMSE_glm
MAE_glm
roc_glm
###########################################################
#Decision Trees
training_data$treatment <- as.numeric(training_data$treatment)
testing_data$treatment <- as.numeric(testing_data$treatment)

Dtree_model <- rpart(treatment ~ ., data = training_data_factor[,-3],method = "class")
Dtree_model
summary(Dtree_model)
rpart.plot(Dtree_model, cex=0.8, )
pred_Dtree <- predict(Dtree_model, testing_data_factor)
pred_Dtree <- as.factor(ifelse(pred_Dtree[,2] < 0.5, "No", "Yes"))

confusionMatrix(pred_Dtree,testing_data_factor$treatment)
accuracy_Dtree <- accuracy(pred_Dtree,testing_data_factor$treatment)
RMSE_Dtree <- RMSE(as.numeric(testing_data_factor$treatment), as.numeric(pred_Dtree))
MAE_Dtree <- MAE(as.numeric(testing_data_factor$treatment), as.numeric(pred_Dtree))
roc_Dtree <- roc(as.numeric(testing_data_factor$treatment), as.numeric(pred_Dtree))
RMSE_Dtree
MAE_Dtree
roc_Dtree
###########################################################
# K-fold Cross Validation
Kfold <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,repeats = 3,savePredictions = TRUE)

cv_glm <- train(treatment ~ ., data = data_factors, 
                method = "glm", 
                trControl = Kfold)

cv_svm <- train(treatment ~ ., data = data_factors, 
                method = "svmRadial", 
                trControl = Kfold)

cv_Dtree <- train(treatment ~ ., data = data_factors, 
                  method = "rpart", 
                  trControl = Kfold)
print(cv_glm)
print(cv_svm)
print(cv_Dtree)
###########################################################
# Comparison

comparison <- data.frame(Models = c("Logistic Regression", "Decision Trees"),
                         MAE = c(MAE_glm,MAE_Dtree), RMSE = c(RMSE_glm,RMSE_Dtree), 
                         AUC = c(roc_glm$auc, roc_Dtree$auc))
comparison


barchart(MAE+RMSE+AUC~Models,data=comparison,run=best, 
         ylab = "Values", 
         xlab = "Models",scales=list(alternating=1),
         auto.key=list(space='right', rows=3,points=FALSE, 
                       rectangles=TRUE,title="Metrics", cex.title=1),
         par.settings=list(superpose.polygon=list()),main="Model Comparison")
###########################################################






