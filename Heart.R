#modelling with logistic regression
heartdata<-read.csv("heart_statlog_cleveland_hungary_final.csv")
library(rpart)
train.index<-sample(1:nrow(heartdata),0.8*nrow(heartdata))
traindata<-heartdata[train.index,]
testdata<-heartdata[-train.index,]
logreg<-glm(target~age+sex+resting.bp.s+cholesterol+fasting.blood.sugar+resting.ecg+max.heart.rate+exercise.angina+oldpeak+ST.slope,  family="binomial", data=traindata)
summary(logreg)
prob.log<-predict(logreg, newdata=testdata, type="response")
predictions<-factor(ifelse(prob.log>0.5,"1","0"))
actual<-factor(testdata$target)
accuracy<-mean(predictions==actual)
accuracy

#using visualisations to convey results
#1-Confusion Matrix
cm <- table(actual, predictions)
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Actual", "Predicted", "Count")
library(ggplot2)

cm_plot <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count),colour="white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Confusion Matrix",
    x = "Predicted Class",
    y = "Actual Class"
  ) +
  theme_minimal()
cm_plot

#2-roc/aoc curve
library(pROC)
roc_obj <- roc(
  actual,
  prob.log,
  levels = c("0", "1"),
  direction = "<"
)
auc(roc_obj)
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

roc_plot <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "grey50") +
  labs(
    title = "ROC Curve",
    x = "False Positive Rate (1 − Specificity)",
    y = "True Positive Rate (Sensitivity)",
    subtitle = paste("AUC =", round(auc(roc_obj), 3))
  ) +
  theme_minimal()
roc_plot
#
all_coords <- coords(roc_obj,
                     x = "all",
                     ret = c("threshold", "sensitivity", "specificity"),
                     transpose = FALSE)

# Filter thresholds with sensitivity >= 0.95
cand <- subset(all_coords, sensitivity >= 0.8)

# Choose the candidate with highest specificity among those
best_95 <- cand[which.max(cand$specificity), ]

best_95
best_thresh <- best_95$threshold
best_thresh
