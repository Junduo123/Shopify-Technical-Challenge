# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
#Set work directory
setwd("~/Desktop")

data <- read.csv("Book1.csv")

#packges

cor(data[sapply(data, is.numeric)],use = "complete.obs")
summary(data[sapply(data, is.numeric)])

hist(data$order_amount)
hist(data[data$shop_id != 42&data$shop_id != 78,]$order_amount)

# MANOVA (1,2,3,4,5) - resample 75 items
table(data$total_items)
`75_items` <- data.frame(matrix(NA, nrow = 375, ncol = 2))
`75_items`[,1] <- c(rep(1,75),rep(2,75),rep(3,75),rep(4,75),rep(5,75))
`75_items`[,2] <- c(sample(1:nrow(data[data$total_items == 1,]),75),
                    sample(1:nrow(data[data$total_items == 2,]),75),
                    sample(1:nrow(data[data$total_items == 3,]),75),
                    sample(1:nrow(data[data$total_items == 4,]),75),
                    sample(1:nrow(data[data$total_items == 5,]),75))

summary(aov(`75_items`[,2] ~ `75_items`[,1], data = `75_items`))
TukeyHSD(aov(`75_items`[,2] ~ factor(`75_items`[,1]), data = `75_items`))

# MANOVA
table(data$payment_method)
summary(aov(order_amount~payment_method, data = data))
TukeyHSD(aov(order_amount ~ factor(payment_method), data = data))

# process
for (i in seq(nrow(data))) {
  data$day[i] <- as.numeric(gsub('(.*)/(.*)/(.*) (.*):(.*)','\\2',data$created_at[i]))
  data$hour[i] <- as.numeric(gsub('(.*)/(.*)/(.*) (.*):(.*)','\\4',data$created_at[i]))
  data$minute[i] <- as.numeric(gsub('(.*)/(.*)/(.*) (.*):(.*)','\\5',data$created_at[i]))
}

cor(data[sapply(data, is.numeric)],use = "complete.obs")

# Log Transformation
for (i in seq(nrow(data))) {if((data$shop_id)[i] %in% c(42,78)){data <- data[-i,]}}
data <- data[,-c(1,3,7)]
for (i in c(2:3)) {if(is.numeric(data[,i])){data[,i] <- log(abs(data[,i]+1))}}

# Model
model1 <- lm(order_amount~., data = data)
summary(model1)

par(mfrow=c(2,2))
plot(model1)

# independence of residuals
durbinWatsonTest(model1)
# normality of residuals
shapiro.test(model1$residuals)

data$standardized.residuals <- rstandard(model1)
possible.outliers <- subset(data,standardized.residuals < -1.96|standardized.residuals>1.96)
data <- data[-as.numeric(rownames(possible.outliers)),]

par(mfrow=c(1,1))
plot(sort(cooks.distance(model1),decreasing = TRUE));max(cooks.distance(model1))

# Model2
model2 <- lm(order_amount~.-standardized.residuals, data = data)
summary(model2)

par(mfrow=c(2,2))
plot(model2)
