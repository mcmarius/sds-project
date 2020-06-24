library(jsonlite)
library(ggplot2)
library(astsa)

json = jsonlite::read_json('data/data_univ.json')

v <- c()
for (s in json$data$ica_data$series) {
    v <- c(v, s$y)
}

read_series <- function(obj_data) {
    v <- c()
    dates <- c()
    for (s in json[['data']][[obj_data]][['series']]) {
        v <- c(v, s$y)
    }
    for (s in json[['data']][[obj_data]][['columns']]) {
        dates <- c(dates, s)
    }
    return(data.frame("value"=v, "date"=as.Date(dates)))
}

plot_qq <- function(model) {
  rs <- model$residuals
  stdres <- rs/sqrt(model$sigma2)
  txt_scale = 0.95
  qqnorm(stdres, main = "Normal Q-Q Plot of Std Residuals", cex.lab=txt_scale, cex.axis=txt_scale, cex.main=txt_scale, cex.sub=txt_scale)
}

dataframe <- read_series('ica_data')
plt <- ggplot(data = dataframe, aes(x=date,y=value))
plt+geom_line( color="steelblue") + geom_point() + xlab("") + ylab("AQI") + ggtitle("Air quality index")

acf(dataframe$value, main="ACF for our data")

X <- diff(dataframe$value)
acf2(X, main="ACF and PACF for differenced data")


ma3 <- arima(X , order=c(0,0,3))
tsdiag(ma3)

print(c("AIC: ", AIC(ma3)))
print(c("BIC: ", BIC(ma3)))
shapiro.test(ma3$residuals)
txt_scale=0.95
hist(ma3$residuals, cex.lab=txt_scale, cex.axis=txt_scale, cex.main=txt_scale, cex.sub=txt_scale)
plot_qq(ma3)


ma5 <- arima(X , order=c(0,0,5))
tsdiag(ma5)

AIC(ma5)
BIC(ma5)
plot_qq(ma5)


ar3 <- arima(X , order=c(3,0,0))
tsdiag(ar3)


AIC(ar3)
BIC(ar3)
plot_qq(ar3)


fore = predict(arima(dataframe$value,order=c(0,1,3)), n.ahead=20)
ts.plot(ts(dataframe$value), fore$pred, col=1:2, xlab='', ylab='AQI')



data2 <- read.delim('data/birchpollen.dat', header = FALSE, col.names = c('diameter', 'species', 'location'), colClasses = c('numeric', rep('integer', 2)))

data2$species  <- factor(data2$species)
data2$location <- factor(data2$location)

groups <- c()
for (li in levels(data2$species)) {
  for (lj in levels(data2$location)) {
    gr <- data2[data2$species == li & data2$location == lj,]
    if(length(gr$diameter) > 1)
      groups <- c(groups, list(gr$diameter))
  }
}

bartlett.test(groups)

groups <- c()
for (li in setdiff(levels(data2$species), c("1", "4", "5"))) {
  for (lj in levels(data2$location)) {
    gr <- data2[data2$species == li & data2$location == lj,]
    if(length(gr$diameter) > 0)
      groups <- c(groups, list(gr$diameter))
  }
}

bartlett.test(groups)

df23 <- data2[data2$species == 2 | data2$species == 3,]
anova2 <- aov(diameter~species*location, data=df23)
summary(anova2)


data3 <- read.delim('data/potato.dat', header = F, col.names = c('growing_area', 'tw_hold_temp', 'size', 'storage_period', 'cooking_method', 'texture', 'flavor', 'moistness'))

data3$growing_area   <- factor(data3$growing_area)
data3$tw_hold_temp   <- factor(data3$tw_hold_temp)
data3$size           <- factor(data3$size)
data3$storage_period <- factor(data3$storage_period)
data3$cooking_method <- factor(data3$cooking_method)

bartlett_for_effect <- function(fact1, fact2, eff) {
  groups <- c()
  for (li in levels(data3[[fact1]])) {
    for (lj in levels(data3[[fact2]])) {
      gr <- data3[data3[[fact1]] == li & data3[[fact2]] == lj,]
      groups <- c(groups, list(gr[[eff]]))
    }
  }
  
  print(bartlett.test(groups))
}

bartlett_for_effect('tw_hold_temp', 'size', 'texture')
bartlett_for_effect('tw_hold_temp', 'size', 'flavor')
bartlett_for_effect('tw_hold_temp', 'size', 'moistness')

print(summary(aov(flavor~tw_hold_temp*size, data = data3)))
print(summary(aov(moistness~tw_hold_temp*size, data = data3)))
print(summary(aov(texture~tw_hold_temp*size, data = data3)))

bartlett_for_effect('cooking_method', 'growing_area', 'texture')
print(summary(aov(texture~cooking_method*growing_area, data = data3)))

bartlett_for_effect('storage_period', 'cooking_method', 'flavor')
print(summary(aov(flavor~storage_period*cooking_method, data = data3)))



datab <- read.delim('data/scottish_recycle.dat', header=FALSE, col.names = c('auth', 'rec_capacity', 'resid_capacity', 'collected', 'yield'))

x <- datab$yield
y1 <- datab$rec_capacity
y2 <- datab$resid_capacity
lmodel <- lm(x~y1+y2)
summary(lmodel)
anova(lmodel)
shapiro.test(lmodel$residuals)

resid <- abs(residuals(lmodel))
pred<-predict(lmodel)
plot(pred,resid)


y3 <- datab$collected
lmodel2 <- lm(x~y1+y2+y3)
summary(lmodel2)
shapiro.test(lmodel2$residuals)

resid <- abs(residuals(lmodel2))
pred<-predict(lmodel2)
plot(pred,resid)

