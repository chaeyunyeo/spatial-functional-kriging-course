install.packages("dplyr")
install.packages("map")
install.packages("geoR")
install.packages("tibble")  
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("tidyr")
install.packages("refund")
install.packages("here")

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tibble) 

library(fda)
library(refund)
library(geoR)
library(fda.usc) 
library(geofd)
library(dplyr)
library(tidyr)
library(here)


loc <- read.csv(here("data", "station_locations.csv"),
  fileEncoding = "EUC-KR"
)

data <- read.csv(
  here("data", "meteorological_data.csv"),
  fileEncoding = "EUC-KR"
)

colnames(loc)
head(loc)
data <- merge(data, loc, by = "지점", all.x = TRUE)
dim(data)


# 필요한 열만 선택 및 정리
data_selected <- data %>%
  select(일시, 지점명, 기온 = 기온..C.) %>%
  complete(일시 = unique(data$일시), 지점명 = unique(data$지점명)) %>%
  arrange(일시, 지점명)  # 정렬

# 고유한 차원 값 확인
times <- unique(data_selected$일시)      # 시간 (1차원)
locations <- unique(data_selected$지점명) # 지점명 (2차원)
variables <- c("기온")       # 기상 데이터 (3차원)

# 빈 배열 생성 (시간 x 지점 x 변수)
weather_array <- array(NA, dim = c(length(times), length(locations), length(variables)),
                       dimnames = list(times, locations, variables))

# 배열 채우기
for (var in variables) {
  for (loc in locations) {
    weather_array[, loc, var] <- data_selected %>%
      filter(지점명 == loc) %>%
      arrange(일시) %>% 
      pull(!!sym(var))
  }
}

# 결과 확인
print(weather_array)


# 시간 이름 추출
time_names <- dimnames(weather_array)[[1]]

# 시간 이름을 Date-Time 형식으로 변환
time_order <- order(as.POSIXct(time_names, format = "%Y-%m-%d %H:%M"))

# 배열을 시간 순서로 정렬
weather_array <- weather_array[time_order, , ]

library(fda)
library(geoR)
source(here("code", "okfd.R"))
source(here("code", "okfd.cv.R"))


place = unique(data$지점명)

loc <- read.csv(here("data", "station_locations.csv"),
                fileEncoding = "EUC-KR"
)


coordinates = loc[,c("지점명","위도","경도")]
rownames(coordinates) = coordinates$지점명
coordinates = coordinates[,-1]
coordinates <- coordinates[,2:1]
#coordinates[,1] <- -coordinates[,1]
colnames(coordinates) = c("W.longitude", "N.latitude")
distant = dist(coordinates)


##############################################################
##### data 정제 및 test/train 분리 
##############################################################

Temperature = weather_array

place = colnames(Temperature)


######### na 값 처리하기



sum(is.na(Temperature))

Temperature_filled <- as.data.frame(Temperature)

# 각 열에 대해 NA 값을 이전 및 이후 값의 평균으로 채우기
for (col in colnames(Temperature_filled)) {
  for (i in which(is.na(Temperature_filled[[col]]))) {
    # 이전 값
    prev <- ifelse(i > 1, Temperature_filled[[col]][i - 1], NA)
    # 이후 값
    nex <- ifelse(i < nrow(Temperature_filled), Temperature_filled[[col]][i + 1], NA)
    # 이전 및 이후 값의 평균 계산 (NA 제외)
    Temperature_filled[[col]][i] <- mean(c(prev, nex), na.rm = TRUE)
  }
}

sum(is.na(Temperature_filled))





##############################################################
###### 고도 보정
##############################################################


### 고도와 기온의 관계 보기 


# loc 데이터를 고도 순으로 정렬
loc_sorted <- loc %>%
  arrange(노장해발고도.m.)

# 고도 순서대로 지역 이름 추출
ordered_regions <- loc_sorted$지점명

# Temperature_filled 데이터를 Long format으로 변환
Temperature_long <- Temperature_filled %>%
  pivot_longer(
    cols = everything(),
    names_to = "Region",
    values_to = "Temperature"
  )

# Region 열을 factor로 설정하여 고도 순으로 정렬
Temperature_long$Region <- factor(Temperature_long$Region, levels = ordered_regions)

# Boxplot 그리기
ggplot(Temperature_long, aes(x = Region, y = Temperature)) +
  geom_boxplot() +
  labs(
    title = "지역별 온도 분포 (고도 순 정렬)",
    x = "지역 (고도 순)",
    y = "온도 (°C)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # X축 레이블 각도 조정


# 기온 감소율 (6.5°C/1000m)
lapse_rate <- 6.5 / 1000

altitudes <- loc$노장해발고도.m.
adjusted_temperatures <- Temperature_filled  # 온도만 추출

# 각 지점에 대해 고도 조정 (0.65°C를 더함)
for (i in 1:ncol(adjusted_temperatures)) {
  station <- colnames(adjusted_temperatures)[i]
  altitude <- loc[loc$지점명 == station, "노장해발고도.m."]
  altitude_factor <- lapse_rate * altitude
  adjusted_temperatures[, i] <- adjusted_temperatures[, i] + altitude_factor
  adjusted_temperatures = round(adjusted_temperatures,1)
}

head(adjusted_temperatures)


##############################################################
###### 한개 빼기 (장진 지역을 예측함)
##############################################################


geo.dist.27 <- dist(coordinates) 
n<-dim(Temperature_filled)[2]  
nt <- dim(Temperature_filled)[1]
i.0 <- which(rownames(coordinates)=="장진")
coord.0 <- coordinates[i.0,]
Tempe.26 <- adjusted_temperatures %>% select(-장진)

coord.26 <- coordinates[-i.0,]
library(fda)
library(geoR)
source(here("code", "okfd.R"))
source(here("code", "okfd.cv.R"))

#################################################################
# Distance among sampling sites
#################################################################


distance <- dist(coordinates, method="euclidean", diag=TRUE, upper=TRUE)
dd <- as.matrix(distance)




#################################################################
# Distances to the prediction site
#################################################################


# 유클리디안 거리 계산 함수
euclidean_distance <- function(coord1, coord2) {
  return(sqrt((coord2[1] - coord1[1])^2 + (coord2[2] - coord1[2])^2))
}

# coord.0와 각 지역 간의 거리 계산
distances <- sapply(1:nrow(coordinates), function(i) {
  euclidean_distance(c(coord.0$W.longitude, coord.0$N.latitude), 
                     c(coordinates$W.longitude[i], coordinates$N.latitude[i]))
})

dd.0 = as.matrix(distances)
rownames(dd.0) = rownames(coordinates)
dd.0 <- dd.0[rownames(dd.0) != "장진", ]  
dd.0=matrix(dd.0)
rownames(dd.0) = rownames(coordinates)[-10]

#################################################################
# Data set 
#################################################################

Tempe.26 = as.matrix(Tempe.26) # 2927 26 
nrows <- 2927
ncols <- 26
time <- matrix(rep(1:nrows, ncols), nrow = nrows, ncol = ncols)
colnames(time) = colnames(Tempe.26)


matplot(Tempe.26, type="l", lty=1,col="grey", ylab="Temperature (degrees C)", xlab="Day")
abline(h=0,lty=2)

###########################################################################
# Prediction at 장진
###########################################################################

coord.0 = as.matrix(coord.0)
Tempe.26 = as.matrix(Tempe.26)
coordinates = as.matrix(coordinates[-10,])
n<-dim(Tempe.26)[1]
nbasis<-200
argvals<-seq(1,n, by=1)
okfd.res<-okfd(coord=coordinates,data=Tempe.26,argvals=argvals,nbasis=nbasis,
               new.coord=coord.0)  


Temp.0 = adjusted_temperatures %>% select(장진)
residual<-Temp.0-okfd.res$krig.new.data
residual2<-residual*residual
summary(residual2)
residual2 <- unlist(residual2)
sd(residual2)
sum(residual2)
mean(residual2)



fourier.basis <- create.fourier.basis(rangeval=range(time),nbasis=65)
temp.fd.Fb.0 <- Data2fd(argvals=argvals, y=as.matrix(adjusted_temperatures[,"장진"]),basisobj=fourier.basis) # 실제 

names(okfd.res)
plot(okfd.res$datafd,lty=1,col=8, xlab="Day", ylab="Temperature (Degres C)")
lines(okfd.res$krig.new.data,col=1,lwd=2) 
#lines(as.matrix(adjusted_temperatures[,"장진"]),  type="p", pch=20,cex=0.6)
lines(temp.fd.Fb.0,lwd=2,type="p", pch=20,cex=0.6) 

legend(0, 30, legend = c("Smoothed", "Prediction", "Real data"),lty = c(1,1,-1), pch=c(-1,-1,20),lwd = c(1,1,1), col=c(8,1,1),pt.cex =0.5)


###############################################
# Prediction variance
###############################################

okfd.res$pred.var # 예측 분산 

###############################################
# SSE analysis
###############################################

Temp.0 = adjusted_temperatures %>% select(장진)
residual<-Temp.0-okfd.res$krig.new.data
residual2<-residual*residual
summary(residual2)
residual2 <- unlist(residual2)
sd(residual2)
sum(residual2)
sqrt(mean(residual2))



###########################################################################
# Cross-validation analysis 
###########################################################################




n<-dim(Tempe.26)[1]
argvals<-seq(1,n, by=1)
array.nbasis <- seq(5,by=100, length = 5)
okfd.cv.res <- okfd.cv(coord=coordinates, data=Tempe.26, argvals=argvals, 
                       array.nbasis=array.nbasis, 
                       max.dist.variogram=NULL,nugget.fix=NULL)



##########################################################################
# Results of cross-validation analysis
##########################################################################

names(okfd.cv.res)
okfd.cv.res$k.opt
resultsMSE.CV<-cbind(array.nbasis,okfd.cv.res$MSE.CV)
plot(array.nbasis,okfd.cv.res$MSE.CV,type="l",xlab="Number of basis functions", ylab="MSE of cross-validation")
matplot(okfd.cv.res$krig.CV[1,,],type="l",col=1, lty=1, ylab="Temperature (Degress C)",xlab="Day")
abline(h=0, lty=2)

residuals<-okfd.cv.res$krig.CV[1,,]-Tempe.26
SSE.stations<-apply(residuals*residuals,2,sum)
SSE.stations<-as.matrix(SSE.stations)
SSE.stations
summary(SSE.stations)
sd(SSE.stations)
sum(SSE.stations)


##########################################################
# Plot of cross-validation predictions 예측 및 오차 시각화 
##########################################################

k <- 65
n<-dim(Tempe.26)[1]
evalarg<-seq(1,n, by=1)
range  <- c(1,n)
period <- n
nbasis <- 200
basis <- create.fourier.basis(range, nbasis, period)
datafd <- Data2fd(Tempe.26,argvals=evalarg,basis)
ajustes<-eval.fd(evalarg,datafd,Lfdobj=0)


predcv<-okfd.cv.res$krig.CV[1,,]
plot(Tempe.26[,26], main="Aroostook", type="p",lty=1,lwd=1, xlab="Day", ylab="Temperature (degrees C)",cex=.5)
lines(ajustes[,26], type="l", lty=1, lwd=2, col=1)
lines(predcv[,26], type="l", lty=1, lwd=1, col=1)
legend(0, 20, legend = c("Observed", "Smoothed", "Predicted"),lty = c(-1,1,1), pch=c(1,-1,-1),lwd = c(1,2,1), col=c(1,1,1),pt.cex =0.5)
abline(h=0,lty=2)

residual<-Tempe.26[,26]-predcv[,26]
summary(residual)
summary(residual*residual)
plot(residual*residual,type="l")
