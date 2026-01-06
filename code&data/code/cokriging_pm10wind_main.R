
library(dplyr)
library(tidyr)


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
library(sp)
library(here)


pm10loc <- read.csv(here("data", "pm10loc.csv"), fileEncoding = "EUC-KR")

pm10 <- read.csv(here("data", "pm10.csv"), fileEncoding = "EUC-KR")

wind <- read.csv(here("data", "wind_speed.csv"),fileEncoding = "EUC-KR")

head(pm10)
head(pm10loc)
head(wind)





pm10 <- pm10 %>%
  group_by(지점명,지점) %>%
  summarize(평균_미세먼지 = mean(as.numeric(X1시간평균.미세먼지농도.....), na.rm = TRUE))


head(pm10)
head(pm10loc)
pm = pm10 %>% left_join(pm10loc %>% select(지점명, 위도, 경도,지점), by = "지점") %>% distinct(지점, .keep_all = TRUE)
head(pm)

wind_wide <- wind %>%
  mutate(시간 = format(as.POSIXct(일시), "%H")) %>%  # 시간 추출
  select(지점, 시간, 풍속.m.s.) %>%                  # 필요한 열 선택
  pivot_wider(names_from = 시간, values_from = 풍속.m.s., names_prefix = "시간_") # 시간별 풍속 가로 정리

# pm 데이터에 wind_wide 병합
pm_with_wind <- pm %>%
  left_join(wind_wide, by = "지점")


# NA가 포함된 행 제거
pm_with_wind_clean <- pm_with_wind %>%
  drop_na()

pm10wind = pm_with_wind_clean %>% select(-지점명.y, -지점)

pm10wind 

## 좌표계 변환
library(sf)
coords <- pm10wind %>% select(위도, 경도)
pm10wind_sf <- st_as_sf(coords, coords = c("경도", "위도"), crs = 4326)  # WGS84 (EPSG:4326)
utm_crs <- st_crs("+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs")
pm10wind_utm <- st_transform(pm10wind_sf, crs = utm_crs)
print(pm10wind_utm)
st_coordinates(pm10wind_utm)

## setwd("C:/Users/user/OneDrive - 이화여자대학교/바탕 화면/FDA PROJECT/code/RCodeArticleCokriging (1)/RCodeArticleCokriging")


# Data

data = pm10wind
pm10 = data$평균_미세먼지 # scalar 
wind = data[,5:ncol(data)]
wind = as.matrix(wind)
coord = st_coordinates(pm10wind_utm)
time = c(1:ncol(wind))
matplot(time,t(wind),type="l", lty=1, ylab="Wind speed (m/s)" ,xlab="Hour")
var(pm10)



# Smoothing wind speed data by using a Fourier basis
# Optimum basis number k=7 was previously established (내꺼)

n = dim(wind)[2]
range = c(1,n)
nbasis = 7      
argvals = seq(1,n, by=1)
basis = create.fourier.basis(range, 7, period=12)
datafd = Data2fd(argvals, t(wind), basis) # 바람을 함수형 데이터로 바꾼다. 
datafd
matplot(time,t(wind),type="l", lty=1, ylab="Wind speed (m/s)" ,xlab="Hour")
plot (datafd, lty=1, ylab="Wind speed (m/s)" ,xlab="Hour")




library(gstat)

# Linear model of coregionalization (공간적 상관 구조 모델링)
k=nbasis
coeficients<-matrix(datafd$coefs,nrow=k)
coeficients
coef<-t(coeficients)
coef<-cbind(coord,coef)
coef
datcok<-cbind(coef, pm10)
datcok<-as.data.frame(datcok)
datcok
n2<-paste(expression(phi),1:(k+1), sep="")
names(datcok)<-c("x","y",n2)
coordinates(datcok)= ~x+y
g<-NULL
for (i in 1:(k+1))
{
  g <- gstat(g,formula= as.formula(paste(n2[i],"~1")), data=datcok)
}
v <- variogram(g)
plot(v, xlab="Distance")
g <- gstat(g, model=vgm(4000, "Gau", 80000), fill.all=TRUE)
g.fit <- fit.lmc(v,g,   fit.lmc=TRUE, correct.diagonal = 1.01)
plot(v, g.fit, xlab="Distance", ylab="Simple and cross semivariance", main ="")
names(g.fit)
g.fit$model


################# 

loc <- read.csv(here("data", "station_locations.csv"),fileEncoding = "EUC-KR")
## 좌표계 변환
library(sf)
coords <- loc[,c("위도","경도")]
loc_sf <- st_as_sf(coords, coords = c("경도", "위도"), crs = 4326)  # WGS84 (EPSG:4326)
utm_crs <- st_crs("+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs")
loc_utm <- st_transform(loc_sf, crs = utm_crs)
print(loc_utm)
loc_utm = as.data.frame(loc_utm)

grid = expand.grid(Longitude=seq(min(coord[,1]),max(coord[,1]),5000), Latitude=seq(min(coord[,2]),max(coord[,2]),5000))
#grid = expand.grid(Longitude=seq(900000,1070000,500), Latitude=seq(940000,1200000,500))
grid = as.data.frame(grid)
summary(grid)
names(grid) = c("x","y")
coordinates(grid)=~x+y
prediction= predict(g.fit, newdata=grid)
summary(prediction)
names(prediction)

# PM10 prediction

pm10_prediction=prediction["var8.pred"]
summary(pm10_prediction)

# Prediction variance

pm10_prediction_var=prediction["var8.var"]
pm10_prediction_var=prediction[16]
summary(pm10_prediction_var)
pm10_prediction_var=as.data.frame(prediction[16])
summary(sqrt(pm10_prediction_var))


# Maps using gstat

?spplot
spplot(prediction["var8.pred"], cuts=4, scales=list(draw = TRUE), 
       col.regions=rev(gray(seq(0.3,0.7,.01))), do.log=T)
spplot(prediction["var8.var"], cuts=4,  scales=list(draw = TRUE), 
       col.regions=gray(rev(seq(0.4,0.8,.01))), do.log=T)











