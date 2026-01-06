install.packages("dplyr")
install.packages("map")
install.packages("geoR")
install.packages("tibble")  
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("tidyr")

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

loc <- read.csv(here("data", "station_locations.csv"),fileEncoding = "EUC-KR")

data <- read.csv(here("data", "meteorological_data.csv"),fileEncoding = "EUC-KR")


colnames(loc)
head(loc)
data <- merge(data, loc, by = "지점", all.x = TRUE)
dim(data)


# 필요한 열만 선택 및 정리
data_selected <- data %>%
  select(일시, 지점명, 기온 = 기온..C., 상대습도 = 상대습도..., 전운량 = 전운량.할., 풍속 = 풍속.m.s.) %>%
  complete(일시 = unique(data$일시), 지점명 = unique(data$지점명)) %>%
  arrange(일시, 지점명)  # 정렬

# 고유한 차원 값 확인
times <- unique(data_selected$일시)      # 시간 (1차원)
locations <- unique(data_selected$지점명) # 지점명 (2차원)
variables <- c("기온", "상대습도","전운량","풍속")       # 기상 데이터 (3차원)

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




##############################################################
### 지도에 현재 위치들 찍어 보기
##############################################################

place = unique(data$지점명)

loc <- read.csv(here("data", "station_locations.csv"),fileEncoding = "EUC-KR")

coordinates = loc[,c("지점명","위도","경도")]
rownames(coordinates) = coordinates$지점명
coordinates = coordinates[,-1]
coordinates <- coordinates[,2:1]
#coordinates[,1] <- -coordinates[,1]
colnames(coordinates) = c("W.longitude", "N.latitude")
distant = dist(coordinates)


library(maps)
par(mar=c(2,2,1,1))
map('world',region="north korea")
points(coordinates,col='blue',pch=19)
text(coordinates$W.longitude, coordinates$N.latitude, 
     labels = rownames(coordinates), pos = 4, cex = 0.8, col = 'blue')

## 예측할 지역 위도 경도 정보

# 도시명, 위도, 경도를 데이터로 정리
# 도시명, 위도, 경도 데이터를 데이터프레임으로 생성
predict_data <- data.frame(
  지점명 = c("평성시", "순천시", "덕천시", "라선특별시", "정주시", "회령시", "송림시", 
          "문천시", "만포시", "삼천군", "영광군", "연산군", "고원군", "안변군", 
          "문덕군", "신천군", "은률군", "평원군", "숙천군", "정평군", "함주군", 
          "청단군", "신평군", "재령군", "강동군", "상원군", "법동군"),
  위도 = c(39.2482905, 39.4196379, 39.7657075, 42.2497316, 39.6929225, 42.4342802, 
         38.7535598, 39.2370094, 41.1547179, 38.3401331, 40.11726805, 38.875244949999995, 
         39.4403967, 39.047481399999995, 39.50704925, 38.3157667, 38.559682949999996, 
         39.27804115, 39.412191, 39.7277688, 39.8893074, 37.91591085, 38.95653045, 
         38.4007789, 39.11551935, 38.85286335, 38.9974411),
  경도 = c(125.8721799, 125.9476531, 126.3132673, 130.3113296, 125.2101297, 129.7542469, 
         125.6460599, 127.3598714, 126.2876899, 125.31969195348606, 127.34221350044098, 
         126.34306088620286, 127.2438233, 127.54026211826559, 125.5826078251131, 
         125.50414005012456, 125.1983980140381, 125.55190682665878, 125.6242544, 
         127.40560665265733, 127.31527704098332, 125.86687328298824, 126.75511623872042, 
         125.65229565160001, 126.10848853968027, 126.08869107336034, 127.15851588424239)
)
# 백암읍, 랑립읍, 김형권읍, 대관군, 단천시, 금천군, 창도군, 고산군, 요덕읍
# 북한 지역들의 위도와 경도 데이터프레임 생성
outer_data <- data.frame(
  지점명 = c("백암읍", "랑림읍", "명천군", "대관군", "단천시", "금천군", "창도군", "고산군", "요덕읍"),
  위도 = c(41.567001, 40.974687, 41.075884, 40.2104252, 40.4590505, 38.19590085, 38.670747649999996, 38.91106845, 39.6103185),
  경도 = c(128.8120594, 127.128766, 129.42760, 125.1904734677795, 128.9038646, 126.53163871452773, 127.76822265000001, 127.39752017924428, 126.8409369)
)
# 데이터 확인
head(predict_data)


# 데이터 확인
head(predict_data)

coordinates_p = predict_data[,c("지점명","위도","경도")]
rownames(coordinates_p) = coordinates_p$지점명
coordinates_p = coordinates_p[,-1]
coordinates_p <- coordinates_p[,2:1]
#coordinates[,1] <- -coordinates[,1]
colnames(coordinates_p) = c("W.longitude", "N.latitude")
distant_p = dist(coordinates_p)



coordinates_o = outer_data[,c("지점명","위도","경도")]
rownames(coordinates_o) = coordinates_o$지점명
coordinates_o = coordinates_o[,-1]
coordinates_o <- coordinates_o[,2:1]
#coordinates[,1] <- -coordinates[,1]
colnames(coordinates_o) = c("W.longitude", "N.latitude")
distant_o = dist(coordinates_o)

library(maps)
par(mar=c(2,2,1,1))
map('world',region="north korea")
points(coordinates,col='blue',pch=19)
points(coordinates_p,col='red',pch=19)
points(coordinates_o,col='green',pch=19)

text(coordinates$W.longitude, coordinates$N.latitude, 
     labels = rownames(coordinates), pos = 4, cex = 0.6, col = 'blue')
text(coordinates_o$W.longitude, coordinates_o$N.latitude, 
     labels = rownames(coordinates_o), pos = 4, cex = 0.6, col = 'green')





# North Korea의 shapefile 데이터 가져오기
world <- ne_countries(scale = "medium", returnclass = "sf")
north_korea <- world[world$name == "North Korea", ]


# ggplot으로 지도 시각화
ggplot(data = north_korea) +
  geom_sf(fill = "lightgreen", color = "black") + # North Korea 지도
  geom_point(data = coordinates, aes(x = W.longitude, y = N.latitude), 
             color = "blue", size = 3) + 
  geom_text(data = coordinates, aes(x = W.longitude, y =  N.latitude, label = rownames(coordinates)), 
            hjust = -0.1, vjust = 0, size = 4, color = "black") +
  geom_point(data = coordinates_p, aes(x = W.longitude, y = N.latitude), 
             color = "red", size = 3) +
  geom_point(data = coordinates_o, aes(x = W.longitude, y = N.latitude), 
             color = "red", size = 3) +
  coord_sf(xlim = c(124, 131), ylim = c(37, 43)) + # 범위 지정
  theme_minimal() +
  labs(title = "Map of North Korea", x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("관측소" = "blue", "예측할 곳" = "red"))+theme(legend.position = "right")







##############################################################
##### data 정제 및 test/train 분리 
##############################################################

Temperature = weather_array[,,1]
Humidity = weather_array[,,2]
Cloud = weather_array[,,3]
Wind = weather_array[,,4]
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

sum(is.na(Humidity))

Humidity_filled <- as.data.frame(Humidity)

# 각 열에 대해 NA 값을 이전 및 이후 값의 평균으로 채우기
for (col in colnames(Humidity_filled)) {
  for (i in which(is.na(Humidity_filled[[col]]))) {
    # 이전 값
    prev <- ifelse(i > 1, Humidity_filled[[col]][i - 1], NA)
    # 이후 값
    nex <- ifelse(i < nrow(Humidity_filled), Humidity_filled[[col]][i + 1], NA)
    # 이전 및 이후 값의 평균 계산 (NA 제외)
    Humidity_filled[[col]][i] <- mean(c(prev, nex), na.rm = TRUE)
  }
}

sum(is.na(Humidity_filled))


sum(is.na(Cloud))

Cloud_filled <- as.data.frame(Cloud)

# 각 열에 대해 NA 값을 이전 및 이후 값의 평균으로 채우기
for (col in colnames(Cloud_filled)) {
  for (i in which(is.na(Cloud_filled[[col]]))) {
    # 이전 값
    prev <- ifelse(i > 1, Cloud_filled[[col]][i - 1], NA)
    # 이후 값
    nex <- ifelse(i < nrow(Cloud_filled), Cloud_filled[[col]][i + 1], NA)
    # 이전 및 이후 값의 평균 계산 (NA 제외)
    Cloud_filled[[col]][i] <- mean(c(prev, nex), na.rm = TRUE)
  }
}

# 결과 확인
sum(is.na(Cloud_filled))

sum(is.na(Wind))

Wind_filled <- as.data.frame(Wind)

# 각 열에 대해 NA 값을 이전 및 이후 값의 평균으로 채우기
for (col in colnames(Wind_filled)) {
  for (i in which(is.na(Wind_filled[[col]]))) {
    # 이전 값
    prev <- ifelse(i > 1, Wind_filled[[col]][i - 1], NA)
    # 이후 값
    nex <- ifelse(i < nrow(Wind_filled), Wind_filled[[col]][i + 1], NA)
    # 이전 및 이후 값의 평균 계산 (NA 제외)
    Wind_filled[[col]][i] <- mean(c(prev, nex), na.rm = TRUE)
  }
}

# 결과 확인
sum(is.na(Wind_filled))



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
Humi.26 <- Humidity_filled %>% select(-장진)
Cloud.26 <- Cloud_filled %>% select(-장진)
Wind.26 <- Wind_filled %>% select(-장진)
coord.26 <- coordinates[-i.0,]



# ggplot으로 지도 시각화
ggplot(data = north_korea) +
  geom_sf(fill = "lightgreen", color = "black") + # North Korea 지도
  geom_point(data = coordinates, aes(x = W.longitude, y = N.latitude), 
             color = "blue", size = 3) + 
  geom_text(data = coordinates, aes(x = W.longitude, y =  N.latitude, label = rownames(coordinates)), 
            hjust = -0.1, vjust = 0, size = 4, color = "black") +
  geom_point(data = coord.0, aes(x = W.longitude, y = N.latitude), 
             color = "red", size = 3) +
  coord_sf(xlim = c(124, 131), ylim = c(37, 43)) + # 범위 지정
  theme_minimal() +
  labs(title = "Map of North Korea", x = "Longitude", y = "Latitude") 



##############################################################
##### data함수형으로 바꾸기(일반, 패널티) 
##############################################################

# 온도
time = 1:2927
K <- 99
fourier.basis <- create.fourier.basis(rangeval=
                                        range(time),nbasis=K)
temp.fd.Fb.26 <- Data2fd(argvals=time, y=as.matrix(Tempe.26), 
                         basisobj=fourier.basis)
temp.fd.Fb.0 <- Data2fd(argvals=time, y=as.matrix(adjusted_temperatures[,"장진"]),basisobj=fourier.basis)


par(mar=c(4,4,1,1))
plot(temp.fd.Fb.26,col="grey",
     xlab="time",ylab="Temperature (degrees C)",
     main="Temperatures in north korea")
lines(temp.fd.Fb.0,lwd=2) 



# 습도 
time = 1:2927
K <- 99
fourier.basis <- create.fourier.basis(rangeval=
                                        range(time),nbasis=K)
humi.fd.Fb.26 <- Data2fd(argvals=time, y=as.matrix(Humi.26), 
                         basisobj=fourier.basis)
humi.fd.Fb.0 <- Data2fd(argvals=time, y=as.matrix(Humidity_filled[,"장진"]),basisobj=fourier.basis)

par(mar=c(4,4,1,1))
plot(humi.fd.Fb.26,col="grey",
     xlab="time",ylab="Humidity",
     main="Humidity in north korea")
lines(humi.fd.Fb.0,lwd=2)

# 구름  
time = 1:2927
K <- 99
fourier.basis <- create.fourier.basis(rangeval=
                                        range(time),nbasis=K)
cloud.fd.Fb.26 <- Data2fd(argvals=time, y=as.matrix(Cloud.26), 
                         basisobj=fourier.basis)
cloud.fd.Fb.0 <- Data2fd(argvals=time, y=as.matrix(Cloud_filled[,"장진"]),basisobj=fourier.basis)

par(mar=c(4,4,1,1))
plot(cloud.fd.Fb.26,col="grey",
     xlab="time",ylab="Humidity",
     main="Cloud in north korea")
lines(cloud.fd.Fb.0,lwd=2)

# 바람  
time = 1:2927
K <- 99
fourier.basis <- create.fourier.basis(rangeval=
                                        range(time),nbasis=K)
wind.fd.Fb.26 <- Data2fd(argvals=time, y=as.matrix(Wind.26), 
                          basisobj=fourier.basis)
wind.fd.Fb.0 <- Data2fd(argvals=time, y=as.matrix(Wind_filled[,"장진"]),basisobj=fourier.basis)

par(mar=c(4,4,1,1))
plot(wind.fd.Fb.26,col="grey",
     xlab="time",ylab="Wind",
     main="Wind in north korea")
lines(wind.fd.Fb.0,lwd=2)

## 4분할 그래프 
par(mfrow = c(2, 2))
plot(temp.fd.Fb.26,col="grey",
     xlab="time",ylab="Temperature (degrees C)",
     main="Temperatures in north korea")
lines(temp.fd.Fb.0,lwd=2) 

plot(humi.fd.Fb.26,col="grey",
     xlab="time",ylab="Humidity",
     main="Humidity in north korea")
lines(humi.fd.Fb.0,lwd=2)

plot(cloud.fd.Fb.26,col="grey",
     xlab="time",ylab="Humidity",
     main="Cloud in north korea")
lines(cloud.fd.Fb.0,lwd=2)
plot(wind.fd.Fb.26,col="grey",
     xlab="time",ylab="Wind",
     main="Wind in north korea")
lines(wind.fd.Fb.0,lwd=2)
##############################################################
##### empirical variogram 그리기 및 fitting 시키기 
##############################################################


# computing L2 norms between functions, 
#  using Fourier basis expansions
# Note if the basis is not orthogonal, 
#  then a weight matrix is needed.
L2norm.Fb.26 <- dist(t(temp.fd.Fb.26$coefs))^2

# Calculating the empiricial trace bin variogram
# Fitted variogram assumes an exponential covariance.
emp.trace.vari.26 <- trace.variog(coords=coord.26, 
                                  L2norm=as.matrix(L2norm.Fb.26), bin=TRUE)
# fitting an  guassian
sigma2.0 <- quantile(emp.trace.vari.26$v, 0.75)
phi.0    <- quantile(emp.trace.vari.26$Eu.d, 0.25)
fit.vari.26.gau <- variofit(emp.trace.vari.26, 
                        ini.cov.pars=c(sigma2.0,phi.0),cov.model="gaussian")
fit.vari.26.gau

# fitting an  exponential
sigma2.0 <- quantile(emp.trace.vari.26$v, 0.25)
phi.0    <- quantile(emp.trace.vari.26$Eu.d, 0.25)
fit.vari.26.exp <- variofit(emp.trace.vari.26, 
                        ini.cov.pars=c(sigma2.0,phi.0),cov.model="exponential")
fit.vari.26.exp

# fitting an  spherical
sigma2.0 <- quantile(emp.trace.vari.26$v, 0.75)
phi.0    <- quantile(emp.trace.vari.26$Eu.d, 0.5)
fit.vari.26.sph <- variofit(emp.trace.vari.26, 
                            ini.cov.pars=c(sigma2.0,phi.0),cov.model="spherical")
fit.vari.26.sph


fit.vari.26.gau <- variofit(emp.trace.vari.26, 
                            ini.cov.pars=c(sigma2.0,phi.0),cov.model="gaussian")
fit.vari.26.exp <- variofit(emp.trace.vari.26, 
                            ini.cov.pars=c(sigma2.0,phi.0),cov.model="exponential")
fit.vari.26.sph <- variofit(emp.trace.vari.26, 
                            ini.cov.pars=c(sigma2.0,phi.0),cov.model="spherical")






par(mfrow = c(1, 1))
plot(as.dist(emp.trace.vari.26$Eu.d),L2norm.Fb.26,col="grey",
     xlab="Geographical distances", ylab="L2 distances",
     main="Empirical variogram",xlim=c(0,3))
points(emp.trace.vari.26$u,emp.trace.vari.26$v,col="black",pch
       =19)
lines(fit.vari.26.gau,col="black",lwd=2)
lines(fit.vari.26.exp, col="blue", lwd=2)  # Exponential
lines(fit.vari.26.sph, col="red", lwd=2)
legend("topleft", c("Exponential", "Gaussian","spherical"),
       col=c("blue", "black","red"), lwd=2)


##############################################################
##### weight값 구하고 그려보기 
##############################################################


geo.dist.27 <- dist(coordinates) 
######## gaussian
hat.C.26.gau <- cov.spatial(emp.trace.vari.26$Eu.d,
                        cov.model= fit.vari.26.gau$cov.model,
                        cov.pars=fit.vari.26.gau$cov.pars)
geo.dist.0.26 <- as.matrix(geo.dist.27)[-i.0,i.0]
hat.C.0.gau <- cov.spatial(geo.dist.0.26,
                       cov.model= fit.vari.26.gau$cov.model,
                       cov.pars=fit.vari.26.gau$cov.pars)
# kriging weights
w.k.g <- solve(hat.C.26.gau,hat.C.0.gau)
# mean est weights
inv.hat.C.26.gau <- solve(hat.C.26.gau)
ones<-matrix(1,nrow=26,ncol=1)
w.m.g <- inv.hat.C.26.gau%*%ones 
w.m.g<- w.m.g/c(t(ones)%*%inv.hat.C.26.gau%*%ones) 
sum(w.m.g); sum(w.k.g)

######### exp 
hat.C.26.exp <- cov.spatial(emp.trace.vari.26$Eu.d,
                            cov.model= fit.vari.26.exp$cov.model,
                            cov.pars=fit.vari.26.exp$cov.pars)
geo.dist.0.26 <- as.matrix(geo.dist.27)[-i.0,i.0]
hat.C.0.exp <- cov.spatial(geo.dist.0.26,
                           cov.model= fit.vari.26.exp$cov.model,
                           cov.pars=fit.vari.26.exp$cov.pars)
# kriging weights
w.k.e <- solve(hat.C.26.exp,hat.C.0.exp)
# mean est weights
inv.hat.C.26.exp <- solve(hat.C.26.exp)
ones<-matrix(1,nrow=26,ncol=1)
w.m.e <- inv.hat.C.26.exp%*%ones 
w.m.e<- w.m.e/c(t(ones)%*%inv.hat.C.26.exp%*%ones) 
sum(w.m.e); sum(w.k.e)

######### sph
hat.C.26.sph <- cov.spatial(emp.trace.vari.26$Eu.d,
                            cov.model= fit.vari.26.sph$cov.model,
                            cov.pars=fit.vari.26.sph$cov.pars)
geo.dist.0.26 <- as.matrix(geo.dist.27)[-i.0,i.0]
hat.C.0.sph <- cov.spatial(geo.dist.0.26,
                           cov.model= fit.vari.26.sph$cov.model,
                           cov.pars=fit.vari.26.sph$cov.pars)
# kriging weights
w.k.s <- solve(hat.C.26.sph,hat.C.0.sph)
# mean est weights
inv.hat.C.26.sph <- solve(hat.C.26.sph)
ones<-matrix(1,nrow=26,ncol=1)
w.m.s <- inv.hat.C.26.sph%*%ones 
w.m.s<- w.m.s/c(t(ones)%*%inv.hat.C.26.sph%*%ones) 
sum(w.m.s); sum(w.k.s)




# weight 그래프 그림 
par(mfrow = c(1, 3))
plot(geo.dist.0.26,w.k.g, main = "Kriging weights with Gaussian")
abline(h=0,lty=2)
plot(geo.dist.0.26,w.k.e, main = "Kriging weights with exponential")
abline(h=0,lty=2)
plot(geo.dist.0.26,w.k.s, main = "Kriging weights with Spherical")
abline(h=0,lty=2)






# mean function estimation 한 그래프 그림

# gaussian
par(mfrow = c(1, 3))
par(mar=c(2,2,2,2))
mean_coef.g<-temp.fd.Fb.26$coefs%*%w.m.g
mean_f.g<-fd(coef=mean_coef.g,basis=fourier.basis)
plot(temp.fd.Fb.26,col="grey",main = "mean estimation with gaussian")
plot(mean_f.g,lwd=2,add=TRUE,col = "blue")


# exponential
par(mar=c(2,2,2,2)) 
mean_coef.e<-temp.fd.Fb.26$coefs%*%w.m.e
mean_f.e<-fd(coef=mean_coef.e,basis=fourier.basis)
plot(temp.fd.Fb.26,col="grey",main = "mean estimation with exponential")
plot(mean_f.e,lwd=2,add=TRUE, col = "blue")


# sphrical
par(mar=c(2,2,2,2))
mean_coef.s<-temp.fd.Fb.26$coefs%*%w.m.s
mean_f.s<-fd(coef=mean_coef.s,basis=fourier.basis)
plot(temp.fd.Fb.26,col="grey",main = "mean estimation with spherical")
plot(mean_f.s,lwd=2,add=TRUE, col = "blue")


########################################################
######## 예측 
########################################################

par(mfrow = c(1, 3))

##### gaussian 
a.g = 1 - sum(w.k.g)
tmp_coef.g <- temp.fd.Fb.26$coef%*%w.k.g
tmp_fun.g <- fd(coef=tmp_coef.g,basis=fourier.basis)
Xhat.g<-a.g*mean_f.g + tmp_fun.g # 예측값

par(mar=c(2,2,2,2))
plot(temp.fd.Fb.26,col="grey",main = "Predicted Temp with gaussian")
plot(temp.fd.Fb.0,add=TRUE,lwd=2) # 실제
plot(Xhat.g,add=TRUE,lty=2,lwd=2,col="red") # 예측된 곳


##### exponential
a.e = 1 - sum(w.k.e)
tmp_coef.e <- temp.fd.Fb.26$coef%*%w.k.e
tmp_fun.e <- fd(coef=tmp_coef.e,basis=fourier.basis)
Xhat.e<-a.e*mean_f.e + tmp_fun.e # 예측값

par(mar=c(2,2,2,2))
plot(temp.fd.Fb.26,col="grey",main = "Predicted Temp with exponential")
plot(temp.fd.Fb.0,add=TRUE,lwd=2) # 실제
plot(Xhat.e,add=TRUE,lty=2,lwd=2,col="red") # 예측된 곳


##### spherical model 
a.s = 1 - sum(w.k.s)
tmp_coef.s <- temp.fd.Fb.26$coef%*%w.k.s
tmp_fun.s <- fd(coef=tmp_coef.s,basis=fourier.basis)
Xhat.s<-a.s*mean_f.s + tmp_fun.s # 예측값

par(mar=c(2,2,2,2))
plot(temp.fd.Fb.26,col="grey",main = "Predicted Temp with Spherical")
plot(temp.fd.Fb.0,add=TRUE,lwd=2) # 실제
plot(Xhat.s,add=TRUE,lty=2,lwd=2,col="red") # 예측된 곳




# 예측된 결과에 rmse 구하기

compute_rmse <- function(actual_fd, predicted_fd, eval_points = seq(1, 2927, length.out = 100)) {
  # Evaluate the actual and predicted functional data on a grid of points
  actual_values <- eval.fd(eval_points, actual_fd)
  predicted_values <- eval.fd(eval_points, predicted_fd)
  
  # Compute RMSE
  sqrt(mean((actual_values - predicted_values)^2))
}


rmse.g <- compute_rmse(temp.fd.Fb.0, Xhat.g)
rmse.e <- compute_rmse(temp.fd.Fb.0, Xhat.e)
rmse.s <- compute_rmse(temp.fd.Fb.0, Xhat.s)


rmse.g
rmse.e
rmse.s






























