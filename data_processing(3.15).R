
#패키지 ----
library(tidyverse)
library(ggplot2)
library(lubridate)

# Data screening -----
med1 <- read_csv("MEDICINE/MEDICINE (1).csv") %>% 
  group_by(WK_COMPN_4, USE_YEAR, USE_MONTH) %>% 
  summarise(pres = sum(PRSCRPTN_TNDN_CNT),
            pt = sum(PATIENT_CNT))

# 반복 작업을 위한 함수 정의
process_med_file <- function(file_path) {
  med_df <- read_csv(file_path)
  result <- med_df %>%
    mutate(USE_YEAR = factor(USE_YEAR),
           USE_MONTH = factor(USE_MONTH)) %>% 
    group_by(WK_COMPN_4, USE_YEAR, USE_MONTH) %>%
    summarise(pres = sum(PRSCRPTN_TNDN_CNT),
              pt = sum(PATIENT_CNT))
  return(result)
} #pres는 처방수, pt는 환자수


# 결과를 저장할 리스트 생성
MEDICINE_list <- list()

# MEDINE(1)부터 MEDINE(70)까지의 파일에 대해 반복 작업 수행
for (i in 1:70) {
       file_path <- paste0("MEDICINE/MEDICINE (", i, ").csv")
    if(!file.exists(file_path)){next}
    result <- process_med_file(file_path)
    MEDICINE_list[[i]] <- result
}

# 결과를 하나의 데이터프레임으로 병합
MEDICINE_result <- do.call(rbind, MEDICINE_list)

# 결과 확인
print(MEDICINE_result)


#다른 변수들

##환자 성별



#개별 약제 추세----
# 1007 - acebrophylline
data_1007<- MEDICINE_result %>% 
  filter(WK_COMPN_4 =="1007")

quet.ts <- ts(data_1007[,5], frequency=12, start=c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2013.5, col="gray", lty="dashed", lwd=2)

# 1009 - aceclofenac
data_1009<- MEDICINE_result %>% 
  filter(WK_COMPN_4 =="1009")

quet.ts <- ts(data_1009[,5], frequency=12, start=c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2013.5, col="gray", lty="dashed", lwd=2)

# 1013 - acteaminophen
data_1013<- MEDICINE_result %>% 
  filter(WK_COMPN_4 =="1013")

quet.ts <- ts(data_1013[,5], frequency=12, start=c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2013.5, col="gray", lty="dashed", lwd=2)

# 1226 - candesartan
data_1226 <- MEDICINE_result %>% 
  filter(WK_COMPN_4 =="1226")

quet.ts <- ts(data_1226[,5], frequency=12, start=c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2013.5, col="gray", lty="dashed", lwd=2)

# 1115 - atorvastatin
data_1115 <- MEDICINE_result %>% 
  filter(WK_COMPN_4 =="1115")

quet.ts <- ts(data_1115[,5], frequency=12, start=c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2013.5, col="gray", lty="dashed", lwd=2)

# 1081 - atorvastatin
data_1081 <- MEDICINE_result %>% 
  filter(WK_COMPN_4 =="1081")

quet.ts <- ts(data_1081[,5], frequency=12, start=c(2018,1))

# 1279 - atorvastatin
data_1279 <- MEDICINE_result %>% 
  filter(WK_COMPN_4 =="1279")

quet.ts2 <- ts(data_1279[,5], frequency=12, start=c(2018,1))

quet.ts2

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,1000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts2, ylim=c(0,1000000), type='l', col="red", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2013.5, col="gray", lty="dashed", lwd=2)

# Medication name ----
medi_inform <- read_csv("MEDICINE/MEDICINE_CODE_240301/medi.csv") #data loading

medi_inform <- medi_inform %>% 
  mutate(WK_COMPN_4 = substr(jo,1,4)) %>% 
  select(-jo)

length(unique(medi_inform$WK_COMPN_4))

# jo에서 중복된 번호를 제거 - 2251개
medi_inform <- medi_inform[-which(duplicated(medi_inform$WK_COMPN_4)), ]

medi_inform <- medi_inform %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))

# left join
MEDICINE_result <- MEDICINE_result %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))
  
medicine_result2 <- left_join(MEDICINE_result, medi_inform, by='WK_COMPN_4')

head(medicine_result2$pt)

colSums(is.na(medicine_result2)) #결측치 확인(과거에 사용되었지만 현재 사라진 약제)

###결측치 검사(없어진 약제들) -> 날려버리기(omit을 사용해도 됨, 어차피 같은 열에서 결측치가 있으니까)
medicine_result2 <- na.omit(medicine_result2)

colSums(is.na(medicine_result2))


## class별 약품데이터 정리 ----
#clsaa별로 의약품들 합치기
medicine_result3 <- medicine_result2 %>% 
  group_by(class, USE_YEAR, USE_MONTH) %>% 
  summarise(class_pres = sum(pres),
            class_pt = sum(pt))

#날짜 만들기
medicine_result3$date = paste0(medicine_result3$USE_YEAR,medicine_result3$USE_MONTH)
medicine_result3$date <- as.Date(paste(as.character(medicine_result3$date), '01'), format='%Y%m%d')


###3/14 2차 미팅----
# 214 (혈압강하제)
class_214<- final_result3 %>% 
  filter(class =="214")

quet.ts <- ts(class_214[,4], frequency=12, start=c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2013.5, col="gray", lty="dashed", lwd=2)

# 611 (주로 그람양성균에 작용하는 것)
class_611<- final_result3 %>% 
  filter(class =="611")

quet.ts <- ts(class_611[,4], frequency=12, start=c(2018,1))

quet.ts

class_612<- final_result3 %>% 
  filter(class =="612")

quet.ts2 <- ts(class_612[,4], frequency=12, start=c(2018,1))

quet.ts2

class_614<- final_result3 %>% 
  filter(class =="614")

quet.ts3 <- ts(class_614[,4], frequency=12, start=c(2018,1))

quet.ts3

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,1000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts2, ylim=c(0,1000000), type='l', col="red", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts3, ylim=c(0,1000000), type='l', col="green", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020.2, col="gray", lty="dashed", lwd=2)


### 3/20 class 600대 약제----
{
class_600 <- medicine_result3 %>% 
  filter(class >= "600", class <= "700") %>% 
  group_by(date) %>% 
  summarise(class_pres = sum(class_pres), class_pt = sum(class_pt))

quet.ts <- ts(class_600[,3],frequency = 12,start = c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)

###환자수 그래프
quet.ts2 <- ts(class_600[,4],frequency = 12, start = c(2018,1))

quet.ts2

options(scipen = 5)
plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기

abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가

###처방건수, 환자수 같이 그리기

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)
}

#class 610 항생물질제제
#- 기존 방식으로 그래프를 그리는 것에서 오류가 발생함, 어쭤볼 것

{
  class_610 <- medicine_result3 %>% 
    filter(class >= "610", class <= "619") %>% 
    group_by(date) %>% 
    summarise(class_pres = sum(class_pres), class_pt = sum(class_pt))
  

###처방건수 그래프
quet.ts <- ts(class_610[,3],frequency = 12,start = c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)

###환자수 그래프
quet.ts2 <- ts(class_610[,4],frequency = 12, start = c(2018,1))

quet.ts2

options(scipen = 5)
plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기

abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가

###처방건수, 환자수 같이 그리기

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)
} #기존방식(노가다)



# See the figure of attempt suicide 
ggplot(data=class_600, aes(x = date, y = class_pt)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_point(data=class_610,aes(x = date, y = class_pt), color = "blue", size=1.5) +
  geom_line(data=class_610,aes(x = date, y = class_pt), color = "blue") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,20000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


##class 611 주로 그람양성균에 작용하는 것
{
  class_611 <- medicine_result3 %>% 
  filter(class == "611")

###처방건수 그래프
quet.ts <- ts(class_611[,4],frequency = 12,start = c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)

###환자수 그래프
quet.ts2 <- ts(class_611[,5],frequency = 12, start = c(2018,1))

quet.ts2

options(scipen = 5)
plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기

abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가

###처방건수, 환자수 같이 그리기

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,1000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts2, ylim=c(0,1000000), type='l', col="red", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)
}

##class 612 주로 그람음성균에 작용하는 것
{
  class_612 <- medicine_result3 %>% 
  filter(class=="612")

###처방건수 그래프
quet.ts <- ts(class_612[,4],frequency = 12,start = c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)

###환자수 그래프
quet.ts2 <- ts(class_612[,5],frequency = 12, start = c(2018,1))

quet.ts2

options(scipen = 5)
plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기

abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가

###처방건수, 환자수 같이 그리기

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,1000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts2, ylim=c(0,1000000), type='l', col="red", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)
}


## class 613 주로 항산성균에 작용하는 것
#- 데이터가 없음
{
  class_613 <- medicine_result3 %>% 
    filter(class == "613")
  
  ###처방건수 그래프
  quet.ts <- ts(class_613[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_613[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,1000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,1000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

## class 614 주로 그람양성균, 리케치아, 비루스에 작용하는 것
{
  class_614 <- medicine_result3 %>% 
    filter(class == "614")
  
  ###처방건수 그래프
  quet.ts <- ts(class_614[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_614[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,1000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,1000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}


## class 615 주로 그람음성균, 리케치아, 비루스에 작용하는 것
{
class_615 <- medicine_result3 %>% 
  filter(class == "615")

###처방건수 그래프
quet.ts <- ts(class_615[,4],frequency = 12,start = c(2018,1))

quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)

###환자수 그래프
quet.ts2 <- ts(class_615[,5],frequency = 12, start = c(2018,1))

quet.ts2

options(scipen = 5)
plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기

abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가

###처방건수, 환자수 같이 그리기

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,300000), type='l', col="blue", xlab="Month", ylab="Dispensings")
par(new=TRUE)
plot(quet.ts2, ylim=c(0,300000), type='l', col="red", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020, col="gray", lty="dashed", lwd=2)
}

## class 616 주로 곰팡이, 원충에 작용하는 것 
#- 23년 10월에 사용되었다 말고는 데이터가 없음
{
  class_616 <- medicine_result3 %>% 
    filter(class == "616")
  
  ###처방건수 그래프
  quet.ts <- ts(class_616[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_616[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,300000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,300000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}


## class 617 주로 악성종양에 작용하는 것 
#- 처방건수,환자수 같이 있는 그래프 그릴 때, y축 scale을 일부 생략하고 그릴 수는 없을 까?
{
  class_617 <- medicine_result3 %>% 
    filter(class == "617")
  
  ###처방건수 그래프
  quet.ts <- ts(class_617[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_617[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,10000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,10000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

## class 618 주로 그람양성, 음성균에 작용하는 것
#- 2023년 10월 데이터가 존재함, 아직 완벽한 10월 데이터가 되지 않아서 추후 새로이 데이터 업로드 될 경우, 다시 돌리든가 해야할듯
{
  class_618 <- medicine_result3 %>% 
    filter(class == "618")
  
  ###처방건수 그래프
  quet.ts <- ts(class_618[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_618[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}


## class 619기타의 항생물질 제제(복합항생물질제제를 포함)
#-2023 10월 데이터 포함
{
  class_619 <- medicine_result3 %>% 
    filter(class == "619")
  
  ###처방건수 그래프
  quet.ts <- ts(class_619[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_619[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,2000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,2000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

# class 611~619 개별 그래프 모임
ggplot(data=class_611, aes(x = date, y = class_pt)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_point(data=class_612,aes(x = date, y = class_pt), color = "blue", size=1.5) +
  geom_line(data=class_612,aes(x = date, y = class_pt), color = "blue") +
  geom_point(data=class_614,aes(x = date, y = class_pt), color = "red", size=1.5) +
  geom_line(data=class_614,aes(x = date, y = class_pt), color = "red") +
  geom_point(data=class_615,aes(x = date, y = class_pt), color = "green", size=1.5) +
  geom_line(data=class_615,aes(x = date, y = class_pt), color = "green") +
  geom_point(data=class_617,aes(x = date, y = class_pt), color = "gray", size=1.5) +
  geom_line(data=class_617,aes(x = date, y = class_pt), color = "gray") +
  geom_point(data=class_619,aes(x = date, y = class_pt), color = "purple", size=1.5) +
  geom_line(data=class_619,aes(x = date, y = class_pt), color = "purple") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,2000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


ggplot(data=class_611, aes(x = date, y = class_pt)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_point(data=class_612,aes(x = date, y = class_pt), color = "blue", size=1.5) +
  geom_line(data=class_612,aes(x = date, y = class_pt), color = "blue") +
  geom_point(data=class_615,aes(x = date, y = class_pt), color = "green", size=1.5) +
  geom_line(data=class_615,aes(x = date, y = class_pt), color = "green") +
  geom_point(data=class_617,aes(x = date, y = class_pt), color = "gray", size=1.5) +
  geom_line(data=class_617,aes(x = date, y = class_pt), color = "gray") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,600000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

ggplot(data=class_611, aes(x = date, y = class_pt)) + 
  geom_point(data=class_615,aes(x = date, y = class_pt), color = "green", size=1.5) +
  geom_line(data=class_615,aes(x = date, y = class_pt), color = "green") +
  geom_point(data=class_617,aes(x = date, y = class_pt), color = "gray", size=1.5) +
  geom_line(data=class_617,aes(x = date, y = class_pt), color = "gray") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,200000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


ggplot(data=class_611, aes(x = date, y = class_pt)) + 
  geom_point(data=class_617,aes(x = date, y = class_pt), color = "gray", size=1.5) +
  geom_line(data=class_617,aes(x = date, y = class_pt), color = "gray") +

  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,10000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())



ggplot(data=class_611, aes(x = date, y = class_pres)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_point(data=class_612,aes(x = date, y = class_pres), color = "blue", size=1.5) +
  geom_line(data=class_612,aes(x = date, y = class_pres), color = "blue") +
  geom_point(data=class_614,aes(x = date, y = class_pres), color = "red", size=1.5) +
  geom_line(data=class_614,aes(x = date, y = class_pres), color = "red") +
  geom_point(data=class_615,aes(x = date, y = class_pres), color = "green", size=1.5) +
  geom_line(data=class_615,aes(x = date, y = class_pres), color = "green") +
  geom_point(data=class_617,aes(x = date, y = class_pres), color = "gray", size=1.5) +
  geom_line(data=class_617,aes(x = date, y = class_pres), color = "gray") +
  geom_point(data=class_619,aes(x = date, y = class_pres), color = "purple", size=1.5) +
  geom_line(data=class_619,aes(x = date, y = class_pres), color = "purple") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,2000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())



# class 620 화학료법제
{
  class_620 <- medicine_result3 %>% 
    filter(class >= "620", class <= "629") %>% 
    group_by(USE_YEAR,USE_MONTH) %>% 
    summarise(class_pres = sum(class_pres), class_pt = sum(class_pt))
  
  
  ###처방건수 그래프
  quet.ts <- ts(class_620[,3],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_620[,4],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,5000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,5000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}


## class 621 설화제
#-2018.1~2021.3월 데이터밖에 없음
{
  class_621 <- medicine_result3 %>% 
    filter(class == "621")
  
  ###처방건수 그래프
  quet.ts <- ts(class_621[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_621[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,100000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,100000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

## class 622 항결핵제
{
  class_622 <- medicine_result3 %>% 
    filter(class == "622")
  
  ###처방건수 그래프
  quet.ts <- ts(class_622[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_622[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

## class 623 치나제
#-데이터 없음
{
  class_623 <- medicine_result3 %>% 
    filter(class == "623")
  
  ###처방건수 그래프
  quet.ts <- ts(class_623[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_623[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 624 구매제
#-데이터 없음
{
  class_624 <- medicine_result3 %>% 
    filter(class == "624")
  
  ###처방건수 그래프
  quet.ts <- ts(class_624[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_624[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 625 후란계 제제
#-데이터 없음
{
  class_625 <- medicine_result3 %>% 
    filter(class == "625")
  
  ###처방건수 그래프
  quet.ts <- ts(class_625[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_625[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 629 기타의 화학료법제
{
  class_629 <- medicine_result3 %>% 
    filter(class == "629")
  
  ###처방건수 그래프
  quet.ts <- ts(class_629[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_629[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,5000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,5000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

ggplot(data=class_629, aes(x = date, y = class_pt)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,5000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())



# class 630 생물학적제제

## class 631 백신류
#-데이터 없음
{
  class_631 <- medicine_result3 %>% 
    filter(class == "631")
  
  ###처방건수 그래프
  quet.ts <- ts(class_631[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_631[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 632 독소류 및 톡소이드류
{
  class_632 <- medicine_result3 %>% 
    filter(class == "632")
  
  ###처방건수 그래프
  quet.ts <- ts(class_632[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_632[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,2000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,2000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 633 항독소 및 렙토스피라혈청류
#-데이터 없음
{
  class_633 <- medicine_result3 %>% 
    filter(class == "633")
  
  ###처방건수 그래프
  quet.ts <- ts(class_633[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_633[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 634 혈액제제류
#-데이터 없음
{
  class_634 <- medicine_result3 %>% 
    filter(class == "634")
  
  ###처방건수 그래프
  quet.ts <- ts(class_634[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_634[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 635 생물학적 시험용제제류
#-데이터 없음
{
  class_635 <- medicine_result3 %>% 
    filter(class == "635")
  
  ###처방건수 그래프
  quet.ts <- ts(class_635[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_635[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 636 생물학적 제제
#-데이터 없음
{
  class_636 <- medicine_result3 %>% 
    filter(class == "636")
  
  ###처방건수 그래프
  quet.ts <- ts(class_636[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_636[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}
## class 639 기타의 생물학적 제제
#-23.10월 데이터 말고 없음
{
  class_639 <- medicine_result3 %>% 
    filter(class == "639")
  
  ###처방건수 그래프
  quet.ts <- ts(class_639[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_639[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

# class 640 기생동물에 대한 약품
{
  class_640 <- medicine_result3 %>% 
    filter(class >= "640", class <= "690") %>% 
    group_by(USE_YEAR,USE_MONTH) %>% 
    summarise(class_pres = sum(class_pres), class_pt = sum(class_pt))
  
  
  ###처방건수 그래프
  quet.ts <- ts(class_640[,3],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_640[,4],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,1000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,1000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

## class 641 항원충제
{
  class_641 <- medicine_result3 %>% 
    filter(class == "641")
  
  ###처방건수 그래프
  quet.ts <- ts(class_641[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_641[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,500000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,500000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

ggplot(data=class_641, aes(x = date, y = class_pt)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="Incidence rate",limits = c(0,500000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

## class 642 구충제
#-18.1~19.4,20.5~21.12,22.7~23.9까지만 데이터가 있음
#-그렸지만 이상함
{
  class_642 <- medicine_result3 %>% 
    filter(class == "642")
  
  ###처방건수 그래프
  quet.ts <- ts(class_642[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_642[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,20000000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,20000000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

# class_642 데이터 로드
class_642 <- medicine_result3 %>% 
  filter(class == "642")


# 'USE_YEAR'와 'USE_MONTH' 열을 숫자로 변환
class_642$USE_YEAR <- as.numeric(class_642$USE_YEAR)
class_642$USE_MONTH <- as.numeric(class_642$USE_MONTH)

# 날짜 생성
class_642$date <- make_date(year = class_642$USE_YEAR, month = class_642$USE_MONTH)

# 변환된 데이터 확인
print(class_642$date)


# 데이터 정리
class_642$date <- ymd(class_642$date)  # 날짜 형식으로 변환

# 시계열 그래프 그리기
ggplot(class_642, aes(x = date, y =class_pres)) +
  geom_line(color = "blue") +
  labs(title = "처방 건수 시계열 그래프", x = "날짜짜", y = "처방 건수") +
  theme_minimal()


## class 649 기타의 기생동물에 대한 의약품
#-데이터 없음
{
  class_649 <- medicine_result3 %>% 
    filter(class == "649")
  
  ###처방건수 그래프
  quet.ts <- ts(class_649[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_649[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,500000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,500000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}

## class 690 기타의 병원생물에 대한 의약품
#-데이터 없음
{
  class_690 <- medicine_result3 %>% 
    filter(class == "690")
  
  ###처방건수 그래프
  quet.ts <- ts(class_690[,4],frequency = 12,start = c(2018,1))
  
  quet.ts
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, type='l', col="blue", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
  
  ###환자수 그래프
  quet.ts2 <- ts(class_690[,5],frequency = 12, start = c(2018,1))
  
  quet.ts2
  
  options(scipen = 5)
  plot(quet.ts2,type='l',col="red",xlab="Month",ylab="Dispensings") #그래프 그리기
  
  abline(v=2020,col="gray",lty="dashed",lwd=2) #구분선 추가
  
  ###처방건수, 환자수 같이 그리기
  
  # Plot data to visualise time series
  options(scipen=5)
  plot(quet.ts, ylim=c(0,500000), type='l', col="blue", xlab="Month", ylab="Dispensings")
  par(new=TRUE)
  plot(quet.ts2, ylim=c(0,500000), type='l', col="red", xlab="Month", ylab="Dispensings")
  # Add vertical line indicating date of intervention (January 1, 2014)
  abline(v=2020, col="gray", lty="dashed", lwd=2)
}


### 4/8 성분별 분류 ----

#항생제 분류(class,jo,prod,category)
medi_inform_anti <- read.csv("filtered_class_antibiotics.csv") %>% 
  mutate(WK_COMPN_4 = jo) %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4)) %>% 
  select(-jo)


medicine_result_anti <- left_join(MEDICINE_result, medi_inform_anti, by='WK_COMPN_4')

colSums(is.na(medicine_result_anti)) #항생제가 아닌 약제 제거(class 611~619)
medicine_result_anti <- na.omit(medicine_result_anti)

colSums(is.na(medicine_result_anti))
head(medicine_result_anti$pt)
  
#category로 합치기
medicine_result_anti2 <- medicine_result_anti %>% 
  group_by(USE_YEAR,USE_MONTH,category) %>% 
  summarise(category_pres = sum(pres), category_pt = sum(pt))

#날짜 만들기
medicine_result_anti2$date = paste0(medicine_result_anti2$USE_YEAR,medicine_result_anti2$USE_MONTH)
medicine_result_anti2$date <- as.Date(paste(as.character(medicine_result_anti2$date), '01'), format='%Y%m%d')

###각 성분별 분류
{
  ##penicillin
peni <- medicine_result_anti2 %>% 
  filter(category == "Penicillins")

##Cephalosporins
cepha <- medicine_result_anti2 %>% 
  filter(category == "Cephalosporins")

##Tetracyclines
tetra <- medicine_result_anti2 %>% 
  filter(category == "Tetracyclines")

##Macrolides
macro <- medicine_result_anti2 %>% 
  filter(category == "Macrolides")

##Glycopeptides
glyco <- medicine_result_anti2 %>% 
  filter(category == "Glycopeptides")

##Aminoglycosides
amino <- medicine_result_anti2 %>% 
  filter(category == "Aminoglycosides")

##Amphenicols
amphe <- medicine_result_anti2 %>% 
  filter(category == "Amphenicols")

##Ketolides
keto <- medicine_result_anti2 %>% 
  filter(category == "Ketolides")

##other
other <- medicine_result_anti2 %>% 
  filter(category == "Other")
}


####그래프 - pt ----
options(scipen = 5) #과학적 스케일 적용

#성분별 그래프
##penicillins
ggplot(data=peni, aes(x = date, y = category_pt)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1500000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


##Cephalosporins
ggplot(data=cepha, aes(x = date, y = category_pt)) + 
  geom_point(color="red", size=1.5) +
  geom_line(color="red") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,6000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Tetracyclines
ggplot(data=tetra, aes(x = date, y = category_pt)) + 
  geom_point(color="blue", size=1.5) +
  geom_line(color="blue") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,250000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


##Macrolides
ggplot(data=macro, aes(x = date, y = category_pt)) + 
  geom_point(color="green", size=1.5) +
  geom_line(color="green") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,3000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Glycopeptides
ggplot(data=glyco, aes(x = date, y = category_pt)) + 
  geom_point(color="pink", size=1.5) +
  geom_line(color="pink") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,30000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Aminoglycosides
ggplot(data=amino, aes(x = date, y = category_pt)) + 
  geom_point(color="purple", size=1.5) +
  geom_line(color="purple") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Amphenicols
ggplot(data=amphe, aes(x = date, y = category_pt)) + 
  geom_point(color="gray", size=1.5) +
  geom_line(color="gray") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Ketolides
ggplot(data=keto, aes(x = date, y = category_pt)) + 
  geom_point(color="brown", size=1.5) +
  geom_line(color="brown") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,500000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##other
ggplot(data=other, aes(x = date, y = category_pt)) + 
  geom_point(color="orange", size=1.5) +
  geom_line(color="orange") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,50000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

ggplot(data=other, aes(x = date, y = category_pt)) + 
  geom_point(color="orange", size=1.5) +
  geom_line(color="orange") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,10000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())



####그래프 - pres ----
options(scipen = 5) #과학적 스케일 적용

#성분별 그래프
##penicillins
ggplot(data=peni, aes(x = date, y = category_pres)) + 
    geom_point(color="black", size=1.5) +
    geom_line(color="black") +
    geom_hline(yintercept=0, linetype="solid") +
    geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
    #
    scale_y_continuous(name="pres",limits = c(0,2000000)) +
    scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
    #
    theme_gray(20)+
    theme(panel.background=element_blank(),
          panel.grid.major=element_line(color="grey90", size=0.5),
          axis.ticks=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          plot.title=element_text(size=rel(1.0)),
          legend.title=element_blank())


##Cephalosporins
ggplot(data=cepha, aes(x = date, y = category_pres)) + 
  geom_point(color="red", size=1.5) +
  geom_line(color="red") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,6000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Tetracyclines
ggplot(data=tetra, aes(x = date, y = category_pres)) + 
  geom_point(color="blue", size=1.5) +
  geom_line(color="blue") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,300000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


##Macrolides
ggplot(data=macro, aes(x = date, y = category_pres)) + 
  geom_point(color="green", size=1.5) +
  geom_line(color="green") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,3000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Glycopeptides
ggplot(data=glyco, aes(x = date, y = category_pres)) + 
  geom_point(color="pink", size=1.5) +
  geom_line(color="pink") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,30000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Aminoglycosides
ggplot(data=amino, aes(x = date, y = category_pres)) + 
  geom_point(color="purple", size=1.5) +
  geom_line(color="purple") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1100000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Amphenicols
ggplot(data=amphe, aes(x = date, y = category_pres)) + 
  geom_point(color="gray", size=1.5) +
  geom_line(color="gray") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##Ketolides
ggplot(data=keto, aes(x = date, y = category_pres)) + 
  geom_point(color="brown", size=1.5) +
  geom_line(color="brown") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,500000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##other
ggplot(data=other, aes(x = date, y = category_pres)) + 
  geom_point(color="orange", size=1.5) +
  geom_line(color="orange") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,50000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-01'))) +
  #
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())







