
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
  ##total
total <- medicine_result_anti %>% 
    group_by(USE_YEAR,USE_MONTH) %>% 
    summarise(category_pres = sum(pres), category_pt = sum(pt))
  
  #날짜 만들기
  total$date = paste0(total$USE_YEAR,total$USE_MONTH)
  total$date <- as.Date(paste(as.character(total$date), '01'), format='%Y%m%d')
  
  
    
    
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
##전체
ggplot(data=peni, aes(x = date, y = category_pt)) + 
  geom_point(aes(color="Black"), size=1.5) +
  geom_line(aes(color="Black")) +
  geom_point(data=cepha, aes(x = date, y = category_pt, color = "Blue"), size=1.5) +
  geom_line(data=cepha, aes(x = date, y = category_pt, color = "Blue")) +
  geom_point(data=tetra, aes(x = date, y = category_pt, color = "Red"), size=1.5) +
  geom_line(data=tetra, aes(x = date, y = category_pt, color = "Red")) +
  geom_point(data=amino, aes(x = date, y = category_pt, color = "Green"), size=1.5) +
  geom_line(data=amino, aes(x = date, y = category_pt, color = "Green")) +
  geom_point(data=amphe, aes(x = date, y = category_pt, color = "Purple"), size=1.5) +
  geom_line(data=amphe, aes(x = date, y = category_pt, color = "Purple")) +
  geom_point(data=glyco, aes(x = date, y = category_pt, color = "Brown"), size=1.5) +
  geom_line(data=glyco, aes(x = date, y = category_pt, color = "Brown")) +
  geom_point(data=keto, aes(x = date, y = category_pt, color = "Gray"), size=1.5) +
  geom_line(data=keto, aes(x = date, y = category_pt, color = "Gray")) +
  geom_point(data=macro, aes(x = date, y = category_pt, color = "Orange"), size=1.5) +
  geom_line(data=macro, aes(x = date, y = category_pt, color = "Orange")) +
  geom_point(data=other, aes(x = date, y = category_pt, color = "Pink"), size=1.5) +
  geom_line(data=other, aes(x = date, y = category_pt, color = "Pink")) +
  geom_point(data=total, aes(x = date, y = category_pt, color = "turquoise"), size=1.5) +
  geom_line(data=total, aes(x = date, y = category_pt, color = "turquoise")) +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2022-01-01'))) +
  scale_color_manual(values=c("Black", "Blue", "Red", "Green", "Purple", "Brown", "Gray", "Orange", "Pink","turquoise"),
                     labels=c("Peni", "Cepha", "Tetra", "Amino", "Amphe", "Glyco", "Keto", "Macro", "Other","Total")) +
  theme_gray(20) +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


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







