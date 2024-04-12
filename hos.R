
#패키지지
library(tidyverse)
library(ggplot2)
library(lubridate)

#### 요양기관 종별을 포함한 환자 데이터 추출 ----
# 반복 작업을 위한 함수 정의
process_med_file <- function(file_path) {
  med_df_hos <- read_csv(file_path)
  result <- med_df_hos %>%
    mutate(USE_YEAR = factor(USE_YEAR),
           USE_MONTH = factor(USE_MONTH)) %>% 
    group_by(WK_COMPN_4, OUT_IN_HOS,USE_YEAR, USE_MONTH) %>%
    summarise(pres = sum(PRSCRPTN_TNDN_CNT),
              pt = sum(PATIENT_CNT))
  return(result)
} #pres는 처방수, pt는 환자수


# 결과를 저장할 리스트 생성
MEDICINE_list_hos <- list()

# MEDINE(1)부터 MEDINE(70)까지의 파일에 대해 반복 작업 수행
for (i in 1:70) {
  file_path <- paste0("MEDICINE/MEDICINE (", i, ").csv")
  if(!file.exists(file_path)){next}
  result <- process_med_file(file_path)
  MEDICINE_list_hos[[i]] <- result
}

# 결과를 하나의 데이터프레임으로 병합
MEDICINE_result_hos <- do.call(rbind, MEDICINE_list_hos)

# 결과 확인
print(MEDICINE_result_hos)


####데이터 전처리 및 항생제 사용 환자 추출 ----
MEDICINE_result_hos <- MEDICINE_result_hos %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))

medicine_result_anti_hos <- inner_join(MEDICINE_result_hos, medi_inform_anti, by='WK_COMPN_4')

colSums(is.na(medicine_result_anti_hos))
head(medicine_result_anti$pt)


#category로 합치기
medicine_result_anti_hos2 <- medicine_result_anti_hos %>% 
  group_by(USE_YEAR,USE_MONTH,category,OUT_IN_HOS) %>% 
  summarise(category_pres = sum(pres), category_pt = sum(pt))

#날짜 만들기
medicine_result_anti_hos2$date = paste0(medicine_result_anti_hos2$USE_YEAR,medicine_result_anti_hos2$USE_MONTH)
medicine_result_anti_hos2$date <- as.Date(paste(as.character(medicine_result_anti_hos2$date), '01'), format='%Y%m%d')



####그래프 pt ----

ggplot(data=medicine_result_anti_hos2, aes(x = date, y = category_pt)) + 
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


