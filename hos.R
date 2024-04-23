
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


#항생제 분류(class,jo,prod,category)
medi_inform_anti <- read.csv("filtered_class_antibiotics.csv") %>% 
  mutate(WK_COMPN_4 = jo) %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4)) %>% 
  select(-jo)


####데이터 전처리 및 항생제 사용 환자 추출 ----
MEDICINE_result_hos <- MEDICINE_result_hos %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))

medicine_result_anti_hos <- inner_join(MEDICINE_result_hos, medi_inform_anti, by='WK_COMPN_4')

colSums(is.na(medicine_result_anti_hos))
head(medicine_result_anti_hos$pt)


#category로 합치기
medicine_result_anti_hos2 <- medicine_result_anti_hos %>% 
  group_by(USE_YEAR,USE_MONTH,category,OUT_IN_HOS) %>% 
  summarise(category_pres = sum(pres), category_pt = sum(pt))

#날짜 만들기
medicine_result_anti_hos2$date = paste0(medicine_result_anti_hos2$USE_YEAR,medicine_result_anti_hos2$USE_MONTH)
medicine_result_anti_hos2$date <- as.Date(paste(as.character(medicine_result_anti_hos2$date), '01'), format='%Y%m%d')


#각 성분별 분류
##병원내 IN 항생제 분류
IN_anti <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  group_by(USE_MONTH,USE_YEAR, OUT_IN_HOS,date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt))

IN_peni <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Penicillins")

IN_cepha <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Cephalosporins")

IN_cepha <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Cephalosporins")

IN_tetra <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Tetracyclines")

IN_macro <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Macrolides")

IN_glyco <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Glycopeptides")

IN_amino <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Aminoglycosides")

IN_amphe <- medicine_result_anti_hos2 %>%
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Amphenicols")

IN_keto <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Ketolides")

IN_other <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  filter(category == "Other")


##병원외 OUT 항생제 분류
OUT_anti <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  group_by(USE_MONTH, USE_YEAR, OUT_IN_HOS, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt))

OUT_peni <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Penicillins")

OUT_cepha <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Cephalosporins")

OUT_tetra <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Tetracyclines")

OUT_macro <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Macrolides")

OUT_glyco <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Glycopeptides")

OUT_amino <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Aminoglycosides")

OUT_amphe <- medicine_result_anti_hos2 %>%
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Amphenicols")

OUT_keto <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Ketolides")

OUT_other <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  filter(category == "Other")






####그래프 pt IN ----
ggplot(data=IN_peni, aes(x = date, y = category_pt)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  # 범주: total(anti)는 black,
  scale_y_continuous(name="patient number", limits = c(0,10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values=c("Black", "Blue", "Red", "Green", "Purple", "Brown", "Gray", "Orange", "Pink","turquoise"),
                     labels=c()) +
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




#### 그래프 pt OUT ----

#### 그래프 pres IN ----

#### 그래프 pres OUT ----



