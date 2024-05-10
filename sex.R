
#패키지 ----
library(tidyverse)
library(ggplot2)
library(lubridate)



#성별을 포함한 환자 데이터 추출 ----
# 반복 작업을 위한 함수 정의
process_med_file <- function(file_path) {
  med_df_sex <- read_csv(file_path)
  result <- med_df_sex %>%
    mutate(USE_YEAR = factor(USE_YEAR),
           USE_MONTH = factor(USE_MONTH)) %>% 
    group_by(WK_COMPN_4, SEX_TYPE,USE_YEAR, USE_MONTH) %>%
    summarise(pres = sum(PRSCRPTN_TNDN_CNT),
              pt = sum(PATIENT_CNT),
              amt = sum(PRSCRPTN_AMT))
  return(result)
} #pres는 처방수, pt는 환자수


# 결과를 저장할 리스트 생성
MEDICINE_list_sex <- list()

# MEDINE(1)부터 MEDINE(69)까지의 파일에 대해 반복 작업 수행
for (i in 1:69) {
  file_path <- paste0("MEDICINE/MEDICINE (", i, ").csv")
  if(!file.exists(file_path)){next}
  result <- process_med_file(file_path)
  MEDICINE_list_sex[[i]] <- result
}

# 결과를 하나의 데이터프레임으로 병합
MEDICINE_result_sex <- do.call(rbind, MEDICINE_list_sex)

# 결과 확인
print(MEDICINE_result_sex)


#항생제 분류(class,jo,prod,category)
medi_inform_anti <- read.csv("filtered_class_antibiotics.csv") %>% 
  mutate(WK_COMPN_4 = jo) %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4)) %>% 
  select(-jo)


####데이터 전처리 및 항생제 사용 환자 추출 ----
MEDICINE_result_sex <- MEDICINE_result_sex %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))

medicine_result_anti_sex <- inner_join(MEDICINE_result_sex, medi_inform_anti, by='WK_COMPN_4')

colSums(is.na(medicine_result_anti_sex))
head(medicine_result_anti_sex$pt)


#category로 합치기
medicine_result_anti_sex2 <- medicine_result_anti_sex %>% 
  group_by(USE_YEAR,USE_MONTH,category,SEX_TYPE) %>% 
  summarise(category_pres = sum(pres), category_pt = sum(pt))

#날짜 만들기
medicine_result_anti_sex2$date = paste0(medicine_result_anti_sex2$USE_YEAR,medicine_result_anti_sex2$USE_MONTH)
medicine_result_anti_sex2$date <- as.Date(paste(as.character(medicine_result_anti_sex2$date), '01'), format='%Y%m%d')

#데이터 정제
medicine_result_anti_sex2 <- medicine_result_anti_sex2 %>% 
  mutate(SEX_TYPE = factor(SEX_TYPE, levels = c(1,2), labels = c("M","F")))



#각 성분별 분류
##남성 항생제 분류
M_anti <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  group_by(USE_MONTH, USE_YEAR, SEX_TYPE, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

M_peni <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Penicillins")

M_cepha <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Cephalosporins")

M_tetra <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Tetracyclines")

M_macro <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Macrolides")

M_glyco <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Glycopeptides")

M_amino <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Aminoglycosides")

M_amphe <- medicine_result_anti_sex2 %>%
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Amphenicols")

M_keto <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Ketolides")

M_other <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "M") %>% 
  filter(category == "Other")

##여성 항생제 분류
F_anti <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  group_by(USE_MONTH, USE_YEAR, SEX_TYPE, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

F_peni <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Penicillins")

F_cepha <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Cephalosporins")

F_tetra <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Tetracyclines")

F_macro <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Macrolides")

F_glyco <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Glycopeptides")

F_amino <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Aminoglycosides")

F_amphe <- medicine_result_anti_sex2 %>%
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Amphenicols")

F_keto <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Ketolides")

F_other <- medicine_result_anti_sex2 %>% 
  filter(SEX_TYPE == "F") %>% 
  filter(category == "Other")





####그래프 ----
options(scipen = 5)
#남녀 pt
# M과 F 데이터에 "Source" 열 추가
M_anti$Source <- "M"
F_anti$Source <- "F"

# 데이터 결합
sex_anti<- rbind(M_anti, F_anti)

# 다른 항생제 카테고리에도 동일하게 적용
# Penicillins 데이터 결합
M_peni$Source <- "M"
F_peni$Source <- "F"
sex_peni <- rbind(M_peni, F_peni)

# Cephalosporins 데이터 결합
M_cepha$Source <- "M"
F_cepha$Source <- "F"
sex_cepha <- rbind(M_cepha, F_cepha)

# Tetracyclines 데이터 결합
M_tetra$Source <- "M"
F_tetra$Source <- "F"
sex_tetra <- rbind(M_tetra, F_tetra)

# Macrolides 데이터 결합
M_macro$Source <- "M"
F_macro$Source <- "F"
sex_macro <- rbind(M_macro, F_macro)

# Glycopeptides 데이터 결합
M_glyco$Source <- "M"
F_glyco$Source <- "F"
sex_glyco <- rbind(M_glyco, F_glyco)

# Aminoglycosides 데이터 결합
M_amino$Source <- "M"
F_amino$Source <- "F"
sex_amino <- rbind(M_amino, F_amino)

# Amphenicols 데이터 결합
M_amphe$Source <- "M"
F_amphe$Source <- "F"
sex_amphe <- rbind(M_amphe, F_amphe)

# Ketolides 데이터 결합
M_keto$Source <- "M"
F_keto$Source <- "F"
sex_keto <- rbind(M_keto, F_keto)

# Other 카테고리 데이터 결합
M_other$Source <- "M"
F_other$Source <- "F"
sex_other <- rbind(M_other, F_other)


#그래프
ggplot(data = sex_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = sex_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = sex_peni, aes(color = "Penicillins")) +
  geom_point(data = sex_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = sex_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = sex_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = sex_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = sex_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = sex_macro, aes(color = "Macrolides")) +
  geom_point(data = sex_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = sex_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = sex_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = sex_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = sex_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = sex_amphe, aes(color = "Amphenicols")) +
  geom_point(data = sex_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = sex_keto, aes(color = "Ketolides")) +
  geom_point(data = sex_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = sex_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 7500000)) +
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols",
                                                   "Ketolides", "Other")) +
  facet_wrap(~Source) +
  labs(title = "남녀 pt") +  # 그래프 제목 추가
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())
        


#남녀 pres
ggplot(data = sex_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = sex_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = sex_peni, aes(color = "Penicillins")) +
  geom_point(data = sex_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = sex_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = sex_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = sex_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = sex_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = sex_macro, aes(color = "Macrolides")) +
  geom_point(data = sex_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = sex_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = sex_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = sex_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = sex_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = sex_amphe, aes(color = "Amphenicols")) +
  geom_point(data = sex_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = sex_keto, aes(color = "Ketolides")) +
  geom_point(data = sex_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = sex_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 7500000)) +
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols",
                                                   "Ketolides", "Other")) +
  facet_wrap(~Source) +
  labs(title = "남녀 pres") +  # 그래프 제목 추가
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())




#남성 pt
ggplot(data = M_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = M_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = M_peni, aes(color = "Penicillins")) +
  geom_point(data = M_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = M_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = M_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = M_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = M_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = M_macro, aes(color = "Macrolides")) +
  geom_point(data = M_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = M_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = M_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = M_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = M_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = M_amphe, aes(color = "Amphenicols")) +
  geom_point(data = M_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = M_keto, aes(color = "Ketolides")) +
  geom_point(data = M_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = M_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,6000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  labs(title = "남성 pt") +  # 그래프 제목 추가
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



#남성 pres
ggplot(data = M_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = M_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = M_peni, aes(color = "Penicillins")) +
  geom_point(data = M_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = M_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = M_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = M_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = M_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = M_macro, aes(color = "Macrolides")) +
  geom_point(data = M_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = M_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = M_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = M_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = M_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = M_amphe, aes(color = "Amphenicols")) +
  geom_point(data = M_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = M_keto, aes(color = "Ketolides")) +
  geom_point(data = M_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = M_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,6000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  labs(title = "남성 pres") +  # 그래프 제목 추가
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

#여성 pt
ggplot(data = F_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = F_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = F_peni, aes(color = "Penicillins")) +
  geom_point(data = F_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = F_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = F_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = F_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = F_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = F_macro, aes(color = "Macrolides")) +
  geom_point(data = F_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = F_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = F_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = F_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = F_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = F_amphe, aes(color = "Amphenicols")) +
  geom_point(data = F_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = F_keto, aes(color = "Ketolides")) +
  geom_point(data = F_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = F_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,6000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  labs(title = "여성 pt") +  # 그래프 제목 추가
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

#여성 pres
ggplot(data = F_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = F_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = F_peni, aes(color = "Penicillins")) +
  geom_point(data = F_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = F_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = F_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = F_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = F_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = F_macro, aes(color = "Macrolides")) +
  geom_point(data = F_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = F_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = F_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = F_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = F_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = F_amphe, aes(color = "Amphenicols")) +
  geom_point(data = F_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = F_keto, aes(color = "Ketolides")) +
  geom_point(data = F_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = F_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,8000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  labs(title = "여성 pres") +  # 그래프 제목 추가
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())










