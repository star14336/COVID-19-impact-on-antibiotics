
#패키지 ----
library(tidyverse)
library(ggplot2)
library(lubridate)



#연령을 포함한 환자 데이터 추출 ----
# 반복 작업을 위한 함수 정의
process_med_file <- function(file_path) {
  med_df_age <- read_csv(file_path)
  result <- med_df_age %>%
    mutate(USE_YEAR = factor(USE_YEAR),
           USE_MONTH = factor(USE_MONTH)) %>% 
    group_by(WK_COMPN_4, AGE_G,USE_YEAR, USE_MONTH) %>%
    summarise(pres = sum(PRSCRPTN_TNDN_CNT),
              pt = sum(PATIENT_CNT), amt = sum(PRSCRPTN_AMT))
  return(result)
} #pres는 처방수, pt는 환자수


# 결과를 저장할 리스트 생성
MEDICINE_list_age <- list()

# MEDINE(1)부터 MEDINE(69)까지의 파일에 대해 반복 작업 수행
for (i in 1:72) {
  file_path <- paste0("MEDICINE/MEDICINE (", i, ").csv")
  if(!file.exists(file_path)){next}
  result <- process_med_file(file_path)
  MEDICINE_list_age[[i]] <- result
}

# 결과를 하나의 데이터프레임으로 병합
MEDICINE_result_age <- do.call(rbind, MEDICINE_list_age)

# 결과 확인
print(MEDICINE_result_age)


#항생제 분류(class,jo,prod,category)
medi_inform_anti <- read.csv("filtered_class_antibiotics.csv") %>% 
  mutate(WK_COMPN_4 = jo) %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4)) %>% 
  select(-jo)


####데이터 전처리 및 항생제 사용 환자 추출 ----
MEDICINE_result_age <- MEDICINE_result_age %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))

medicine_result_anti_age <- inner_join(MEDICINE_result_age, medi_inform_anti, by='WK_COMPN_4')

colSums(is.na(medicine_result_anti_age))
head(medicine_result_anti_age$pt)


#category로 합치기
medicine_result_anti_age2 <- medicine_result_anti_age %>% 
  group_by(USE_YEAR,USE_MONTH,category,AGE_G) %>% 
  summarise(category_pres = sum(pres), category_pt = sum(pt), category_amt = sum(amt))

#날짜 만들기
medicine_result_anti_age2$date = paste0(medicine_result_anti_age2$USE_YEAR,medicine_result_anti_age2$USE_MONTH)
medicine_result_anti_age2$date <- as.Date(paste(as.character(medicine_result_anti_age2$date), '01'), format='%Y%m%d')


#데이터 정제
medicine_result_anti_age2 <- medicine_result_anti_age2 %>% 
  mutate(AGE_G = case_when(AGE_G < 20~ 0,
                           AGE_G >= 20~ 1,
                           AGE_G < 60~ 1,
                           AGE_G >= 60~ 2)) %>%
  mutate(AGE_G = factor(AGE_G, levels = c(0,1,2), labels = c("junior","adult" ,"senior")))



#각 성분별 분류
## Senior 항생제 분류
S_anti <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR, AGE_G, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

S_peni <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Penicillins")

S_cepha <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Cephalosporins")

S_tetra <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Tetracyclines")

S_macro <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Macrolides")

S_glyco <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Glycopeptides")

S_amino <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Aminoglycosides")

S_amphe <- medicine_result_anti_age2 %>%
  filter(AGE_G == "senior") %>% 
  filter(category == "Amphenicols")

S_keto <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Ketolides")

S_other <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  filter(category == "Other")



## Junior 항생제 분류
J_anti <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR, AGE_G, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

J_peni <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Penicillins")

J_cepha <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Cephalosporins")

J_tetra <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Tetracyclines")

J_macro <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Macrolides")

J_glyco <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Glycopeptides")

J_amino <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Aminoglycosides")

J_amphe <- medicine_result_anti_age2 %>%
  filter(AGE_G == "junior") %>% 
  filter(category == "Amphenicols")

J_keto <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Ketolides")

J_other <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  filter(category == "Other")


####그래프 ----
options(scipen = 5)
#Senior(60세 이상), Junior(60세 미만) pt
# S과 J 데이터에 "Source" 열 추가
S_anti$Source <- "senior"
J_anti$Source <- "junior"

# 데이터 결합
age_anti<- rbind(S_anti, J_anti)

# Penicillins 데이터 결합
S_peni$Source <- "senior"
J_peni$Source <- "junior"
age_peni <- rbind(S_peni, J_peni)

# Cephalosporins 데이터 결합
S_cepha$Source <- "senior"
J_cepha$Source <- "junior"
age_cepha <- rbind(S_cepha, J_cepha)

# Tetracyclines 데이터 결합
S_tetra$Source <- "senior"
J_tetra$Source <- "junior"
age_tetra <- rbind(S_tetra, J_tetra)

# Macrolides 데이터 결합
S_macro$Source <- "senior"
J_macro$Source <- "junior"
age_macro<- rbind(S_macro, J_macro)

# Glycopeptides 데이터 결합
S_glyco$Source <- "senior"
J_glyco$Source <- "junior"
age_glyco<- rbind(S_glyco, J_glyco)

# Aminoglycosides 데이터 결합
S_amino$Source <- "senior"
J_amino$Source <- "junior"
age_amino <- rbind(S_amino, J_amino)

# Amphenicols 데이터 결합
S_amphe$Source <- "senior"
J_amphe$Source <- "junior"
age_amphe <- rbind(S_amphe, J_amphe)

# Ketolides 데이터 결합
S_keto$Source <- "senior"
J_keto$Source <- "junior"
age_keto <- rbind(S_keto, J_keto)

# Other 카테고리 데이터 결합
S_other$Source <- "senior"
J_other$Source <- "junior"
age_other <- rbind(S_other, J_other)


#그래프
ggplot(data = age_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = peni_anti, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = peni_anti, aes(color = "Penicillins")) +
  geom_point(data = cepha_anti, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = cepha_anti, aes(color = "Cephalosporins")) +
  geom_point(data = tetra_anti, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = tetra_anti, aes(color = "Tetracyclines")) +
  geom_point(data = macro_anti, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = macro_anti, aes(color = "Macrolides")) +
  geom_point(data = glyco_anti, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = glyco_anti, aes(color = "Glycopeptides")) +
  geom_point(data = amino_anti, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = amino_anti, aes(color = "Aminoglycosides")) +
  geom_point(data = amphe_anti, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = amphe_anti, aes(color = "Amphenicols")) +
  geom_point(data = keto_anti, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = keto_anti, aes(color = "Ketolides")) +
  geom_point(data = other_anti, aes(color = "Other"), size = 1.5) +
  geom_line(data = other_anti, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 8000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y%M", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "Patient number by Age Group") +
  facet_wrap(~Source) +  # 패싯을 추가하여 senior와 junior 구분
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



#Senior, Junior pres
ggplot(data = age_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = peni_anti, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = peni_anti, aes(color = "Penicillins")) +
  geom_point(data = cepha_anti, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = cepha_anti, aes(color = "Cephalosporins")) +
  geom_point(data = tetra_anti, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = tetra_anti, aes(color = "Tetracyclines")) +
  geom_point(data = macro_anti, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = macro_anti, aes(color = "Macrolides")) +
  geom_point(data = glyco_anti, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = glyco_anti, aes(color = "Glycopeptides")) +
  geom_point(data = amino_anti, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = amino_anti, aes(color = "Aminoglycosides")) +
  geom_point(data = amphe_anti, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = amphe_anti, aes(color = "Amphenicols")) +
  geom_point(data = keto_anti, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = keto_anti, aes(color = "Ketolides")) +
  geom_point(data = other_anti, aes(color = "Other"), size = 1.5) +
  geom_line(data = other_anti, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "Prescription number by Age Group") +
  facet_wrap(~Source) +  # 패싯을 추가하여 senior와 junior 구분
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


#그래프 senior(60세 이상) (pt,pres)
#senior pt
ggplot(data = S_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = S_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = S_peni, aes(color = "Penicillins")) +
  geom_point(data = S_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = S_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = S_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = S_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = S_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = S_macro, aes(color = "Macrolides")) +
  geom_point(data = S_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = S_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = S_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = S_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = S_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = S_amphe, aes(color = "Amphenicols")) +
  geom_point(data = S_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = S_keto, aes(color = "Ketolides")) +
  geom_point(data = S_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = S_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 4000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "60세 이상(senior) pt") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


#senior pres
ggplot(data = S_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = S_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = S_peni, aes(color = "Penicillins")) +
  geom_point(data = S_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = S_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = S_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = S_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = S_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = S_macro, aes(color = "Macrolides")) +
  geom_point(data = S_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = S_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = S_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = S_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = S_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = S_amphe, aes(color = "Amphenicols")) +
  geom_point(data = S_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = S_keto, aes(color = "Ketolides")) +
  geom_point(data = S_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = S_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 4000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "60세 이상(senior) pres") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


#그래프 junior(60세 미만) (pt,pres)
#junior pt
ggplot(data = J_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = J_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = J_peni, aes(color = "Penicillins")) +
  geom_point(data = J_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = J_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = J_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = J_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = J_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = J_macro, aes(color = "Macrolides")) +
  geom_point(data = J_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = J_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = J_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = J_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = J_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = J_amphe, aes(color = "Amphenicols")) +
  geom_point(data = J_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = J_keto, aes(color = "Ketolides")) +
  geom_point(data = J_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = J_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 8000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "60세 미만(junior) pt") + 
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


#junior pres
ggplot(data = J_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = J_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = J_peni, aes(color = "Penicillins")) +
  geom_point(data = J_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = J_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = J_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = J_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = J_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = J_macro, aes(color = "Macrolides")) +
  geom_point(data = J_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = J_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = J_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = J_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = J_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = J_amphe, aes(color = "Amphenicols")) +
  geom_point(data = J_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = J_keto, aes(color = "Ketolides")) +
  geom_point(data = J_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = J_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "60세 미만(junior) pres") +  # 그래프 제목도 수정
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


