
#패키지 ----
library(tidyverse)
library(ggplot2)
library(lubridate)



#요양기관 종별을 포함한 환자 데이터 추출 ----
# 반복 작업을 위한 함수 정의
process_med_file <- function(file_path) {
  med_df_class <- read_csv(file_path)
  result <- med_df_class %>%
    mutate(USE_YEAR = factor(USE_YEAR),
           USE_MONTH = factor(USE_MONTH)) %>% 
    group_by(WK_COMPN_4, YOYANG_CLSFC_CD_ADJ,USE_YEAR, USE_MONTH) %>%
    summarise(pres = sum(PRSCRPTN_TNDN_CNT),
              pt = sum(PATIENT_CNT), amt = sum(PRSCRPTN_AMT))
  return(result)
} #pres는 처방수, pt는 환자수


# 결과를 저장할 리스트 생성
MEDICINE_list_class <- list()

# MEDINE(1)부터 MEDINE(69)까지의 파일에 대해 반복 작업 수행
for (i in 1:72) {
  file_path <- paste0("MEDICINE/MEDICINE (", i, ").csv")
  if(!file.exists(file_path)){next}
  result <- process_med_file(file_path)
  MEDICINE_list_class[[i]] <- result
}

# 결과를 하나의 데이터프레임으로 병합
MEDICINE_result_class <- do.call(rbind, MEDICINE_list_class)

# 결과 확인
print(MEDICINE_result_class)


#항생제 분류(class,jo,prod,category)
medi_inform_anti <- read.csv("filtered_class_antibiotics.csv") %>% 
  mutate(WK_COMPN_4 = jo) %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4)) %>% 
  select(-jo)


####데이터 전처리 및 항생제 사용 환자 추출 ----
MEDICINE_result_class <- MEDICINE_result_class %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4))

medicine_result_anti_class <- inner_join(MEDICINE_result_class, medi_inform_anti, by='WK_COMPN_4')

colSums(is.na(medicine_result_anti_class))
head(medicine_result_anti_class$pt)


#category로 합치기
medicine_result_anti_class2 <- medicine_result_anti_class %>% 
  group_by(USE_YEAR,USE_MONTH,category,YOYANG_CLSFC_CD_ADJ) %>% 
  summarise(category_pres = sum(pres), category_pt = sum(pt))

#날짜 만들기
medicine_result_anti_class2$date = paste0(medicine_result_anti_class2$USE_YEAR,medicine_result_anti_class2$USE_MONTH)
medicine_result_anti_class2$date <- as.Date(paste(as.character(medicine_result_anti_class2$date), '01'), format='%Y%m%d')

#데이터 정제
medicine_result_anti_class2 <- medicine_result_anti_class2 %>% 
  mutate(YOYANG_CLSFC_CD_ADJ = factor(YOYANG_CLSFC_CD_ADJ, levels = c(0,1,2,3), labels = c("상급종합병원", "종합병원", "병원", "의원")))

#각 성분별 분류
##상급종합병원 항생제 분류
SC_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

SC_peni <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Penicillins")

SC_cepha <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Cephalosporins")

SC_tetra <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Tetracyclines")

SC_macro <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Macrolides")

SC_glyco <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Glycopeptides")

SC_amino <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Aminoglycosides")

SC_amphe <- medicine_result_anti_class2 %>%
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Amphenicols")

SC_keto <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Ketolides")

SC_other <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  filter(category == "Other")


##종합병원 항생제 분류
C_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

C_peni <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Penicillins")

C_cepha <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Cephalosporins")

C_tetra <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Tetracyclines")

C_macro <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Macrolides")

C_glyco <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Glycopeptides")

C_amino <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Aminoglycosides")

C_amphe <- medicine_result_anti_class2 %>%
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Amphenicols")

C_keto <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Ketolides")

C_other <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  filter(category == "Other")


##병원 항생제 분류
H_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

H_peni <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Penicillins")

H_cepha <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Cephalosporins")

H_tetra <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Tetracyclines")

H_macro <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Macrolides")

H_glyco <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Glycopeptides")

H_amino <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Aminoglycosides")

H_amphe <- medicine_result_anti_class2 %>%
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Amphenicols")

H_keto <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Ketolides")

H_other <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  filter(category == "Other")


##의원 항생제 분류
P_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

library(dplyr)

## 의원 항생제 분류
P_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), .groups = 'drop')

P_peni <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Penicillins")

P_cepha <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Cephalosporins")

P_tetra <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Tetracyclines")

P_macro <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Macrolides")

P_glyco <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Glycopeptides")

P_amino <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Aminoglycosides")

P_amphe <- medicine_result_anti_class2 %>%
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Amphenicols")

P_keto <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Ketolides")

P_other <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  filter(category == "Other")



####그래프 ----

#전체(pt, pres)
options(scipen = 5)
#전체 pt
# 상급종합병원, 종합병원, 병원, 의원 데이터에 Source 열 추가
SC_anti$Source <- "상급종합병원"
C_anti$Source <- "종합병원"
H_anti$Source <- "병원"
P_anti$Source <- "의원"

# 데이터 결합
class_anti <- rbind(SC_anti, C_anti, H_anti, P_anti)

# 다른 항생제 카테고리에도 동일하게 적용
SC_peni$Source <- "상급종합병원"
C_peni$Source <- "종합병원"
H_peni$Source <- "병원"
P_peni$Source <- "의원"
class_peni <- rbind(SC_peni, C_peni, H_peni, P_peni)

SC_cepha$Source <- "상급종합병원"
C_cepha$Source <- "종합병원"
H_cepha$Source <- "병원"
P_cepha$Source <- "의원"
class_cepha <- rbind(SC_cepha, C_cepha, H_cepha, P_cepha)

# 나머지 데이터셋도 동일한 방법으로 처리
SC_tetra$Source <- "상급종합병원"
C_tetra$Source <- "종합병원"
H_tetra$Source <- "병원"
P_tetra$Source <- "의원"
class_tetra <- rbind(SC_tetra, C_tetra, H_tetra, P_tetra)

SC_macro$Source <- "상급종합병원"
C_macro$Source <- "종합병원"
H_macro$Source <- "병원"
P_macro$Source <- "의원"
class_macro <- rbind(SC_macro, C_macro, H_macro, P_macro)

SC_glyco$Source <- "상급종합병원"
C_glyco$Source <- "종합병원"
H_glyco$Source <- "병원"
P_glyco$Source <- "의원"
class_glyco <- rbind(SC_glyco, C_glyco, H_glyco, P_glyco)

SC_amino$Source <- "상급종합병원"
C_amino$Source <- "종합병원"
H_amino$Source <- "병원"
P_amino$Source <- "의원"
class_amino <- rbind(SC_amino, C_amino, H_amino, P_amino)

SC_amphe$Source <- "상급종합병원"
C_amphe$Source <- "종합병원"
H_amphe$Source <- "병원"
P_amphe$Source <- "의원"
class_amphe <- rbind(SC_amphe, C_amphe, H_amphe, P_amphe)

SC_keto$Source <- "상급종합병원"
C_keto$Source <- "종합병원"
H_keto$Source <- "병원"
P_keto$Source <- "의원"
class_keto <- rbind(SC_keto, C_keto, H_keto, P_keto)

SC_other$Source <- "상급종합병원"
C_other$Source <- "종합병원"
H_other$Source <- "병원"
P_other$Source <- "의원"
class_other <- rbind(SC_other, C_other, H_other, P_other)


#그래프
ggplot(data = class_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = class_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = class_peni, aes(color = "Penicillins")) +
  geom_point(data = class_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = class_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = class_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = class_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = class_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = class_macro, aes(color = "Macrolides")) +
  geom_point(data = class_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = class_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = class_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = class_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = class_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = class_amphe, aes(color = "Amphenicols")) +
  geom_point(data = class_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = class_keto, aes(color = "Ketolides")) +
  geom_point(data = class_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = class_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 8000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "Patient number by Hospital Classification") +
  facet_wrap(~Source) +  # 패싯을 추가하여 각 병원 등급 구분
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



#전체 pres
ggplot(data = class_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = class_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = class_peni, aes(color = "Penicillins")) +
  geom_point(data = class_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = class_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = class_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = class_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = class_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = class_macro, aes(color = "Macrolides")) +
  geom_point(data = class_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = class_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = class_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = class_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = class_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = class_amphe, aes(color = "Amphenicols")) +
  geom_point(data = class_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = class_keto, aes(color = "Ketolides")) +
  geom_point(data = class_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = class_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescriptiont Number", limits = c(0, 10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "Prescription number by Hospital Classification") +
  facet_wrap(~Source) +  # 패싯을 추가하여 각 병원 등급 구분
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



#상급종합병원(pt,pres)
#상급종합병원 pt
ggplot(data = SC_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = SC_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = SC_peni, aes(color = "Penicillins")) +
  geom_point(data = SC_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = SC_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = SC_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = SC_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = SC_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = SC_macro, aes(color = "Macrolides")) +
  geom_point(data = SC_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = SC_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = SC_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = SC_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = SC_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = SC_amphe, aes(color = "Amphenicols")) +
  geom_point(data = SC_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = SC_keto, aes(color = "Ketolides")) +
  geom_point(data = SC_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = SC_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 250000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "상급종합병원 pt") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

#상급종합병원 pres
ggplot(data = SC_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = SC_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = SC_peni, aes(color = "Penicillins")) +
  geom_point(data = SC_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = SC_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = SC_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = SC_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = SC_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = SC_macro, aes(color = "Macrolides")) +
  geom_point(data = SC_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = SC_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = SC_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = SC_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = SC_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = SC_amphe, aes(color = "Amphenicols")) +
  geom_point(data = SC_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = SC_keto, aes(color = "Ketolides")) +
  geom_point(data = SC_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = SC_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 250000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "상급종합병원 pt") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

#종합병원(pt,pres)
# 종합병원 pt
ggplot(data = C_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = C_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = C_peni, aes(color = "Penicillins")) +
  geom_point(data = C_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = C_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = C_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = C_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = C_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = C_macro, aes(color = "Macrolides")) +
  geom_point(data = C_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = C_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = C_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = C_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = C_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = C_amphe, aes(color = "Amphenicols")) +
  geom_point(data = C_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = C_keto, aes(color = "Ketolides")) +
  geom_point(data = C_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = C_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 1000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "종합병원 pt") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

# 종합병원 pt
ggplot(data = C_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = C_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = C_peni, aes(color = "Penicillins")) +
  geom_point(data = C_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = C_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = C_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = C_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = C_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = C_macro, aes(color = "Macrolides")) +
  geom_point(data = C_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = C_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = C_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = C_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = C_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = C_amphe, aes(color = "Amphenicols")) +
  geom_point(data = C_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = C_keto, aes(color = "Ketolides")) +
  geom_point(data = C_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = C_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 1000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "종합병원 pres") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

#병원(pt,pres)
#병원 pt
ggplot(data = H_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = H_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = H_peni, aes(color = "Penicillins")) +
  geom_point(data = H_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = H_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = H_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = H_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = H_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = H_macro, aes(color = "Macrolides")) +
  geom_point(data = H_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = H_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = H_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = H_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = H_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = H_amphe, aes(color = "Amphenicols")) +
  geom_point(data = H_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = H_keto, aes(color = "Ketolides")) +
  geom_point(data = H_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = H_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 1500000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "병원 pt") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

#병원 pres
ggplot(data = H_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = H_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = H_peni, aes(color = "Penicillins")) +
  geom_point(data = H_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = H_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = H_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = H_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = H_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = H_macro, aes(color = "Macrolides")) +
  geom_point(data = H_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = H_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = H_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = H_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = H_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = H_amphe, aes(color = "Amphenicols")) +
  geom_point(data = H_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = H_keto, aes(color = "Ketolides")) +
  geom_point(data = H_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = H_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 1500000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "병원 pres") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



#의원(pt,pres)
#의원 pt
ggplot(data = P_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = P_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = P_peni, aes(color = "Penicillins")) +
  geom_point(data = P_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = P_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = P_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = P_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = P_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = P_macro, aes(color = "Macrolides")) +
  geom_point(data = P_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = P_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = P_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = P_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = P_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = P_amphe, aes(color = "Amphenicols")) +
  geom_point(data = P_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = P_keto, aes(color = "Ketolides")) +
  geom_point(data = P_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = P_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 10000000)) +
  scale_x_date(name=" ", breaks = "3 months", date_labels = "%Y%M", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "의원 pt") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

#의원 pres
ggplot(data = P_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = P_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = P_peni, aes(color = "Penicillins")) +
  geom_point(data = P_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = P_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = P_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = P_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = P_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = P_macro, aes(color = "Macrolides")) +
  geom_point(data = P_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = P_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = P_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = P_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = P_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = P_amphe, aes(color = "Amphenicols")) +
  geom_point(data = P_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = P_keto, aes(color = "Ketolides")) +
  geom_point(data = P_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = P_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 15000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "의원 pres") +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



