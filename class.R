
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
  summarise(category_pres = sum(pres), category_pt = sum(pt), category_amt = sum(amt))

#날짜 만들기
medicine_result_anti_class2$date = paste0(medicine_result_anti_class2$USE_YEAR,medicine_result_anti_class2$USE_MONTH)
medicine_result_anti_class2$date <- as.Date(paste(as.character(medicine_result_anti_class2$date), '01'), format='%Y%m%d')

#데이터 정제
medicine_result_anti_class2 <- medicine_result_anti_class2 %>% 
  mutate(YOYANG_CLSFC_CD_ADJ = factor(YOYANG_CLSFC_CD_ADJ, levels = c(0,1,2,3), labels = c("상급종합병원", "종합병원", "병원", "의원")))



#각 성분별 분류----
##상급종합병원 항생제 분류----
SC_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "상급종합병원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt),.groups = 'drop')

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

# csv 파일로 저장(상급종합병원)
write.csv(SC_anti, "MEDICINE/data/class/sc/SC_anti.csv", row.names = FALSE)
write.csv(SC_peni, "MEDICINE/data/class/sc/SC_penicillins.csv", row.names = FALSE)
write.csv(SC_cepha, "MEDICINE/data/class/sc/SC_cephalosporins.csv", row.names = FALSE)
write.csv(SC_tetra, "MEDICINE/data/class/sc/SC_tetracyclines.csv", row.names = FALSE)
write.csv(SC_macro, "MEDICINE/data/class/sc/SC_macrolides.csv", row.names = FALSE)
write.csv(SC_glyco, "MEDICINE/data/class/sc/SC_glycopeptides.csv", row.names = FALSE)
write.csv(SC_amino, "MEDICINE/data/class/sc/SC_aminoglycosides.csv", row.names = FALSE)
write.csv(SC_amphe, "MEDICINE/data/class/sc/SC_amphenicols.csv", row.names = FALSE)
write.csv(SC_keto, "MEDICINE/data/class/sc/SC_ketolides.csv", row.names = FALSE)
write.csv(SC_other, "MEDICINE/data/class/sc/SC_other_anti.csv", row.names = FALSE)

head(SC_peni)



##종합병원 항생제 분류----
C_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "종합병원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt),.groups = 'drop')

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


# csv 파일로 저장
write.csv(C_anti, "MEDICINE/data/class/c/C_anti.csv", row.names = FALSE)
write.csv(C_peni, "MEDICINE/data/class/c/C_penicillins.csv", row.names = FALSE)
write.csv(C_cepha, "MEDICINE/data/class/c/C_cephalosporins.csv", row.names = FALSE)
write.csv(C_tetra, "MEDICINE/data/class/c/C_tetracyclines.csv", row.names = FALSE)
write.csv(C_macro, "MEDICINE/data/class/c/C_macrolides.csv", row.names = FALSE)
write.csv(C_glyco, "MEDICINE/data/class/c/C_glycopeptides.csv", row.names = FALSE)
write.csv(C_amino, "MEDICINE/data/class/c/C_aminoglycosides.csv", row.names = FALSE)
write.csv(C_amphe, "MEDICINE/data/class/c/C_amphenicols.csv", row.names = FALSE)
write.csv(C_keto, "MEDICINE/data/class/c/C_ketolides.csv", row.names = FALSE)
write.csv(C_other, "MEDICINE/data/class/c/C_other_anti.csv", row.names = FALSE)

head(C_peni)


##병원 항생제 분류----
H_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "병원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt) ,.groups = 'drop')

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

# csv 파일로 저장
write.csv(H_anti, "MEDICINE/data/class/h/H_anti.csv", row.names = FALSE)
write.csv(H_peni, "MEDICINE/data/class/h/H_penicillins.csv", row.names = FALSE)
write.csv(H_cepha, "MEDICINE/data/class/h/H_cephalosporins.csv", row.names = FALSE)
write.csv(H_tetra, "MEDICINE/data/class/h/H_tetracyclines.csv", row.names = FALSE)
write.csv(H_macro, "MEDICINE/data/class/h/H_macrolides.csv", row.names = FALSE)
write.csv(H_glyco, "MEDICINE/data/class/h/H_glycopeptides.csv", row.names = FALSE)
write.csv(H_amino, "MEDICINE/data/class/h/H_aminoglycosides.csv", row.names = FALSE)
write.csv(H_amphe, "MEDICINE/data/class/h/H_amphenicols.csv", row.names = FALSE)
write.csv(H_keto, "MEDICINE/data/class/h/H_ketolides.csv", row.names = FALSE)
write.csv(H_other, "MEDICINE/data/class/h/H_other_anti.csv", row.names = FALSE)

head(H_peni)



##의원 항생제 분류----
P_anti <- medicine_result_anti_class2 %>% 
  filter(YOYANG_CLSFC_CD_ADJ == "의원") %>% 
  group_by(USE_MONTH, USE_YEAR, YOYANG_CLSFC_CD_ADJ, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt) ,.groups = 'drop')

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

# csv 파일로 저장
write.csv(P_anti, "MEDICINE/data/class/p/P_anti.csv", row.names = FALSE)
write.csv(P_peni, "MEDICINE/data/class/p/P_penicillins.csv", row.names = FALSE)
write.csv(P_cepha, "MEDICINE/data/class/p/P_cephalosporins.csv", row.names = FALSE)
write.csv(P_tetra, "MEDICINE/data/class/p/P_tetracyclines.csv", row.names = FALSE)
write.csv(P_macro, "MEDICINE/data/class/p/P_macrolides.csv", row.names = FALSE)
write.csv(P_glyco, "MEDICINE/data/class/p/P_glycopeptides.csv", row.names = FALSE)
write.csv(P_amino, "MEDICINE/data/class/p/P_aminoglycosides.csv", row.names = FALSE)
write.csv(P_amphe, "MEDICINE/data/class/p/P_amphenicols.csv", row.names = FALSE)
write.csv(P_keto, "MEDICINE/data/class/p/P_ketolides.csv", row.names = FALSE)
write.csv(P_other, "MEDICINE/data/class/p/P_other_anti.csv", row.names = FALSE)

head(P_peni)



#그래프 ----

##전체(pt, pres,amt) 데이터 로딩 ----
#하위폴더 불러오기
SC_anti <- read_csv("MEDICINE/data/class/sc/SC_anti.csv")
SC_peni <- read_csv("MEDICINE/data/class/sc/SC_penicillins.csv")
SC_cepha <- read_csv("MEDICINE/data/class/sc/SC_cephalosporins.csv")
SC_tetra <- read_csv("MEDICINE/data/class/sc/SC_tetracyclines.csv")
SC_macro <- read_csv("MEDICINE/data/class/sc/SC_macrolides.csv")
SC_glyco <- read_csv("MEDICINE/data/class/sc/SC_glycopeptides.csv")
SC_amino <- read_csv("MEDICINE/data/class/sc/SC_aminoglycosides.csv")
SC_amphe <- read_csv("MEDICINE/data/class/sc/SC_amphenicols.csv")
SC_keto <- read_csv("MEDICINE/data/class/sc/SC_ketolides.csv")
SC_other <- read_csv("MEDICINE/data/class/sc/SC_other_anti.csv")

head(SC_peni)

# 저장된 데이터 불러오기
C_anti <- read_csv("MEDICINE/data/class/c/C_anti.csv")
C_peni <- read_csv("MEDICINE/data/class/c/C_penicillins.csv")
C_cepha <- read_csv("MEDICINE/data/class/c/C_cephalosporins.csv")
C_tetra <- read_csv("MEDICINE/data/class/c/C_tetracyclines.csv")
C_macro <- read_csv("MEDICINE/data/class/c/C_macrolides.csv")
C_glyco <- read_csv("MEDICINE/data/class/c/C_glycopeptides.csv")
C_amino <- read_csv("MEDICINE/data/class/c/C_aminoglycosides.csv")
C_amphe <- read_csv("MEDICINE/data/class/c/C_amphenicols.csv")
C_keto <- read_csv("MEDICINE/data/class/c/C_ketolides.csv")
C_other <- read_csv("MEDICINE/data/class/c/C_other_anti.csv")

head(C_peni)

# 저장된 데이터 불러오기
H_anti <- read_csv("MEDICINE/data/class/h/H_anti.csv")
H_peni <- read_csv("MEDICINE/data/class/h/H_penicillins.csv")
H_cepha <- read_csv("MEDICINE/data/class/h/H_cephalosporins.csv")
H_tetra <- read_csv("MEDICINE/data/class/h/H_tetracyclines.csv")
H_macro <- read_csv("MEDICINE/data/class/h/H_macrolides.csv")
H_glyco <- read_csv("MEDICINE/data/class/h/H_glycopeptides.csv")
H_amino <- read_csv("MEDICINE/data/class/h/H_aminoglycosides.csv")
H_amphe <- read_csv("MEDICINE/data/class/h/H_amphenicols.csv")
H_keto <- read_csv("MEDICINE/data/class/h/H_ketolides.csv")
H_other <- read_csv("MEDICINE/data/class/h/H_other_anti.csv")

head(H_peni)


# 저장된 데이터 불러오기
P_anti <- read_csv("MEDICINE/data/class/p/P_anti.csv")
P_peni <- read_csv("MEDICINE/data/class/p/P_penicillins.csv")
P_cepha <- read_csv("MEDICINE/data/class/p/P_cephalosporins.csv")
P_tetra <- read_csv("MEDICINE/data/class/p/P_tetracyclines.csv")
P_macro <- read_csv("MEDICINE/data/class/p/P_macrolides.csv")
P_glyco <- read_csv("MEDICINE/data/class/p/P_glycopeptides.csv")
P_amino <- read_csv("MEDICINE/data/class/p/P_aminoglycosides.csv")
P_amphe <- read_csv("MEDICINE/data/class/p/P_amphenicols.csv")
P_keto <- read_csv("MEDICINE/data/class/p/P_ketolides.csv")
P_other <- read_csv("MEDICINE/data/class/p/P_other_anti.csv")

head(P_peni)

#데이터 통합
options(scipen = 5)
class_anti <- rbind(SC_anti, C_anti, H_anti, P_anti)
class_peni <- rbind(SC_peni, C_peni, H_peni, P_peni)
class_cepha <- rbind(SC_cepha, C_cepha, H_cepha, P_cepha)
class_tetra <- rbind(SC_tetra, C_tetra, H_tetra, P_tetra)
class_macro <- rbind(SC_macro, C_macro, H_macro, P_macro)
class_glyco <- rbind(SC_glyco, C_glyco, H_glyco, P_glyco)
class_keto <- rbind(SC_keto, C_keto, H_keto, P_keto)
class_amino <- rbind(SC_amino, C_amino, H_amino, P_amino)
class_amphe <- rbind(SC_amphe, C_amphe, H_amphe, P_amphe)
class_other <- rbind(SC_other, C_other, H_other, P_other)



##그래프 ----
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
  scale_x_date(name=" ", breaks = "3 months", date_labels = paste0("%Y",".","%m"), limits = as.Date(c('2018-01-01','2023-12-31'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "Patient number by Hospital Classification") +
  facet_wrap(~YOYANG_CLSFC_CD_ADJ) +  # 패싯을 추가하여 각 병원 등급 구분
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



