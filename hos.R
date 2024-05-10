
#패키지
library(tidyverse)
library(ggplot2)
library(lubridate)

#### 원내외 종별을 포함한 환자 데이터 추출 ----
# 반복 작업을 위한 함수 정의
process_med_file <- function(file_path) {
  med_df_hos <- read_csv(file_path)
  result <- med_df_hos %>%
    mutate(USE_YEAR = factor(USE_YEAR),
           USE_MONTH = factor(USE_MONTH)) %>% 
    group_by(WK_COMPN_4, OUT_IN_HOS,USE_YEAR, USE_MONTH) %>%
    summarise(pres = sum(PRSCRPTN_TNDN_CNT),
              pt = sum(PATIENT_CNT), amt = sum(PRSCRPTN_AMT))
  return(result)
} #pres는 처방수, pt는 환자수


# 결과를 저장할 리스트 생성
MEDICINE_list_hos <- list()

# MEDINE(1)부터 MEDINE(69)까지의 파일에 대해 반복 작업 수행
for (i in 1:69) {
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
  summarise(category_pres = sum(pres), category_pt = sum(pt), category_amt = sum(amt))

#날짜 만들기
medicine_result_anti_hos2$date = paste0(medicine_result_anti_hos2$USE_YEAR,medicine_result_anti_hos2$USE_MONTH)
medicine_result_anti_hos2$date <- as.Date(paste(as.character(medicine_result_anti_hos2$date), '01'), format='%Y%m%d')






#각 성분별 분류
##병원내 IN 항생제 분류
IN_anti <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "IN") %>% 
  group_by(USE_MONTH,USE_YEAR, OUT_IN_HOS,date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt))

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


#csv파일로 저장
write.csv(anti, "MEDICINE/data/total/anti.csv")
write.csv(peni, "MEDICINE/data/total/penicillin.csv")
write.csv(cepha, "MEDICINE/data/total/cephalosporins.csv")
write.csv(tetra, "MEDICINE/data/total/tetracyclines.csv")
write.csv(macro, "MEDICINE/data/total/macrolides.csv")
write.csv(glyco, "MEDICINE/data/total/glycopeptides.csv")
write.csv(amino, "MEDICINE/data/total/aminoglycosides.csv")
write.csv(amphe, "MEDICINE/data/total/amphenicols.csv")
write.csv(keto, "MEDICINE/data/total/ketolides.csv")
write.csv(other, "MEDICINE/data/total/other_anti.csv")

#하위 폴더 바로 불러오기
anti <- read_csv("MEDICINE/data/total/anti.csv")
peni <- read_csv("MEDICINE/data/total/penicillins.csv")
cepha <- read_csv("MEDICINE/data/total/cephalosporins.csv")
tetra <- read_csv("MEDICINE/data/total/tetracyclines.csv")
macro <- read_csv("MEDICINE/data/total/macrolides.csv")
glyco <- read_csv("MEDICINE/data/total/glycopeptides.csv")
amino <- read_csv("MEDICINE/data/total/aminoglycosides.csv")
amphe <- read_csv("MEDICINE/data/total/amphenicols.csv")
keto <- read_csv("MEDICINE/data/total/ketolides.csv")
other <- read_csv("MEDICINE/data/total/other_antibiotics.csv")


####그래프 pt IN,OUT ----
options(scipen = 5) #과학적 스케일 적용
# IN과 OUT 데이터에 "Source" 열 추가
IN_anti$Source <- "IN"
OUT_anti$Source <- "OUT"

# 데이터 결합
combined_data <- rbind(IN_anti, OUT_anti)

# 다른 항생제 카테고리에도 동일하게 적용
IN_peni$Source <- "IN"
OUT_peni$Source <- "OUT"
combined_peni <- rbind(IN_peni, OUT_peni)

IN_cepha$Source <- "IN"
OUT_cepha$Source <- "OUT"
combined_cepha <- rbind(IN_cepha, OUT_cepha)

IN_tetra$Source <- "IN"
OUT_tetra$Source <- "OUT"
combined_tetra <- rbind(IN_tetra, OUT_tetra)

IN_macro$Source <- "IN"
OUT_macro$Source <- "OUT"
combined_macro <- rbind(IN_macro, OUT_macro)

IN_glyco$Source <- "IN"
OUT_glyco$Source <- "OUT"
combined_glyco <- rbind(IN_glyco, OUT_glyco)

IN_amino$Source <- "IN"
OUT_amino$Source <- "OUT"
combined_amino <- rbind(IN_amino, OUT_amino)

IN_amphe$Source <- "IN"
OUT_amphe$Source <- "OUT"
combined_amphe <- rbind(IN_amphe, OUT_amphe)

IN_keto$Source <- "IN"
OUT_keto$Source <- "OUT"
combined_keto <- rbind(IN_keto, OUT_keto)

IN_other$Source <- "IN"
OUT_other$Source <- "OUT"
combined_other <- rbind(IN_other, OUT_other)


#그래프
ggplot(data = combined_data, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = combined_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = combined_peni, aes(color = "Penicillins")) +
  geom_point(data = combined_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = combined_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = combined_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = combined_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = combined_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = combined_macro, aes(color = "Macrolides")) +
  geom_point(data = combined_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = combined_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = combined_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = combined_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = combined_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = combined_amphe, aes(color = "Amphenicols")) +
  geom_point(data = combined_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = combined_keto, aes(color = "Ketolides")) +
  geom_point(data = combined_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = combined_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 10000000)) +
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols",
                                                   "Ketolides", "Other")) +
  facet_wrap(~Source) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())




####그래프 pres IN,OUT ----
ggplot(data = combined_data, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = combined_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = combined_peni, aes(color = "Penicillins")) +
  geom_point(data = combined_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = combined_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = combined_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = combined_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = combined_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = combined_macro, aes(color = "Macrolides")) +
  geom_point(data = combined_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = combined_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = combined_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = combined_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = combined_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = combined_amphe, aes(color = "Amphenicols")) +
  geom_point(data = combined_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = combined_keto, aes(color = "Ketolides")) +
  geom_point(data = combined_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = combined_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 10000000)) +
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols",
                                                   "Ketolides", "Other")) +
  facet_wrap(~Source) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



####그래프 pt IN ----
options(scipen = 5)

ggplot(data = IN_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = IN_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = IN_peni, aes(color = "Penicillins")) +
  geom_point(data = IN_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = IN_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = IN_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = IN_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = IN_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = IN_macro, aes(color = "Macrolides")) +
  geom_point(data = IN_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = IN_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = IN_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = IN_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = IN_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = IN_amphe, aes(color = "Amphenicols")) +
  geom_point(data = IN_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = IN_keto, aes(color = "Ketolides")) +
  geom_point(data = IN_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = IN_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,2000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


#### 그래프 pt OUT ----
ggplot(data = OUT_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = OUT_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = OUT_peni, aes(color = "Penicillins")) +
  geom_point(data = OUT_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = OUT_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = OUT_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = OUT_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = OUT_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = OUT_macro, aes(color = "Macrolides")) +
  geom_point(data = OUT_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = OUT_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = OUT_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = OUT_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = OUT_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = OUT_amphe, aes(color = "Amphenicols")) +
  geom_point(data = OUT_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = OUT_keto, aes(color = "Ketolides")) +
  geom_point(data = OUT_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = OUT_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


#### 그래프 pres IN ----
ggplot(data = IN_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = IN_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = IN_peni, aes(color = "Penicillins")) +
  geom_point(data = IN_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = IN_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = IN_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = IN_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = IN_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = IN_macro, aes(color = "Macrolides")) +
  geom_point(data = IN_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = IN_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = IN_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = IN_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = IN_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = IN_amphe, aes(color = "Amphenicols")) +
  geom_point(data = IN_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = IN_keto, aes(color = "Ketolides")) +
  geom_point(data = IN_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = IN_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="prescription number", limits = c(0,2500000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())


#### 그래프 pres OUT ----
ggplot(data = OUT_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = OUT_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = OUT_peni, aes(color = "Penicillins")) +
  geom_point(data = OUT_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = OUT_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = OUT_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = OUT_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = OUT_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = OUT_macro, aes(color = "Macrolides")) +
  geom_point(data = OUT_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = OUT_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = OUT_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = OUT_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = OUT_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = OUT_amphe, aes(color = "Amphenicols")) +
  geom_point(data = OUT_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = OUT_keto, aes(color = "Ketolides")) +
  geom_point(data = OUT_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = OUT_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="prescription number", limits = c(0,10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines", 
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols", 
                                                   "Ketolides", "Other")) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())



