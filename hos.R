
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
for (i in 1:72) {
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


#csv파일로 저장
write.csv(IN_anti, "MEDICINE/data/hos/in/IN_anti.csv",row.names = FALSE)
write.csv(IN_peni, "MEDICINE/data/hos/in/IN_penicillins.csv",row.names = FALSE)
write.csv(IN_cepha, "MEDICINE/data/hos/in/IN_cephalosporins.csv",row.names = FALSE)
write.csv(IN_tetra, "MEDICINE/data/hos/in/IN_tetracyclines.csv",row.names = FALSE)
write.csv(IN_macro, "MEDICINE/data/hos/in/IN_macrolides.csv",row.names = FALSE)
write.csv(IN_glyco, "MEDICINE/data/hos/in/IN_glycopeptides.csv",row.names = FALSE)
write.csv(IN_amino, "MEDICINE/data/hos/in/IN_aminoglycosides.csv",row.names = FALSE)
write.csv(IN_amphe, "MEDICINE/data/hos/in/IN_amphenicols.csv",row.names = FALSE)
write.csv(IN_keto, "MEDICINE/data/hos/in/IN_ketolides.csv",row.names = FALSE)
write.csv(IN_other, "MEDICINE/data/hos/in/IN_other_anti.csv",row.names = FALSE)

#하위 폴더 바로 불러오기
IN_anti <- read_csv("MEDICINE/data/hos/in/IN_anti.csv")
IN_peni <- read_csv("MEDICINE/data/hos/in/IN_penicillins.csv")
IN_cepha <- read_csv("MEDICINE/data/hos/in/IN_cephalosporins.csv")
IN_tetra <- read_csv("MEDICINE/data/hos/in/IN_tetracyclines.csv")
IN_macro <- read_csv("MEDICINE/data/hos/in/IN_macrolides.csv")
IN_glyco <- read_csv("MEDICINE/data/hos/in/IN_glycopeptides.csv")
IN_amino <- read_csv("MEDICINE/data/hos/in/IN_aminoglycosides.csv")
IN_amphe <- read_csv("MEDICINE/data/hos/in/IN_amphenicols.csv")
IN_keto <- read_csv("MEDICINE/data/hos/in/IN_ketolides.csv")
IN_other <- read_csv("MEDICINE/data/hos/in/IN_other_anti.csv")


##병원외 OUT 항생제 분류
OUT_anti <- medicine_result_anti_hos2 %>% 
  filter(OUT_IN_HOS == "OUT") %>% 
  group_by(USE_MONTH, USE_YEAR, OUT_IN_HOS, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt), category_amt = sum(category_amt))

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
write.csv(OUT_anti, "MEDICINE/data/hos/out/OUT_anti.csv",row.names = FALSE)
write.csv(OUT_peni, "MEDICINE/data/hos/out/OUT_penicillins.csv",row.names = FALSE)
write.csv(OUT_cepha, "MEDICINE/data/hos/out/OUT_cephalosporins.csv",row.names = FALSE)
write.csv(OUT_tetra, "MEDICINE/data/hos/out/OUT_tetracyclines.csv",row.names = FALSE)
write.csv(OUT_macro, "MEDICINE/data/hos/out/OUT_macrolides.csv",row.names = FALSE)
write.csv(OUT_glyco, "MEDICINE/data/hos/out/OUT_glycopeptides.csv",row.names = FALSE)
write.csv(OUT_amino, "MEDICINE/data/hos/out/OUT_aminoglycosides.csv",row.names = FALSE)
write.csv(OUT_amphe, "MEDICINE/data/hos/out/OUT_amphenicols.csv",row.names = FALSE)
write.csv(OUT_keto, "MEDICINE/data/hos/out/OUT_ketolides.csv",row.names = FALSE)
write.csv(OUT_other, "MEDICINE/data/hos/out/OUT_other_anti.csv",row.names = FALSE)

#하위 폴더 바로 불러오기
OUT_anti <- read_csv("MEDICINE/data/hos/out/OUT_anti.csv")
OUT_peni <- read_csv("MEDICINE/data/hos/out/OUT_penicillins.csv")
OUT_cepha <- read_csv("MEDICINE/data/hos/out/OUT_cephalosporins.csv")
OUT_tetra <- read_csv("MEDICINE/data/hos/out/OUT_tetracyclines.csv")
OUT_macro <- read_csv("MEDICINE/data/hos/out/OUT_macrolides.csv")
OUT_glyco <- read_csv("MEDICINE/data/hos/out/OUT_glycopeptides.csv")
OUT_amino <- read_csv("MEDICINE/data/hos/out/OUT_aminoglycosides.csv")
OUT_amphe <- read_csv("MEDICINE/data/hos/out/OUT_amphenicols.csv")
OUT_keto <- read_csv("MEDICINE/data/hos/out/OUT_ketolides.csv")
OUT_other <- read_csv("MEDICINE/data/hos/out/OUT_other_anti.csv")


# 데이터 결합
hos_anti <- rbind(IN_anti, OUT_anti)
hos_peni <- rbind(IN_peni, OUT_peni)
hos_cepha <- rbind(IN_cepha, OUT_cepha)
hos_tetra <- rbind(IN_tetra, OUT_tetra)
hos_macro <- rbind(IN_macro, OUT_macro)
hos_glyco <- rbind(IN_glyco, OUT_glyco)
hos_amino <- rbind(IN_amino, OUT_amino)
hos_amphe <- rbind(IN_amphe, OUT_amphe)
hos_keto <- rbind(IN_keto, OUT_keto)
hos_other <- rbind(IN_other, OUT_other)

#csv파일로 저장
write.csv(hos_anti, "MEDICINE/data/hos/hos_anti.csv",row.names = FALSE)
write.csv(hos_peni, "MEDICINE/data/hos/hos_penicillins.csv",row.names = FALSE)
write.csv(hos_cepha, "MEDICINE/data/hos/hos_cephalosporins.csv",row.names = FALSE)
write.csv(hos_tetra, "MEDICINE/data/hos/hos_tetracyclines.csv",row.names = FALSE)
write.csv(hos_macro, "MEDICINE/data/hos/hos_macrolides.csv",row.names = FALSE)
write.csv(hos_glyco, "MEDICINE/data/hos/hos_glycopeptides.csv",row.names = FALSE)
write.csv(hos_amino, "MEDICINE/data/hos/hos_aminoglycosides.csv",row.names = FALSE)
write.csv(hos_amphe, "MEDICINE/data/hos/hos_amphenicols.csv",row.names = FALSE)
write.csv(hos_keto, "MEDICINE/data/hos/hos_ketolides.csv",row.names = FALSE)
write.csv(hos_other, "MEDICINE/data/hos/hos_other_anti.csv",row.names = FALSE)

#하위 폴더 바로 불러오기
hos_anti <- read_csv("MEDICINE/data/hos/hos_anti.csv")
hos_peni <- read_csv("MEDICINE/data/hos/hos_penicillins.csv")
hos_cepha <- read_csv("MEDICINE/data/hos/hos_cephalosporins.csv")
hos_tetra <- read_csv("MEDICINE/data/hos/hos_tetracyclines.csv")
hos_macro <- read_csv("MEDICINE/data/hos/hos_macrolides.csv")
hos_glyco <- read_csv("MEDICINE/data/hos/hos_glycopeptides.csv")
hos_amino <- read_csv("MEDICINE/data/hos/hos_aminoglycosides.csv")
hos_amphe <- read_csv("MEDICINE/data/hos/hos_amphenicols.csv")
hos_keto <- read_csv("MEDICINE/data/hos/hos_ketolides.csv")
hos_other <- read_csv("MEDICINE/data/hos/hos_other_anti.csv")



#그래프 pt hos ----
options(scipen = 5) #과학적 스케일 적용
ggplot(data = hos_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = hos_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = hos_peni, aes(color = "Penicillins")) +
  geom_point(data = hos_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = hos_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = hos_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = hos_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = hos_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = hos_macro, aes(color = "Macrolides")) +
  geom_point(data = hos_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = hos_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = hos_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = hos_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = hos_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = hos_amphe, aes(color = "Amphenicols")) +
  geom_point(data = hos_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = hos_keto, aes(color = "Ketolides")) +
  geom_point(data = hos_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = hos_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 10000000)) +
  scale_x_date(name="Date", breaks = "3 months", date_labels = paste0("%Y",".","%m"), limits = as.Date(c('2018-01-01','2023-12-1'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols",
                                                   "Ketolides", "Other")) +
  facet_wrap(~OUT_IN_HOS) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())




####그래프 pres hos ----
ggplot(data = hos_anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = hos_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = hos_peni, aes(color = "Penicillins")) +
  geom_point(data = hos_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = hos_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = hos_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = hos_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = hos_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = hos_macro, aes(color = "Macrolides")) +
  geom_point(data = hos_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = hos_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = hos_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = hos_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = hos_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = hos_amphe, aes(color = "Amphenicols")) +
  geom_point(data = hos_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = hos_keto, aes(color = "Ketolides")) +
  geom_point(data = hos_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = hos_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Prescription Number", limits = c(0, 12000000)) +
  scale_x_date(name="Date",breaks = "3 months", date_labels = paste0("%Y",".","%m"), limits = as.Date(c('2018-01-01','2023-12-1'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
                                                   "Macrolides", "Glycopeptides", "Aminoglycosides", "Amphenicols",
                                                   "Ketolides", "Other")) +
  facet_wrap(~OUT_IN_HOS) +
  theme_gray(20) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(size = rel(1.0)),
        legend.title = element_blank())

####그래프 amt hos ----


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


#### 그래프 amt IN ----



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



#### 그래프 amt OUT ----







