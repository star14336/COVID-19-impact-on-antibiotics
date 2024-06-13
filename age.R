
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
                           AGE_G >= 20 & AGE_G < 60~ 1,
                           AGE_G >= 60~ 2)) %>%
  mutate(AGE_G = factor(AGE_G, levels = c(0,1,2), labels = c("junior","adult","senior")))





#각 성분별 분류----
## Senior 항생제 분류----
S_anti <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR, AGE_G, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop')

S_peni <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
              category_amt = sum(category_amt), .groups = 'drop') %>% 
  filter(category == "Penicillins")

S_cepha <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Cephalosporins")

S_tetra <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Tetracyclines")

S_macro <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Macrolides")

S_glyco <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Glycopeptides")

S_amino <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Aminoglycosides")

S_amphe <- medicine_result_anti_age2 %>%
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Amphenicols")

S_keto <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Ketolides")

S_other <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "senior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Other")

# csv 파일로 저장(senior)
write.csv(S_anti, "MEDICINE/data/age/senior/S_anti.csv", row.names = FALSE)
write.csv(S_peni, "MEDICINE/data/age/senior/S_penicillins.csv", row.names = FALSE)
write.csv(S_cepha, "MEDICINE/data/age/senior/S_cephalosporins.csv", row.names = FALSE)
write.csv(S_tetra, "MEDICINE/data/age/senior/S_tetracyclines.csv", row.names = FALSE)
write.csv(S_macro, "MEDICINE/data/age/senior/S_macrolides.csv", row.names = FALSE)
write.csv(S_glyco, "MEDICINE/data/age/senior/S_glycopeptides.csv", row.names = FALSE)
write.csv(S_amino, "MEDICINE/data/age/senior/S_aminoglycosides.csv", row.names = FALSE)
write.csv(S_amphe, "MEDICINE/data/age/senior/S_amphenicols.csv", row.names = FALSE)
write.csv(S_keto, "MEDICINE/data/age/senior/S_ketolides.csv", row.names = FALSE)
write.csv(S_other, "MEDICINE/data/age/senior/S_other_anti.csv", row.names = FALSE)

head(S_peni)


## Adult 항생제 분류----
A_anti <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR, AGE_G, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop')

A_peni <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>% 
  filter(category == "Penicillins")

A_cepha <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Cephalosporins")

A_tetra <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Tetracyclines")

A_macro <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Macrolides")

A_glyco <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Glycopeptides")

A_amino <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Aminoglycosides")

A_amphe <- medicine_result_anti_age2 %>%
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Amphenicols")

A_keto <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Ketolides")

A_other <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "adult") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Other")

# csv 파일로 저장(adult)
write.csv(A_anti, "MEDICINE/data/age/adult/A_anti.csv", row.names = FALSE)
write.csv(A_peni, "MEDICINE/data/age/adult/A_penicillins.csv", row.names = FALSE)
write.csv(A_cepha, "MEDICINE/data/age/adult/A_cephalosporins.csv", row.names = FALSE)
write.csv(A_tetra, "MEDICINE/data/age/adult/A_tetracyclines.csv", row.names = FALSE)
write.csv(A_macro, "MEDICINE/data/age/adult/A_macrolides.csv", row.names = FALSE)
write.csv(A_glyco, "MEDICINE/data/age/adult/A_glycopeptides.csv", row.names = FALSE)
write.csv(A_amino, "MEDICINE/data/age/adult/A_aminoglycosides.csv", row.names = FALSE)
write.csv(A_amphe, "MEDICINE/data/age/adult/A_amphenicols.csv", row.names = FALSE)
write.csv(A_keto, "MEDICINE/data/age/adult/A_ketolides.csv", row.names = FALSE)
write.csv(A_other, "MEDICINE/data/age/adult/A_other_anti.csv", row.names = FALSE)

head(A_peni)



## Junior 항생제 분류----
J_anti <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR, AGE_G, date) %>% 
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop')

J_peni <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>% 
  filter(category == "Penicillins")

J_cepha <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Cephalosporins")

J_tetra <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Tetracyclines")

J_macro <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Macrolides")

J_glyco <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Glycopeptides")

J_amino <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Aminoglycosides")

J_amphe <- medicine_result_anti_age2 %>%
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Amphenicols")

J_keto <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Ketolides")

J_other <- medicine_result_anti_age2 %>% 
  filter(AGE_G == "junior") %>% 
  group_by(USE_MONTH, USE_YEAR,category ,AGE_G, date)%>%
  summarise(category_pres = sum(category_pres), category_pt = sum(category_pt),
            category_amt = sum(category_amt), .groups = 'drop') %>%
  filter(category == "Other")


head(J_peni)

# csv 파일로 저장(junior)
write.csv(J_anti, "MEDICINE/data/age/junior/J_anti.csv", row.names = FALSE)
write.csv(J_peni, "MEDICINE/data/age/junior/J_penicillins.csv", row.names = FALSE)
write.csv(J_cepha, "MEDICINE/data/age/junior/J_cephalosporins.csv", row.names = FALSE)
write.csv(J_tetra, "MEDICINE/data/age/junior/J_tetracyclines.csv", row.names = FALSE)
write.csv(J_macro, "MEDICINE/data/age/junior/J_macrolides.csv", row.names = FALSE)
write.csv(J_glyco, "MEDICINE/data/age/junior/J_glycopeptides.csv", row.names = FALSE)
write.csv(J_amino, "MEDICINE/data/age/junior/J_aminoglycosides.csv", row.names = FALSE)
write.csv(J_amphe, "MEDICINE/data/age/junior/J_amphenicols.csv", row.names = FALSE)
write.csv(J_keto, "MEDICINE/data/age/junior/J_ketolides.csv", row.names = FALSE)
write.csv(J_other, "MEDICINE/data/age/junior/J_other_anti.csv", row.names = FALSE)

head(J_peni)


#그래프 ----
# 하위 폴더 바로 불러오기
S_anti <- read_csv("MEDICINE/data/age/senior/S_anti.csv")
S_peni <- read_csv("MEDICINE/data/age/senior/S_penicillins.csv")
S_cepha <- read_csv("MEDICINE/data/age/senior/S_cephalosporins.csv")
S_tetra <- read_csv("MEDICINE/data/age/senior/S_tetracyclines.csv")
S_macro <- read_csv("MEDICINE/data/age/senior/S_macrolides.csv")
S_glyco <- read_csv("MEDICINE/data/age/senior/S_glycopeptides.csv")
S_amino <- read_csv("MEDICINE/data/age/senior/S_aminoglycosides.csv")
S_amphe <- read_csv("MEDICINE/data/age/senior/S_amphenicols.csv")
S_keto <- read_csv("MEDICINE/data/age/senior/S_ketolides.csv")
S_other <- read_csv("MEDICINE/data/age/senior/S_other_anti.csv")

head(S_peni)

# 저장된 데이터 불러오기
A_anti <- read_csv("MEDICINE/data/age/adult/A_anti.csv")
A_peni <- read_csv("MEDICINE/data/age/adult/A_penicillins.csv")
A_cepha <- read_csv("MEDICINE/data/age/adult/A_cephalosporins.csv")
A_tetra <- read_csv("MEDICINE/data/age/adult/A_tetracyclines.csv")
A_macro <- read_csv("MEDICINE/data/age/adult/A_macrolides.csv")
A_glyco <- read_csv("MEDICINE/data/age/adult/A_glycopeptides.csv")
A_amino <- read_csv("MEDICINE/data/age/adult/A_aminoglycosides.csv")
A_amphe <- read_csv("MEDICINE/data/age/adult/A_amphenicols.csv")
A_keto <- read_csv("MEDICINE/data/age/adult/A_ketolides.csv")
A_other <- read_csv("MEDICINE/data/age/adult/A_other_anti.csv")

head(A_peni)

# 저장된 데이터 불러오기
J_anti <- read_csv("MEDICINE/data/age/junior/J_anti.csv")
J_peni <- read_csv("MEDICINE/data/age/junior/J_penicillins.csv")
J_cepha <- read_csv("MEDICINE/data/age/junior/J_cephalosporins.csv")
J_tetra <- read_csv("MEDICINE/data/age/junior/J_tetracyclines.csv")
J_macro <- read_csv("MEDICINE/data/age/junior/J_macrolides.csv")
J_glyco <- read_csv("MEDICINE/data/age/junior/J_glycopeptides.csv")
J_amino <- read_csv("MEDICINE/data/age/junior/J_aminoglycosides.csv")
J_amphe <- read_csv("MEDICINE/data/age/junior/J_amphenicols.csv")
J_keto <- read_csv("MEDICINE/data/age/junior/J_ketolides.csv")
J_other <- read_csv("MEDICINE/data/age/junior/J_other_anti.csv")

head(J_peni)


options(scipen = 5)
#senior(60세 이상),adult(20세 이상 60세 미만) junior(20세 미만) pt
# 데이터 결합
age_anti<- rbind(S_anti, A_anti,J_anti)
age_peni <- rbind(S_peni,A_peni, J_peni)
age_cepha <- rbind(S_cepha,A_cepha, J_cepha)
age_tetra <- rbind(S_tetra,A_tetra,J_tetra)
age_macro<- rbind(S_macro,A_macro, J_macro)
age_glyco<- rbind(S_glyco,A_glyco, J_glyco)
age_amino <- rbind(S_amino,A_amino, J_amino)
age_amphe <- rbind(S_amphe,A_amphe, J_amphe)
age_keto <- rbind(S_keto,A_keto, J_keto)
age_other <- rbind(S_other,A_other, J_other)


#그래프
ggplot(data = age_anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = age_peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = age_peni, aes(color = "Penicillins")) +
  geom_point(data = age_cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = age_cepha, aes(color = "Cephalosporins")) +
  geom_point(data = age_tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = age_tetra, aes(color = "Tetracyclines")) +
  geom_point(data = age_macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = age_macro, aes(color = "Macrolides")) +
  geom_point(data = age_glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = age_glyco, aes(color = "Glycopeptides")) +
  geom_point(data = age_amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = age_amino, aes(color = "Aminoglycosides")) +
  geom_point(data = age_amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = age_amphe, aes(color = "Amphenicols")) +
  geom_point(data = age_keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = age_keto, aes(color = "Ketolides")) +
  geom_point(data = age_other, aes(color = "Other"), size = 1.5) +
  geom_line(data = age_other, aes(color = "Other")) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 8000000)) +
  scale_x_date(name=" ", breaks = "3 months", date_labels = paste0("%Y",".","%m"), limits = as.Date(c('2018-01-01','2023-12-31'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red", 
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown", 
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Category") +
  labs(title = "Patient number by Age Group") +
  facet_wrap(~AGE_G) +  # 패싯을 추가하여 senior와 adult, junior 구분 - adult, junior,senior순으로 나타남 해당 순서를 어떻게 바꿀지는 나중에 그래프 전체적인 코드 수정시 할 예정 
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


