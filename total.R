
#패키지
library(tidyverse)
library(ggplot2)
library(lubridate)


### 4/8 성분별 분류 ----

#항생제 분류(class,jo,prod,category)
medi_inform_anti <- read.csv("filtered_class_antibiotics.csv") %>% 
  mutate(WK_COMPN_4 = jo) %>% 
  mutate(WK_COMPN_4 = factor(WK_COMPN_4)) %>% 
  select(-jo)


medicine_result_anti <- inner_join(MEDICINE_result, medi_inform_anti, by='WK_COMPN_4')

colSums(is.na(medicine_result_anti))
head(medicine_result_anti$pt)

#category로 합치기
medicine_result_anti2 <- medicine_result_anti %>% 
  group_by(USE_YEAR,USE_MONTH,category) %>% 
  summarise(category_pres = sum(pres), category_pt = sum(pt), category_amt = sum(amt))

#날짜 만들기
medicine_result_anti2$date = paste0(medicine_result_anti2$USE_YEAR,medicine_result_anti2$USE_MONTH)
medicine_result_anti2$date <- as.Date(paste(as.character(medicine_result_anti2$date), '01'), format='%Y%m%d')

###각 성분별 분류
{
  ##total
  anti <- medicine_result_anti %>% 
    group_by(USE_YEAR,USE_MONTH) %>% 
    summarise(category_pres = sum(pres), category_pt = sum(pt),category_amt = sum(amt))
  
  #날짜 만들기
  anti$date = paste0(anti$USE_YEAR,anti$USE_MONTH)
  anti$date <- as.Date(paste(as.character(anti$date), '01'), format='%Y%m%d')
  
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

#csv파일로 저장
write.csv(anti, "MEDICINE/data/total/anti.csv",row.names = FALSE)
write.csv(peni, "MEDICINE/data/total/penicillins.csv",row.names = FALSE)
write.csv(cepha, "MEDICINE/data/total/cephalosporins.csv",row.names = FALSE)
write.csv(tetra, "MEDICINE/data/total/tetracyclines.csv",row.names = FALSE)
write.csv(macro, "MEDICINE/data/total/macrolides.csv",row.names = FALSE)
write.csv(glyco, "MEDICINE/data/total/glycopeptides.csv",row.names = FALSE)
write.csv(amino, "MEDICINE/data/total/aminoglycosides.csv",row.names = FALSE)
write.csv(amphe, "MEDICINE/data/total/amphenicols.csv",row.names = FALSE)
write.csv(keto, "MEDICINE/data/total/ketolides.csv",row.names = FALSE)
write.csv(other, "MEDICINE/data/total/other_anti.csv",row.names = FALSE)

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
other <- read_csv("MEDICINE/data/total/other_anti.csv")

####그래프 - pt ----
options(scipen = 5) #과학적 스케일 적용

#성분별 그래프
##전체
ggplot(data = anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = peni, aes(color = "Penicillins")) +
  geom_point(data = cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = cepha, aes(color = "Cephalosporins")) +
  geom_point(data = tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = tetra, aes(color = "Tetracyclines")) +
  geom_point(data = macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = macro, aes(color = "Macrolides")) +
  geom_point(data = glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = glyco, aes(color = "Glycopeptides")) +
  geom_point(data = amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = amino, aes(color = "Aminoglycosides")) +
  geom_point(data = amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = amphe, aes(color = "Amphenicols")) +
  geom_point(data = keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = keto, aes(color = "Ketolides")) +
  geom_point(data = other, aes(color = "Other"), size = 1.5) +
  geom_line(data = other, aes(color = "Other")) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=1)+
  geom_hline(yintercept=0, linetype="solid", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-16")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 10000000)) +
  scale_x_date(name=" ", breaks = "3 months", date_labels = paste0("%Y",".","%m"), limits = as.Date(c('2018-01-01','2023-12-01'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Antibiotic Category",labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
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


#수정헀는데 날짜 표시가 이상함, 확인 필요 ----
ggplot(data = anti, aes(x = date, y = category_pt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = peni, aes(color = "Penicillins")) +
  geom_point(data = cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = cepha, aes(color = "Cephalosporins")) +
  geom_point(data = tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = tetra, aes(color = "Tetracyclines")) +
  geom_point(data = macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = macro, aes(color = "Macrolides")) +
  geom_point(data = glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = glyco, aes(color = "Glycopeptides")) +
  geom_point(data = amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = amino, aes(color = "Aminoglycosides")) +
  geom_point(data = amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = amphe, aes(color = "Amphenicols")) +
  geom_point(data = keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = keto, aes(color = "Ketolides")) +
  geom_point(data = other, aes(color = "Other"), size = 1.5) +
  geom_line(data = other, aes(color = "Other")) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-16")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 10000000)) +
  scale_x_date(name=" ", 
               breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "3 months"),
               date_labels = "%Y.%m", 
               limits = as.Date(c('2018-01-01','2023-12-01'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Antibiotic Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
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




####그래프 - pres ----
options(scipen = 5) #과학적 스케일 적용

#성분별 그래프
#전제ggplot(data = anti, aes(x = date, y = category_pt)) +
ggplot(data = anti, aes(x = date, y = category_pres)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = peni, aes(color = "Penicillins")) +
  geom_point(data = cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = cepha, aes(color = "Cephalosporins")) +
  geom_point(data = tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = tetra, aes(color = "Tetracyclines")) +
  geom_point(data = macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = macro, aes(color = "Macrolides")) +
  geom_point(data = glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = glyco, aes(color = "Glycopeptides")) +
  geom_point(data = amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = amino, aes(color = "Aminoglycosides")) +
  geom_point(data = amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = amphe, aes(color = "Amphenicols")) +
  geom_point(data = keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = keto, aes(color = "Ketolides")) +
  geom_point(data = other, aes(color = "Other"), size = 1.5) +
  geom_line(data = other, aes(color = "Other")) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=1)+
  geom_hline(yintercept=0, linetype="solid", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-16")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 15000000)) +
  scale_x_date(name=" ", breaks = "3 months", date_labels = paste0("%Y",".","%m"), limits = as.Date(c('2018-01-01','2023-12-01'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Antibiotic Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
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


####그래프 - amt ----
#전체
ggplot(data = anti, aes(x = date, y = category_amt)) +
  geom_point(aes(color = "Total(anti)"), size = 1.5) +
  geom_line(aes(color = "Total(anti)")) +
  geom_point(data = peni, aes(color = "Penicillins"), size = 1.5) +
  geom_line(data = peni, aes(color = "Penicillins")) +
  geom_point(data = cepha, aes(color = "Cephalosporins"), size = 1.5) +
  geom_line(data = cepha, aes(color = "Cephalosporins")) +
  geom_point(data = tetra, aes(color = "Tetracyclines"), size = 1.5) +
  geom_line(data = tetra, aes(color = "Tetracyclines")) +
  geom_point(data = macro, aes(color = "Macrolides"), size = 1.5) +
  geom_line(data = macro, aes(color = "Macrolides")) +
  geom_point(data = glyco, aes(color = "Glycopeptides"), size = 1.5) +
  geom_line(data = glyco, aes(color = "Glycopeptides")) +
  geom_point(data = amino, aes(color = "Aminoglycosides"), size = 1.5) +
  geom_line(data = amino, aes(color = "Aminoglycosides")) +
  geom_point(data = amphe, aes(color = "Amphenicols"), size = 1.5) +
  geom_line(data = amphe, aes(color = "Amphenicols")) +
  geom_point(data = keto, aes(color = "Ketolides"), size = 1.5) +
  geom_line(data = keto, aes(color = "Ketolides")) +
  geom_point(data = other, aes(color = "Other"), size = 1.5) +
  geom_line(data = other, aes(color = "Other")) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=1)+
  geom_hline(yintercept=0, linetype="solid", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-16")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 100000000000)) +
  scale_x_date(name=" ", breaks = "3 months", date_labels = paste0("%Y",".","%m"), limits = as.Date(c('2018-01-01','2023-09-01'))) +
  scale_color_manual(values = c("Total(anti)" = "Black", "Penicillins" = "Blue", "Cephalosporins" = "Red",
                                "Tetracyclines" = "Green", "Macrolides" = "Purple", "Glycopeptides" = "Brown",
                                "Aminoglycosides" = "Gray", "Amphenicols" = "Orange", "Ketolides" = "Pink", "Other" = "Turquoise"),
                     name = "Antibiotic Category", labels = c("Total(anti)", "Penicillins", "Cephalosporins", "Tetracyclines",
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



