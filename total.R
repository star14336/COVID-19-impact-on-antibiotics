
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
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-16")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 10000000)) +
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
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



##penicillins
ggplot(data=peni, aes(x = date, y = category_pt)) + 
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


##Cephalosporins
ggplot(data=cepha, aes(x = date, y = category_pt)) + 
  geom_point(color="red", size=1.5) +
  geom_line(color="red") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,6000000)) +
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

##Tetracyclines
ggplot(data=tetra, aes(x = date, y = category_pt)) + 
  geom_point(color="blue", size=1.5) +
  geom_line(color="blue") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,250000)) +
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


##Macrolides
ggplot(data=macro, aes(x = date, y = category_pt)) + 
  geom_point(color="green", size=1.5) +
  geom_line(color="green") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,3000000)) +
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

##Glycopeptides
ggplot(data=glyco, aes(x = date, y = category_pt)) + 
  geom_point(color="pink", size=1.5) +
  geom_line(color="pink") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,30000)) +
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

##Aminoglycosides
ggplot(data=amino, aes(x = date, y = category_pt)) + 
  geom_point(color="purple", size=1.5) +
  geom_line(color="purple") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1000000)) +
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

##Amphenicols
ggplot(data=amphe, aes(x = date, y = category_pt)) + 
  geom_point(color="gray", size=1.5) +
  geom_line(color="gray") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1000)) +
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

##Ketolides
ggplot(data=keto, aes(x = date, y = category_pt)) + 
  geom_point(color="brown", size=1.5) +
  geom_line(color="brown") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,500000)) +
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

##other
ggplot(data=other, aes(x = date, y = category_pt)) + 
  geom_point(color="orange", size=1.5) +
  geom_line(color="orange") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,50000)) +
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

ggplot(data=other, aes(x = date, y = category_pt)) + 
  geom_point(color="orange", size=1.5) +
  geom_line(color="orange") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,10000)) +
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



####그래프 - pres ----
options(scipen = 5) #과학적 스케일 적용

#성분별 그래프
#전제
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
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-16")), linetype="dashed") +
  scale_y_continuous(name="Patient Number", limits = c(0, 15000000)) +
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
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



##penicillins
ggplot(data=peni, aes(x = date, y = category_pres)) + 
  geom_point(color="black", size=1.5) +
  geom_line(color="black") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="pres",limits = c(0,2000000)) +
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


##Cephalosporins
ggplot(data=cepha, aes(x = date, y = category_pres)) + 
  geom_point(color="red", size=1.5) +
  geom_line(color="red") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,6000000)) +
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

##Tetracyclines
ggplot(data=tetra, aes(x = date, y = category_pres)) + 
  geom_point(color="blue", size=1.5) +
  geom_line(color="blue") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,300000)) +
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


##Macrolides
ggplot(data=macro, aes(x = date, y = category_pres)) + 
  geom_point(color="green", size=1.5) +
  geom_line(color="green") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,3000000)) +
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

##Glycopeptides
ggplot(data=glyco, aes(x = date, y = category_pres)) + 
  geom_point(color="pink", size=1.5) +
  geom_line(color="pink") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,30000)) +
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

##Aminoglycosides
ggplot(data=amino, aes(x = date, y = category_pres)) + 
  geom_point(color="purple", size=1.5) +
  geom_line(color="purple") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1100000)) +
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

##Amphenicols
ggplot(data=amphe, aes(x = date, y = category_pres)) + 
  geom_point(color="gray", size=1.5) +
  geom_line(color="gray") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,1000)) +
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

##Ketolides
ggplot(data=keto, aes(x = date, y = category_pres)) + 
  geom_point(color="brown", size=1.5) +
  geom_line(color="brown") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,500000)) +
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

##other
ggplot(data=other, aes(x = date, y = category_pres)) + 
  geom_point(color="orange", size=1.5) +
  geom_line(color="orange") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  #
  scale_y_continuous(name="number",limits = c(0,50000)) +
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
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-16")), linetype="dashed") +
  scale_y_continuous(name="Prescription price", limits = c(0, 100000000000)) +
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
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



