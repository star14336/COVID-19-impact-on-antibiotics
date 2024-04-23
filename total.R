
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
  summarise(category_pres = sum(pres), category_pt = sum(pt))

#날짜 만들기
medicine_result_anti2$date = paste0(medicine_result_anti2$USE_YEAR,medicine_result_anti2$USE_MONTH)
medicine_result_anti2$date <- as.Date(paste(as.character(medicine_result_anti2$date), '01'), format='%Y%m%d')

###각 성분별 분류
{
  ##total
  anti <- medicine_result_anti %>% 
    group_by(USE_YEAR,USE_MONTH) %>% 
    summarise(category_pres = sum(pres), category_pt = sum(pt))
  
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
ggplot(data=peni, aes(x = date, y = category_pt)) + 
  geom_point(aes(color="Black"), size=1.5) +
  geom_line(aes(color="Black")) +
  geom_point(data=cepha, aes(x = date, y = category_pt, color = "Blue"), size=1.5) +
  geom_line(data=cepha, aes(x = date, y = category_pt, color = "Blue")) +
  geom_point(data=tetra, aes(x = date, y = category_pt, color = "Red"), size=1.5) +
  geom_line(data=tetra, aes(x = date, y = category_pt, color = "Red")) +
  geom_point(data=amino, aes(x = date, y = category_pt, color = "Green"), size=1.5) +
  geom_line(data=amino, aes(x = date, y = category_pt, color = "Green")) +
  geom_point(data=amphe, aes(x = date, y = category_pt, color = "Purple"), size=1.5) +
  geom_line(data=amphe, aes(x = date, y = category_pt, color = "Purple")) +
  geom_point(data=glyco, aes(x = date, y = category_pt, color = "Brown"), size=1.5) +
  geom_line(data=glyco, aes(x = date, y = category_pt, color = "Brown")) +
  geom_point(data=keto, aes(x = date, y = category_pt, color = "Gray"), size=1.5) +
  geom_line(data=keto, aes(x = date, y = category_pt, color = "Gray")) +
  geom_point(data=macro, aes(x = date, y = category_pt, color = "Orange"), size=1.5) +
  geom_line(data=macro, aes(x = date, y = category_pt, color = "Orange")) +
  geom_point(data=other, aes(x = date, y = category_pt, color = "Pink"), size=1.5) +
  geom_line(data=other, aes(x = date, y = category_pt, color = "Pink")) +
  geom_point(data=anti, aes(x = date, y = category_pt, color = "turquoise"), size=1.5) +
  geom_line(data=anti, aes(x = date, y = category_pt, color = "turquoise")) +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="patient number", limits = c(0,10000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values=c("Black", "Blue", "Red", "Green", "Purple", "Brown", "Gray", "Orange", "Pink","turquoise"),
                     labels=c("Peni", "Cepha", "Tetra", "Amino", "Amphe", "Glyco", "Keto", "Macro", "Other","Total(anti)")) +
  theme_gray(20) +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


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

ggplot(data=peni, aes(x = date, y = category_pres)) + 
  geom_point(aes(color="Black"), size=1.5) +
  geom_line(aes(color="Black")) +
  geom_point(data=cepha, aes(x = date, y = category_pres, color = "Blue"), size=1.5) +
  geom_line(data=cepha, aes(x = date, y = category_pres, color = "Blue")) +
  geom_point(data=tetra, aes(x = date, y = category_pres, color = "Red"), size=1.5) +
  geom_line(data=tetra, aes(x = date, y = category_pres, color = "Red")) +
  geom_point(data=amino, aes(x = date, y = category_pres, color = "Green"), size=1.5) +
  geom_line(data=amino, aes(x = date, y = category_pres, color = "Green")) +
  geom_point(data=amphe, aes(x = date, y = category_pres, color = "Purple"), size=1.5) +
  geom_line(data=amphe, aes(x = date, y = category_pres, color = "Purple")) +
  geom_point(data=glyco, aes(x = date, y = category_pres, color = "Brown"), size=1.5) +
  geom_line(data=glyco, aes(x = date, y = category_pres, color = "Brown")) +
  geom_point(data=keto, aes(x = date, y = category_pres, color = "Gray"), size=1.5) +
  geom_line(data=keto, aes(x = date, y = category_pres, color = "Gray")) +
  geom_point(data=macro, aes(x = date, y = category_pres, color = "Orange"), size=1.5) +
  geom_line(data=macro, aes(x = date, y = category_pres, color = "Orange")) +
  geom_point(data=other, aes(x = date, y = category_pres, color = "Pink"), size=1.5) +
  geom_line(data=other, aes(x = date, y = category_pres, color = "Pink")) +
  geom_point(data=total, aes(x = date, y = category_pres, color = "turquoise"), size=1.5) +
  geom_line(data=total, aes(x = date, y = category_pres, color = "turquoise")) +
  geom_hline(yintercept=0, linetype="solid") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed") +
  scale_y_continuous(name="prescription number", limits = c(0,13000000)) +
  scale_x_date(name=" ", breaks = "12 months", date_labels = "%Y", limits = as.Date(c('2018-01-01','2023-09-30'))) +
  scale_color_manual(values=c("Black", "Blue", "Red", "Green", "Purple", "Brown", "Gray", "Orange", "Pink","turquoise"),
                     labels=c("Peni", "Cepha", "Tetra", "Amino", "Amphe", "Glyco", "Keto", "Macro", "Other","Total")) +
  theme_gray(20) +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

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









