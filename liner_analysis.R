library(tidyverse)
library(nlme)
library(AICcmodavg)
library(writexl)
library(readxl)


# total pt ----
df <- read_excel("MEDICINE/analysis/liner/total_liner.xlsx")
View(df)

# add date
df$date <- as.Date(paste(as.character(df$date), '01'), format='%Y%m%d')

##total pt Figure ----
options(scipen = 10)
ggplot(data=df, aes(x = date, y = pt)) + 
  ggtitle("Toal Patient Number") +
  geom_point(color="black", size=3, alpha=0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  geom_label(aes(x = as.Date("2020-02-01"), y = max(pt) * 1.2), label = "2020-02-01", fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  geom_label(aes(x = as.Date("2022-02-01"), y = max(pt) * 1.2), label = "2022-02-01", fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  
  scale_y_continuous(name="Patient Number", limits = c(0, max(df$pt) * 1.2)) +
  scale_x_date(name="Date", breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "1 year"),
               limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1)) +
  
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0), hjust = 0.5, face = 'bold'),
        legend.title=element_blank())


##total pt liner -----
mod.1 = pt ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

model.a = gls(pt ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df, method="ML")

# Show a summary of the model
summary(model.a)

# Extract the summary of the model
summary_model <- summary(model.a)

# Manually extract the coefficients and relevant statistics
coefficients <- summary_model$tTable
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL
coefficients_df <- coefficients_df %>%
  column_to_rownames(var = "Variable")
coefficients_df <- as.data.frame(t(coefficients_df))
coefficients_df <- coefficients_df[,-1]

coefficients_df <- cbind(rownames(coefficients_df), coefficients_df)
colnames(coefficients_df)[1] <- "Variable" # 새 열 이름 지정


# Write the data frame to an Excel file
write_xlsx(coefficients_df, "MEDICINE/analysis/liner/table2/total_pt_liner.xlsx")

df <- df %>% mutate(
  model.a.predictions = predictSE.gls (model.a, df, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, df, se.fit=T)$se
)

# 반사실 그래프화
df2<-filter(df,Time<26)

model.b = gls(pt ~ Time, data = df2, method="ML")

df<-df %>% mutate(
  model.b.predictions = predictSE.gls (model.b, newdata = df, se.fit=T)$fit,
  model.b.se = predictSE.gls (model.b, df, se.fit=T)$se
)

df3<-filter(df,Time<50)
model.c = gls(pt ~ Time + Intervention + Post.intervention.time, data = df3, method="ML")

df<-df %>% mutate(
  model.c.predictions = predictSE.gls (model.c, newdata = df, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, df, se.fit=T)$se
)


ggplot(data=df, aes(x = date, y = pt)) + 
  ggtitle("Toal Patient Number") +
  geom_ribbon(aes(ymin = model.c.predictions - (1.96*model.c.se), ymax = model.c.predictions + (1.96*model.c.se)), fill = "lightblue",alpha=0.5)+
  geom_line(aes(date,model.c.predictions),color="blue", lty=2, size=1.5, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.b.predictions - (1.96*model.b.se), ymax = model.b.predictions + (1.96*model.b.se)), fill = "pink",alpha=0.5)+
  geom_line(aes(date,model.b.predictions),color="red", lty=2, size=1.5, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.a.se), ymax = model.a.predictions + (1.96*model.a.se)), fill = "lightgrey",alpha=0.75)+
  geom_line(aes(date,model.a.predictions),color="black", lty=1, size=1.5, alpha=0.75)+
  geom_point(color="black", size=3, alpha=0.3) +
  
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  geom_label(aes(x = as.Date("2020-02-01"), y = max(pt) * 1.2), label = "2020-02",
             fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  geom_label(aes(x = as.Date("2022-02-01"), y = max(pt) * 1.2), label = "2022-02",
             fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  
  scale_y_continuous(name="Patient Number", limits = c(0, max(df$pt) * 1.2)) +
  scale_x_date(name="Date", breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "1 year"),
               limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1),date_labels = "%Y") +
  
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0), hjust = 0.5, face = 'bold'),
        legend.title=element_blank())


#total pres ----
df <- read_excel("MEDICINE/analysis/liner/total_liner.xlsx")
View(df)

# add date
df$date <- as.Date(paste(as.character(df$date), '01'), format='%Y%m%d')

##total pres Figure ----
options(scipen = 10)
ggplot(data=df, aes(x = date, y = pres)) + 
  ggtitle("Toal Perscription Number") +
  geom_point(color="black", size=3, alpha=0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  geom_label(aes(x = as.Date("2020-02-01"), y = max(pres) * 1.2), label = "2020-02-01", fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  geom_label(aes(x = as.Date("2022-02-01"), y = max(pres) * 1.2), label = "2022-02-01", fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  
  scale_y_continuous(name="Perscription Number", limits = c(0, max(df$pres) * 1.2)) +
  scale_x_date(name="Date", breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "1 year"),
               limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1)) +
  
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0), hjust = 0.5, face = 'bold'),
        legend.title=element_blank())



##total pres liner -----
mod.1 = pres ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

model.a = gls(pres ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df, method="ML")

# Show a summary of the model
summary(model.a)

# Extract the summary of the model
summary_model <- summary(model.a)

# Manually extract the coefficients and relevant statistics
coefficients <- summary_model$tTable
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL
coefficients_df <- coefficients_df %>%
  column_to_rownames(var = "Variable")
coefficients_df <- as.data.frame(t(coefficients_df))
coefficients_df <- coefficients_df[,-1]

coefficients_df <- cbind(rownames(coefficients_df), coefficients_df)
colnames(coefficients_df)[1] <- "Variable" # 새 열 이름 지정


# Write the data frame to an Excel file
write_xlsx(coefficients_df, "MEDICINE/analysis/liner/table2/total_pres_liner.xlsx")

df <- df %>% mutate(
  model.a.predictions = predictSE.gls (model.a, df, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, df, se.fit=T)$se
)

# 반사실 그래프화
df2<-filter(df,Time<26)

model.b = gls(pres ~ Time, data = df2, method="ML")

df<-df %>% mutate(
  model.b.predictions = predictSE.gls (model.b, newdata = df, se.fit=T)$fit,
  model.b.se = predictSE.gls (model.b, df, se.fit=T)$se
)

df3<-filter(df,Time<50)
model.c = gls(pres ~ Time + Intervention + Post.intervention.time, data = df3, method="ML")

df<-df %>% mutate(
  model.c.predictions = predictSE.gls (model.c, newdata = df, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, df, se.fit=T)$se
)


ggplot(data=df, aes(x = date, y = pres)) + 
  ggtitle("Toal Perscription Number") +
  geom_ribbon(aes(ymin = model.c.predictions - (1.96*model.c.se), ymax = model.c.predictions + (1.96*model.c.se)), fill = "lightblue",alpha=0.5)+
  geom_line(aes(date,model.c.predictions),color="blue", lty=2, size=1.5, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.b.predictions - (1.96*model.b.se), ymax = model.b.predictions + (1.96*model.b.se)), fill = "pink",alpha=0.5)+
  geom_line(aes(date,model.b.predictions),color="red", lty=2, size=1.5, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.a.se), ymax = model.a.predictions + (1.96*model.a.se)), fill = "lightgrey",alpha=0.5)+
  geom_line(aes(date,model.a.predictions),color="black", lty=1, size=1.5, alpha=0.75)+
  geom_point(color="black", size=3, alpha=0.3) +
  
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  geom_label(aes(x = as.Date("2020-02-01"), y = max(pres) * 1.2), label = "2020-02",
             fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  geom_label(aes(x = as.Date("2022-02-01"), y = max(pres) * 1.2), label = "2022-02",
             fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  
  scale_y_continuous(name="Perscription Number", limits = c(0, max(df$pres) * 1.2)) +
  scale_x_date(name="Date", breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "1 year"),
               limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1),date_label = "%Y") +
  
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0), hjust = 0.5, face = 'bold'),
        legend.title=element_blank())



#total amt ----
df <- read_excel("MEDICINE/analysis/liner/total_liner.xlsx")
View(df)

# add date
df$date <- as.Date(paste(as.character(df$date), '01'), format='%Y%m%d')

##total amt Figure ----
options(scipen = 10)
ggplot(data=df, aes(x = date, y = amt)) + 
  ggtitle("Toal Perscription Amount") +
  geom_point(color="black", size=3, alpha=0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  geom_label(aes(x = as.Date("2020-02-01"), y = max(amt) * 1.2), label = "2020-02-01", fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  geom_label(aes(x = as.Date("2022-02-01"), y = max(amt) * 1.2), label = "2022-02-01", fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  
  scale_y_continuous(name="Perscription Amount", limits = c(0, max(df$amt) * 1.2)) +
  scale_x_date(name="Date", breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "1 year"),
               limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1)) +
  
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0), hjust = 0.5, face = 'bold'),
        legend.title=element_blank())



##total amt liner -----
mod.1 = amt ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

model.a = gls(amt ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df, method="ML")

# Show a summary of the model
summary(model.a)

# Extract the summary of the model
summary_model <- summary(model.a)

# Manually extract the coefficients and relevant statistics
coefficients <- summary_model$tTable
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL
coefficients_df <- coefficients_df %>%
  column_to_rownames(var = "Variable")
coefficients_df <- as.data.frame(t(coefficients_df))
coefficients_df <- coefficients_df[,-1]

coefficients_df <- cbind(rownames(coefficients_df), coefficients_df)
colnames(coefficients_df)[1] <- "Variable" # 새 열 이름 지정


# Write the data frame to an Excel file
write_xlsx(coefficients_df, "MEDICINE/analysis/liner/table2/total_amt_liner.xlsx")

df <- df %>% mutate(
  model.a.predictions = predictSE.gls (model.a, df, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, df, se.fit=T)$se
)

# 반사실 그래프화
df2<-filter(df,Time<26)

model.b = gls(amt ~ Time, data = df2, method="ML")

df<-df %>% mutate(
  model.b.predictions = predictSE.gls (model.b, newdata = df, se.fit=T)$fit,
  model.b.se = predictSE.gls (model.b, df, se.fit=T)$se
)

df3<-filter(df,Time<50)
model.c = gls(amt ~ Time + Intervention + Post.intervention.time, data = df3, method="ML")

df<-df %>% mutate(
  model.c.predictions = predictSE.gls (model.c, newdata = df, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, df, se.fit=T)$se
)


ggplot(data=df, aes(x = date, y = amt)) + 
  ggtitle("Toal Perscription Amount") +
  geom_ribbon(aes(ymin = model.c.predictions - (1.96*model.c.se), ymax = model.c.predictions + (1.96*model.c.se)), fill = "lightblue",alpha=0.5)+
  geom_line(aes(date,model.c.predictions),color="blue", lty=2, size=1.5, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.b.predictions - (1.96*model.b.se), ymax = model.b.predictions + (1.96*model.b.se)), fill = "pink",alpha=0.5)+
  geom_line(aes(date,model.b.predictions),color="red", lty=2, size=1.5, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.a.se), ymax = model.a.predictions + (1.96*model.a.se)), fill = "lightgrey",alpha=0.5)+
  geom_line(aes(date,model.a.predictions),color="black", lty=1, size=1.5, alpha=0.75)+
  geom_point(color="black", size=3, alpha=0.3) +
  
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  geom_label(aes(x = as.Date("2020-02-01"), y = max(amt) * 1.2), label = "2020-02",
             fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  geom_label(aes(x = as.Date("2022-02-01"), y = max(amt) * 1.2), label = "2022-02",
             fill = "white", color = "black", angle = 0, vjust = -0.5, hjust = 0.5, size = 4) +
  
  scale_y_continuous(name="Perscription Amount", limits = c(0, max(df$amt) * 1.2)) +
  scale_x_date(name="Date", breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "1 year"),
               limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1),date_labels = "%Y") +
  
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0), hjust = 0.5, face = 'bold'),
        legend.title=element_blank())









