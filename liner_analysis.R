library(tidyverse)
library(nlme)
library(AICcmodavg)
library(writexl)
library(forecast)

total_pt <- read_excel("total_pt_liner.xlsx")
View(total_pt)

#### Part Two - Uncontrolled ITS, two interventions ----
df <- total_pt

# add date
df$date <- as.Date(paste(as.character(df$date), '01'), format='%Y%m%d')

# Figure
ggplot(data=df, aes(x = date, y = quantity.x)) + 
  ggtitle("IA70, 2018-2023") +
  geom_point(color="black", size=3, alpha=0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  scale_x_date(name="Date", breaks = "12 months", date_labels = "%Y-%m", limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1)) +
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())


#####  total pt -----
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

model.a = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df, method="ML")

# Show a summary of the model
summary(model.a)

# Extract the summary of the model
summary_model <- summary(model.a)

# Manually extract the coefficients and relevant statistics
coefficients <- summary_model$tTable
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL

# Write the data frame to an Excel file
write_xlsx(coefficients_df, "analysis/toal_liner.xlsx")

df <- df %>% mutate(
  model.a.predictions = predictSE.gls (model.a, df, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, df, se.fit=T)$se
)

# 반사실 그래프화
df2<-filter(df,Time<26)

model.b = gls(quantity.x ~ Time, data = df2, method="ML")

df<-df %>% mutate(
  model.b.predictions = predictSE.gls (model.b, newdata = df, se.fit=T)$fit,
  model.b.se = predictSE.gls (model.b, df, se.fit=T)$se
)

df3<-filter(df,Time<50)
model.c = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = df3, method="ML")

df<-df %>% mutate(
  model.c.predictions = predictSE.gls (model.c, newdata = df, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, df, se.fit=T)$se
)

options(scipen = 8)
ggplot(data=df, aes(x = date, y = quantity.x)) + 
  geom_ribbon(aes(ymin = model.c.predictions - (1.96*model.c.se), ymax = model.c.predictions + (1.96*model.c.se)), fill = "lightblue",alpha=0.5)+
  geom_line(aes(date,model.c.predictions),color="blue", lty=2, size=2, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.b.predictions - (1.96*model.b.se), ymax = model.b.predictions + (1.96*model.b.se)), fill = "pink",alpha=0.5)+
  geom_line(aes(date,model.b.predictions),color="red", lty=2, size=2, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.a.se), ymax = model.a.predictions + (1.96*model.a.se)), fill = "lightgrey",alpha=0.5)+
  geom_line(aes(date,model.a.predictions),color="black", lty=1, size=2, alpha=0.75)+
  geom_point(color="black", size=3, alpha=0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  scale_y_continuous(name="Patient Number") +
  scale_x_date(name="Date", breaks = "6 months", date_labels = "%Y-%m", limits = as.Date(c('2018-01-01','2023-12-01')),expand = c(0,1))+
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())

##### toal pres -----
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

model.a = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df, method="ML")

# Show a summary of the model
summary(model.a)

# Extract the summary of the model
summary_model <- summary(model.a)

# Manually extract the coefficients and relevant statistics
coefficients <- summary_model$tTable
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL

# Write the data frame to an Excel file
write_xlsx(coefficients_df, "analysis/total_liner.xlsx")

df <- df %>% mutate(
  model.a.predictions = predictSE.gls (model.a, df, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, df, se.fit=T)$se
)

# 반사실 그래프화
df2<-filter(df,Time<26)

model.b = gls(quantity.x ~ Time, data = df2, method="ML")

df<-df %>% mutate(
  model.b.predictions = predictSE.gls (model.b, newdata = df, se.fit=T)$fit,
  model.b.se = predictSE.gls (model.b, df, se.fit=T)$se
)

df3<-filter(df,Time<50)
model.c = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = df3, method="ML")

df<-df %>% mutate(
  model.c.predictions = predictSE.gls (model.c, newdata = df, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, df, se.fit=T)$se
)

ggplot(data=df, aes(x = date, y = quantity.x)) + 
  geom_ribbon(aes(ymin = model.c.predictions - (1.96*model.c.se), ymax = model.c.predictions + (1.96*model.c.se)), fill = "lightblue")+
  geom_line(aes(date,model.c.predictions),color="blue", lty=2, size=2, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.b.predictions - (1.96*model.b.se), ymax = model.b.predictions + (1.96*model.b.se)), fill = "pink")+
  geom_line(aes(date,model.b.predictions),color="red", lty=2, size=2, alpha=0.75)+
  
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.a.se), ymax = model.a.predictions + (1.96*model.a.se)), fill = "lightgrey")+
  geom_line(aes(date,model.a.predictions),color="black", lty=1, size=2, alpha=0.75)+
  geom_point(color="black", size=3, alpha=0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dashed", size=1) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype="dashed", size=1) +
  geom_hline(yintercept=0, linetype="solid", size=2) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="solid", size=2)+
  scale_y_continuous(name="Incidence rate") +
  scale_x_date(name="Date", breaks = "6 months", date_labels = "%Y-%m", limits = as.Date(c('2018-01-01','2023-07-01')))+
  theme_gray(20)+
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey90", size=0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title=element_text(size=rel(1.0)),
        legend.title=element_blank())
