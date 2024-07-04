library(tidyverse)
library(nlme)
library(AICcmodavg)

#Uncontrolled ITS, two interventions ----
df1 <- readxl::read_xlsx("total.xlsx")

#model 설정 및 ARMA 설정
## P와 Q 탐지
mod.1 = quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval,qval){summary(gls(mod.1, data = df1, correlation= corARMA(p=pval,q=qval, form = ~ Time),method="ML"))$AIC}

p = summary(gls(mod.1, data = df1,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for(i in 2:nrow(autocorrel)){p[i] = try(summary(gls(mod.1, data = df1, correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ Time),method="ML"))$AIC)}

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel



#분석 model 형성
model.a = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df1,method="ML", correlation= corARMA(p=1,q=0, form = ~ Time))

# Show a summary of the model
summary(model.a)

df1<-df1 %>% mutate(
  model.a.predictions = predictSE.gls (model.a, df1, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, df1, se.fit=T)$se
)

ggplot(df1,aes(Time,quantity.x2))+
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.a.se), ymax = model.a.predictions + (1.96*model.a.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.a.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)


# 반사실 그래프화
df2<-filter(df1,Time<25)

model.b = gls(quantity.x2 ~ Time, data = df2, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

df1<-df1 %>% mutate(
  model.b.predictions = predictSE.gls (model.b, newdata = df1, se.fit=T)$fit,
  model.b.se = predictSE.gls (model.b, df1, se.fit=T)$se
)

df3<-filter(df1,Time<49)
model.c = gls(quantity.x2 ~ Time + Intervention + Post.intervention.time, data = df3, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

df1<-df1 %>% mutate(
  model.c.predictions = predictSE.gls (model.C, newdata = df1, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, df1, se.fit=T)$se
)

# log -> exponential(원본 데이터에서 quantity.x2를 로그로 바꾸면 써야함)
df1 <- df1 %>% 
  mutate(quantity.x2 = exp(quantity.x2),
         model.a.predictions = exp(model.a.predictions),
         model.a.se = exp(model.a.se),
         model.b.predictions = exp(model.b.predictions),
         model.b.se = exp(model.b.se),
         model.c.predictions = exp(model.c.predictions),
         model.c.se = exp(model.c.se))


#전체 그래프
ggplot(df1,aes(Time,quantity.x2))+
  geom_ribbon(aes(ymin = model.c.predictions - (1.96*model.d.se), ymax = model.c.predictions + (1.96*model.e.se)), fill = "lightblue")+
  geom_line(aes(Time,model.c.predictions),color="blue",lty=2)+
  geom_ribbon(aes(ymin = model.b.predictions - (1.96*model.d.se), ymax = model.b.predictions + (1.96*model.e.se)), fill = "pink")+
  geom_line(aes(Time,model.b.predictions),color="red",lty=2)+
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.d.se), ymax = model.a.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.a.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)
