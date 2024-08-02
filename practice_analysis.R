#### Part One - Uncontrolled ITS, one intervention ----

library(tidyverse)
library(nlme)
library(AICcmodavg)

df<-tibble(
  Time = 1:100,
  Intervention = c(rep(0,50),rep(1,50)),
  Post.intervention.time = c(rep(0,50),1:50),
  quantity.x = c(sort(sample(200:300,size = 50,replace = T),decreasing = T)+sample(-20:20,50,replace = T),c(sort(sample(20:170,size = 50,replace = T),decreasing = T)+sample(-40:40,50,replace = T)))
)

model.a = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = df,method="ML")

# Show a summary of the model
summary(model.a)

df<-df %>% mutate(
  model.a.predictions = predictSE.gls (model.a, df, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, df, se.fit=T)$se
)

ggplot(df,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.a.predictions - (1.96*model.a.se), ymax = model.a.predictions + (1.96*model.a.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.a.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time

fx = function(pval,qval){summary(gls(mod.1, data = df, correlation= corARMA(p=pval,q=qval, form = ~ Time),method="ML"))$AIC}

p = summary(gls(mod.1, data = df,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:2, qval = 0:2)

for(i in 2:nrow(autocorrel)){p[i] = try(summary(gls(mod.1, data = df, correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ Time),method="ML"))$AIC)}

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.b = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = df,method="ML", correlation= corARMA(p=2,q=2, form = ~ Time))

coefficients(model.a)
coefficients(model.b)

df<- df %>% 
  mutate(
    model.b.predictions = predictSE.gls (model.b, df, se.fit=T)$fit,
    model.b.se = predictSE.gls (model.b, df, se.fit=T)$se
  )

df2<-filter(df,Time<51)

model.c = gls(quantity.x ~ Time, data = df2, correlation= corARMA(p=1, q=1, form = ~ Time),method="ML")

coefficients(model.a)

coefficients(model.c)

df<-df %>% mutate(
  model.c.predictions = predictSE.gls (model.c, newdata = df, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, df, se.fit=T)$se
)

ggplot(df,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.c.predictions - (1.96*model.c.se), ymax = model.c.predictions + (1.96*model.c.se)), fill = "pink")+
  geom_line(aes(Time,model.c.predictions),color="red",lty=2)+
  geom_ribbon(aes(ymin = model.b.predictions - (1.96*model.b.se), ymax = model.b.predictions + (1.96*model.b.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.b.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

#### Part Two - Uncontrolled ITS, two interventions ----

df3<-tibble(
  Time = 1:150,
  Intervention = c(rep(0,50),rep(1,100)),
  Post.intervention.time = c(rep(0,50),1:100),
  Intervention.2 = c(rep(0,100),rep(1,50)),
  Post.intervention.2.time = c(rep(0,100),1:50),
  quantity.x = c(sort(sample(2000:2500,size = 50,replace = T),decreasing = T)+sample(-20:20,50,replace = T),c(sort(sample(200:1700,size = 50,replace = T),decreasing = T)+sample(-40:40,50,replace = T)),c(sort(sample(200:450,size = 50,replace = T),decreasing = F)+sample(-40:40,50,replace = T)))
)

# 여기서부터 수정
df3 <- readxl::read_xlsx("total.xlsx")

# P와 Q 탐지
mod.1 = quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time

fx = function(pval,qval){summary(gls(mod.1, data = df3, correlation= corARMA(p=pval,q=qval, form = ~ Time),method="ML"))$AIC}

p = summary(gls(mod.1, data = df3,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:5, qval = 0:5)

for(i in 2:nrow(autocorrel)){p[i] = try(summary(gls(mod.1, data = df3, correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ Time),method="ML"))$AIC)}

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)

autocorrel

model.d = gls(quantity.x ~ Time + Intervention + Post.intervention.time + Intervention.2 + Post.intervention.2.time, data = df3,method="ML", correlation= corARMA(p=1,q=0, form = ~ Time))

# Show a summary of the model
summary(model.d)

df3<-df3 %>% mutate(
  model.d.predictions = predictSE.gls (model.d, df3, se.fit=T)$fit,
  model.d.se = predictSE.gls (model.d, df3, se.fit=T)$se
)

ggplot(df3,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), ymax = model.d.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.d.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

# 반사실 그래프화
df4<-filter(df3,Time<25)

model.e = gls(quantity.x ~ Time, data = df4, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

df3<-df3 %>% mutate(
  model.e.predictions = predictSE.gls (model.e, newdata = df3, se.fit=T)$fit,
  model.e.se = predictSE.gls (model.e, df3, se.fit=T)$se
)

df5<-filter(df3,Time<49)
model.f = gls(quantity.x ~ Time + Intervention + Post.intervention.time, data = df5, correlation= corARMA(p=1, q=0, form = ~ Time),method="ML")

df3<-df3 %>% mutate(
  model.f.predictions = predictSE.gls (model.f, newdata = df3, se.fit=T)$fit,
  model.f.se = predictSE.gls (model.f, df3, se.fit=T)$se
)

# log -> exponential
df3 <- df3 %>% 
  mutate(quantity.x = exp(quantity.x),
         model.d.predictions = exp(model.d.predictions),
         model.d.se = exp(model.d.se),
         model.e.predictions = exp(model.e.predictions),
         model.e.se = exp(model.e.se),
         model.f.predictions = exp(model.f.predictions),
         model.f.se = exp(model.f.se))

ggplot(df3,aes(Time,quantity.x))+
  geom_ribbon(aes(ymin = model.f.predictions - (1.96*model.d.se), ymax = model.f.predictions + (1.96*model.e.se)), fill = "lightblue")+
  geom_line(aes(Time,model.f.predictions),color="blue",lty=2)+
  geom_ribbon(aes(ymin = model.e.predictions - (1.96*model.d.se), ymax = model.e.predictions + (1.96*model.e.se)), fill = "pink")+
  geom_line(aes(Time,model.e.predictions),color="red",lty=2)+
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), ymax = model.d.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(Time,model.d.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)
