library(PNADcIBGE);library(survey);library(Hmisc);library(tidyverse);library(ineq);library(foreign);library(DescTools)
#V403312 - Qual era o rendimento bruto/retirada mensal que ... recebia/fazia normalmente nesse trabalho ? (valor em dinheiro)
#V1008 - Número de seleção do domicílio
#V1027 - Peso do domicílio e das pessoas sem pós estratificação
#V1028 - Peso do domicílio e das pessoas sem pós estratificação
#V2001 - Número de pessoas no municipio


df1 <- read.spss("./PNADC_MG_2012_2019TRAB_visita1.sav")
df2 <- data.frame(df1) %>% 
  mutate(VD_FAIXA_IDADE2 = str_squish(as.character(VD_FAIXA_IDADE))) %>% 
  mutate(VD_FAIXA_IDADE3 = ifelse(VD_FAIXA_IDADE2 %in% c("25 a 39 anos","40 a 59 anos"),"25 a 59 anos",VD_FAIXA_IDADE2)) %>% 
  mutate(VD_ESCOLA2 = recode(VD_ESCOLA,
                             "Sem instrução e menos de 1 ano de estudo" = "Fundamental Incompleto",
                             "Médio Incompleto" = "Fundamental Completo",
                             "Médio Completo e Superior incompleto" = "Médio Completo"))
  
df2.1 <- df2 %>% dplyr::select(Ano,V1032,VD5008,CO2_H) %>% 
  group_by(Ano) %>%
  summarise(VD5008 = Gini(VD5008*CO2_H, n = V1032,na.rm = TRUE)) 

df2.2 <- df2 %>% dplyr::select(Ano,V1032,VD5008,CO2_H) %>%
  drop_na() %>% 
  mutate(rendadefl = VD5008*CO2_H) %>% 
  group_by(Ano) %>% summarise(VD5008 = gini(rendadefl,V1032)) 

dfginitrab <- df2 %>% dplyr::select(Ano,V1032,VD4017,CO2_H) %>% 
  drop_na() %>% 
  mutate(rendadefl = VD4017*CO2_H) %>% 
  group_by(Ano) %>% summarise(VD4017 = Gini(rendadefl, n = V1032, na.rm = TRUE))

dfginitrab2 <- df2 %>% dplyr::select(Ano,V1032,VD4020,CO2_H) %>% 
  drop_na() %>% 
  mutate(rendadefl = VD4020*CO2_H) %>% 
  group_by(Ano) %>% summarise(VD4020 = Gini(rendadefl, n = V1032, na.rm = TRUE))

rm(df1)

#VD4016 Rendimento mensal habitual do trabalho principal para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
#VD4017 Rendimento mensal efetivo do trabalho principal para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
#VD2003 Número de componentes do domicílio (exclusive as pessoas cuja condição no domicílio era pensionista, empregado doméstico ou parente do empregado doméstico)
#VD4019 Rendimento mensal habitual de todos os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
#VD4020 Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
#VD4022 Rendimento mensal efetivo de todas as fontes (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho ou que receberam rendimentos em dinheiro de outras fontes)
#V1031 peso sem pós estratificação
#V1032 peso com pós estratificação

# -------------- função do gini ---------------
gini <- function (x, weights = rep(1, length = length(x))) 
{
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}

#-------- calculo Gini para todas as rendas ------------------

#aqui agrupei por ano e depois criei uma variavel que calcula o gini para cada tipo de renda presente

df2 %>% select_(Ano,V1032, V1008,UPA,VD2003,VD4016,VD4017,VD4019,VD4020,VD4022,VD5005,VD5008,VD5011,CO2_H) %>% 
  group_by(Ano) %>%
  summarise(VD4016 = Gini(VD4016, n = V1032,na.rm = TRUE),
                              VD4017 = Gini(VD4017*CO2_H, n = V1032,na.rm = TRUE),
                              VD4019 = Gini(VD4019*CO2_H, n = V1032,na.rm = TRUE),
                              VD4020 = Gini(VD4020*CO2_H, n = V1032,na.rm = TRUE),
                              VD4022 = Gini(VD4022*CO2_H, n = V1032,na.rm = TRUE),
                              VD5005 = Gini(VD5005*CO2_H, n = V1032,na.rm = TRUE),
                              VD5008 = Gini(VD5008*CO2_H, n = V1032,na.rm = TRUE),
                              VD5011 = Gini(VD5011*CO2_H, n = V1032,na.rm = TRUE)) %>% 
  pivot_longer(cols = c("VD4016","VD4017","VD4019","VD4020","VD4022","VD5005","VD5008","VD5011"),names_to = "Gini") %>% 
  ggplot(aes(x = Ano, y = value)) +
  geom_line()+ 
  facet_wrap(~Gini)

  
  # ----------- calculo de gini 1 (Desc.tools) ------------
df2.1 %>% ggplot(aes(x = Ano, y = VD5008)) +
  geom_line(size=1.5)+ylim(0.47,0.525)+
  geom_point(size = 2)+
  ylab(" Coeficiente de Gini")+
  theme_light()+
  labs(title = "Coeficiente de Gini da Renda domiciliar per capita*",
       subtitle = "*Rendimento domiciliar per capita (habitual de todos os trabalhos e efetivo de outras fontes) 
 (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= round(VD5008,3),vjust = -1.5))+
  theme(plot.subtitle = element_text(size = 8), 
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))



  # ----------- calculo de gini 2 (global enviroment) ------------
df2.2 %>% ggplot(aes(x = Ano, y = VD5008)) +
  geom_line(size=1.5)+ylim(0.47,0.525)+
  geom_point(size = 2)+
  ylab(" Coeficiente de Gini")+
  theme_light()+
  labs(title = "Coeficiente de Gini da Renda domiciliar per capita*",
       subtitle = "*Rendimento domiciliar per capita 
 (habitual de todos os trabalhos e efetivo de outras fontes) 
 (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, 
 empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= round(VD5008,3),vjust = -1.5))+
  theme(plot.subtitle = element_text(size = 8), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))


  # ----------- calculo de gini 3 weighted gini------------
df2012renda <- df2 %>% filter(Ano == 2012) %>% dplyr::select(VD5008, CO2_H, V1032)
df2013renda <- df2 %>% filter(Ano == 2013) %>% dplyr::select(VD5008, CO2_H, V1032)
df2014renda <- df2 %>% filter(Ano == 2014) %>% dplyr::select(VD5008, CO2_H, V1032)
df2015renda <- df2 %>% filter(Ano == 2015) %>% dplyr::select(VD5008, CO2_H, V1032)
df2016renda <- df2 %>% filter(Ano == 2016) %>% dplyr::select(VD5008, CO2_H, V1032)
df2017renda <- df2 %>% filter(Ano == 2017) %>% dplyr::select(VD5008, CO2_H, V1032)
df2018renda <- df2 %>% filter(Ano == 2018) %>% dplyr::select(VD5008, CO2_H, V1032)
df2019renda <- df2 %>% filter(Ano == 2019) %>% dplyr::select(VD5008, CO2_H, V1032)

ginis <- c("","","","","","","","")
ginis[1]<- weighted.gini(df2012renda$VD5008*df2012renda$CO2_H, df2012renda$V1032)[1]
ginis[2]<-weighted.gini(df2013renda$VD5008*df2013renda$CO2_H, df2013renda$V1032)[1]
ginis[3]<-weighted.gini(df2014renda$VD5008*df2014renda$CO2_H, df2014renda$V1032)[1]
ginis[4]<-weighted.gini(df2015renda$VD5008*df2015renda$CO2_H, df2015renda$V1032)[1]
ginis[5]<-weighted.gini(df2016renda$VD5008*df2016renda$CO2_H, df2016renda$V1032)[1]
ginis[6]<-weighted.gini(df2017renda$VD5008*df2017renda$CO2_H, df2017renda$V1032)[1]
ginis[7]<-weighted.gini(df2018renda$VD5008*df2018renda$CO2_H, df2018renda$V1032)[1]
ginis[8]<-weighted.gini(df2019renda$VD5008*df2019renda$CO2_H, df2019renda$V1032)[1]

ginis <- t(as.data.frame(ginis))
ginis$Ano <- seq(2012,2020,by = 1)
ginis <- t(ginis)
ginis <- (as.data.frame(ginis))
ginis$Ano <- seq(2012,2020,by = 1)
ginis <- ginis[1:8,]
ginis %>% dplyr::select(Ano,V1) %>% 
  mutate(VD5008 = round(as.numeric(V1),3)) %>% 
  ggplot(aes(x = Ano, y = VD5008)) +
  geom_line(size=1.5)+ylim(0.47,0.525)+
  geom_point(size = 2)+
  ylab(" Coeficiente de Gini")+
  theme_light()+
  labs(title = "Coeficiente de Gini da Renda domiciliar per capita*",
       subtitle = "*Rendimento domiciliar per capita 
 (habitual de todos os trabalhos e efetivo de outras fontes) 
 (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, 
 empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= round(VD5008,3),vjust = -1.5))+
  theme(plot.subtitle = element_text(size = 8), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))

  


#-------- calculo Gini do trabalho ----
dfginitrab %>% ggplot(aes(x = Ano, y = VD4017)) +
  geom_line(size=1.5)+ylim(0.47,0.525)+
  geom_point(size = 2)+
  ylab(" Coeficiente de Gini")+
  theme_light()+
  labs(title = "Coeficiente de Gini da Renda do trabalho*",
       subtitle = "*Rendimento mensal efetivo do trabalho principal para pessoas de 14 anos ou mais de idade 
(apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= round(VD4017,3),vjust = -1.5))+
  theme(plot.subtitle = element_text(size = 8), 
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))


dftrabdom <- left_join(dfginitrab,df2.1, by = "Ano")

dftrabdom %>% ggplot(aes(x = Ano))+
  geom_line(aes(y = VD4017), color = "blue",size=1.5)+
  geom_line(aes(y = VD5008), color = "red",size=1.5)+
  ylim(0.47,0.525)+
  geom_text(aes(y = VD4017,label= round(VD4017,3),vjust = -1.5), color = "blue")+
  geom_text(aes(y = VD5008,label= round(VD5008,3),vjust = -1.5), color = "red")+
  geom_text(aes(y = 0.52, x = 2013, label = "Gini da renda domiciliar per capita"), color = "red",check_overlap = TRUE, hjust = 0)+
  geom_text(aes(y = 0.516, x = 2013, label = "Gini da renda do trabalho"), color = "blue",check_overlap = TRUE, hjust = 0)+
  ylab(" Coeficiente de Gini")+
  theme_light()+
  labs(title = "Coeficiente de Gini da Renda do trabalho e rendimento domiciliar percapita*",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  theme(plot.subtitle = element_text(size = 8), 
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))
  
  

#-------- calculo de renda média para todas as rendas ------------------
# aqui eu agrupei por ano, depois criei uma variavel para cada tipo de renda
#calculando sua renda média em cada ano, considerando os pesos

df2 %>% select(Ano,V1032, V1008,UPA,VD2003,VD4016,VD4017,VD4019,VD4020,VD4022,VD5005,VD5008,VD5011,CO2_H) %>% 
    group_by(Ano) %>% summarise(VD4016 = weighted.mean(VD4016*CO2_H,V1032, na.rm = TRUE),
                              VD4017 = weighted.mean(VD4017*CO2_H,V1032,na.rm = TRUE),
                              VD4019 = weighted.mean(VD4019*CO2_H,V1032,na.rm = TRUE),
                              VD4020 = weighted.mean(VD4020*CO2_H,V1032,na.rm = TRUE),
                              VD4022 = weighted.mean(VD4022*CO2_H,V1032,na.rm = TRUE),
                              VD5005 = weighted.mean(VD5005*CO2_H,V1032,na.rm = TRUE),
                              VD5008 = weighted.mean(VD5008*CO2_H,V1032,na.rm = TRUE),
                              VD5011 = weighted.mean(VD5011*CO2_H,V1032,na.rm = TRUE)) %>% 
  pivot_longer(cols = c("VD4016","VD4017","VD4019","VD4020","VD4022","VD5005","VD5008","VD5011"),names_to = "renda",values_to = "medias") %>% 
  ggplot(aes(x = Ano, y = medias)) +
  geom_line()+ 
  facet_wrap(~renda) 

df2rendamedia<-df2 %>% select(Ano,V1032, V1008,UPA,VD2003,VD4016,VD4017,VD4019,VD4020,VD4022,VD5005,VD5008,VD5011,CO2_H) %>% 
  group_by(Ano) %>% summarise(VD5008 = weighted.mean(VD5008*CO2_H,V1032,na.rm = TRUE))

df2rendamedia %>%  ggplot(aes(x = Ano, y = VD5008)) +
  geom_line(size=1.5)+
  geom_point(size = 2)+
  ylab(" Renda Média(R$)")+
  ylim(1100,1500)+
  theme_light()+
  labs(title = "Renda Domiciliar per capita*(em R$ de 2019)",
       subtitle = "*Rendimento domiciliar per capita (habitual de todos os trabalhos e efetivo de outras fontes) 
 (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption ="Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= paste("R$",round(VD5008,1)),vjust = -2),size=3)+
  theme(plot.subtitle = element_text(size = 8), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))


#---------- calculo da do componente do trabalho na renda domiciliar per capita -------------
# aqui peguei as variaveis referentes ao rendimento de todos os trabalho (v4020) e à refrerente
# a todas as fontes (v4022), agrupeo por UPA, que imagino que são os domicilios,
# e por fim somei a renda de todos os individuos de cada municipio e dividi pela média
# de componentes em cada um. Desse modo obti a renda pc do trabalho e a renda pc de todas as fontes.
# em seguida agrupei por ano e calculei o Gini para a renda pc do trabalho de cada ano, por fim
# gerando gráfico de gini e no gráfico debaixo fiz o gráfico da renda média de cada ano.


#usando a média do número de componentes do domicilio para calcular a renda domiciliar per capita

df3 <- df2 %>% select(Ano,V1032, V1008,UPA,VD2003,VD4016,VD4017,VD4019,VD4020,VD4022,VD5005,VD5008,VD5011,CO2_H) %>% 
  mutate(domicilio = paste(UPA,"",V1008)) %>% 
  mutate(VD4022 = VD4022*CO2_H) %>% 
  mutate(VD4020 = VD4020*CO2_H) %>% 
  group_by(Ano,domicilio) %>% 
  summarise(componentes = mean(VD2003),
            rendatotal = sum(VD4022*V1032,na.rm = TRUE),
            rendatotalspeso = sum(VD4022,na.rm = TRUE),
            rendafontes = sum(VD4022*V1032,na.rm = TRUE)/mean(VD2003,na.rm = TRUE),
            rendatrab = sum(VD4020*V1032,na.rm = TRUE)/mean(VD2003,na.rm = TRUE),
            rendafontesspeso = sum(VD4022,na.rm = TRUE)/mean(VD2003,na.rm = TRUE),
            rendatrabspeso = sum(VD4020,na.rm = TRUE)/mean(VD2003,na.rm = TRUE)) %>% 
  mutate(ptrab = rendatrab/rendafontes ) %>% 
  mutate(ptrabspeso = rendatrabspeso/rendafontesspeso) %>% 
  group_by(Ano) %>% 
  summarise(rendafontmedia = mean(rendafontes),
            rendatrabmedia = mean(rendatrab),
            rendafontmediaspeso = mean(rendafontesspeso),
            rentrabmediaspeso = mean(rendatrabspeso)) %>% 
  mutate(ptrab = rendatrabmedia/rendafontmedia) %>% 
  mutate(ptrabspeso = rentrabmediaspeso/rendafontmediaspeso)

df4 <- df2 %>% select(Ano,V1032, V1008,UPA,VD2003,VD4016,VD4017,VD4019,VD4020,VD4022,VD5005,VD5008,VD5011,CO2_H) %>% 
  mutate(domicilio = paste(UPA,"",V1008)) %>% 
  mutate(VD4022 = VD4022*CO2_H) %>% 
  mutate(VD4020 = VD4020*CO2_H) %>% 
  group_by(Ano,domicilio) %>% 
  mutate(componentes = mean(VD2003),
            rendatotal = sum(VD4022*V1032,na.rm = TRUE),
            rendatotalspeso = sum(VD4022,na.rm = TRUE),
            rendafontes = sum(VD4022*V1032,na.rm = TRUE)/mean(VD2003,na.rm = TRUE),
            rendatrab = sum(VD4020*V1032,na.rm = TRUE)/mean(VD2003,na.rm = TRUE),
            rendafontesspeso = sum(VD4022,na.rm = TRUE)/mean(VD2003,na.rm = TRUE),
            rendatrabspeso = sum(VD4020,na.rm = TRUE)/mean(VD2003,na.rm = TRUE)) %>% 
  mutate(ptrab = rendatrab/rendafontes ) %>% 
  mutate(ptrabspeso = rendatrabspeso/rendafontesspeso) %>% 
  arrange(domicilio)
  
dfginidotrab <- df4 %>% group_by(Ano) %>% 
  summarise(ginidotrab = Gini(rendatrabspeso,n = V1032, na.rm = TRUE)) 

dftrabedom2 <- left_join(df2.1,dfginidotrab, by = "Ano")


dftrabedom2 %>% pivot_longer(cols = c(VD5008,ginidotrab), names_to = "renda") %>% 
  ggplot(aes(x = Ano, y = value, colour = renda)) +
  geom_line(size=1.5)+ylim(0.47,0.61)+
  geom_point(size = 2)+
  ylab(" Coeficiente de Gini ")+
  theme_light()+
  labs(title = "Coeficiente de Gini da Renda do trabalho efetiva domiciliar per capita*
e renda domiciliar per capita",
       subtitle = "*Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos
ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou
mercadorias em qualquer trabalho)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= round(value,3),vjust = -1.5))+
  theme(plot.subtitle = element_text(size = 8), 
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_color_manual(values = c("#E05046","#2F87E0"),
    labels = c("renda efetiva do trabalho domiciliar per capita",
                                "renda domiciliar per capita"))+
  theme(legend.position="bottom")
  

df3 %>% ggplot(aes(x = Ano, y = ptrab))+
  geom_line(size=1.5)+
  geom_point(size = 2)+
  ylab("% do trabalho")+
  theme_light()+
  labs(title = "Componente do trabalho na renda domiciliar per capita*",
       subtitle = 
       "*Calculado com base no Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade 
(apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
e no Rendimento mensal efetivo de todas as fontes (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho ou
que receberam rendimentos em dinheiro de outras fontes), com valores em R$ de 2019",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= paste(round(ptrab*100,1),"%"),
                vjust = -1.5),
            size=3)+
  theme(plot.subtitle = element_text(size = 5),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, limits = c(0.6,0.75))

df3 %>% ggplot(aes(x = Ano, y = ptrabspeso))+
  geom_line(size=1.5)+
  geom_point(size = 2)+
  ylab("% do trabalho")+
  theme_light()+
  labs(title = "Componente do trabalho na renda domiciliar per capita*",
       subtitle = 
         "*Calculado com base no Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade 
(apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
e no Rendimento mensal efetivo de todas as fontes (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho ou
que receberam rendimentos em dinheiro de outras fontes)com valores em R$ de 2019",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)")+
  geom_text(aes(label= paste(round(ptrab*100,1),"%"),
                vjust = -2,
                hjust = -0.02),
            size=3)+
  theme(plot.subtitle = element_text(size = 5), 
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0.625,0.7))

# ------------ variação da renda dos quantis entre os anos ------------

deflator2012 = 1371.0966/928.3957
deflator2015 = 1371.0966/1157.9499
ano <- c(2012,2015,2019)
deflator <- c(deflator2012,deflator2015,1)
deflatores <- data.frame(ano,deflator)
# criando df com rendas médias de 2015
Ano1 = 2015
quantis = 100
Quantis <- function(Ano1,quantis){


  dfAno1.1 <- df2 %>% dplyr::select(Ano, VD5008,V1032,CO2_H) %>% 
          filter(Ano == Ano1)

  rendaquantil <- wtd.quantile(dfAno1.1$VD5008*dfAno1.1$CO2_H,
                                 weights = dfAno1.1$V1032,
                                 probs = seq(1/quantis,1,by = 1/quantis),
                                 na.rm = TRUE, type = "(i-1)/(n-1)")

  dfAno1.2 <- df2 %>% 
    dplyr::select(Ano,VD5008,V1032,CO2_H) %>% 
    filter(Ano %in% c(Ano1)) %>% 
    mutate(VD5008 = VD5008*CO2_H) %>% 
    arrange(VD5008) %>% 
    mutate(quantil = ntile(VD5008,quantis)) %>%
    group_by(quantil) %>% 
    summarise(rendamedia = max(VD5008)) %>%
    drop_na() %>% 
    mutate(rendamedia = rendaquantil) 
  return(dfAno1.2)
  
}

df2019 <- Quantis(2019,100)
df2012 <- Quantis(2012,100)
df2015 <- Quantis(2015,100)

df20122019 <- left_join(df2012,df2019, by = "quantil")  
df20122019 %>% 
  mutate(var =(rendamedia.y/rendamedia.x) - 1) %>% 
  ggplot(aes(x = quantil,y=var))+
  geom_point()+
  geom_smooth()
  ylim(-0.5,0.5)

df20152019 <- left_join(df2015,df2019, by = "quantil")  
df20152019 %>%  
  mutate(var =(rendamedia.y/rendamedia.x) - 1) %>% 
  ggplot()+
  geom_point(aes(x = quantil,y=var),alpha = 0.2)+
  geom_smooth(aes(x = quantil,y=var), span = 0.2, se = FALSE, size = 1.5)+
  geom_line(aes (x = quantil, y = 0))+
  ylab("Variação da renda")+
  xlab("Percentis")+
  theme_light()+
  labs(title = "Variação da renda domiciliar per capita* entre 2015 e 2019",
       subtitle = 
         "*Rendimento domiciliar per capita 
 (habitual de todos os trabalhos e efetivo de outras fontes) 
 (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, 
 empregado doméstico ou parente do empregado doméstico),com valores em R$ de 2019
       ",
       caption = "Fonte: Microdados da PNAD Contínua(2015/2019)")+
  theme(plot.subtitle = element_text(size = 5), 
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, n.breaks = 10)


df20122015 <- left_join(df2012,df2015, by = "quantil")  
df20122015 %>%  
  mutate(var =(rendamedia.y/rendamedia.x) - 1) %>% 
  ggplot()+
  geom_point(aes(x = quantil,y=var),color = "grey")+
  geom_smooth(aes(x = quantil,y=var), span = 0.4, se = FALSE, size = 1.5)+
  geom_line(aes (x = quantil, y = 0))+
  ylab("Variação da renda")+
  xlab("Percentis")+
  theme_light()+
  labs(title = "Variação da renda domiciliar per capita* entre 2012 e 2015",
       subtitle = 
         "*Rendimento domiciliar per capita 
 (habitual de todos os trabalhos e efetivo de outras fontes) 
 (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, 
 empregado doméstico ou parente do empregado doméstico),com valores em R$ de 2019
       ",
       caption = "Fonte: Microdados da PNAD Contínua(2012/2015)")+
  theme(plot.subtitle = element_text(size = 5), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, n.breaks = 10)

  

#------------- percentagem de pobres-----------

df2 %>% select(Ano, VD5009,V1032) %>% 
  drop_na() %>% 
  count(Ano,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Até ¼ salário mínimo"," Mais de ¼ até ½ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = VD5009),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = VD5009),size = 2)+
  geom_text(aes(x = Ano, y = freq,label = round(freq*100,1),vjust = -1))+
  labs(title = "Taxa de Pobreza para duas linhas de pobreza- Minas Gerais, 2012-2019",
       subtitle = "* Calculado com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  geom_text(aes(x = 2016, y=0.12,label = "Até  ¼ salário mínimo",colour = " Até ¼ salário mínimo"),check_overlap = TRUE)+
  geom_text(aes(x = 2016, y=0.23,label = "Mais de ¼ até ½ salário mínimo",colour = " Mais de ¼ até ½ salário mínimo"),check_overlap = TRUE)+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"),
        legend.position="None")+
  scale_y_continuous(labels = scales::percent, limits = c(0,0.25))



# -------------- Variação da renda por grupos sociais ----------

df2 <- df2 %>%  mutate(V1023.2 = substr(V1023,1,15))

#função para gerar gráficos por ano e categoria da renda média
varAnorenda <- function(coluna = "V2007",ano1 = 2012,ano2 = 2019, limite = 2000){
  dfvarrenda <- df2 %>% 
    filter(Ano %in% c(ano1,ano2)) %>% 
    select(Ano, V1032, VD5008, CO2_H, matches(coluna)) %>% 
    mutate(VD5008infl = VD5008*CO2_H) %>% 
    group_by_at(c(1,5)) %>% 
    drop_na() %>% 
    summarise(n = mean(VD5008infl)) %>% 
    pivot_wider(names_from = Ano, values_from = n)
 colnames(dfvarrenda)<- c("variavel","ano1","ano2")
 
 dfvarrenda <-dfvarrenda %>%   mutate(tx = ((`ano2`/`ano1`)-1)*100)
 dfvarrenda1<- dfvarrenda %>%
   pivot_longer(cols = c(ano1,ano2), names_to = "Ano")  
  
  
 
 ylimite <- max(dfvarrenda1$value)*1.2
 g <- 
   dfvarrenda1%>% 
   mutate(variavel = fct_reorder(variavel, tx, .desc = TRUE)) %>% 
   ggplot(aes(x = variavel, fill = Ano))+
   geom_col(aes(y = value), position = position_dodge()) +
   stat_identity(geom = "text",aes(y = (value+50), label = round(value,1)), 
                 position = position_dodge(width = 1),
                 size = 3)+
   theme_light()+
   labs(
     x = coluna,
     y = "Renda Domiciliar per capita Média"
   )+
   geom_text(aes(x = variavel,y = limite, 
                 label = paste(round(tx,1),"%")),
             size = 3)+
   theme(axis.text.x = element_text(
     angle = 45,
     hjust = 1))+
   scale_fill_manual( values=c( "#9EC9FB","#53A0FA"),
     labels = c(ano1,ano2))
 return(g)

}

varAnorenda(coluna = "VD_ESCOLA2", 2015,2019, limite = 400)

#função para gerar gráficos por ano e categoria da renda média de linha
varAnorendalinha <- function(coluna = "V2007",ano1 = 2012,ano2 = 2019, limite = 2000){
  dfvarrenda <- df2 %>% 
    filter(Ano %in% c(ano1,ano2)) %>% 
    select(Ano, V1032, VD5008, CO2_H, matches(coluna)) %>% 
    mutate(VD5008infl = VD5008*CO2_H) %>% 
    group_by_at(c(1,5)) %>% 
    drop_na() %>% 
    summarise(n = mean(VD5008infl)) %>% 
    pivot_wider(names_from = Ano, values_from = n)
  colnames(dfvarrenda)<- c("variavel",1,2)
  
  dfvarrenda <-dfvarrenda %>%   mutate(tx = ((`2`/`1`)-1)*100)
  dfvarrenda1<- dfvarrenda %>%
    pivot_longer(cols = c("1","2"), names_to = "Ano") %>% 
    mutate(Ano = ifelse(Ano == 1,ano1,ano2)) %>% 
    group_by(variavel) %>% 
    mutate(media = mean(value))
  
  limmin<- min(dfvarrenda1$value)*0.85
  limmax <- max(dfvarrenda1$value)*1.15
  g <- dfvarrenda1 %>% 
    ggplot(aes(x = Ano, y = value, colour = variavel))+
    geom_line(size = 1.5)+
    geom_point(size = 3)+
    geom_text(aes(label = paste("R$ ",round(value)),y = value*1.05))+
    geom_text(aes(x = mean(Ano),y = media*1.05, colour = variavel, label = paste(round(tx,1),"%")))+
    theme_light()+
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom")+
    ylim(limmin,limmax)+
    scale_x_continuous(breaks = c(ano1,ano2))+
    scale_colour_manual(values = c("#93B4F5","#8D9ECC","#657BA8","#56698F","#3F4D69"))
  
  
  
  return(g)
  
}

varAnorendalinha("VD_ESCOLA2", 2015, 2019)

#função para gerar gráficos por ano e categoria da variação da renda média
varAnorenda2 <- function(coluna = "V2007",ano1 = 2012,ano2 = 2019, limite = 2000){
  dfvarrenda <- df2 %>% 
    filter(Ano %in% c(ano1,ano2)) %>% 
    select(Ano, V1032, VD5008, CO2_H, matches(coluna)) %>% 
    mutate(VD5008infl = VD5008*CO2_H) %>% 
    group_by_at(c(1,5)) %>% 
    drop_na() %>% 
    summarise(n = mean(VD5008infl)) %>% 
    pivot_wider(names_from = Ano, values_from = n)
  colnames(dfvarrenda)<- c("variavel","ano1","ano2")
  
  dfvarrenda <-dfvarrenda %>%   mutate(tx = ((`ano2`/`ano1`)-1)*100)
  dfvarrenda1<- dfvarrenda %>%
    pivot_longer(cols = c(ano1,ano2), names_to = "Ano")  
  
  
  
  ylimite <- max(dfvarrenda1$value)*1.2
  g <- 
    dfvarrenda1%>% 
    mutate(variavel = fct_reorder(variavel, tx, .desc = TRUE)) %>% 
    ggplot(aes(x = variavel, y = tx))+
    geom_col(position = position_dodge(),fill = "#53A0FA") +
    geom_text(aes(y = tx, label = round(tx,2),vjust = ifelse(tx > 0
                                                             , 1.5, -0.5)))+
    theme_light()+
    labs(
      title = paste("Variação da renda entre os anos ",ano1,"e ",ano2),
      x = coluna,
      y = " Variação da Renda Domiciliar per capita Média(%)")+
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1))
     return(g)
  
}

varAnorenda2(coluna = "VD_ESCOLA2", 2015,2019)

#Função para gerar variação da renda ano a anao
varrendaanoaano<- function(coluna = "V2007"){
  dfvarrenda <- df2 %>% 
    select(Ano, V1032, VD5008, CO2_H, matches(coluna)) %>% 
    mutate(VD5008infl = VD5008*CO2_H) %>% 
    group_by_at(c(1,5)) %>% 
    drop_na() %>% 
    summarise(n = mean(VD5008infl))
  colnames(dfvarrenda) <- c("Ano","variavel","n")
  maximo <- max(dfvarrenda$n/1000)*1.1
  minimo <- min(dfvarrenda$n/1000)*0.9
  
  g <- dfvarrenda %>% 
    ggplot(aes(x = Ano, y = n/1000, colour = variavel))+
    geom_line(size = 1.5)+
    ylim(minimo,maximo)+
    geom_text(aes(label = round(n/1000,2), y = n*1.05/1000))+
    labs(
         y = "Média da renda domiciliar per capita(em mil R$)",
         caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)" )+
  theme_light()+
    scale_colour_manual(values = c("#93B4F5","#8D9ECC","#657BA8","#56698F","#3F4D69"))+ 
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())+
    scale_x_continuous(breaks = 2012:2019)
  g
}

varrendaanoaano(coluna = "VD_ESCOLA2")


# V2007
# VD_RAÇA
# VD_ESCOLA
# V2010



Quantis2anos <- function(Ano1,Ano2,quantis){
  
  dfMAno1 <- df2 %>% dplyr::select(Ano, VD5008,V1032,CO2_H,V2007) %>%
    filter(V2007 ==" Masculino") %>% 
    filter(Ano == Ano1)
  
  dfFAno1 <- df2 %>% dplyr::select(Ano, VD5008,V1032,CO2_H,V2007) %>%
    filter(V2007 == " Feminino") %>% 
    filter(Ano == Ano1)
  
  dfMAno2 <- df2 %>% dplyr::select(Ano, VD5008,V1032,CO2_H,V2007) %>% 
    filter(V2007 == " Masculino") %>% 
    filter(Ano == Ano2)
  
  dfFAno2 <- df2 %>% dplyr::select(Ano, VD5008,V1032,CO2_H,V2007) %>%
    filter(V2007 == " Feminino") %>% 
    filter(Ano == Ano2)
  
  rendaquantilMAno1 <- wtd.quantile(dfMAno1$VD5008*dfMAno1$CO2_H,
                               weights = dfMAno1$V1032,
                               probs = seq(1/quantis,1,by = 1/quantis),
                               na.rm = TRUE, type = "(i-1)/(n-1)")
  
  rendaquantilFAno1 <- wtd.quantile(dfFAno1$VD5008*dfFAno1$CO2_H,
                               weights = dfFAno1$V1032,
                               probs = seq(1/quantis,1,by = 1/quantis),
                               na.rm = TRUE, type = "(i-1)/(n-1)")
  
  rendaquantilMAno2 <- wtd.quantile(dfMAno2$VD5008*dfMAno2$CO2_H,
                               weights = dfMAno2$V1032,
                               probs = seq(1/quantis,1,by = 1/quantis),
                               na.rm = TRUE, type = "(i-1)/(n-1)")
  
  rendaquantilFAno2 <- wtd.quantile(dfFAno2$VD5008*dfFAno2$CO2_H,
                               weights = dfFAno2$V1032,
                               probs = seq(1/quantis,1,by = 1/quantis),
                               na.rm = TRUE, type = "(i-1)/(n-1)")
  
  
  dfMAno1.q <- dfMAno1 %>% 
    arrange(VD5008) %>% 
    mutate(quantil = ntile(VD5008,quantis)) %>%
    group_by(quantil) %>% 
    summarise(rendamedia = max(VD5008)) %>%
    drop_na() %>% 
    mutate(rendamedia = rendaquantilMAno1) 
  
  dfMAno2.q <- dfMAno2 %>% 
    arrange(VD5008) %>% 
    mutate(quantil = ntile(VD5008,quantis)) %>%
    group_by(quantil) %>% 
    summarise(rendamedia = max(VD5008)) %>%
    drop_na() %>% 
    mutate(rendamedia = rendaquantilMAno2) 
  
  
  dfFAno1.q <- dfMAno2 %>% 
    arrange(VD5008) %>% 
    mutate(quantil = ntile(VD5008,quantis)) %>%
    group_by(quantil) %>% 
    summarise(rendamedia = max(VD5008)) %>%
    drop_na() %>% 
    mutate(rendamedia = rendaquantilFAno1) 
  
  dfFAno2.q <- dfMAno2 %>% 
    arrange(VD5008) %>% 
    mutate(quantil = ntile(VD5008,quantis)) %>%
    group_by(quantil) %>% 
    summarise(rendamedia = max(VD5008)) %>%
    drop_na() %>% 
    mutate(rendamedia = rendaquantilFAno2) 
  
  
 dfMano12<- left_join(dfMAno1.q,dfMAno2.q, by = "quantil") %>% 
   mutate(Homens = ((rendamedia.y/rendamedia.x)-1)*100) %>% 
   select(quantil,Homens)
 dfFano12<- left_join(dfFAno1.q,dfFAno2.q, by = "quantil") %>% 
   mutate(Mulheres = ((rendamedia.y/rendamedia.x)-1)*100) %>% 
   select(quantil,Mulheres)
 
 dfano12 <- left_join(dfMano12,dfFano12, by = "quantil")
plot1 <- dfano12 %>% 
  pivot_longer(cols = c("Mulheres","Homens"), values_to = "var") %>% 
  ggplot(aes(x = quantil, y = var, colour = name))+
  geom_point(alpha = 0.1)+
  geom_smooth(span = 0.4, se = FALSE, size = 1.5)+
  
  ylab("Variação da renda")+
  xlab("Percentis")+
  theme_light()+
  labs(title = "Variação da renda domiciliar per capita*",
       subtitle = 
         "*Rendimento domiciliar per capita 
 (habitual de todos os trabalhos e efetivo de outras fontes) 
 (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, 
 empregado doméstico ou parente do empregado doméstico),com valores em R$ de 2019
       ",
       caption = "Fonte: Microdados da PNAD Contínua(2012/2015)")+
  theme(plot.subtitle = element_text(size = 5), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))

  
  
 return(plot1)
  
}

Quantis2anos(2015,2019,100)

# junção das categorias de escolaridade e de idade
# -------------- Variação dos pobres por sexo ---------
df2 %>% select(Ano,V2007, VD5009,V1032) %>% 
  drop_na() %>% 
  count(Ano,V2007,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Até ¼ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = V2007),size = 1.2)+
  #geom_point(aes(x = Ano, y = freq,colour = V2007),size = 2)+
  geom_text(aes(x = Ano, y = freq,label = round(freq*100,1),vjust = -1,colour = V2007),size = 4.5)+
  geom_text(aes(x = 2012, y = 0.047, label = "Masculino", fontface = "bold"),colour = "#657BA8" ,hjust = 0, check_overlap = TRUE)+
  geom_text(aes(x = 2012, y = 0.045, label = "Feminino", fontface = "bold"),colour = "#3F4D69" ,hjust = 0, check_overlap = TRUE)+
  labs(caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")+
  scale_y_continuous(labels = scales::percent, limits = c(0.02,0.06))+
  scale_colour_manual(values = c("#657BA8","#3F4D69"))+
  scale_x_continuous(breaks = 2012:2019)

  
df2 %>% select(Ano,V2007, VD5009,V1032) %>% 
  drop_na() %>% 
  count(Ano,V2007,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Mais de ¼ até ½ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = V2007),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = V2007),size = 2)+
  geom_text(aes(x = Ano, y = freq,label = round(freq*100,1),vjust = -1))+
  labs(title = "Taxa de pobreza* Mais de ¼ até ½ salário mínimo por sexo - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, limits = c(0.065,0.11))

# ------------- evolução dos pobres raça 1-----------------------
df2 %>% select(Ano,V2010, VD5009,V1032) %>% 
  drop_na() %>% 
  count(Ano,V2010,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Mais de ¼ até ½ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = V2010),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = V2010),size = 2)+
  geom_text(aes(x = Ano, y = freq,label = round(freq*100,3),vjust = -1))+
  labs(title = "Taxa de pobreza* Mais de ¼ até ½ salário mínimo por raça - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, limits = c(0.0,0.13))

df2 %>% select(Ano,V2010, VD5009,V1032) %>% 
  drop_na() %>% 
  count(Ano,V2010,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Até ¼ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = V2010),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = V2010),size = 2)+
  geom_text(aes(x = Ano, y = freq,label = round(freq*100,3),vjust = -1))+
  labs(title = "Taxa de pobreza* Até ¼ salário mínimo por raça - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, limits = c(0.0,0.06))
# ------------- evolução dos pobres raça 2-----------------------
df2 %>% select(Ano,VD_Raça, VD5009,V1032) %>% 
  drop_na() %>% 
  count(Ano,VD_Raça,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Mais de ¼ até ½ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = VD_Raça),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = VD_Raça),size = 2)+
  geom_text(aes(x = Ano, y = freq,label = round(freq*100,3),vjust = -1))+
  labs(title = "Taxa de pobreza* Mais de ¼ até ½ salário mínimo por raça - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, limits = c(0.03,0.14))

df2 %>% select(Ano,VD_Raça, VD5009,V1032) %>% 
  drop_na() %>% 
  count(Ano,VD_Raça,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Até ¼ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = VD_Raça),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = VD_Raça),size = 2)+
  geom_text(aes(x = Ano, y = freq,label = round(freq*100,3),vjust = -1))+
  labs(title = "Taxa de pobreza* Até ¼ salário mínimo por raça - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, limits = c(0.0,0.07))
# ------------- evolução dos pobres Escolaridade-----------------------

unique(df2$VD_ESCOLA)
df2 %>% select(Ano,VD_ESCOLA, VD5009,V1032) %>% 
  mutate(VD_ESCOLA2 = recode(VD_ESCOLA,
                             "Sem instrução e menos de 1 ano de estudo" = "Fundamental Incompleto",
                             "Médio Incompleto" = "Fundamental Completo",
                             "Médio Completo e Superior incompleto" = "Médio Completo")) %>% 
  drop_na() %>% 
  count(Ano,VD_ESCOLA2,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Mais de ¼ até ½ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = VD_ESCOLA2),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = VD_ESCOLA2),size = 2)+
  #geom_text(aes(x = Ano, y = freq,label = round(freq*100,3),vjust = -1))+
  labs(title = "Taxa de pobreza* Mais de ¼ até ½ salário mínimo por Escolaridade - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita (habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)",
       colour = "Escolaridade")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"),
        legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)

df2 %>% select(Ano,VD_ESCOLA, VD5009,V1032) %>% 
  mutate(VD_ESCOLA2 = recode(VD_ESCOLA,
                             "Sem instrução e menos de 1 ano de estudo" = "Fundamental Incompleto",
                             "Médio Incompleto" = "Fundamental Completo",
                             "Médio Completo e Superior incompleto" = "Médio Completo")) %>% 
  drop_na() %>% 
  count(Ano,VD_ESCOLA2,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Até ¼ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = VD_ESCOLA2),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = VD_ESCOLA2),size = 2)+
  #geom_text(aes(x = Ano, y = freq,label = round(freq*100,1),vjust = -1))+
  labs(title = "Taxa de pobreza* Até ¼ salário mínimo por Escolaridade - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita  (habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)",
       colour = "Escolaridade")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"),
        legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent, limits = c(0.0,0.065), breaks = seq(0,0.065,0.01))

# ------------- evolução dos pobres Idade-----------------------

as.character(df2$VD_FAIXA_IDADE)

df2 %>% select(Ano,VD_FAIXA_IDADE, VD5009,V1032) %>% 
  drop_na() %>% 
  mutate(VD_FAIXA_IDADE2 = str_squish(as.character(VD_FAIXA_IDADE))) %>% 
  mutate(VD_FAIXA_IDADE3 = ifelse(VD_FAIXA_IDADE2 %in% c("25 a 39 anos","40 a 59 anos"),"25 a 59 anos",VD_FAIXA_IDADE2)) %>% 
  count(Ano,VD_FAIXA_IDADE3,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Mais de ¼ até ½ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = VD_FAIXA_IDADE3),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = VD_FAIXA_IDADE3),size = 2)+
  #geom_text(aes(x = Ano, y = freq,label = round(freq*100,3),vjust = -1))+
  labs(title = "Taxa de pobreza* Mais de ¼ até ½ salário mínimo por Faixa idade - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent)

df2 %>% select(Ano,VD_FAIXA_IDADE, VD5009,V1032) %>% 
  drop_na() %>% 
  mutate(VD_FAIXA_IDADE2 = str_squish(as.character(VD_FAIXA_IDADE))) %>% 
  mutate(VD_FAIXA_IDADE3 = ifelse(VD_FAIXA_IDADE2 %in% c("25 a 39 anos","40 a 59 anos"),"25 a 59 anos",VD_FAIXA_IDADE2)) %>% 
  count(Ano,VD_FAIXA_IDADE3,VD5009,wt = V1032) %>% 
  group_by(Ano) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(VD5009 %in% c(" Até ¼ salário mínimo")) %>% 
  ggplot(aes(x = Ano, y = freq))+
  geom_line(aes(x = Ano, y = freq,colour = VD_FAIXA_IDADE3),size = 1.2)+
  geom_point(aes(x = Ano, y = freq,colour = VD_FAIXA_IDADE3),size = 2)+
  #geom_text(aes(x = Ano, y = freq,label = round(freq*100,1),vjust = -1))+
  labs(title = "Taxa de pobreza* Até ¼ salário mínimo por Faixa idade - Minas Gerais, 2012-2019",
       subtitle = "* Calculada com base na Faixa de rendimento domiciliar per capita 
(habitual de todos os trabalhos e efetivo de outras fontes) 
(exclusive o rendimento das pessoas cuja condição na unidade 
domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)",
       caption = "Fonte: Microdados da PNAD Contínua(2012 a 2019)",
       y = "Percentual de Pobres(%)")+
  theme_light()+
  theme(plot.subtitle = element_text(size = 8),
        axis.title.x = element_text(face="bold"), 
        axis.title.y = element_text(face="bold"))+
  scale_y_continuous(labels = scales::percent, limits = c(0.0,0.035))




df1 <- read.spss("./PNADC_MG_2012_2019TRAB_visita1.sav")
df2 <- data.frame(df1) %>% 
  mutate(VD_FAIXA_IDADE2 = str_squish(as.character(VD_FAIXA_IDADE))) %>% 
  mutate(VD_FAIXA_IDADE3 = ifelse(VD_FAIXA_IDADE2 %in% c("25 a 39 anos","40 a 59 anos"),"25 a 59 anos",VD_FAIXA_IDADE2)) %>% 
  mutate(VD_ESCOLA2 = recode(VD_ESCOLA,
                             "Sem instrução e menos de 1 ano de estudo" = "Fundamental Incompleto",
                             "Médio Incompleto" = "Fundamental Completo",
                             "Médio Completo e Superior incompleto" = "Médio Completo"))

dfvarrenda <- df2 %>% 
  select(Ano, V1032, VD5008, CO2_H, matches("VD_ESCOLA2")) %>% 
  mutate(VD5008infl = VD5008*CO2_H) %>% 
  group_by_at(c(1,5)) %>% 
  drop_na() %>% 
  summarise(n = weighted.mean(VD5008*CO2_H,V1032,na.rm = TRUE))

colnames(dfvarrenda) <- c("Year","Scholarly","Average Income")
write.csv(dfvarrenda, "Average income by scholarly.csv")

dfvarrenda <- df2 %>% 
  select(Ano, V1032, VD5008, CO2_H, matches("VD_FAIXA_IDADE3")) %>% 
  mutate(VD5008infl = VD5008*CO2_H) %>% 
  group_by_at(c(1,5)) %>% 
  drop_na() %>% 
  summarise(n = weighted.mean(VD5008*CO2_H,V1032,na.rm = TRUE))

colnames(dfvarrenda) <- c("Year","Age","Average Income")
write.csv(dfvarrenda, "Average income by Age.csv")



maximo <- max(dfvarrenda$n/1000)*1.1
minimo <- min(dfvarrenda$n/1000)*0.9

dfvarrendatotal <- df2 %>% 
  select(Ano, V1032, VD5008, CO2_H) %>% 
  mutate(VD5008infl = VD5008*CO2_H) %>% 
  group_by(Ano) %>% 
  drop_na() %>% 
  summarise(n = weighted.mean(VD5008*CO2_H,V1032,na.rm = TRUE)) %>% 
  mutate(variavel = "Total")

dfvarrenda1 <- rbind(dfvarrendatotal,dfvarrenda) %>% mutate(variavel = factor(variavel, levels = c("Total", as.character(unique(dfvarrenda$variavel)))))
dfvarrenda1$variavel
