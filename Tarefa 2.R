install.packages("basedosdados")
library("basedosdados")

set_billing_id("projeto-teste-1-341512")

query <- bdplyr("mundo_kaggle_olimpiadas.microdados")
olimpiadas <- bd_collect(query)
library(dplyr)
library(ggplot2)

medalhas_bra<-olimpiadas%>%
  filter(pais=="Brazil")%>%
  filter(!is.na(medalha))%>%
  distinct(evento,ano,.keep_all=TRUE)%>%
  count(ano,medalha,name="quantidade")

campeoes_volei_fem<-olimpiadas%>%
  filter(esporte=="Volleyball")%>%
  filter(!is.na(medalha))%>%
  filter(sexo=="F")%>%
  distinct(evento,ano,pais,.keep_all=TRUE)%>%
  count(pais,sexo, name="quantidade")

campeoes_volei_masc<-olimpiadas%>%
  filter(esporte=="Volleyball")%>%
  filter(!is.na(medalha))%>%
  filter(sexo=="M")%>%
  distinct(evento,ano,pais,.keep_all=TRUE)%>%
  count(pais,sexo,medalha, name="quantidade")

library(plyr)

df_cumsum<-ddply(medalhas_bra,"ano",transform,soma_medalhas=cumsum(quantidade))
head(df_cumsum)

library(ggplot2)
Tabela1<-ggplot(medalhas_bra,aes(x=ano,y=quantidade, fill=medalha))+
  geom_bar(stat="identity", color="white")+
  ggtitle("Evolução do Quadro Brasileiro de Medalhas")+
    scale_fill_manual(values=c("tan4","goldenrod2","gray63"),labels=c("Bronze","Ouro","Prata"))+
    guides(fill=guide_legend(title="Medalhas"))+
    xlab("Ano")+
    scale_x_continuous(breaks=seq(1948,2016,4))+
    scale_y_continuous(breaks=seq(0,30,1))
    ylab("Quantidade de Medalhas")+
    theme(axis.text.x=element_text(face="bold", color="#994433", 
                                   size=8,angle=90),
        axis.text.y=element_text(face="bold", color="#994433", 
                                   size=14))

library(scales)

Tabela2 <- ggplot(campeoes_volei_fem,aes(x="",y=quantidade,fill=pais))+
  geom_bar(stat="identity",width=1,color="white")+
  coord_polar("y",start=0)+
  theme_void()+
  geom_text(aes(y = quantidade/2 + c(0, cumsum(quantidade)[-length(quantidade)]), 
              label = quantidade), size=4, color="white")+
  ggtitle("Medalhas (Pódios) por País no Vôlei Feminino")+
  guides(fill=guide_legend(title="Países"))
  