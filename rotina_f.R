#Recados importantes

#07/11/2017
#Fiz backup das três primeiras tabelas de MySQL que crei no BD
#PLUVIOSIDADE para uma pasta dentro da pasta da tese. depois disso
#exclui essas três tabelas Precipitação do BD MySQL. Fiz isso para
#liberar espaço no diretório /

#####################################################
#Passando os arquivos de csv para sql

prec=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/2-Dados trabalhados/Base de dados/Dados/Dados de pluviosidade - Brasil",
              header=T)
#Tem tres tipos de medição. 99% dos dados tem medição 1
#1=pluviômetro (5009532)
#2=Pluviógrafo (53)
#3=Data logger (15640)
#table(prec$TipoMedicaoChuvas)

#Acho que compensa pegar só so valores medidos por pluviômetro

prec=prec[prec$TipoMedicaoChuvas==1,]

library(RMySQL)

mydb=dbConnect(MySQL(),user=' ',password=' ',host=' ')

#Criando banco de dados e definindo que ele será utilizado
dbSendQuery(mydb,"CREATE DATABASE PLUVIOSIDADE;")
dbSendQuery(mydb,"USE PLUVIOSIDADE;")

# reconnecting to database we just created using following command in R :
#Reconectando com o banco de dados recem criado, pluviosidade. Serve também
#para conectar com bancos de dados já existentes
mydb=dbConnect(MySQL(),user='',password='',host='',dbname="PLUVIOSIDADE")

#Salva o data frame prec dentro da tabela precupitacao do bando de dados pluviosidade
dbWriteTable(conn=mydb,name='Precipitacao',value=as.data.frame(prec))


##################################################################
#################################################################
#Verirficando e tratando os dados


library (RMySQL)

#Conectando com o banco de dados pluviosidade
con=dbConnect(MySQL(), user=' ',password=' ',host=' ',dbname='PLUVIOSIDADE')

#Quais tabelas que existem no banco de dados pluviosidade?
#O comando abaixo mostra que só existe a tabela precipitação
dbListTables(con)

#Quais os campos que exixtem na tabela precipitação
#é equivalente ao comando names() do R
dbListFields(con,'Precipitacao')

#Esse comando só salva ou recupera dados da tabela e mantem salvo no servidor sql
#NEsse caso quero que seja selecionado todos os valores com nível de consistência iagual a 2
#Até agora a RAM não está sendo utilizada
chuva=dbSendQuery(con,"select *from Precipitacao where `NivelConsistencia`='2';")

#Agora vou pegar os dados da consulta e salvar em um data frame utilizandoa função fetch
#o argumento n especifica o número de registros que serão recuperados. o valor -1 diz para 
#recuperar todos os registros pendentes
#Agora começa a ocupar ram
dfchuva=fetch(chuva,n=-1)


#Selecionar cmapos específicos com codigo da estação igual a 1036005. ficar atento ao tipo de aspas
chuva2=dbSendQuery(con,"select *from Precipitacao where `X..EstacaoCodigo`='1036005';")
dfchuva2=fetch(chuva2,n=-1)

chuva3=dbSendQuery(con,"select `X..EstacaoCodigo`,`Total`,`NumDiasDeChuva` from Precipitacao where `NivelConsistencia`='2';")
dfchuva3=fetch(chuva3,n=-1)


###################################################################
#################################################################
#Hora de brincar a sério
library(RMySQL)

#Cria conexão com a base de dados pluviosidade
pluvio=dbConnect(MySQL(),user='',password=' ',host=' ',dbname='PLUVIOSIDADE')

#Lista todas as tabelas da base de dados
dbListTables(pluvio)

#Lista os nomes de todas as variáveis da tabela precipitacao
dbListFields(pluvio,'Precipitacao')

#Selecionar uma das linhas da tabela com base no campo row_names
#Carregar linha por linha iria demorar muito... anos talvez
#o que eu posso fazer é carregar por estação. e ai fazer o processo na ram usando uma estação
#de cada vez
codestacao=dbSendQuery(pluvio,"select `X..EstacaoCodigo` from Precipitacao")
dfcodestacao=fetch(codestacao,n=-1)
dfcodestacao=as.data.frame(table(dfcodestacao$X..EstacaoCodigo))
dfcodestacao=as.numeric(levels(dfcodestacao$Var1)[dfcodestacao$Var1])


for (i in 1:length(dfcodestacao)){

  k=dfcodestacao[[i]]
chuva=dbSendQuery(pluvio,paste("select `X..EstacaoCodigo`,`Data`,`Chuva01`,`Chuva02`,`Chuva03`,
`Chuva04`,`Chuva05`,`Chuva06`,`Chuva07`,`Chuva08`,`Chuva09`,`Chuva10`,`Chuva11`,`Chuva12`,
`Chuva13`,`Chuva14`,`Chuva15`,`Chuva16`,`Chuva17`,`Chuva18`,`Chuva19`,`Chuva20`,`Chuva21`,
`Chuva22`,`Chuva23`,`Chuva24`,`Chuva25`,`Chuva26`,`Chuva27`,`Chuva28`,`Chuva29`,`Chuva30`,
`Chuva31` from Precipitacao where `X..EstacaoCodigo`=",k,sep="")) 

dfchuva=fetch(chuva,n=-1)
precl=list()
for (j in 1:dim(dfchuva)[1]){
prec=dfchuva[j,]
prec=as.data.frame(t(prec[,c(3:33)]))
colnames(prec)[1]=c('Precipitacao')
prec$Codigo=dfchuva[j,]$X..EstacaoCodigo
prec$Data=dfchuva[j,]$Data
prec$Dia=1:31
rownames(prec)=NULL
precl[[j]]=prec
}

prec=do.call(rbind.data.frame,precl)
prec=unique(prec)

dbWriteTable(conn=pluvio,name='Precipitacao2',value=as.data.frame(prec),append=T)


}

#descartar tabela
#dbGetQuery(pluvio,"drop table Precipitacao2xxx")
#dbListFields(pluvio,'Precipitacao2')
#teste=dbSendQuery(pluvio,"select *from Precipitacao2")
#test2=fetch(teste,n=-1)

############################################################################
############################################################################
#Agora preciso tratar os dados e interpolar
library(RMySQL)
library(ggplot2)
library(tidyr)
library(gstat)
library(sp)


pluvio=dbConnect(MySQL(),user=' ',password=' ',host=' ',dbname='PLUVIOSIDADE')
dbListTables(pluvio)
dbListFields(pluvio,'Precipitacao2')

#Tenho que chamar o grid de minicipios do brasil e manter fora do looping
#para que mun2 n seja definido como objeto espacial duass vezes
mun=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/lat_long_municipiosbrasil.csv',
             header=T,encoding = "latin1")
mun2=mun[,c(3,4)]

#Defini que os pontos do muncípios onde os dados serão interpolados são objetos espaciais
coordinates(mun2)=~longitude+latitude


for (j in 1:31){
chuva=dbSendQuery(pluvio, paste("select `Precipitacao`,`Codigo`,`Data`,`Dia` from Precipitacao2 where `Dia`=",j,sep=""))
dfchuva=fetch(chuva,n=-1)
#Santa maria com o segundo maior volume de chuva já registrado no mundo nunca passou dos 200 mm emm um dia. Esse é  meu parâmetro máximo então
#http://zh.clicrbs.com.br/rs/noticias/noticia/2015/10/santa-maria-tem-segundo-maior-volume-de-chuva-no-mundo-4865698.html
#Tenho também que excluir valores negativos de precipitação
dfchuva=dfchuva[dfchuva$Precipitacao<200,]
dfchuva=dfchuva[dfchuva$Precipitacao>=0,]
freq=as.data.frame(table(dfchuva$Data))
#Deixar na base antes de interpolar apenas os anos com mais de 200 estações meteorológicas
freq=freq[freq$Freq>=200,]
freq$datacod=1:dim(freq)[1]
dfchuva=merge(dfchuva,freq,by.x=c('Data'),by.y=c('Var1'),all.x=F,all.y=T)
#Agora preciso atribuir coordenadas de latitude e longitude às estações
estacoes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/1-Dados Brutos/Estacoes/Estacao.csv',
                  header=T)
dfchuva=merge(dfchuva,estacoes,by=c('Codigo'),all.x=T,all.y=F)


#Interpolação propriamente dita. Vamos ver se a memória aguenta ou se vou ter que quebrar etapas
lprecipitacao=list()


for (i in 1:max(dfchuva$datacod)){
#Tenho que selecionar agora segundo datacod. todos os dados referentes a um ponto no tempo
dfchuva2=dfchuva[dfchuva$datacod==i,]
dfchuva3=dfchuva2[,c(9,10,3)]
colnames(dfchuva3)=c("latitude","longitude","precip")
#Defini que os dados de precipitação são objetos espaciais
coordinates(dfchuva3)=~longitude+latitude
#Plot the weather station locations and interpolation grid:
#plot(mun2, cex = 1.5, col = "grey")
#points(dfchuva3, pch = 1, col = "red", cex = 1)
precipitacao <- data.frame(idw(precip~1,idp=2, locations=dfchuva3[!is.na(dfchuva3$precip),],newdata=mun2,nmin=2, nmax=4))
precipitacao=cbind(precipitacao,mun,rep(dfchuva2$Data[1],length(precipitacao$var1.pred)),rep(dfchuva2$Dia[1],length(precipitacao$var1.pred)))
precipitacao=precipitacao[,c(6:12,3)]
colnames(precipitacao)=c('Codigo','Municipio','Latitude','Longitude','UF','Data','Dia','Precipitacao')

lprecipitacao[[i]]=precipitacao
}

precipitacao=do.call(rbind.data.frame,lprecipitacao)
precipitacao=separate(precipitacao,Data,c('Dia2,','Mes','Ano'),sep='/')
precipitacao=precipitacao[,c(1:5,9,7,8,10)]

dbWriteTable(conn=pluvio,name='Precipitacao3',value=as.data.frame(precipitacao),append=T)

}
#teste=dbSendQuery(pluvio,"select *from Precipitacao3 where `Codigo`='1302603'")
#test3=fetch(teste,n=-1)
#############dbGetQuery(pluvio,"drop table Precipitacao3xxx")
#test3$Data=as.Date(test3$Data,format="%d/%m/%Y")

#library(tidyr)
#test4=separate(test3,Data,into=c('ano','mes','day'),sep='-')
#prec=aggregate(test4[,c(11)],by=list(Codigo=test4$Codigo,Ano=test4$ano,Mes=test4$mes),sum,na.rm=T)
#prec2=aggregate(test4[,c(11)],by=list(Codigo=test4$Codigo,Ano=test4$ano),sum,na.rm=T)
#mean(prec2$x,na.rm=T)
#aggregate(prec[,(4)],by=list(mes=prec$Mes),mean,na.rm=T)

##############################################################################
###############################################################################
#Medidas
#Esse código talvez precise ser reajustado para funcionar, uma vez que mudei 
# o código acima e recriei a tabela Precipitacao3 de forma que o campo data
#fosse dividido em dia, mês e ano.
library(RMySQL)
library(tidyr)


con=dbConnect(MySQL(),user='',password='’',host='',dbname='PLUVIOSIDADE')
#dbListTables(con)
#Os dados de precipitação interpolados estão na tabela precipitacao 3

#dbListFields(con,'Precipitacao3')
#Existe um campo UF. Nada mal

uf=c('ACRE','ALAGOAS','AMAPÁ','AMAZONAS','BAHIA','CEARÁ','DISTRITO FEDERAL','ESPÍRITO SANTO','GOIÁS',
     'MARANHÃO','MATO GROSSO','MATO GROSSO DO SUL','MINAS GERAIS','PARÁ','PARAÍBA','PARANÁ','PERNAMBUCO',
     'PIAUÍ','RIO DE JANEIRO','RIO GRANDE DO NORTE','RIO GRANDE DO SUL','RONDÔNIA','RORAIMA', 'SÃO PAULO',
     'SANTA CATARINA','SERGIPE','TOCANTINS')


for (i in 1:length(uf)){
  
  j=uf[i]
  sp=dbSendQuery(con, paste("select *from Precipitacao3 where `UF` like '%",j,"%'",sep=""))
  sp2=fetch(sp,n=-1)
  
  gc()
  
    sp2=sp2[,c(2,7:10)]
  
  gc()
  
  #Variáveis agregadas para ano
  #Precipitação total anual
  prectot=aggregate(sp2[,c(5)],by=list(Codigo=sp2$Codigo,Ano=sp2$Ano),sum,na.rm=T)
  #Variabilidae da precipitação anual
  varprec=aggregate(sp2[,c(5)],by=list(Codigo=sp2$Codigo,Ano=sp2$Ano),sd,na.rm=T)
  #Dias com chuva inferior a 1mm
  sp2$seco[sp2$Precipitacao<1]=1
  diasemchover=aggregate(sp2[,c(6)],by=list(Codigo=sp2$Codigo,Ano=sp2$Ano),sum,na.rm=T)
  #Dias com chuva super a 25mm
  sp2$maide25mm[sp2$Precipitacao>=25]=1
  maisde25mm=aggregate(sp2[,c(7)],by=list(Codigo=sp2$Codigo,Ano=sp2$Ano),sum,na.rm=T)
  #Dias extremamente chuvosos, com chuva superior a 50mm
  sp2$maisde50mm[sp2$Precipitacao>=50]=1
  maisde50mm=aggregate(sp2[,c(8)],by=list(Codigo=sp2$Codigo,Ano=sp2$Ano),sum,na.rm=T)
  
  gc()
  
  #Variáveis agregadas a nível de estacao do ano
  sp2$Mes=as.numeric(sp2$Mes)
  sp2$Ano=as.numeric(sp2$Ano)
  
  sp2$Estacoes=NA
  #Primavera
  sp2$Estacoes[(sp2$Dia>=22&sp2$Mes==9)|(sp2$Mes==10|sp2$Mes==11)|(sp2$Dia<21&sp2$Mes==12)]=1
  #Verão
  sp2$Estacoes[(sp2$Dia>=21&sp2$Mes==12)|(sp2$Mes==1|sp2$Mes==2)|(sp2$Dia<20&sp2$Mes==3)]=2
  #Outono
  sp2$Estacoes[(sp2$Dia>=20&sp2$Mes==3)|(sp2$Mes==4|sp2$Mes==5)|(sp2$Dia<21&sp2$Mes==6)]=3
  #Inverno
  sp2$Estacoes[(sp2$Dia>=21&sp2$Mes==6)|(sp2$Mes==7|sp2$Mes==8)|(sp2$Dia<22&sp2$Mes==9)]=4
  
  #Precipitação total anual
  prectot1=aggregate(sp2[,c(5)],by=list(Codigo=sp2$Codigo,Estacao=sp2$Estacoes,Ano=sp2$Ano),sum,na.rm=T)
  #Variabilidae da precipitação anual
  varprec1=aggregate(sp2[,c(5)],by=list(Codigo=sp2$Codigo,Estacao=sp2$Estacoes,Ano=sp2$Ano),sd,na.rm=T)
  #Dias com chuva inferior a 1mm
  diasemchover1=aggregate(sp2[,c(6)],by=list(Codigo=sp2$Codigo,Estacao=sp2$Estacoes,Ano=sp2$Ano),sum,na.rm=T)
  #Dias com chuva super a 25mm
  maisde25mm1=aggregate(sp2[,c(7)],by=list(Codigo=sp2$Codigo,Estacao=sp2$Estacoes,Ano=sp2$Ano),sum,na.rm=T)
  #Dias extremamente chuvosos, com chuva superior a 50mm
  maisde50mm1=aggregate(sp2[,c(8)],by=list(Codigo=sp2$Codigo,Estacao=sp2$Estacoes,Ano=sp2$Ano),sum,na.rm=T)
  
  rm(sp2,sp)
  gc()
  
  sp=merge(prectot1,varprec1,by=c('Codigo','Estacao','Ano'))
  sp=merge(sp,diasemchover1, by=c('Codigo','Estacao','Ano'))
  sp=merge(sp,maisde25mm1, by=c('Codigo','Estacao','Ano'))
  sp=merge(sp,maisde50mm1, by=c('Codigo','Estacao','Ano'))
  
  rm(prectot1,varprec1,diasemchover1,maisde25mm1,maisde50mm1)
  gc()
  
  sp1=sp[sp$Estacao==1,]
  sp2=sp[sp$Estacao==2,]
  sp3=sp[sp$Estacao==3,]
  sp4=sp[sp$Estacao==4,]
  
  sp=merge(sp1,sp2,by=c('Codigo','Ano'))
  sp=merge(sp,sp3,by=c('Codigo','Ano'))
  sp=merge(sp,sp4,by=c('Codigo','Ano'))
  
  rm(sp1,sp2,sp3,sp4)
  gc()
  
  ####Juntando variáveis agregadas por ano com estação do ano
  
  sp=merge(sp,prectot,by=c('Codigo','Ano'))
  sp=merge(sp,varprec,by=c('Codigo','Ano'))
  sp=merge(sp,maisde25mm,by=c('Codigo','Ano'))
  sp=merge(sp,maisde50mm,by=c('Codigo','Ano'))
  sp=merge(sp,diasemchover,by=c('Codigo','Ano'))
  
  rm(prectot,varprec,diasemchover,maisde25mm,maisde50mm)
  gc()
  
  colnames(sp)=c('Codigo','Ano','Primavera','PPrec','PPrecdesv','Pdsemvchuva','Pmaisde25',
                 'Pmaisde50','Verao','VPrec','VPrecdesv','Vdsemvchuva','Vmaisde25',
                 'Vmaisde50','Outono','OPrec','OPrecdesv','Odsemvchuva','Omaisde25',
                 'Omaisde50','Inverno','IPrec','IPrecdesv','Idsemvchuva','Imaisde25',
                 'Imaisde50','Prectotal','Desvpano','Maisde25ano','Maisde50ano','Semchuvano')
  
  sp=sp[sp$Ano<2016,]
  
  dbWriteTable(conn=con,name='Precipitacao4_2',value=as.data.frame(sp),append=T)
}

###########################################################################
###########################################################################
#Gráficos de precipitação e cálculo da tendência por UFs
library(RMySQL)
library(ggplot2)
library(ggpmisc)
prec=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(prec)
dbListFields(prec,'Precipitacao4')

precipitacao=dbGetQuery(prec,'select `Codigo`,`Ano`,`Prectotal` from Precipitacao4')
precipitacao=precipitacao[precipitacao$Ano>=1965&precipitacao$Ano<2015,]

mun=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/lat_long_municipiosbrasil.csv',
             header=T,encoding = "latin1")


precipitacao=merge(precipitacao,mun,by=c('Codigo'))
precipitacao=aggregate(precipitacao[,c(3)],by=list(UF=precipitacao$UF,Ano=precipitacao$Ano),mean,na.rm=T)
precipitacao$regiao[precipitacao$UF=='ACRE'|precipitacao$UF=='AMAPÁ'|precipitacao$UF=='AMAZONAS'|precipitacao$UF=='PARÁ'|precipitacao$UF=='RONDÔNIA'|precipitacao$UF=='RORAIMA'|precipitacao$UF=='TOCANTINS']='Norte'
precipitacao$regiao[precipitacao$UF=='ALAGOAS'|precipitacao$UF=='BAHIA'|precipitacao$UF=='CEARÁ'|precipitacao$UF=='MARANHÃO'|precipitacao$UF=='PARAÍBA'|precipitacao$UF=='PERNAMBUCO'|precipitacao$UF=='PIAUÍ'|precipitacao$UF=='RIO GRANDE DO NORTE'|precipitacao$UF=='SERGIPE']='Nordeste'
precipitacao$regiao[precipitacao$UF=='DISTRITO FEDERAL'|precipitacao$UF=='GOIÁS'|precipitacao$UF=='MATO GROSSO'|precipitacao$UF=='MATO GROSSO DO SUL']='Centro-Oeste'
precipitacao$regiao[precipitacao$UF=='ESPÍRITO SANTO'|precipitacao$UF=='MINAS GERAIS'|precipitacao$UF=='RIO DE JANEIRO'|precipitacao$UF=='SÃO PAULO']='Sudeste'
precipitacao$regiao[precipitacao$UF=='RIO GRANDE DO SUL'|precipitacao$UF=='SANTA CATARINA'|precipitacao$UF=='PARANÁ']='Sul'
colnames(precipitacao)[3]='Precipitação'


codigouf=as.data.frame(table(precipitacao$UF))
codigouf=as.character(codigouf[,c(1)])

lprecuf=list()

for (i in 1:length(codigouf))
{
  uf=codigouf[[i]]
  precuf=precipitacao[precipitacao$UF==uf,]
  precuf=precuf[order(precuf$Ano),]
  precuf=lm(precuf$Precipitação ~ precuf$Ano,data=precuf)
  precuf=cbind(uf,precuf$coefficients[2])
  #mkt=precuf[,c(3)]
  #mkt=mkTrend(mkt,ci=0.95)
  #mkt=cbind(uf,mkt$Z,mkt$p.value,mkt$Zc,mkt$`Corrected p.value`,mkt$tau,mkt$`N/N*s`,mkt$`Sen's Slope`)
  #colnames(mkt)=c('UF','mktZ','mktp.value','mktZc','mktCorrectedp.value','mkttau','mktN/N*s','mktSensSlope')
  lprecuf[[i]]=precuf
}

precuf=do.call(rbind.data.frame,lprecuf)
precuf$V2=as.numeric(levels(precuf$V2)[precuf$V2])
precuf$uf=as.character(precuf$uf)
precuf[25,][1]='SA PAULO'
precuf=precuf[order(precuf$uf),]


library(maptools)
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')

ufp=uf
ufp@data$ordem=1:dim(ufp@data)[1]
ufp@data=ufp@data[order(ufp@data$nome),]
ufp@data=cbind(ufp@data,precuf)
ufp@data$cor[ufp@data$V2<(-2.07)]='goldenrod'
ufp@data$cor[ufp@data$V2>-2.07&ufp@data$V2<0]='khaki'
ufp@data$cor[ufp@data$V2>0&ufp@data$V2<5]="deepskyblue2" 
ufp@data$cor[ufp@data$V2>5&ufp@data$V2<11]="royalblue4"
ufp@data=ufp@data[order(ufp@data$ordem),]
plot(ufp,border=T,lwd=.1,axes=F,las=1,col=ufp@data$cor)
legenda=as.character(c("-3,5 a -2","-2 a 0","0 a 5","5 a 10"))
cores=as.character(c("goldenrod","khaki","deepskyblue2","royalblue4"))
legend(x=-71,y=-18, legenda, fill=cores, bty="n", cex=0.9)
library(maps)
map.scale(x=-49.6, y=-31.2,relwidth=0.05,metric=T,ratio=F,cex=0.8)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))


regprec=aggregate(precipitacao[,c(3)],by=list(Regiao=precipitacao$regiao,Ano=precipitacao$Ano),mean,na.rm=T)
colnames(regprec)[3]='Precipitação'

reg=regprec[regprec$Regiao=='Sul',]
#Com equação da reta
p=qplot(Ano,Precipitação,data=reg)+geom_line()+stat_smooth(method='lm',se=T) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=13, angle=0))+
        xlab("Ano")+ylab("Precipitação (mm)") +theme(axis.text=element_text(size=12),
       axis.title=element_text(size=13,face="bold"))+ stat_poly_eq(formula = y ~ x, 
       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +         
      geom_point()

#Sem equação da reta 
p=qplot(Ano,Precipitação,data=reg)+geom_line()+stat_smooth(method='lm',se=T) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=0),
  axis.text.y = element_text(face="bold", color="black", size=13, angle=0))+
  xlab("Ano")+ylab("Precipitação (mm)") +theme(axis.text=element_text(size=12),
  axis.title=element_text(size=13,face="bold"))

p





#Tentativa de mudar o eixo y
#+ ylim(0,2000)



#Além plotar os gráficos de precipitação, cácular tendência da precipitação nos estados e 
#colocar em uma tabela

########################################################################
#####################################################################
#Antes de começar a estruturar o algoritmo para lidar com eventos extremos de precipitação
#preciso ver a distribuição das estações no espaço e quais as estações tem as séries mais
#longas de precipitação para definir o intervalor que vou tratar

library(RMySQL)

con=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(con)

#A tabela que tem os dados das estações é a tabela precipitação 
dbListFields(con,'Precipitacao')

precip=dbSendQuery(con,"select `X..EstacaoCodigo`,`Data` from Precipitacao")
precip=fetch(precip,n=-1)

library(tidyr)
precip=separate(precip,Data,c('Dia','Mes','Ano'),sep='/')
precip=precip[,c(1,4)]
precip=unique(precip)
colnames(precip)[1]='Codigo'

#Agora preciso atribuir coordenadas de latitude e longitude às estações
estacoes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/1-Dados Brutos/Estacoes/Estacao.csv',
                  header=T)

precip=merge(precip,estacoes,by=c('Codigo'),all.x=T,all.y=F)


#Estacoes meteorológicas com séries mais longas
estserielonga=as.data.frame(table(precip$Codigo))
estserielonga=estserielonga[order(-estserielonga$Freq),]
estserielonga=estserielonga[estserielonga$Freq>=150,]
estserielonga=merge(estserielonga,estacoes,by.x=c('Var1'),by.y=c('Codigo'),all.x=T,all.y=F)

#Preciso ver a distribuição espacial das estações no território brasileiro por períodos
#acho que vou agrupar segundo os períodos que o junior sugeriu
# Além disso quero ver a distribuição no território das 20 ou 15 estações meterológicas
# com séries mais longas. Vou calcular as medidas de extremos de precipitação para
# as estações com séries mais longas que eu escolher. 

library(maptools)
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')
plot(uf)
points(estserielonga$Longitude,estserielonga$Latitude,pch=21,col='darkgreen',bg=adjustcolor("darkgreen",0.5),cex=0.5,lwd=0.9)
#ACho que vai compensar pegar as estações mais velhas de cada região (norte, nordeste, sul, sudeste e centro-oeste). uma ou duas estações mais velhas
# de cada região geográfica ou classificação de koppen


#Vamos ver a qualidade das interpolações plotando o número de estações por período ou mesmo por ano
#Número de estações meteorológicas por ano
estporano=as.data.frame(table(precip$Ano))
estpano=precip[precip$Ano==2014,]
plot(uf)
points(estpano$Longitude,estpano$Latitude,pch=21,col='darkgreen',bg=adjustcolor("darkgreen",0.5),cex=0.5,lwd=0.9)
library(maps)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))


#1912 a 1917
#Boa interpolação apenas para a região nordeste

#1917 a 1920 
#Boa interpolação para a região nordeste, para os estados de RS e Espírito Santo, Rio de Janeiro

#1920 - 1930
#Boa interpolação para a região nordeste, para os estados de RS, Espírito Santo, Rio de Janeiro e para Minas Gerais

#1930 - 1935
#Boa interpolação para a região nordeste, para os estados de RS, Espírito Santo, Rio de Janeiro
#para Minas Gerais, Santa Catarina, e parte do PAraná, parte de São Paulo, Parte do MS, parte de MT,
#parte da AM e parte do PA

#1936 - 1940
#Boa interpolação para a região nordeste, para os estados de RS, Espírito Santo, Rio de Janeiro,
#para Minas Gerais, Santa Catarina e para são paulo (saão paulo ficou excelente e partir desse ano),
#e para parte do PAraná, Parte do MS, parte de MT, #parte da AM e parte do PA

#1941 -1947
#Todo o sudeste e todo o nordeste ficaram excelentes. Sul ficou muito bom com exceção do oeste do Paraná

#1947 - 1954
#Todo o sudeste, todo o nordeste e todo o sul ficaram excelentes,com exceção do oeste do Paraná

#1955-1960
#Todo o sudeste, todo o nordeste e todo o sul ficaram excelentes.

#1961 - 1969
#Todo o sudeste, todo o nordeste e todo o sul ficaram excelentes. Amazonas, MS, PA, GO e MT estão 
#razoáveis

#1970 - 1975
#Todo o sudeste, todo o nordeste e todo o sul ficaram excelentes, MT e MS, e GO também. Amazonas, PA, estão 
#razoáveis

#1976-1979
# Sul, sudeste, nordeste, e centro oeste estão excelentes. e norte está muito bom

#1980 - 2015
# Sul, sudeste, nordeste, centro oeste  e norte estão excelentes

################################################################################
###############################################################################
#Vou criar uma base agregada por mês com as mesmas medidas que utilizei para criar a base
#agregada a nível de ano. Pelo que li o spi(standardized precipitation index) é calculado com 
#base em meses. Então antes de tudo preciso ter a base agregada
library(RMySQL)
prec=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(prec)
dbListFields(prec,'Precipitacao3')


mun=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/lat_long_municipiosbrasil.csv',
             header=T,encoding = "latin1")
mun$UF=as.character(mun$UF)
uf=as.data.frame(table(mun$UF))
uf=as.character(uf$Var1)


for (i in 1:length(uf)){
  
  j=uf[i]
  sp=dbSendQuery(prec, paste("select *from Precipitacao3 where `UF` like '%",j,"%'",sep=""))
  sp2=fetch(sp,n=-1)
  
  gc()
  
  sp2=sp2[,c(2,8,9,10)]
  
  gc()
  
  #Variáveis agregadas para ano
  #Precipitação total anual
  prectot=aggregate(sp2[,c(4)],by=list(Codigo=sp2$Codigo,Mes=sp2$Mes,Ano=sp2$Ano),sum,na.rm=T)
  #Variabilidae da precipitação anual
  varprec=aggregate(sp2[,c(4)],by=list(Codigo=sp2$Codigo,Mes=sp2$Mes,Ano=sp2$Ano),sd,na.rm=T)
  #Dias com chuva inferior a 1mm
  sp2$seco[sp2$Precipitacao<1]=1
  diasemchover=aggregate(sp2[,c(5)],by=list(Codigo=sp2$Codigo,Mes=sp2$Mes,Ano=sp2$Ano),sum,na.rm=T)
  #Dias com chuva super a 25mm
  sp2$maide25mm[sp2$Precipitacao>=25]=1
  maisde25mm=aggregate(sp2[,c(6)],by=list(Codigo=sp2$Codigo,Mes=sp2$Mes,Ano=sp2$Ano),sum,na.rm=T)
  #Dias extremamente chuvosos, com chuva superior a 50mm
  sp2$maisde50mm[sp2$Precipitacao>=50]=1
  maisde50mm=aggregate(sp2[,c(7)],by=list(Codigo=sp2$Codigo,Mes=sp2$Mes,Ano=sp2$Ano),sum,na.rm=T)
  
  sp2=merge(prectot,varprec,by=c('Codigo','Mes','Ano')) 
  sp2=merge(sp2,diasemchover,by=c('Codigo','Mes','Ano'))
  sp2=merge(sp2,maisde25mm,by=c('Codigo','Mes','Ano'))
  sp2=merge(sp2,maisde50mm,by=c('Codigo','Mes','Ano'))
  colnames(sp2)=c('Codigo','Mes','Ano','Prectotalmes','Desvpmes','Dsemchuvmes','Maisde25mes','Maisde50mes')
  sp2=sp2[sp2$Ano<2016,]
  sp2=merge(mun,sp2,by=c('Codigo'))
  
  dbWriteTable(conn=prec,name='Precipitacao5',value=as.data.frame(sp2),append=T)
}


###############################################################################
###############################################################################
##Criei o indice de precipitação padronizada para todos os municípios do Brasil, considerando o período 1975-2014
library(SCI)
library(RMySQL)
prec=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(prec)
dbListFields(prec,'Precipitacao5')

br=dbGetQuery(prec,"select `Codigo`,`mun`,`UF`,`Mes`,`Ano`,`Prectotalmes` from Precipitacao5 where `Ano`>1974")

#and `UF` like 'SÃO PAULO'

mun=as.data.frame(table(br$Codigo))
mun=as.numeric(levels(mun[,c(1)])[mun[,c(1)]])
lmunicipio=list()

for (i in 1:length(mun)){
k=mun[[i]]

municipio=br[br$Codigo==k,]
municipio=municipio[order(municipio$Ano,municipio$Mes),]
chuvamun=municipio[,c(6)]

spi1=fitSCI(chuvamun,first.mon = 1,time.scale = 1,distr = "gamma",p0=T)
spi1=transformSCI(chuvamun,first.mon = 1,obj=spi1)

spi3=fitSCI(chuvamun,first.mon = 1,time.scale = 3,distr = "gamma",p0=T)
spi3=transformSCI(chuvamun,first.mon = 1,obj=spi3)

spi6=fitSCI(chuvamun,first.mon = 1,time.scale = 6,distr = "gamma",p0=T)
spi6=transformSCI(chuvamun,first.mon = 1,obj=spi6)

spi9=fitSCI(chuvamun,first.mon = 1,time.scale = 9,distr = "gamma",p0=T)
spi9=transformSCI(chuvamun,first.mon = 1,obj=spi9)

spi12=fitSCI(chuvamun,first.mon = 1,time.scale = 12,distr = "gamma",p0=T)
spi12=transformSCI(chuvamun,first.mon = 1,obj=spi12)

spi15=fitSCI(chuvamun,first.mon = 1,time.scale = 15,distr = "gamma",p0=T)
spi15=transformSCI(chuvamun,first.mon = 1,obj=spi15)

spi18=fitSCI(chuvamun,first.mon = 1,time.scale = 18,distr = "gamma",p0=T)
spi18=transformSCI(chuvamun,first.mon = 1,obj=spi18)

spi21=fitSCI(chuvamun,first.mon = 1,time.scale = 21,distr = "gamma",p0=T)
spi21=transformSCI(chuvamun,first.mon = 1,obj=spi21)

spi24=fitSCI(chuvamun,first.mon = 1,time.scale = 24,distr = "gamma",p0=T)
spi24=transformSCI(chuvamun,first.mon = 1,obj=spi24)

municipio=cbind(municipio,spi1,spi3,spi6,spi9,spi12,spi15,spi18,spi21,spi24)
#Eureca!!! É isso!!!!
lmunicipio[[i]]=municipio
}

br=do.call(rbind.data.frame,lmunicipio)
dbWriteTable(conn=prec,name='Precipitacao6',value=as.data.frame(br),append=F)
############################################################################
####################################################################################


###############################################################################
###############################################################################
#Vou criar uma tabela similar a Precipitacao6 só que com período correspondente ao das
#variáveis de produção 1990-2014


library(SCI)
library(RMySQL)
prec=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(prec)
dbListFields(prec,'Precipitacao5')

br=dbGetQuery(prec,"select `Codigo`,`mun`,`UF`,`Mes`,`Ano`,`Prectotalmes` from Precipitacao5 where `Ano`>=1990")

#and `UF` like 'SÃO PAULO'

mun=as.data.frame(table(br$Codigo))
mun=as.numeric(levels(mun[,c(1)])[mun[,c(1)]])

lmunicipio=list()

for (i in 1:length(mun)){
  k=mun[[i]]
  
  municipio=br[br$Codigo==k,]
  municipio=municipio[order(municipio$Ano,municipio$Mes),]
  chuvamun=municipio[,c(6)]
  
  spi1=fitSCI(chuvamun,first.mon = 1,time.scale = 1,distr = "gamma",p0=T)
  spi1=transformSCI(chuvamun,first.mon = 1,obj=spi1)
  
  spi3=fitSCI(chuvamun,first.mon = 1,time.scale = 3,distr = "gamma",p0=T)
  spi3=transformSCI(chuvamun,first.mon = 1,obj=spi3)
  
  spi6=fitSCI(chuvamun,first.mon = 1,time.scale = 6,distr = "gamma",p0=T)
  spi6=transformSCI(chuvamun,first.mon = 1,obj=spi6)
  
  spi9=fitSCI(chuvamun,first.mon = 1,time.scale = 9,distr = "gamma",p0=T)
  spi9=transformSCI(chuvamun,first.mon = 1,obj=spi9)
  
  spi12=fitSCI(chuvamun,first.mon = 1,time.scale = 12,distr = "gamma",p0=T)
  spi12=transformSCI(chuvamun,first.mon = 1,obj=spi12)
  
  spi15=fitSCI(chuvamun,first.mon = 1,time.scale = 15,distr = "gamma",p0=T)
  spi15=transformSCI(chuvamun,first.mon = 1,obj=spi15)
  
  spi18=fitSCI(chuvamun,first.mon = 1,time.scale = 18,distr = "gamma",p0=T)
  spi18=transformSCI(chuvamun,first.mon = 1,obj=spi18)
  
  spi21=fitSCI(chuvamun,first.mon = 1,time.scale = 21,distr = "gamma",p0=T)
  spi21=transformSCI(chuvamun,first.mon = 1,obj=spi21)
  
  spi24=fitSCI(chuvamun,first.mon = 1,time.scale = 24,distr = "gamma",p0=T)
  spi24=transformSCI(chuvamun,first.mon = 1,obj=spi24)
  
  municipio=cbind(municipio,spi1,spi3,spi6,spi9,spi12,spi15,spi18,spi21,spi24)
  #Eureca!!! É isso!!!!
  lmunicipio[[i]]=municipio
}

br=do.call(rbind.data.frame,lmunicipio)
dbWriteTable(conn=prec,name='Precipitacao7',value=as.data.frame(br),append=F)

##################################################################################
##################################################################################
#Criando os gráficos de evoluçaõ do SPI por estado da federação
#Agora preciso tratar os dados e interpolar
library(RMySQL)
library(SCI)
pluvio=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(pluvio)
dbListFields(pluvio,'Precipitacao5')

secauf=dbGetQuery(pluvio,"select `Codigo`,`Mes`,`Ano`,`UF`,`Prectotalmes` from Precipitacao5")
secauf=secauf[secauf$Ano>1974,]
secauf=aggregate(secauf[,c(5)],by=list(uf=secauf$UF,Ano=secauf$Ano,Mes=secauf$Mes),mean,na.rm=T)
secauf=secauf[order(secauf$uf,secauf$Ano,secauf$Mes),]

uf=as.data.frame(table(secauf$uf))
uf=as.character(uf[,c(1)])

lsecauf=list()

for (i in 1:length(uf)){
k=uf[[i]]
chuvauf=secauf[secauf$uf==k,]
spi1=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 1,distr = "gamma",p0=T)
spi1=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi1)

spi3=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 3,distr = "gamma",p0=T)
spi3=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi3)

spi6=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 6,distr = "gamma",p0=T)
spi6=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi6)

spi9=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 9,distr = "gamma",p0=T)
spi9=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi9)

spi12=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 12,distr = "gamma",p0=T)
spi12=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi12)

spi15=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 15,distr = "gamma",p0=T)
spi15=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi15)

spi18=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 18,distr = "gamma",p0=T)
spi18=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi18)

spi21=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 21,distr = "gamma",p0=T)
spi21=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi21)

spi24=fitSCI(chuvauf[,c(4)],first.mon = 1,time.scale = 24,distr = "gamma",p0=T)
spi24=transformSCI(chuvauf[,c(4)],first.mon = 1,obj=spi24)

chuvauf=cbind(chuvauf,spi1,spi3,spi6,spi9,spi12,spi15,spi18,spi21,spi24)

lsecauf[[i]]=chuvauf
}

secauf=do.call(rbind.data.frame,lsecauf)

for (i in 1:length(uf)){
k=uf[[i]]
spi=secauf[secauf$uf==k,]

spi=as.matrix(spi[,c(5:13)])

#2D plot
spi.breaks <- c(-2.4,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,2.4)
spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
dates<- seq(from=1975+1/24, to= 2016,by=1/12)
filled.contour(dates,seq(0,24,by=3),spi,col=spi.cols(11),xlab="",ylab="Tempo (meses)",cex.lab=1.5,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI",
               plot.axes = {axis(1,cex.axis=2.2)
                            axis(2,cex.axis=2)})
title(main=paste('SPI -',k),cex.main=2)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Mapas - UF - SPI')
#Codigo para salvar o mapa diretamente
dev.print(pdf, file=paste(k,'.pdf'))
}  


######Vou fazer os mapas da distribuição espacial dos índices com base no número de meses em que cada
#município foi classificado em condição de seca e o número de vezes em que foi classifcado em condição
# de excesso de chuva. Vão ser dois conjuntos de mapas SPI6 e SPI12
# Acho que vale a pena também agrupar o número de eventos extremos em intervalos de cinco em cinco
#e anos e verificar a taxa de crescimento
#verirficar 
#Também tenho que definir se trabalho com eventos muito molhado, extremamente molhado, severamente seco,
#ou extremamente seco indivudalmente, ou se agrego em categorias. mais espeficicamente, se trabalho
#com os eventos mais extremos ou se agrego em uma mesma categoria os eventos mais extremos e 
#os que estão logo mais proximo na escala
#Vou trabalhar com os dados da tabela precipitacao 6

library(RMySQL)
prec=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(prec)
dbListFields(prec,"Precipitacao6")

precipitacao=dbGetQuery(prec,"select `Codigo`,`mun`,`UF`,`Mes`,`Ano`,`spi3`,`spi6`,`spi12`,`spi24` from Precipitacao6")


###############################################################################
################################################################################
#Essa estratégia de contagem não deu em nada. Vou tentar taxa de crescimento de 5 em 5 anos
#
#Vou criar 4 binárias e depois fazer a contagem via aggregate
#molhado1 - SPI entre 1,5 e 1,99  - Muito molhado
#molhado2 - SPI acima de 2 - Extremamente molhado
#seco1 - SPI entre -1,5 e -1,99 - severamente seco
#seco2 - SPI -2 ou menos - extremamente seco

#SPI6
#precipitacao$molhado61[precipitacao$spi6>=1.5&precipitacao$spi6<2]=1
#precipitacao$molhado62[precipitacao$spi6>=2]=1
#precipitacao$seco61[precipitacao$spi6<(-1.5)&precipitacao$spi6>-2]=1
#precipitacao$seco62[precipitacao$spi6<=-2]=1

#spi6=aggregate(precipitacao[,c(12:15)],by=list(Codigo=precipitacao$Codigo),sum,na.rm=T)
#colnames(mun6@data)[9:12]=c('molhado61','molhado62','seco61','seco62')

#library(maptools)
#library(maps)

#municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
#uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')

#mun6=municipios
#mun6@data$ordem=1:dim(mun6@data)[1]
#mun6@data=merge(mun6@data,spi6,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=T,all.y=F)

#mun6@data$cormolhado61[mun6@data$molhado61<17]='lightskyblue'
#mun6@data$cormolhado61[mun6@data$molhado61>=17&mun6@data$molhado61<21]='royalblue3'
#mun6@data$cormolhado61[mun6@data$molhado61>=21&mun6@data$molhado61<25]='royalblue4'
#mun6@data$cormolhado61[mun6@data$molhado61>=25]='navy'

#mun6@data$cormolhado62[mun6@data$molhado62<8]='lightskyblue'
#mun6@data$cormolhado62[mun6@data$molhado62>=8&mun6@data$molhado62<12]='royalblue3'
#mun6@data$cormolhado62[mun6@data$molhado62>=12&mun6@data$molhado62<15]='royalblue4'
#mun6@data$cormolhado62[mun6@data$molhado62>=15]='navy'

#mun6@data$corseco61[mun6@data$seco61<20]='yellow'
#mun6@data$corseco61[mun6@data$seco61>=20&mun6@data$seco61<25]='gold2'  
#mun6@data$corseco61[mun6@data$seco61>=25&mun6@data$seco61<50]='goldenrod'  
#mun6@data$corseco61[mun6@data$seco61>=50]='goldenrod4'

#mun6@data$corseco62[mun6@data$seco62<15]='yellow'
#mun6@data$corseco62[mun6@data$seco62>=15&mun6@data$seco62<20]='gold2'  
#mun6@data$corseco62[mun6@data$seco62>=20&mun6@data$seco62<30]='goldenrod'  
#mun6@data$corseco62[mun6@data$seco62>=30]='goldenrod4'

#mun6@data=mun6@data[order(mun6@data$ordem),]

#plot(mun6,border=F,lwd=.1,axes=F,las=1,col=mun6@data$cormolhado61)
#plot(uf,add=TRUE,lwd=2)

#plot(mun6,border=F,lwd=.1,axes=F,las=1,col=mun6@data$cormolhado62)
#plot(uf,add=TRUE,lwd=2)

#plot(mun6,border=F,lwd=.1,axes=F,las=1,col=mun6@data$corseco61)
#plot(uf,add=TRUE,lwd=2)

#plot(mun6,border=F,lwd=.1,axes=F,las=1,col=mun6@data$corseco62)
#plot(uf,add=TRUE,lwd=2)


  
#map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
#setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
#source(compassRose(-44.4,-27.5))

#SPI12
#precipitacao$molhado121[precipitacao$spi12>=1.5&precipitacao$spi12<2]=1
#precipitacao$molhado122[precipitacao$spi12>=2]=1
#precipitacao$seco121[precipitacao$spi12<(-1.5)&precipitacao$spi12>-2]=1
#precipitacao$seco122[precipitacao$spi12<=-2]=1

#Nâo deu em nada. Vou tentar ver como são as taxas de crescimento para o SPI12. Antes de criar esses valores
#de Spi e SPi12 tenho que criar uma binárias ou uma variável categorica que corresponda aos
#períodos de 5 em 5 anos
#precipitacao$periodo[precipitacao$Ano>1975&precipitacao$Ano<=1980]=1
#precipitacao$periodo[precipitacao$Ano>1980&precipitacao$Ano<=1985]=2
#precipitacao$periodo[precipitacao$Ano>1985&precipitacao$Ano<=1990]=3
#precipitacao$periodo[precipitacao$Ano>1990&precipitacao$Ano<=1995]=4
#precipitacao$periodo[precipitacao$Ano>1995&precipitacao$Ano<=2000]=5
#precipitacao$periodo[precipitacao$Ano>2000&precipitacao$Ano<=2005]=6
#precipitacao$periodo[precipitacao$Ano>2005&precipitacao$Ano<=2010]=7
#precipitacao$periodo[precipitacao$Ano>2010&precipitacao$Ano<=2015]=8


#spi6=aggregate(precipitacao[,c(8:11)],by=list(Codigo=precipitacao$Codigo,Periodo=precipitacao$periodo),sum,na.rm=T)
#spi12=aggregate(precipitacao[,c(12:15)],by=list(Codigo=precipitacao$Codigo,Periodo=precipitacao$periodo),sum,na.rm=T)

#Vou tentar fazer o mapa da taxa de crescimento e anível de município para o SPI12. Se não der em nada
#Vou agregar a nível de estado de alguma forma. Não sei se vale calcular taxa de crescimento. Vou cair em uma regressão
#não linear..... tenho que pensar em outra estratégia se quiser mostrar os dados a nível de município

#Vou calcular o Mann-Kendall modificado para todos os municípios e plotar no gráfico ou valor
#do sen slope em mapa para os casos em que foi verificada tendência pro mann kendall. Vou fazer isso
#para curto/médio prazo 6 meses e médio/longo prazo 12 meses e longo prazo 24
#Mann-Kendall modified trend test for serially correlated data
#http://artax.karlin.mff.cuni.cz/r-help/library/fume/html/mkTrend.html

#library(doMC)
#registerDoMC(4)
library(fume)

codigomun=as.data.frame(table(precipitacao$Codigo))
codigomun=as.numeric(levels(codigomun[,c(1)])[codigomun[,c(1)]])
lprecmun=list()

 #lprecmun=foreach(i=1:10) %dopar% 
for (i in 1:2000)
   {
#library(fume)
municipio=codigomun[[i]]
precmun=precipitacao[precipitacao$Codigo==municipio,]
precmun=precmun[order(precmun$Ano,precmun$Mes),]
mk3=precmun[,c(6)]
mk3=mkTrend(mk3,ci=0.95)
mk6=precmun[,c(7)]
mk6=mkTrend(mk6,ci=0.95)
mk12=precmun[,c(8)]
mk12=mkTrend(mk12,ci=0.95)
mk24=precmun[,c(9)]
mk24=mkTrend(mk24,ci=0.95)
mk=cbind(precmun$Codigo[1],precmun$mun[1],precmun$UF[1],mk3$Z,mk3$p.value,mk3$Zc,mk3$`Corrected p.value`,mk3$tau,mk3$`N/N*s`,mk3$`Sen's Slope`,
         mk6$Z,mk6$p.value,mk6$Zc,mk6$`Corrected p.value`,mk6$tau,mk6$`N/N*s`,mk6$`Sen's Slope`,
         mk12$Z,mk12$p.value,mk12$Zc,mk12$`Corrected p.value`,mk12$tau,mk12$`N/N*s`,mk12$`Sen's Slope`,
         mk24$Z,mk24$p.value,mk24$Zc,mk24$`Corrected p.value`,mk24$tau,mk24$`N/N*s`,mk24$`Sen's Slope`)

colnames(mk)=c('Codigo','Municipio','UF','mk3Z','mk3p.value','mk3Zc','mk3Correctedp.value','mk3tau','mk3N/N*s','mk3SensSlope',
                                         'mk6Z','mk6p.value','mk6Zc','mk6Correctedp.value','mk6tau','mk6N/N*s','mk6SensSlope',
                                         'mk12Z','mk12p.value','mk12Zc','mk12Corrected p.value','mk12tau','mk12N/N*s','mk12SensSlope',
                                         'mk24Z','mk24p.value','mk24Zc','mk24Corrected p.value','mk24tau','mk24$N/N*s','mk24SensSlope')

lprecmun[[i]]=mk
}

precmun=do.call(rbind.data.frame,lprecmun)
dbWriteTable(conn=prec,name='Precipitacao8',value=as.data.frame(precmun),append=T)

########################################################################
########################################################################
#Mapas Mann-Kendall
library(RMySQL)
library(maptools)
library(maps)

prec=dbConnect(MySQL(),dbname='PLUVIOSIDADE',user='',password='''',host='')
dbListTables(prec)

#Colocar na tabela precipitação8 os dados de mankendall calculados no pc do buainain
#library(RSQLite)
#setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL/mktrend/Buainain')
#prectuca=dbConnect(SQLite(),dbname='IDP2001_4000')
#prec2=dbGetQuery(prectuca,'select *from IDP2001_4000')
#prec2=unique(prec2)
#dbWriteTable(conn=prec,name='Precipitacao8',value=as.data.frame(prec2),append=T)

#Colocar na tabela precipitação8 os dados de mankendall calculados no pc do gori
#library(RSQLite)
#setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL/mktrend/Gori/')
#precgori=dbConnect(SQLite(),dbname='IDP4001_5565')
#prec3=dbGetQuery(precgori,'select *from IDP4001_5565')
#prec3=unique(prec3)
#dbWriteTable(conn=prec,name='Precipitacao8',value=as.data.frame(prec3),append=T)


#Colocar na tabela precipitação8 os dados de mankendall calculados no pc do Buainain pela segunda vez
#library(RSQLite)
#setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL/mktrend/Buainain 2/')
#prectuca2=dbConnect(SQLite(),dbname='IDP4001_55652')
#prec4=dbGetQuery(prectuca2,'select *from IDP4001_5565')
#prec4=unique(prec4)
#dbWriteTable(conn=prec,name='Precipitacao8',value=as.data.frame(prec4),append=T)



municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
municipios@data$ordem=1:dim(municipios)[1]
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')



mannkendall=dbGetQuery(prec,'select *from Precipitacao8')

mk3=mannkendall[,c(2,3,4,7,8,11)]
mk3$mk3Correctedp.value=as.numeric(mk3$mk3Correctedp.value)
mk3$mk3SensSlope=as.numeric(mk3$mk3SensSlope)
mk3=mk3[mk3$mk3Correctedp.value<=0.05,]
mk3mun=municipios
mk3mun@data=merge(mk3mun@data,mk3,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=T,all.y=F)
mk3mun@data$cor[mk3mun@data$mk3SensSlope<=(-0.001)]='red'
mk3mun@data$cor[mk3mun@data$mk3SensSlope>-0.001&mk3mun@data$mk3SensSlope<=0.001]='darkorange'
mk3mun@data$cor[mk3mun@data$mk3SensSlope>0&mk3mun@data$mk3SensSlope<=0.001]='blue'
mk3mun@data$cor[mk3mun@data$mk3SensSlope>0.001]='navy'
mk3mun@data=mk3mun@data[order(mk3mun$ordem),]
plot(mk3mun,border=F,lwd=.1,axes=F,las=2,col=mk3mun@data$cor)
plot(uf,add=TRUE,lwd=1.5)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#legenda
legenda=as.character(c("Inferior a -0,001","Entre -0,001 e 0","Entre 0 e 0,001", "Superior a 0,001"))
cores=as.character(c("red","darkorange","blue","navy"))
legend(x=-74.8,y=-17, legenda, fill=cores, bty="n", cex=1.45)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))


mk6=mannkendall[,c(2,3,4,14,15,18)]
mk6$mk6Correctedp.value=as.numeric(mk6$mk6Correctedp.value)
mk6$mk6SensSlope=as.numeric(mk6$mk6SensSlope)
mk6=mk6[mk6$mk6Correctedp.value<=0.05,]
mk6mun=municipios
mk6mun@data=merge(mk6mun@data,mk6,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=T,all.y=F)
mk6mun@data$cor[mk6mun@data$mk6SensSlope<=(-0.001)]='red'
mk6mun@data$cor[mk6mun@data$mk6SensSlope>-0.001&mk6mun@data$mk6SensSlope<=0.001]='darkorange'
mk6mun@data$cor[mk6mun@data$mk6SensSlope>0&mk6mun@data$mk6SensSlope<=0.001]='blue'
mk6mun@data$cor[mk6mun@data$mk6SensSlope>0.001]='navy'
mk6mun@data=mk6mun@data[order(mk6mun$ordem),]
plot(mk6mun,border=F,lwd=.1,axes=F,las=2,col=mk6mun@data$cor)
plot(uf,add=TRUE,lwd=1.5)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#legenda
legenda=as.character(c("Inferior a -0,001","Entre -0,001 e 0","Entre 0 e 0,001", "Superior a 0,001"))
cores=as.character(c("red","darkorange","blue","navy"))
legend(x=-74.8,y=-17, legenda, fill=cores, bty="n", cex=1.45)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))

mk12=mannkendall[,c(2,3,4,21,22,25)]
mk12$`mk12Corrected p.value`=as.numeric(mk12$`mk12Corrected p.value`)
mk12$mk12SensSlope=as.numeric(mk12$mk12SensSlope)
mk12=mk12[mk12$`mk12Corrected p.value`<=0.05,]
mk12mun=municipios
mk12mun@data=merge(mk12mun@data,mk12,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=T,all.y=F)
mk12mun@data=merge(mk6mun@data,mk6,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=T,all.y=F)
mk12mun@data$cor[mk12mun@data$mk12SensSlope<=(-0.001)]='red'
mk12mun@data$cor[mk12mun@data$mk12SensSlope>-0.001&mk12mun@data$mk12SensSlope<=0.001]='darkorange'
mk12mun@data$cor[mk12mun@data$mk12SensSlope>0&mk12mun@data$mk12SensSlope<=0.001]='blue'
mk12mun@data$cor[mk12mun@data$mk12SensSlope>0.001]='navy'
mk12mun@data=mk12mun@data[order(mk12mun$ordem),]
plot(mk12mun,border=F,lwd=.05,axes=F,las=2,col=mk12mun@data$cor)
plot(uf,add=TRUE,lwd=2)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#legenda
legenda=as.character(c("Inferior a -0,001","Entre -0,001 e 0","Entre 0 e 0,001", "Superior a 0,001"))
cores=as.character(c("red","darkorange","blue","navy"))
legend(x=-74.8,y=-17, legenda, fill=cores, bty="n", cex=1.45)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))

mk24=mannkendall[,c(2,3,4,28,29,32)]
mk24$`mk24Corrected p.value`=as.numeric(mk24$`mk24Corrected p.value`)
mk24$mk24SensSlope=as.numeric(mk24$mk24SensSlope)
mk24=mk24[mk24$`mk24Corrected p.value`<=0.05,]
mk24mun=municipios
mk24mun@data=merge(mk24mun@data,mk24,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=T,all.y=F)
mk24mun@data$cor[mk24mun@data$mk24SensSlope<=(-0.001)]='red'
mk24mun@data$cor[mk24mun@data$mk24SensSlope>-0.001&mk24mun@data$mk24SensSlope<=0.001]='darkorange'
mk24mun@data$cor[mk24mun@data$mk24SensSlope>0&mk24mun@data$mk24SensSlope<=0.001]='blue'
mk24mun@data$cor[mk24mun@data$mk24SensSlope>0.001]='navy'
mk24mun@data=mk24mun@data[order(mk24mun$ordem),]
plot(mk24mun,border=F,lwd=.05,axes=F,las=2,col=mk24mun@data$cor)
plot(uf,add=TRUE,lwd=2)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#legenda
legenda=as.character(c("Inferior a -0,001","Entre -0,001 e 0","Entre 0 e 0,001", "Superior a 0,001"))
cores=as.character(c("red","darkorange","blue","navy"))
legend(x=-74.8,y=-17, legenda, fill=cores, bty="n", cex=1.45)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))


################################################################################
###############################################################################
#Hora de começar a modelagem para a segundo capítulo

#Vou fazer mapas de círculos proporcionais para o valor da produção permanente e para a produção temporária
# e mapa para a classificação climática de koppen. Aleḿ disso vou fazer gráficos mostrando
# a evolução dos valores de produção no Brasil. Dois Mapas
#Vou começar por Koppen
#library(XLConnect)
#koppenmun=readWorksheetFromFile('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Classificação climática de Koppen/Koppen Brazilian municipalities.xls',
#                                sheet=2,header=T)

#library(maptools)
#library(maps)
#municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
#municipios@data$ordem=1:dim(municipios)[1]
#uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')

#mkoppen=municipios
#mkoppen@data=merge(mkoppen@data,koppenmun,by.x=c('codigo_ibg'),by.y=c('IBGE.Code'),all.x=T,all.y=T)
#mkoppen@data$cor[mkoppen@data$Köppen=='Af']="#211f63"
#mkoppen@data$cor[mkoppen@data$Köppen=='Am']="#324a99"
#mkoppen@data$cor[mkoppen@data$Köppen=='As']="#90c1e0"
#mkoppen@data$cor[mkoppen@data$Köppen=='Aw']="#3c7dc7"
#mkoppen@data$cor[mkoppen@data$Köppen=='BSh']="#d19226"
#mkoppen@data$cor[mkoppen@data$Köppen=='Cfa']="#acc21b"
#mkoppen@data$cor[mkoppen@data$Köppen=='Cfb']="#457337"
#mkoppen@data$cor[mkoppen@data$Köppen=='Cwa']="#549948"
#mkoppen@data$cor[mkoppen@data$Köppen=='Cwb']="#7dad66"
#mkoppen@data=mkoppen@data[order(mkoppen@data$ordem),]
#plot(mkoppen,border=F,lwd=.05,axes=F,las=2,col=mkoppen@data$cor)
#plot(uf,add=TRUE,lwd=1)
#map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
#setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
#source(compassRose(-44.4,-27.5))

###################################################################
#Ok vou utilizar regiões brasileiras no modelo de dados em painel.
#O que vou fazer agora é caracterizar o valor da produção

#Inflação

#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[20]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}
ipca=ipca[,c(1,5)]



#1 Gráfico com linha de tendência mostrando a evolução do valor da produção
# de culuras temporárias e permanentes no BR e a linha de tendência
library(tidyr)
library(ggplot2)
library(ggpmisc)

#carregar os dados a nível de UF
#permanentes
perm=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/permbr-2.csv',
              sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252')
perm=perm[c(1:567),]
perm=separate(perm,Unidade.da.Federação,c('Codigo','UF'),sep='-')
perm$Vtotal=rowSums(perm[,c(4:38)],na.rm=T)
perm=merge(perm,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)
perm$Vtotalreal=perm$Vtotal*perm$V3
perm=aggregate(perm[,c(41)],by=list(Ano=perm$Ano),sum,na.rm=T)
perm=perm[c(1:20),]
colnames(perm)=c('Ano','VBP')
perm$VBP=perm$VBP/1000000

#Já salvei direto  em eps na pasta do latex. Quando salva em eps, Automaticamente salva
#na mesma pasta
#Com equação da reta
p=qplot(Ano,VBP,data=perm)+geom_line()+stat_smooth(method='lm',se=T) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=0),
  axis.text.y = element_text(face="bold", color="black", size=13, angle=0))+
  xlab("Ano")+ylab("VBP (bilhões de R$)") +theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+ stat_poly_eq(formula = y ~ x, 
  aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +         
  geom_point()
 p

#Temporárias
temp=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/tempbr-1.csv',
               sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252') 
temp=temp[c(1:567),]
temp=separate(temp,Unidade.da.Federação,c('Codigo','UF'),sep='-')
temp$Vtotal=rowSums(temp[,c(4:34)],na.rm=T)
temp=merge(temp,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)
temp$Vtotalreal=temp$Vtotal*temp$V3
temp=aggregate(temp[,c(37)],by=list(Ano=temp$Ano),sum,na.rm=T)
temp=temp[c(1:20),]
colnames(temp)=c('Ano','VBP')
temp$VBP=temp$VBP/1000000


#Com equação da reta
p=qplot(Ano,VBP,data=temp)+geom_line()+stat_smooth(method='lm',se=T) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=0),
  axis.text.y = element_text(face="bold", color="black", size=13, angle=0))+
  xlab("Ano")+ylab("VBP (bilhões de R$)") +theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+ stat_poly_eq(formula = y ~ x, 
  aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +         
  geom_point()
p


#2 Mapas de circulos proporcionais mostrando os valores da produção
#de culturas temporárias e permanentes em 1995 e 2014. Encontrar shapefiles para
#as regiões ou pintá-las com cores diferentes a partir do shapefile de unidades da 
#federação
library(maptools)
library(maps)
municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
municipios@data$ordem=1:dim(municipios)[1]
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')

regiao=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Shapefiles/regioes_2010/regioes_2010.shp')
regiao@data$nome=as.character(regiao@data$nome)
regiao@data$cor[regiao@data$nome=="Norte"]=adjustcolor("forestgreen",0.15)
regiao@data$cor[regiao@data$nome=="Centro-Oeste"]=adjustcolor("gold2",0.15)
regiao@data$cor[regiao@data$nome=="Nordeste"]=adjustcolor("tan2",0.15)
regiao@data$cor[regiao@data$nome=="Sudeste"]=adjustcolor("firebrick2",0.15)
regiao@data$cor[regiao@data$nome=="Sul"]=adjustcolor("royalblue2",0.15)






#Carregar dados de culturas permanentes a nível de municípios
permanentes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/culturas permanentes-1.csv',
                     sep=';',header=F,skip=0,na.strings = c('-','NA'))
permanentes=permanentes[c(5:144694),]
library(tidyr)
permanentes=separate(permanentes,V2,c('Ano','x1','x2'),sep=' ')
permanentes=permanentes[permanentes$Ano>=1995,c(1,2,5)]
colnames(permanentes)=c('Codigo','Ano','VPP')
permanentes=permanentes[permanentes$VPP>1,]
permanentes=permanentes[!is.na(permanentes$VPP),]
permanentes=merge(permanentes,ipca,by.x=c('Ano'),by.y=c('Data'))
permanentes$VPPR=permanentes$VPP*permanentes$V3
permanentes$logvppr=log(permanentes$VPPR)

#permanentes 1995
perm1995=permanentes[permanentes$Ano==1995,]
munperm1995=municipios
coordsmun=as.data.frame(coordinates(munperm1995))
munperm1995@data=cbind(munperm1995@data, coordsmun)
munperm1995@data=merge(munperm1995@data,perm1995,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=F)
munperm1995@data=munperm1995@data[order(munperm1995@data$ordem),]
plot(uf,lwd=1)
plot(regiao,add=TRUE,lwd=1,col=regiao@data$cor)
plot(munperm1995,add=T,border=F)
points(munperm1995$V1,munperm1995$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=munperm1995@data$VPPR/48000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("160", "320","480","640"),x=-74.2,y=-11.5, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.5,title="Milhões R$",
       pt.cex =c(641000/48000/4,641000/48000/2,641000/48000*3/4,641000/48000))


#permanentes 2014
perm2014=permanentes[permanentes$Ano==2014,]
munperm2014=municipios
coordsmun=as.data.frame(coordinates(munperm2014))
munperm2014@data=cbind(munperm2014@data, coordsmun)
munperm2014@data=merge(munperm2014@data,perm2014,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=F)
munperm2014@data=munperm2014@data[order(munperm2014@data$ordem),]
plot(uf,lwd=1)
plot(regiao,add=TRUE,lwd=1,col=regiao@data$cor)
plot(munperm2014,add=T,border=F)
points(munperm2014@data$V1,munperm2014@data$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=munperm2014@data$VPPR/48000,lwd=0.9)
plot(uf,add=TRUE,lwd=1)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("160", "320","480","640"),x=-74.2,y=-11.5, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.5,title="Milhões R$",
       pt.cex =c(641000/48000/4,641000/48000/2,641000/48000*3/4,641000/48000))


#Carregar dados de culturas temporárias a nível de municípios
temporarias=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/culturas temporarias-1.csv',
                     sep=';',header=F,skip=0,na.strings = c('-','NA'))
temporarias=temporarias[c(135:144824),]
library(tidyr)
temporarias=separate(temporarias,V2,c('Ano','x1','x2'),sep=' ')
temporarias=temporarias[temporarias$Ano>=1995,c(1,2,5)]
colnames(temporarias)=c('Codigo','Ano','VPT')
temporarias=temporarias[temporarias$VPT>1,]
temporarias=temporarias[!is.na(temporarias$VPT),]
temporarias=merge(temporarias,ipca,by.x=c('Ano'),by.y=c('Data'))
temporarias$VPTR=temporarias$VPT*temporarias$V3
temporarias$logvptr=log(temporarias$VPTR)



#temporarias 1995
temp1995=temporarias[temporarias$Ano==1995,]
muntemp1995=municipios
coordsmun=as.data.frame(coordinates(muntemp1995))
muntemp1995@data=cbind(muntemp1995@data, coordsmun)
muntemp1995@data=merge(muntemp1995@data,temp1995,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=F)
muntemp1995@data=muntemp1995@data[order(muntemp1995@data$ordem),]
plot(uf,lwd=1)
plot(regiao,add=TRUE,lwd=1,col=regiao@data$cor)
plot(muntemp1995,add=T,border=F)
points(muntemp1995@data$V1,muntemp1995@data$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=muntemp1995@data$VPTR/180000,lwd=0.9)
plot(uf,add=TRUE,lwd=1)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("0,5", "1,1","1,7","2,2"),x=-74.2,y=-11.5, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.5,title="Bilhões R$",
       pt.cex =c(2249390/180000/4,2249390/180000/2,2249390/180000*3/4,2249390/180000))


#temporarias 2014
temp2014=temporarias[temporarias$Ano==2014,]
muntemp2014=municipios
coordsmun=as.data.frame(coordinates(muntemp2014))
muntemp2014@data=cbind(muntemp2014@data, coordsmun)
muntemp2014@data=merge(muntemp2014@data,temp2014,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=F)
muntemp2014@data=muntemp2014@data[order(muntemp2014@data$ordem),]
plot(uf,lwd=1)
plot(regiao,add=TRUE,lwd=1,col=regiao@data$cor)
plot(muntemp2014,add=T,border=F)
points(muntemp2014@data$V1,muntemp2014@data$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=muntemp2014@data$VPTR/180000,lwd=0.9)
plot(uf,add=TRUE,lwd=1)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("0,5", "1,1","1,7","2,2"),x=-74.2,y=-11.5, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.5,title="Bilhões R$",
       pt.cex =c(2249390/180000/4,2249390/180000/2,2249390/180000*3/4,2249390/180000))

###########################################################

#3 15 ou 20 Principais culturas permanentes e temporárias em 1995 e em 2014 no Brasil
library(xtable)

#Carregar dados a níveis de uf e aggregar a nível de país por cultura agrícola, ordenar e fazer
#uma tabela em latex
#Inflação

#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[20]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}
ipca=ipca[,c(1,5)]

library(tidyr)
library(Hmisc)

#permanentes
perm=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/permbr-2.csv',
              sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252')
perm=perm[c(1:567),]
perm=separate(perm,Unidade.da.Federação,c('Codigo','UF'),sep='-')
perm$Vtotal=rowSums(perm[,c(4:38)],na.rm=T)
perm=merge(perm,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)

#1995
perm1995=perm[perm$Ano==1995,]
per1995matrix=as.matrix(perm1995[,c(4:38)])
per1995matrix=per1995matrix*3.771212
perm1995=cbind(perm1995[,c(1:3)],per1995matrix)
perm1995=aggregate(perm1995[,c(4:38)],by=list(Ano=perm1995$Ano),sum,na.rm=T)
perm1995=as.data.frame(cbind(rep(1995,35),t(perm1995[,c(2:36)])))
Produto=c(rownames(perm1995))
perm1995=cbind(perm1995,Produto)
rownames(perm1995)=NULL
perm1995=perm1995[,c(3,2)]
colnames(perm1995)=c('Produto','VBP')
perm1995=perm1995[c(1:7,10:35),]


#2014
perm2014=perm[perm$Ano==2014,]
per2014matrix=as.matrix(perm2014[,c(4:38)])
per2014matrix=per2014matrix*1
perm2014=cbind(perm2014[,c(1:3)],per2014matrix)
perm2014=aggregate(perm2014[,c(4:38)],by=list(Ano=perm2014$Ano),sum,na.rm=T)
perm2014=as.data.frame(cbind(rep(2014,35),t(perm2014[,c(2:36)])))
Produto=c(rownames(perm2014))
perm2014=cbind(perm2014,Produto)
rownames(perm2014)=NULL
perm2014=perm2014[,c(3,2)]
colnames(perm2014)=c('Produto','Valor da Produção')
perm2014=perm2014[c(1:7,10:35),]

prodnome=c('Abacate','Algodão arbóreo','Azeitona','Banana','Borracha','Cacau','Café','Caqui','Castanha de Caju','Chá da Índia','Coco da baía','Dendê','Erva Mate',
           'Figo','Goiaba','Guaraná','Laranja','Limão','Maçã','Mamão','Manga','Maracujá','Marmelo','Noz',
           'Palmito','Pera','Pêssego','Pimenta do reino','Sisal','Tangerina','Tungue','Urucum','Uva')

permanente1995=cbind(perm1995,prodnome)
permanente2014=cbind(perm2014,prodnome)

permanente1995$Percentual_1995=(permanente1995$VBP/sum(permanente1995$VBP))*100
permanente2014$Percentual_2014=(permanente2014$`Valor da Produção`/sum(permanente2014$`Valor da Produção`))*100

permanente1995=permanente1995[order(-permanente1995$Percentual_1995),]
permanente2014=permanente2014[order(-permanente2014$Percentual_2014),]

permanente=cbind(permanente1995,permanente2014)
permanente=permanente[,c(3,2,4,7,6,8)]
permanente$VBP=permanente$VBP/1000000
permanente$`Valor da Produção`=permanente$`Valor da Produção`/1000000
outrosperm=permanente[c(21:33),]
outrosperm=cbind('Outros',sum(outrosperm$VBP),sum(outrosperm$Percentual_1995),'Outros',
                 sum(outrosperm$`Valor da Produção`),sum(outrosperm$Percentual_2014))
colnames(outrosperm)=colnames(permanente)
permanente=rbind(permanente[c(1:20),],outrosperm)
permanente$VBP=as.numeric(permanente$VBP)
permanente$VBP=round(permanente$VBP,2)
permanente$Percentual_1995=as.numeric(permanente$Percentual_1995)
permanente$Percentual_1995=round(permanente$Percentual_1995,0)
permanente$`Valor da Produção`=as.numeric(permanente$`Valor da Produção`)
permanente$`Valor da Produção`=round(permanente$`Valor da Produção`,2)
permanente$Percentual_2014=as.numeric(permanente$Percentual_2014)
permanente$Percentual_2014=round(permanente$Percentual_2014,0)
colnames(permanente)=c('Produto','VBP','%','Produto','VBP','%')
xtable(permanente)

#Temporárias
temp=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/tempbr-1.csv',
              sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252') 
temp=temp[c(1:567),]
temp=separate(temp,Unidade.da.Federação,c('Codigo','UF'),sep='-')
temp$Vtotal=rowSums(temp[,c(4:34)],na.rm=T)
temp=merge(temp,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)


#1995
temp1995=temp[temp$Ano==1995,]
temp1995matrix=as.matrix(temp1995[,c(4:34)])
temp1995matrix=temp1995matrix*3.771212
temp1995=cbind(temp1995[,c(1:3)],temp1995matrix)
temp1995=aggregate(temp1995[,c(4:34)],by=list(Ano=temp1995$Ano),sum,na.rm=T)
temp1995=as.data.frame(cbind(rep(1995,31),t(temp1995[,c(2:32)])))
Produto=c(rownames(temp1995))
temp1995=cbind(temp1995,Produto)
rownames(temp1995)=NULL
temp1995=temp1995[,c(3,2)]
colnames(temp1995)=c('Produto','Valor da Produção')


#2014
temp2014=temp[temp$Ano==2014,]
temp2014matrix=as.matrix(temp2014[,c(4:34)])
temp2014matrix=temp2014matrix*1
temp2014=cbind(temp2014[,c(1:3)],temp2014matrix)
temp2014=aggregate(temp2014[,c(4:34)],by=list(Ano=temp2014$Ano),sum,na.rm=T)
temp2014=as.data.frame(cbind(rep(2014,31),t(temp2014[,c(2:32)])))
Produto=c(rownames(temp2014))
temp2014=cbind(temp2014,Produto)
rownames(temp2014)=NULL
temp2014=temp2014[,c(3,2)]
colnames(temp2014)=c('Produto','Valor da Produção')

prodnome=c('Abacaxi','Algodão herbáceo','Alho','Amendoim','Arroz','Aveia','Batata doce','Batata inglesa',
           'Cana-de-açúcar','Cebola','Centeio','Cevada','Ervilha','Fava','Feijão','Fumo','Girassol','Juta',
           'Linho','Malva','Mamona','Mandioca','Melancia','Melão','Milho','Rami','Soja','Sorgo','Tomate',
           'Trigo','Triticale')

temporaria1995=cbind(temp1995,prodnome)
temporaria2014=cbind(temp2014,prodnome)

temporaria1995$Percentual_1995=(temporaria1995$`Valor da Produção`/sum(temporaria1995$`Valor da Produção`))*100
temporaria2014$Percentual_2014=(temporaria2014$`Valor da Produção`/sum(temporaria2014$`Valor da Produção`))*100

temporaria1995=temporaria1995[order(-temporaria1995$Percentual_1995),]
temporaria2014=temporaria2014[order(-temporaria2014$Percentual_2014),]

temporaria=cbind(temporaria1995,temporaria2014)
temporaria=temporaria[,c(3,2,4,7,6,8)]
temporaria$`Valor da Produção`=temporaria$`Valor da Produção`/1000000
temporaria$`Valor da Produção.1`=temporaria$`Valor da Produção.1`/1000000
outrostemp=temporaria[c(21:31),]
outrostemp=cbind('Outros',sum(outrostemp$`Valor da Produção`),sum(outrostemp$Percentual_1995),'Outros',
                 sum(outrostemp$`Valor da Produção.1`),sum(outrostemp$Percentual_2014))
colnames(outrostemp)=colnames(temporaria)
temporaria=rbind(temporaria[c(1:20),],outrostemp)
temporaria$`Valor da Produção`=as.numeric(temporaria$`Valor da Produção`)
temporaria$`Valor da Produção`=round(temporaria$`Valor da Produção`,2)
temporaria$Percentual_1995=as.numeric(temporaria$Percentual_1995)
temporaria$Percentual_1995=round(temporaria$Percentual_1995,1)
temporaria$`Valor da Produção.1`=as.numeric(temporaria$`Valor da Produção.1`)
temporaria$`Valor da Produção.1`=round(temporaria$`Valor da Produção.1`,2)
temporaria$Percentual_2014=as.numeric(temporaria$Percentual_2014)
temporaria$Percentual_2014=round(temporaria$Percentual_2014,1)
colnames(temporaria)=c('Produto','VBP','%','Produto','VBP','%')
xtable(temporaria)

#4 Gráfico mostrando a evolução dos valores das culturas temporárias
# e permanentes nas cinco regiões brasileiras 
#Carregar as informações a nível de uf e agregar por região
#Inflação

#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[20]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}
ipca=ipca[,c(1,5)]



#1 Gráfico com linha de tendência mostrando a evolução do valor da produção
# de culuras temporárias e permanentes no BR e a linha de tendência
library(tidyr)
library(ggplot2)
library(ggpmisc)

#carregar os dados a nível de UF
#permanentes
perm=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/permbr-2.csv',
              sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252')
perm=perm[c(1:567),]
perm=separate(perm,Unidade.da.Federação,c('Codigo','UF'),sep='-')
perm$Vtotal=rowSums(perm[,c(4:38)],na.rm=T)
perm=merge(perm,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)
perm$Vtotalreal=perm$Vtotal*perm$V3
perm$regiao[perm$UF==' Acre'|perm$UF==' Amapá'|perm$UF==' Amazonas'|perm$UF==' Pará'|perm$UF==' Rondônia'|perm$UF==' Roraima'|perm$UF==' Tocantins']='Norte'
perm$regiao[perm$UF==' Alagoas'|perm$UF==' Bahia'|perm$UF==' Ceará'|perm$UF==' Maranhão'|perm$UF==' Paraíba'|perm$UF==' Pernambuco'|perm$UF==' Piauí'|perm$UF==' Rio Grande do Norte'|perm$UF==' Sergipe']='Nordeste'
perm$regiao[perm$UF==' Distrito Federal'|perm$UF==' Goiás'|perm$UF==' Mato Grosso'|perm$UF==' Mato Grosso do Sul']='Centro-Oeste'
perm$regiao[perm$UF==' Espírito Santo'|perm$UF==' Minas Gerais'|perm$UF==' Rio de Janeiro'|perm$UF==' São Paulo']='Sudeste'
perm$regiao[perm$UF==' Rio Grande do Sul'|perm$UF==' Santa Catarina'|perm$UF==' Paraná']='Sul'
perm=aggregate(perm[,c(41)],by=list(Ano=perm$Ano,Regiao=perm$regiao),sum,na.rm=T)
colnames(perm)=c('Ano','Região','VBP')
perm$VBP=perm$VBP/1000000
perm=perm[perm$Ano<2015,]

g=ggplot(data=perm, aes(x=Ano, y=VBP, group = Região, colour = Região)) +
geom_line() + geom_point()+theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=0),
axis.text.y = element_text(face="bold", color="black", size=13, angle=0))+
xlab("Ano")+ylab("VBP (bilhões de R$") +theme(axis.text=element_text(size=12),
axis.title=element_text(size=13,face="bold"))+ 
theme(legend.title = element_text(colour="black", size=12, face="bold"))+
theme(legend.text = element_text(colour="black", size = 12))+
theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
theme(legend.position=c(.15, .7)) 
g


#Temporárias
temp=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/tempbr-1.csv',
              sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252') 
temp=temp[c(1:567),]
temp=separate(temp,Unidade.da.Federação,c('Codigo','UF'),sep='-')
temp$Vtotal=rowSums(temp[,c(4:34)],na.rm=T)
temp=merge(temp,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)
temp$Vtotalreal=temp$Vtotal*temp$V3
temp$regiao[temp$UF==' Acre'|temp$UF==' Amapá'|temp$UF==' Amazonas'|temp$UF==' Pará'|temp$UF==' Rondônia'|temp$UF==' Roraima'|temp$UF==' Tocantins']='Norte'
temp$regiao[temp$UF==' Alagoas'|temp$UF==' Bahia'|temp$UF==' Ceará'|temp$UF==' Maranhão'|temp$UF==' Paraíba'|temp$UF==' Pernambuco'|temp$UF==' Piauí'|temp$UF==' Rio Grande do Norte'|temp$UF==' Sergipe']='Nordeste'
temp$regiao[temp$UF==' Distrito Federal'|temp$UF==' Goiás'|temp$UF==' Mato Grosso'|temp$UF==' Mato Grosso do Sul']='Centro-Oeste'
temp$regiao[temp$UF==' Espírito Santo'|temp$UF==' Minas Gerais'|temp$UF==' Rio de Janeiro'|temp$UF==' São Paulo']='Sudeste'
temp$regiao[temp$UF==' Rio Grande do Sul'|temp$UF==' Santa Catarina'|temp$UF==' Paraná']='Sul'
temp=aggregate(temp[,c(37)],by=list(Ano=temp$Ano,Regiao=temp$regiao),sum,na.rm=T)
colnames(temp)=c('Ano','Região','VBP')
temp$VBP=temp$VBP/1000000
temp=temp[temp$Ano<2015,]

h=ggplot(data=temp, aes(x=Ano, y=VBP, group = Região, colour = Região)) +
  geom_line() + geom_point()+theme(axis.text.x = element_text(face="bold", color="black", size=12, angle=0),
  axis.text.y = element_text(face="bold", color="black", size=13, angle=0))+
  xlab("Ano")+ylab("VBP (bilhões de R$") +theme(axis.text=element_text(size=12),
  axis.title=element_text(size=13,face="bold"))+ 
  theme(legend.title = element_text(colour="black", size=12, face="bold"))+
  theme(legend.text = element_text(colour="black", size = 12))+
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  theme(legend.position=c(.15, .7)) 
h

#Dicas ggplot2  -Legenda
#Because group, the variable in the legend, is mapped to the color fill, it is necessary 
#to use scale_fill_xxx, where xxx is a method of mapping each factor level of group to different 
#colors. The default is to use a different hue on the color wheel for each factor level, but it
#is also possible to manually specify the colors for each level.
#Quando uso group
bp + scale_fill_discrete(name="Experimental\nCondition",
                         breaks=c("ctrl", "trt1", "trt2"),
                        labels=c("Control", "Treatment 1", "Treatment 2"))
# Using a manual scale instead of hue
bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                       name="Experimental\nCondition",
                       breaks=c("ctrl", "trt1", "trt2"),
                       labels=c("Control", "Treatment 1", "Treatment 2"))
# Title appearance
bp + theme(legend.title = element_text(colour="blue", size=16, face="bold"))
# Label appearance
bp + theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))
#This changes the order of items to trt1, ctrl, trt2:
bp + scale_fill_discrete(breaks=c("trt1","ctrl","trt2"))
#Reversing the order of items in the legend.To reverse the legend order:
# These two methods are equivalent:
bp + guides(fill = guide_legend(reverse=TRUE))
bp + scale_fill_discrete(guide = guide_legend(reverse=TRUE))
# You can also modify the scale directly:
bp + scale_fill_discrete(breaks = rev(levels(PlantGrowth$group)))
#By default, the legend will not have a box around it. To add a box and modify its properties:
bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#Position legend outside the plotting area (left/right/top/bottom):
bp + theme(legend.position="top")
#It is also possible to position the legend inside the plotting area. Note that the numeric
#position below is relative to the entire area, including titles and labels, not just the 
#plotting area.
# Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
bp + theme(legend.position=c(.5, .5))
    
##########################################################
#5 Particicipação das cincos principai culturas temporárias e permanentes no total 
#do valor da produção da região em 1995 e 2014
#Inflação
library(tidyr)
library(xtable)
#permanentes
perm=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/permbr-2.csv',
              sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252')
perm=perm[c(1:567),]
perm=separate(perm,Unidade.da.Federação,c('Codigo','UF'),sep='-')
perm$Vtotal=rowSums(perm[,c(4:38)],na.rm=T)
perm$regiao[perm$UF==' Acre'|perm$UF==' Amapá'|perm$UF==' Amazonas'|perm$UF==' Pará'|perm$UF==' Rondônia'|perm$UF==' Roraima'|perm$UF==' Tocantins']='Norte'
perm$regiao[perm$UF==' Alagoas'|perm$UF==' Bahia'|perm$UF==' Ceará'|perm$UF==' Maranhão'|perm$UF==' Paraíba'|perm$UF==' Pernambuco'|perm$UF==' Piauí'|perm$UF==' Rio Grande do Norte'|perm$UF==' Sergipe']='Nordeste'
perm$regiao[perm$UF==' Distrito Federal'|perm$UF==' Goiás'|perm$UF==' Mato Grosso'|perm$UF==' Mato Grosso do Sul']='Centro-Oeste'
perm$regiao[perm$UF==' Espírito Santo'|perm$UF==' Minas Gerais'|perm$UF==' Rio de Janeiro'|perm$UF==' São Paulo']='Sudeste'
perm$regiao[perm$UF==' Rio Grande do Sul'|perm$UF==' Santa Catarina'|perm$UF==' Paraná']='Sul'
perm=aggregate(perm[,c(4:38)],by=list(Regiao=perm$regiao,Ano=perm$Ano),sum,na.rm=T)

#Codigo para construir tabelas para os anos de 1995 e 2014
reg=as.data.frame(table(perm$Regiao))
reg=as.character(reg[,c(1)])

lperm2=list()
j=1995

while (j<=2014){
permano=perm[perm$Ano==j,]

lperm=list()

for (i in 1:length(reg)){
regiao=reg[[i]]
regperm=permano[permano$Regiao==regiao,]
regperm=as.data.frame(cbind(rep(regperm$Regiao,35),rep(regperm$Ano,35),t(regperm[,c(3:37)])))
produto=rownames(regperm)
regperm=cbind(regperm,produto)
regperm=regperm[c(1:7,10:35),]
prodnome=c('Abacate','Algodão arbóreo','Azeitona','Banana','Borracha','Cacau','Café','Caqui','Castanha de Caju','Chá da Índia','Coco da baía','Dendê','Erva Mate',
           'Figo','Goiaba','Guaraná','Laranja','Limão','Maçã','Mamão','Manga','Maracujá','Marmelo','Noz',
           'Palmito','Pera','Pêssego','Pimenta do reino','Sisal','Tangerina','Tungue','Urucum','Uva')
regperm=cbind(regperm,prodnome)
colnames(regperm)[3]='Valor'
rownames(regperm)=NULL
regperm$Valor=as.numeric(levels(regperm$Valor)[regperm$Valor])
regperm=regperm[,c(5,3)]
regperm$Participação=regperm$Valor/sum(regperm$Valor)
colnames(regperm)=c('Produto','VBP','Participação')
regperm=regperm[order(-regperm$VBP),]
regperm1=regperm[c(1:5),]
regperm2=regperm[c(6:35),]
regperm2=cbind('Outros',sum(regperm2$VBP,na.rm=T),sum(regperm2$Participação,na.rm=T))
colnames(regperm2)=c('Produto','VBP','Participação')
regperm=as.data.frame(rbind(regperm1,regperm2))
regperm$VBP=as.numeric(regperm$VBP)
regperm$Participação=as.numeric(regperm$Participação)
lperm[[regiao]]=regperm
}
j=j+19
lperm2[[permano$Ano[1]]]=lperm
}

lperm2[[1995]]
lperm2[[2014]]

co=cbind(lperm2[[1995]]$`Centro-Oeste`,lperm2[[2014]]$`Centro-Oeste`)
ne=cbind(lperm2[[1995]]$Nordeste,lperm2[[2014]]$Nordeste)
n=cbind(lperm2[[1995]]$Norte,lperm2[[2014]]$Norte)
se=cbind(lperm2[[1995]]$Sudeste,lperm2[[2014]]$Sudeste)
s=cbind(lperm2[[1995]]$Sul,lperm2[[2014]]$Sul)

permanente=rbind(co,ne,n,se,s)
permanente$VBP=3.77*permanente$VBP
permanente$VBP=permanente$VBP/1000
colnames(permanente)[5]='VBP2'
permanente$VBP2=permanente$VBP2/1000
permanente$Participação=permanente$Participação*100
colnames(permanente)[6]='Part'
permanente$Part=permanente$Part*100

permanente$VBP=round(permanente$VBP,0)
permanente$VBP2=round(permanente$VBP2,0)
permanente$Participação=round(permanente$Participação,0)
permanente$Part=round(permanente$Part,0)
rownames(permanente)=NULL
xtable(permanente)

#Culturas temporárias
#Temporárias
temp=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/VPA nível de UF/tempbr-1.csv',
              sep=';',header=T,skip=3,na.strings = c('-','NA'),fileEncoding = 'WINDOWS-1252') 
temp=temp[c(1:567),]
temp=separate(temp,Unidade.da.Federação,c('Codigo','UF'),sep='-')
temp$Vtotal=rowSums(temp[,c(4:34)],na.rm=T)
temp=merge(temp,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)
temp$Vtotalreal=temp$Vtotal*temp$V3
temp$regiao[temp$UF==' Acre'|temp$UF==' Amapá'|temp$UF==' Amazonas'|temp$UF==' Pará'|temp$UF==' Rondônia'|temp$UF==' Roraima'|temp$UF==' Tocantins']='Norte'
temp$regiao[temp$UF==' Alagoas'|temp$UF==' Bahia'|temp$UF==' Ceará'|temp$UF==' Maranhão'|temp$UF==' Paraíba'|temp$UF==' Pernambuco'|temp$UF==' Piauí'|temp$UF==' Rio Grande do Norte'|temp$UF==' Sergipe']='Nordeste'
temp$regiao[temp$UF==' Distrito Federal'|temp$UF==' Goiás'|temp$UF==' Mato Grosso'|temp$UF==' Mato Grosso do Sul']='Centro-Oeste'
temp$regiao[temp$UF==' Espírito Santo'|temp$UF==' Minas Gerais'|temp$UF==' Rio de Janeiro'|temp$UF==' São Paulo']='Sudeste'
temp$regiao[temp$UF==' Rio Grande do Sul'|temp$UF==' Santa Catarina'|temp$UF==' Paraná']='Sul'
temp=aggregate(temp[,c(4:34)],by=list(Regiao=temp$regiao,Ano=temp$Ano),sum,na.rm=T)

#Codigo para construir tabelas para os anos de 1995 e 2014
reg=as.data.frame(table(temp$Regiao))
reg=as.character(reg[,c(1)])

ltemp2=list()
j=1995

while (j<=2014){
  tempano=temp[temp$Ano==j,]
  
  ltemp=list()
  
  for (i in 1:length(reg)){
    regiao=reg[[i]]
    regtemp=tempano[tempano$Regiao==regiao,]
    regtemp=as.data.frame(cbind(rep(regtemp$Regiao,31),rep(regtemp$Ano,31),t(regtemp[,c(3:33)])))
    produto=rownames(regtemp)
    regtemp=cbind(regtemp,produto)
    prodnome=c('Abacaxi','Algodão herbáceo','Alho','Amendoim','Arroz','Aveia','Batata doce','Batata inglesa',
               'Cana-de-açúcar','Cebola','Centeio','Cevada','Ervilha','Fava','Feijão','Fumo','Girassol','Juta',
               'Linho','Malva','Mamona','Mandioca','Melancia','Melão','Milho','Rami','Soja','Sorgo','Tomate',
               'Trigo','Triticale')
    regtemp=cbind(regtemp,prodnome)
    colnames(regtemp)[3]='Valor'
    rownames(regtemp)=NULL
    regtemp$Valor=as.numeric(levels(regtemp$Valor)[regtemp$Valor])
    regtemp=regtemp[,c(5,3)]
    regtemp$Participação=regtemp$Valor/sum(regtemp$Valor)
    colnames(regtemp)=c('Produto','VBP','Participação')
    regtemp=regtemp[order(-regtemp$VBP),]
    regtemp1=regtemp[c(1:5),]
    regtemp2=regtemp[c(6:31),]
    regtemp2=cbind('Outros',sum(regtemp2$VBP,na.rm=T),sum(regtemp2$Participação,na.rm=T))
    colnames(regtemp2)=c('Produto','VBP','Participação')
    regtemp=as.data.frame(rbind(regtemp1,regtemp2))
    regtemp$VBP=as.numeric(regtemp$VBP)
    regtemp$Participação=as.numeric(regtemp$Participação)
    ltemp[[regiao]]=regtemp
  }
  j=j+19
  ltemp2[[tempano$Ano[1]]]=ltemp
}

ltemp2[[1995]]
ltemp2[[2014]]



co=cbind(ltemp2[[1995]]$`Centro-Oeste`,ltemp2[[2014]]$`Centro-Oeste`)
ne=cbind(ltemp2[[1995]]$Nordeste,ltemp2[[2014]]$Nordeste)
n=cbind(ltemp2[[1995]]$Norte,ltemp2[[2014]]$Norte)
se=cbind(ltemp2[[1995]]$Sudeste,ltemp2[[2014]]$Sudeste)
s=cbind(ltemp2[[1995]]$Sul,ltemp2[[2014]]$Sul)

temporaria=rbind(co,ne,n,se,s)
temporaria$VBP=3.77*temporaria$VBP
temporaria$VBP=temporaria$VBP/1000000
colnames(temporaria)[5]='VBP2'
temporaria$VBP2=temporaria$VBP2/1000000
temporaria$Participação=temporaria$Participação*100
colnames(temporaria)[6]='Part'
temporaria$Part=temporaria$Part*100

temporaria$VBP=round(temporaria$VBP,2)
temporaria$VBP2=round(temporaria$VBP2,2)
temporaria$Participação=round(temporaria$Participação,0)
temporaria$Part=round(temporaria$Part,0)
rownames(temporaria)=NULL
xtable(temporaria)


######################################################################
#####################################################################
#Modelos de dados em painel

###carrega dados climáticos
#carrega dados anuais
load("/home/bmiyamoto/Documentos/Pesquisa/Tese/INMET/Medidas/Arquivos finais/dadosclimaanobr.RData")
dadosclimaanobr=dadosclimaanobr[dadosclimaanobr$Ano>=1995,c(1:7,11)]

#Dados de precipitação e de eventos extremos
library(RMySQL)
prec=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')

#Tabela com dados de precipitacao é precipitacao 4
precipitacao=dbGetQuery(prec,'select *from Precipitacao4')
precipitacao=precipitacao[precipitacao$Ano>=1995,c(2,3,28:32)]
precipitacao=unique(precipitacao)

#Tabela com SPI 1990-2014 é precipitacao 7
spi=dbGetQuery(prec,"select *from Precipitacao7")
#Vou utilizar spi 6 e spi 12
spi=spi[spi$Ano>=1995,c(2:6,10,12)]
#Mapear se pelo menos em um dos meses houve evento extremo de precipitacao, excesso ou seca
#Muita chuva >1.5
#extremamente chuvoso >2
#severamente seco <-1.5
#extremamente seco <-2
#Primeiro vou tentar só 12
spi$mc[spi$spi6>1.5&spi$spi6<2]=1
spi$ec[spi$spi6>=2]=1
spi$sc[spi$spi6<(-1.5)&spi$spi6>-2]=1
spi$es[spi$spi6<(-2)]=1

#severamente seco ou extremamente seco
spi$sec[spi$spi6<(-1.5)]=1
#muito chuvoso ou extremanente chuvoso
spi$chuv[spi$spi6>1.5]=1


spi=aggregate(spi[,c(8:13)],by=list(Codigo=spi$Codigo,Ano=spi$Ano),sum,na.rm=T)

#Classificação climática de koppen
koppen=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Classificação climática de Koppen/classificação climática de koppen por municipio')


#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[20]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}
ipca=ipca[,c(1,5)]



############################################################
#Modelo de dados em painel para culturas permanentes
#Dados de culturas permanentes
permanentes=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/culturas permanentes-1.csv',
                     sep=';',header=F,skip=0,na.strings = c('-','NA'))
permanentes=permanentes[c(5:144694),]
library(tidyr)
permanentes=separate(permanentes,V2,c('Ano','x1','x2'),sep=' ')
permanentes=permanentes[permanentes$Ano>=1995,c(1,2,5)]
colnames(permanentes)=c('Codigo','Ano','VPP')
permanentes=permanentes[permanentes$VPP>1,]
permanentes=permanentes[!is.na(permanentes$VPP),]
permanentes=merge(permanentes,ipca,by.x=c('Ano'),by.y=c('Data'))
permanentes$VPPR=permanentes$VPP*permanentes$V3
permanentes$logvppr=log(permanentes$VPPR)

#Hora de colocar a porra toda em uma tabela só

permanentes=merge(permanentes,dadosclimaanobr,by=c('Codigo','Ano'),all.x=T,all.y=F)
permanentes=merge(permanentes,precipitacao,by=c('Codigo','Ano'),all.x=T,all.y=F)
permanentes=merge(permanentes,spi,by=c('Codigo','Ano'),all.x=T,all.y=F)
permanentes=merge(permanentes,koppen,by.x=c('Codigo'),by.y=c('munic_code'),all.x=T,all.y=F)

#Opção 1 = Unidades da federação
library(plm)
l2=list()
ufref=as.data.frame(table(permanentes$estado))
ufref=ufref[c(1:6,8:27),c(1)]
ufref=as.character(ufref)

for (i in 1:length(ufref)){


l1=list()
ufref2=ufref[i]
uf=permanentes[permanentes$estado==ufref2,]
uf=unique(uf)
uf$prec100=uf$Prectotal/100
uf=uf[,c(1,2,6,11,31,22,23)]
uf=uf[!is.na(uf$Codigo),]
uf=cbind(uf,model.matrix(~uf$Ano-1))
uf=plm.data(uf)
uf=unique(uf)
#vcultperm$Ano=as.numeric(vcultperm$Ano)
Y=as.matrix(uf[,c(3)])
X=as.matrix(uf[,c(6:7,9:27)])


fm=Y~X


###################
#Efeitos fixos e aleatórios não espacial + teste de hausmann
fe=plm(formula=fm,data=uf,model="within")
summary(fe)

re=plm(formula=fm,data=uf,model="random")
summary(re)

haus=phtest(re, fe)

  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[ufref2]]=l1
}

#Opção 2 Grupos climáticos de Koppen
library(plm)
l2=list()
koppenref=as.data.frame(table(permanentes$koppen))
koppenref=koppenref[,c(1)]
koppenref=as.character(koppenref)

for (i in 1:length(koppenref)){
  
  
  l1=list()
  koppen2=koppenref[i]
  uf=permanentes[permanentes$koppen==koppen2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,29,19,21,28)]
  uf=uf[!is.na(uf$Codigo),]
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  #melhor ajuste até o momento
  X=as.matrix(uf[,c(6:7)])
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within",effect = 'twoways')
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  #haus
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[koppen2]]=l1
}

summary(l2$Af[[1]])
summary(l2$Am[[1]])
summary(l2$As[[1]])
summary(l2$Aw[[1]])
summary(l2$BSh[[1]])
summary(l2$Cfa[[1]])
summary(l2$Cfb[[1]])
summary(l2$Cwa[[1]])
summary(l2$Cwb[[1]])


#Opção 3 Grupos climáticos de Koppen - com binárias ao invés de twoways
permanentes2=permanentes
permanentes2=cbind(permanentes2,model.matrix(~permanentes2$Ano-1))
library(plm)
l2=list()
koppenref=as.data.frame(table(permanentes2$koppen))
koppenref=koppenref[,c(1)]
koppenref=as.character(koppenref)

for (i in 1:length(koppenref)){
  
  
  l1=list()
  koppen2=koppenref[i]
  uf=permanentes2[permanentes2$koppen==koppen2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,49,19,21,28:48)]
  uf=uf[!is.na(uf$Codigo),]
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  #melhor ajuste até o momento
  X=as.matrix(uf[,c(6:7,10:28)])
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  #haus
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[koppen2]]=l1
}


summary(l2$Af[[1]])
summary(l2$Am[[1]])
summary(l2$As[[1]])
summary(l2$Aw[[1]])
summary(l2$BSh[[1]])
summary(l2$Cfa[[1]])
summary(l2$Cfb[[1]])
summary(l2$Cwa[[1]])
summary(l2$Cwb[[1]])

#Opção 4 . Grandes regiões brasileiras

library(plm)
l2=list()
regref=as.data.frame(table(permanentes$regiao))
regref=regref[,c(1)]
regref=as.character(regref)

for (i in 1:length(regref)){
  
  
  l1=list()
  regref2=regref[i]
  uf=permanentes[permanentes$regiao==regref2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,31,22,23)]
  uf=uf[!is.na(uf$Codigo),]
  uf=cbind(uf,model.matrix(~uf$Ano-1))
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  X=as.matrix(uf[,c(6:7,9:27)])
  
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  summary(re)
  
  haus=phtest(re, fe)
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[regref2]]=l1
}

library(xtable)

co=summary(l2$`Centro-Oeste`[[1]])
co=as.data.frame(co$coefficients)
co=co[c(1,2),]

ne=summary(l2$Nordeste[[1]])
ne=as.data.frame(ne$coefficients)
ne=ne[c(1,2),]

n=summary(l2$Norte[[1]])
n=as.data.frame(n$coefficients)
n=n[c(1,2),]

se=summary(l2$Sudeste[[1]])
se=as.data.frame(se$coefficients)
se=se[c(1,2),]

s=summary(l2$Sul[[1]])
s=as.data.frame(s$coefficients)
s=s[c(1,2),]

permanente=rbind(co,ne,n,se,s)
permanente$Estimate=round(permanente$Estimate,4)
permanente$`Std. Error`=round(permanente$`Std. Error`,4)
permanente$`t-value`=round(permanente$`t-value`,2)
permanente$`Pr(>|t|)`=round(permanente$`Pr(>|t|)`,2)
colnames(permanente)=c('Estimativa','Erro Padrão','t-valor','Pr(>|t|)')

xtable(permanente)

##############################################################################
############################################################
#Modelo para culturas temporárias
#Dados de culturas temporárias
temporarias=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/culturas temporarias-1.csv',
                     sep=';',header=F,skip=0,na.strings = c('-','NA'))
temporarias=temporarias[c(135:144824),]
library(tidyr)
temporarias=separate(temporarias,V2,c('Ano','x1','x2'),sep=' ')
temporarias=temporarias[temporarias$Ano>=1995,c(1,2,5)]
colnames(temporarias)=c('Codigo','Ano','VPT')
temporarias=temporarias[temporarias$VPT>1,]
temporarias=temporarias[!is.na(temporarias$VPT),]
temporarias=merge(temporarias,ipca,by.x=c('Ano'),by.y=c('Data'))
temporarias$VPTR=temporarias$VPT*temporarias$V3
temporarias$logvptr=log(temporarias$VPTR)

#Hora de colocar a porra toda em uma tabela só

temporarias=merge(temporarias,dadosclimaanobr,by=c('Codigo','Ano'),all.x=T,all.y=F)
temporarias=merge(temporarias,precipitacao,by=c('Codigo','Ano'),all.x=T,all.y=F)
temporarias=merge(temporarias,spi,by=c('Codigo','Ano'),all.x=T,all.y=F)
temporarias=merge(temporarias,koppen,by.x=c('Codigo'),by.y=c('munic_code'),all.x=T,all.y=F)

#Opção 1 = Unidades da federação
library(plm)
l2=list()
ufref=as.data.frame(table(temporarias$estado))
ufref=ufref[c(1:6,8:27),c(1)]
ufref=as.character(ufref)

temporarias2=temporarias
temporarias2=cbind(temporarias2,model.matrix(~temporarias2$Ano-1))

for (i in 1:length(ufref)){
  
  
  l1=list()
  ufref2=ufref[i]
  uf=temporarias2[temporarias2$estado==ufref2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,49,19,21,28:48)]
  uf=uf[!is.na(uf$Codigo),]
 
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  X=as.matrix(uf[,c(6:7,10:28)])
  
  
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[ufref2]]=l1
}

#Opção 2 Grupos climáticos de Koppen
library(plm)
l2=list()
koppenref=as.data.frame(table(temporarias$koppen))
koppenref=koppenref[,c(1)]
koppenref=as.character(koppenref)

for (i in 1:length(koppenref)){
  
  
  l1=list()
  koppen2=koppenref[i]
  uf=temporarias[temporarias$koppen==koppen2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,29,19,21,28)]
  uf=uf[!is.na(uf$Codigo),]
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  #melhor ajuste até o momento
  X=as.matrix(uf[,c(6:7)])
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within",effect = 'twoways')
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  #haus
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[koppen2]]=l1
}


summary(l2$Af[[1]])
summary(l2$Am[[1]])
summary(l2$As[[1]])
summary(l2$Aw[[1]])
summary(l2$BSh[[1]])
summary(l2$Cfa[[1]])
summary(l2$Cfb[[1]])
summary(l2$Cwa[[1]])
summary(l2$Cwb[[1]])


#Opção 3 Grupos climáticos de Koppen - com binárias ao invés de twoways
temporarias2=temporarias
temporarias2=cbind(temporarias2,model.matrix(~temporarias2$Ano-1))
library(plm)
l2=list()
koppenref=as.data.frame(table(temporarias2$koppen))
koppenref=koppenref[,c(1)]
koppenref=as.character(koppenref)

for (i in 1:length(koppenref)){
  
  
  l1=list()
  koppen2=koppenref[i]
  uf=temporarias2[temporarias2$koppen==koppen2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,49,19,21,28:48)]
  uf=uf[!is.na(uf$Codigo),]
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  #melhor ajuste até o momento
  X=as.matrix(uf[,c(6:7,10:28)])
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  #haus
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[koppen2]]=l1
}


summary(l2$Af[[1]])
summary(l2$Am[[1]])
summary(l2$As[[1]])
summary(l2$Aw[[1]])
summary(l2$BSh[[1]])
summary(l2$Cfa[[1]])
summary(l2$Cfb[[1]])
summary(l2$Cwa[[1]])
summary(l2$Cwb[[1]])

#Opção 4 . Grandes regiões brasileiras

library(plm)
l2=list()
regref=as.data.frame(table(temporarias$regiao))
regref=regref[,c(1)]
regref=as.character(regref)

for (i in 1:length(regref)){
  
  
  l1=list()
  regref2=regref[i]
  uf=temporarias[temporarias$regiao==regref2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,31,22,23)]
  uf=uf[!is.na(uf$Codigo),]
  uf=cbind(uf,model.matrix(~uf$Ano-1))
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  X=as.matrix(uf[,c(6:7,9:27)])
  
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  summary(re)
  
  haus=phtest(re, fe)
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[regref2]]=l1
}

library(xtable)

co=summary(l2$`Centro-Oeste`[[1]])
co=as.data.frame(co$coefficients)
co=co[c(1,2),]

ne=summary(l2$Nordeste[[1]])
ne=as.data.frame(ne$coefficients)
ne=ne[c(1,2),]

n=summary(l2$Norte[[1]])
n=as.data.frame(n$coefficients)
n=n[c(1,2),]

se=summary(l2$Sudeste[[1]])
se=as.data.frame(se$coefficients)
se=se[c(1,2),]

s=summary(l2$Sul[[1]])
s=as.data.frame(s$coefficients)
s=s[c(1,2),]

temporaria=rbind(co,ne,n,se,s)
temporaria$Estimate=round(temporaria$Estimate,4)
temporaria$`Std. Error`=round(temporaria$`Std. Error`,4)
temporaria$`t-value`=round(temporaria$`t-value`,2)
temporaria$`Pr(>|t|)`=round(temporaria$`Pr(>|t|)`,2)
colnames(temporaria)=c('Estimativa','Erro Padrão','t-valor','Pr(>|t|)')

xtable(temporaria)



###########################################################################################
#########################################################################################
#Modelo com controle espacial
library(maptools)
#Malha de municípios
municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
municipios@data$ordem=1:dim(municipios)[1]

#Criar matrizes espaciais para cada uma das regiões brasileiras
library(spdep)

municipios@data$regiao[municipios@data$uf=='AC'|municipios@data$uf=='AP'|municipios@data$uf=='AM'|municipios@data$uf=='PA'|municipios@data$uf=='RO'|municipios@data$uf=='RR'|municipios@data$uf=='TO']='Norte'
municipios@data$regiao[municipios@data$uf=='AL'|municipios@data$uf=='BA'|municipios@data$uf=='CE'|municipios@data$uf=='MA'|municipios@data$uf=='PB'|municipios@data$uf=='PE'|municipios@data$uf=='PI'|municipios@data$uf=='RN'|municipios@data$uf=='SE']='Nordeste'
municipios@data$regiao[municipios@data$uf=='DF'|municipios@data$uf=='GO'|municipios@data$uf=='MT'|municipios@data$uf=='MS']='Centro-Oeste'
municipios@data$regiao[municipios@data$uf=='ES'|municipios@data$uf=='MG'|municipios@data$uf=='RJ'|municipios@data$uf=='SP']='Sudeste'
municipios@data$regiao[municipios@data$uf=='RS'|municipios@data$uf=='SC'|municipios@data$uf=='PR']='Sul'

#Norte
nortemun=municipios[municipios@data$regiao=='Norte',]
neighborsnorte=poly2nb(nortemun)
coordsnorte=coordinates(nortemun)
plot(neighborsnorte,coordsnorte,col="black")
#####Spatial weight matrix based on contiguity
norteweigthmatrix=nb2listw(neighborsnorte)
summary(norteweigthmatrix)

#Nordeste
nordestemun=municipios[municipios@data$regiao=='Nordeste',]
nordestemun=nordestemun[c(1:1792),]
neighborsnordeste=poly2nb(nordestemun)
coordsnordeste=coordinates(nordestemun)
plot(neighborsnordeste,coordsnordeste,col="black")
#####Spatial weight matrix based on contiguity
nordesteweigthmatrix=nb2listw(neighborsnordeste)
summary(norteweigthmatrix)


#Centro-Oeste
comun=municipios[municipios@data$regiao=='Centro-Oeste',]
comun@data$ordem2=1:dim(comun@data)[1]
comun=comun[c(1:11,13:466),]
neighborsco=poly2nb(comun)
coordsco=coordinates(comun)
plot(neighborsco,coordsco,col="black")
#####Spatial weight matrix based on contiguity
coweigthmatrix=nb2listw(neighborsco)
summary(norteweigthmatrix)

#Sudeste
sudestemun=municipios[municipios@data$regiao=='Sudeste',]
sudestemun@data$ordem2=1:dim(sudestemun@data)[1]
sudestemun=sudestemun[c(1:109,111:1668),]
neighborssudeste=poly2nb(sudestemun)
coordssudeste=coordinates(sudestemun)
plot(neighborssudeste,coordssudeste,col="black")
#####Spatial weight matrix based on contiguity
sudesteweigthmatrix=nb2listw(neighborssudeste)
summary(sudesteweigthmatrix)

#Sul
sulmun=municipios[municipios@data$regiao=='Sul',]





library(plm)
library(splm)


###carrega dados climáticos
#carrega dados anuais
load("/home/bmiyamoto/Documentos/Pesquisa/Tese/INMET/Medidas/Arquivos finais/dadosclimaanobr.RData")
dadosclimaanobr=dadosclimaanobr[dadosclimaanobr$Ano>=1995,c(1:7,11)]

#Dados de precipitação e de eventos extremos
library(RMySQL)
prec=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')

#Tabela com dados de precipitacao é precipitacao 4
precipitacao=dbGetQuery(prec,'select *from Precipitacao4')
precipitacao=precipitacao[precipitacao$Ano>=1995,c(2,3,28:32)]
precipitacao=unique(precipitacao)

#Tabela com SPI 1990-2014 é precipitacao 7
spi=dbGetQuery(prec,"select *from Precipitacao7")
#Vou utilizar spi 6 e spi 12
spi=spi[spi$Ano>=1995,c(2:6,10,12)]
#Mapear se pelo menos em um dos meses houve evento extremo de precipitacao, excesso ou seca
#Muita chuva >1.5
#extremamente chuvoso >2
#severamente seco <-1.5
#extremamente seco <-2
#Primeiro vou tentar só 12
spi$mc[spi$spi6>1.5&spi$spi6<2]=1
spi$ec[spi$spi6>=2]=1
spi$sc[spi$spi6<(-1.5)&spi$spi6>-2]=1
spi$es[spi$spi6<(-2)]=1

#severamente seco ou extremamente seco
spi$sec[spi$spi6<(-1.5)]=1
#muito chuvoso ou extremanente chuvoso
spi$chuv[spi$spi6>1.5]=1


spi=aggregate(spi[,c(8:13)],by=list(Codigo=spi$Codigo,Ano=spi$Ano),sum,na.rm=T)

#Classificação climática de koppen
koppen=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Classificação climática de Koppen/classificação climática de koppen por municipio')


#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[20]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}
ipca=ipca[,c(1,5)]


##############################################################################
############################################################
#Modelo para culturas temporárias
#Dados de culturas temporárias
temporarias=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Regressões-Cap2/Culturas agricolas/culturas temporarias-1.csv',
                     sep=';',header=F,skip=0,na.strings = c('-','NA'))
temporarias=temporarias[c(135:144824),]
library(tidyr)
temporarias=separate(temporarias,V2,c('Ano','x1','x2'),sep=' ')
temporarias=temporarias[temporarias$Ano>=1995,c(1,2,5)]
colnames(temporarias)=c('Codigo','Ano','VPT')
temporarias=temporarias[temporarias$VPT>1,]
temporarias=temporarias[!is.na(temporarias$VPT),]
temporarias=merge(temporarias,ipca,by.x=c('Ano'),by.y=c('Data'))
temporarias$VPTR=temporarias$VPT*temporarias$V3
temporarias$logvptr=log(temporarias$VPTR)

#Hora de colocar a porra toda em uma tabela só

temporarias=merge(temporarias,dadosclimaanobr,by=c('Codigo','Ano'),all.x=T,all.y=F)
temporarias=merge(temporarias,precipitacao,by=c('Codigo','Ano'),all.x=T,all.y=F)
temporarias=merge(temporarias,spi,by=c('Codigo','Ano'),all.x=T,all.y=F)
temporarias=merge(temporarias,koppen,by.x=c('Codigo'),by.y=c('munic_code'),all.x=T,all.y=F)


temporarias=temporarias[temporarias$regiao=='Sul',]

#Função para deixar o painel balanceado
balanced<-function(data, ID, TIME, VARS, required=c("all","shared")) {
  if(is.character(ID)) {
    ID <- match(ID, names(data))
  }
  if(is.character(TIME)) {
    TIME <- match(TIME, names(data))
  }
  if(missing(VARS)) { 
    VARS <- setdiff(1:ncol(data), c(ID,TIME))
  } else if (is.character(VARS)) {
    VARS <- match(VARS, names(data))
  }
  required <- match.arg(required)
  idf <- do.call(interaction, c(data[, ID, drop=FALSE], drop=TRUE))
  timef <- do.call(interaction, c(data[, TIME, drop=FALSE], drop=TRUE))
  complete <- complete.cases(data[, VARS])
  tbl <- table(idf[complete], timef[complete])
  if (required=="all") {
    keep <- which(rowSums(tbl==1)==ncol(tbl))
    idx <- as.numeric(idf) %in% keep
  } else if (required=="shared") {
    keep <- which(colSums(tbl==1)==nrow(tbl))
    idx <- as.numeric(timef) %in% keep
  }
  data[idx, ]
}

#Comando para deixar o painel balanceado
temporarias=balanced(temporarias, "Codigo","Ano")


#Tenho que excluir todos os municípios que não estão presentes no painel de dados
#Atribuindo 1 a variável bin para os elementos que estão presentes em munpanel e spmunicipios2014 simultaneamente
temporarias$Codigo=as.numeric(levels(temporarias$Codigo)[temporarias$Codigo])
munpanel=temporarias[temporarias$Ano==2014,]
sulmun@data$codigo_ibg=as.numeric(levels(sulmun@data$codigo_ibg)[sulmun@data$codigo_ibg])
sulmun$bin=0
for (i in 1:length(sulmun$codigo_ibg)){
  for (j in 1:length(munpanel$Codigo)){
    if (sulmun$codigo_ibg[[i]]==munpanel$Codigo[[j]]){
      sulmun$bin[[i]]=1
    }
    
  }
  
}

#Pegando em spmunicipios2014 apenas os elementos com valor de bin =1  e atribuindo novamente a spmunicipios2014
sulmun=sulmun[sulmun$bin==1,]



neighborssul=poly2nb(sulmun)
coordssul=coordinates(sulmun)
plot(neighborssul,coordssul,col="black")
#####Spatial weight matrix based on contiguity
sulweigthmatrix=nb2listw(neighborssul)
summary(sulweigthmatrix)


temporarias=unique(temporarias)
temporarias$prec100=temporarias$Prectotal/100
temporarias=temporarias[,c(1,2,6,11,31,22,23)]
temporarias=temporarias[!is.na(temporarias$Codigo),]
temporarias=cbind(temporarias,model.matrix(~temporarias$Ano-1))
temporarias=plm.data(temporarias)
temporarias=unique(temporarias)
#vcultperm$Ano=as.numeric(vcultperm$Ano)
Y=as.matrix(temporarias[,c(3)])
X=as.matrix(temporarias[,c(6:7,9:27)])


fm=Y~X

#Modelos espaciais de efeitos fixo e efeitos aleatórios com erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=temporarias,index=NULL,listw=sulweigthmatrix, model="within",lag=F, spatial.error = T)
summary(fe)

re=spgm(formula=fm,data=temporarias,index=NULL,listw=sulweigthmatrix, model="random",lag=T,spatial.error = T)
summary(re)

sphtest(x = re, x2 = fe)


##############################################################
###############################################################
###############################################################
###############################################################
#Pós qualificação


####Para o capítulo 1
#Para o capítulo I. Ao invés de tentar usar e intepretar aqueles gráficos
#de SPI para todas as unidades da federação do Brasil, vou só descrever a base de
#dados gerada e colocar gráfico de SPI quatro municípios do BR para mostrar
#o tipo de dados que a base tem
#Como municípios vou selecionar Campinas-SP, Patos-PB, São Joaquim-SC,
# e Medicilândia-PA

library(RMySQL)
spi=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListFields(spi,'Precipitacao6')

vspi=dbGetQuery(spi,'Select *from Precipitacao6 where mun="MANAUS" OR
                mun="RECIFE" OR mun="SÃO PAULO" OR mun="FLORIANÓPOLIS"')
uf=c('MANAUS','RECIFE','SÃO PAULO','FLORIANÓPOLIS')

for (i in 1:length(uf)){
  k=uf[[i]]
  spi=vspi[vspi$mun==k,]
  
  spi=as.matrix(spi[,c(8:16)])
  
  #2D plot
  spi.breaks <- c(-2.4,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,2.4)
  spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
  dates<- seq(from=1975+1/24, to= 2016,by=1/12)
  filled.contour(dates,seq(0,24,by=3),spi,col=spi.cols(11),xlab="",ylab="Tempo (meses)",cex.lab=1.5,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI",
                 plot.axes = {axis(1,cex.axis=2.2)
                   axis(2,cex.axis=2)})
  title(main=paste('SPI -',k),cex.main=2)
  setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/4-Resultados/Graficos-Mun-SPI')
  #Codigo para salvar o mapa diretamente
  dev.print(pdf, file=paste(k,'.pdf'))
}  

#TENTATIVA 1
#Pós - qualificação. Preciso ver se identifico clusters de vulnerabilidade aos eventos
#extremos com base nos valores de SPI

library(RMySQL)
spi=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListFields(spi,'Precipitacao8')


mannk=dbGetQuery(spi,'Select *from Precipitacao8')

#Converter muitas colunas de caracter para numérico forçando o data frame a virar matriz
mk=data.matrix(mannk[,c(5:32)],rownames.force = NA)
mk=as.data.frame(mk)

mannk=mannk[,c(2:4)]
mannk=cbind(mannk,mk)


#Vamos testar clusterização com duas versões
#Versão que só clusteriza municipios que tiveram teste de mk significativo para os 4 spi
#mannk1=mannk[mannk$mk3Correctedp.value<=0.05&mannk$mk6Correctedp.value<=0.05&
#              mannk$`mk12Corrected p.value`<=0.05&mannk$`mk24Corrected p.value`<=0.05,]

#Versão que tiveram teste sginficativo para pelo menos 1
mk3=mannk[mannk$mk3Correctedp.value<=0.05,c(1:3,6:8,10)]
mk6=mannk[mannk$mk6Correctedp.value<=0.05,c(1:3,13:15,17)]
mk12=mannk[mannk$`mk12Corrected p.value`<=0.05,c(1:3,20:22,24)]
mk24=mannk[mannk$`mk24Corrected p.value`<=0.05,c(1:3,27:29,31)]

mannk2=merge(mk3,mk6,by=c('Codigo','Municipio','UF'),all.x=T,all.y=T)
mannk2=merge(mannk2,mk12,by=c('Codigo','Municipio','UF'),all.x=T,all.y=T)
mannk2=merge(mannk2,mk24,by=c('Codigo','Municipio','UF'),all.x=T,all.y=T)
mannk2=mannk2[!is.na(mannk2$Codigo),]


#E se eu inserir as coordenadas do shape antes de clusterizar?


library(maptools)
library(maps)

municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')

coordenadas=coordinates(municipios)
coordenadas=as.data.frame(cbind(municipios@data,coordenadas))
coordenadas=coordenadas[,c(7:9)]

#coo=coordenadas[c(7,8,9)]

mannk2=merge(mannk2,coordenadas,by.x=c('Codigo'),by.y=c('codigo_ibg'),all.x=T,all.y=F)

cluster=mannk2[,c(7,11,15,19,20,21)]
#cluster=coo[,c(2,3)]


#hclust
library(FactoMineR)
hclustermannk2=HCPC(cluster,metric="euclidean",method="ward",nb.clust = 5)
#View(hclustermannk2$data.clust)
mannk2=cbind(mannk2,hclustermannk2$data.clust)
mannk2=mannk2[,c(1:3,22:28)]


#mannk2=cbind(coordenadas,hclustermannk2$data.clust)

#setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/Clusters Gori')
#write.csv(mannk2,file='Clusterização hierárquica para inclinação de Sen (SPI3, SPI6, SPI12, SPI24.csv',
#          row.names=F)

summary(hclustermannk2$data.clust[hclustermannk2$data.clust$clust==1,])
summary(hclustermannk2$data.clust[hclustermannk2$data.clust$clust==2,])
summary(hclustermannk2$data.clust[hclustermannk2$data.clust$clust==3,])
summary(hclustermannk2$data.clust[hclustermannk2$data.clust$clust==4,])
summary(hclustermannk2$data.clust[hclustermannk2$data.clust$clust==5,])

library(maptools)
library(maps)

municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
municipios@data$ordem=1:dim(municipios)[1]
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')


mkcluster=municipios
#mkcluster@data=merge(mkcluster@data,mannk2,by=c('codigo_ibg'),all.x=T,all.y=T)
mkcluster@data=merge(mkcluster@data,mannk2,by.x=c('codigo_ibg'),by.y=c('Codigo'),all.x=T,all.y=F)
mkcluster@data$cor[mkcluster@data$clust==1]='navy'
mkcluster@data$cor[mkcluster@data$clust==2]='red'
mkcluster@data$cor[mkcluster@data$clust==3]='lightblue'
mkcluster@data$cor[mkcluster@data$clust==4]='darkorange'
mkcluster@data$cor[mkcluster@data$clust==5]='yellow'
#mkcluster@data$cor[mkcluster@data$clust==6]='firebrick'
#mkcluster@data$cor[mkcluster@data$clust==7]='darkmagenta'
#mkcluster@data$cor[mkcluster@data$clust==8]='darkseagreen'
#mkcluster@data$cor[mkcluster@data$clust==9]='mediumpurple2'
mkcluster@data=mkcluster@data[order(mkcluster$ordem),]
plot(mkcluster,border=F,lwd=.1,axes=F,las=2,col=mkcluster@data$cor)
plot(uf,add=TRUE,lwd=1.5)
#map.scale(x=-49.6, y=-31.5,relwidth=0.06,metric=T,ratio=F,cex=1)
#legenda
legenda=as.character(c("1","2","3","4","5"))
cores=as.character(c("navy","red","lightblue","darkorange","yellow"))
legend(x=-72.8,y=-17, legenda, fill=cores, bty="n", cex=0.8,title = 'Clusters')
#setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
#source(compassRose(-44.4,-27.5))
#####################################
#TENTATIVA 2
library(flexclust)
x <- matrix(10*runif(1000), ncol=2)
## maximum distrance of point to cluster center is 3
cl1 <- qtclust(x, radius=3)

## maximum distrance of point to cluster center is 1
## -> more clusters, longer runtime
cl2 <- qtclust(x, radius=1)

opar <- par(c("mfrow","mar"))
par(mfrow=c(2,1), mar=c(2.1,2.1,1,1))

plot(x, col=predict(cl1), xlab="", ylab="")
plot(x, col=predict(cl2), xlab="", ylab="")
par(opar)


#TENTATIVA 3 26/02/2017
# Vou tentar fazer clusterização espacial usando um algoritmo de python
#(max-p-regions) do módulo clusterpy
#Inicialmente preciso gravar os dados de taxa de crescimento de SPI
#em um shapefile com os municípios brasileiros, e depois salvar esse
#novo shapefile para ser importado no python

library(RMySQL)
library(maptools)

spi=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
dbListTables(spi)
dbListFields(spi,'Precipitacao8')

tpi=dbGetQuery(spi,"Select *from Precipitacao8")
tpi=tpi[,c(2,7,14,21,28)]
tpi2=data.matrix(tpi[,c(2:5)],rownames.force = NA)
tpi2=as.data.frame(tpi2)

tpi=tpi[,c(1)]
tpi=cbind(tpi,tpi2)



municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
municipios@data$ordem=1:dim(municipios@data)[1]
municipios$mun=1
municipios=merge(municipios,tpi,by.x=c('codigo_ibg'),by.y=c('tpi'),all.x=T,all.y=F)
municipios@data$populacao=as.numeric(levels(municipios@data$populacao)[municipios@data$populacao])
municipios$mk3Zc[municipios$mk3Zc=='NaN']=''
municipios$mk6Zc[municipios$mk6Zc=='NaN']=''
municipios$mk12Zc[municipios$mk12Zc=='NaN']=''
municipios$mk24Zc[municipios$mk24Zc=='NaN']=''
municipios=municipios[!is.na(municipios@data$mk3Zc),]
municipios=municipios[order(municipios@data$ordem),]
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/Clusters Gori/Tentativa 3 - clusterpy/shape')
#salvar shapefile com modificações
writePolyShape(municipios,fn='spimunicipios')


teste=readShapePoly("/home/bmiyamoto/Documentos/Pesquisa/Tese/Clusters Gori/Tentativa 3 - clusterpy/shapecluster/shapetest.shp")
View(teste@data)

###################################################################
#######################
#Criar banco de dados de produção agrícola da PAM e do IPEADATA
##########PAM

library(tidyr)
library(RMySQL)

pam=dbConnect(MySQL(),user='',password='''',host='',dbname='PAM')

#Produção permanente
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Permanentes')
em=list.files(patter=' ')


for (i in 1:(length(em)-1)){
perm=read.csv(paste("/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Permanentes/",em[[i]],sep=''),
              fileEncoding = 'WINDOWS-1252',sep=';',skip=3,na.strings = c('-','NA'))
perm=perm[c(1:144690),]
perm=separate(perm,X.1,c('Ano','v1','v2'),sep=' ')
perm=perm[,c(1,2,5:8)]

cult=readLines(paste("/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Permanentes/",em[[i]],sep=''),n=2)
cult=cult[2]
cult=as.data.frame(cult)
cult=separate(cult,cult,c('a','b'),sep=';')
cult=as.data.frame(cult$b)

perm$cultura=cult$`cult$b`
perm=perm[,c(1,2,7,3:6)]
colnames(perm)[1]=c('Municipio')

dbWriteTable(pam,'Permanentes',value=perm,append=T)
}

#Por ser UTF8 e não WIN-12-52 vou incluir uva separadamente na tabela permanentes
uva=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Permanentes/culturas permanentes(uva).csv',
             sep=';',skip=3,na.strings = c('-','NA'))
uva=uva[c(1:144690),]
uva=separate(uva,X.1,c('Ano','v1','v2'),sep=' ')
uva=uva[,c(1,2,5:8)]
uva$cultura='uva'
uva=uva[,c(1,2,7,3:6)]
colnames(uva)[1]=c('Municipio')
uva$Quantidade.produzida..Toneladas.[uva$Quantidade.produzida..Toneladas.=='...']=NA
uva$Valor.da.produção[uva$Valor.da.produção=='...']=NA
uva$Área.destinada.à.colheita..Hectares.[uva$Área.destinada.à.colheita..Hectares.=='...']=NA
uva$Área.colhida..Hectares.[uva$Área.colhida..Hectares.=='...']=NA

uva$cultura=as.factor(uva$cultura)
uva$Quantidade.produzida..Toneladas.=as.numeric(levels(uva$Quantidade.produzida..Toneladas.)[uva$Quantidade.produzida..Toneladas.])
uva$Valor.da.produção=as.numeric(levels(uva$Valor.da.produção)[uva$Valor.da.produção])
uva$Área.destinada.à.colheita..Hectares.=as.numeric(levels(uva$Área.destinada.à.colheita..Hectares.)[uva$Área.destinada.à.colheita..Hectares.])
uva$Área.colhida..Hectares.=as.numeric(levels(uva$Área.colhida..Hectares.)[uva$Área.colhida..Hectares.])
dbWriteTable(pam,'Permanentes',value=uva,append=T)



#Produção temporária
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Temporarias/')
em=list.files(patter=' ')


for (i in 1:length(em)){
  temp=read.csv(paste("/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Temporarias/",em[[i]],sep=''),
                fileEncoding = 'WINDOWS-1252',sep=';',skip=3,na.strings = c('-','NA'))
  temp=temp[c(1:144690),]
  temp=separate(temp,X.1,c('Ano','v1','v2'),sep=' ')
  temp=temp[,c(1,2,5:9)]
  
  cult=readLines(paste("/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Temporarias/",em[[i]],sep=''),n=5)
  cult=cult[2]
  cult=as.data.frame(cult)
  cult=separate(cult,cult,c('a','b'),sep=';')
  cult=as.data.frame(cult$b)
  
  temp$cultura=cult$`cult$b`
  temp=temp[,c(1,2,8,3:7)]
  colnames(temp)[1]=c('Municipio')
  
  dbWriteTable(pam,'Temporarias',value=temp,append=T)
}

####IPEADATA
library(RMySQL)
library(tidyr)

con=dbConnect(MySQL(),user='',password='''',host='',dbname='IPEA')

setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/IPEADATA')
ipea=list.files(pattern='')

for (i in 28:length(ipea)){
ipeadata=read.csv(paste("/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/IPEADATA/",ipea[[i]],sep=''),
                  skip=1,sep=';')

lipeadata=list()
nome=as.data.frame(readLines(paste("/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/IPEADATA/",ipea[[i]],sep=''),n=1))
colnames(nome)='nome'
nome=separate(nome,nome,c('Variável','Produto'),sep='-')
ano=1973:2010

for (j in 1:dim(ipeadata)[1]){
ipeadata1=as.data.frame(cbind(rep(ipeadata[j,2],38),ano,rep(nome[1,1],38),rep(nome[1,2],38),t(ipeadata[j,c(4:41)])))
colnames(ipeadata1)=c('Municipio','Ano','Variavel','Produto','Valor')
ipeadata1$Municipio=as.numeric(levels(ipeadata1$Municipio)[ipeadata1$Municipio])
ipeadata1$Ano=as.numeric(levels(ipeadata1$Ano)[ipeadata1$Ano])
ipeadata1$Variavel=as.character(ipeadata1$Variavel)
ipeadata1$Produto=as.character(ipeadata1$Produto)
ipeadata1$Valor=as.numeric(levels(ipeadata1$Valor)[ipeadata1$Valor])
dbWriteTable(con,'IPEA',value = ipeadata1,append=T)
}
}

#Aquivos do ipea que não forem inseridos aumaticamente por não terem 1973
#como ano de início 25 26 27
#Não inseri esses arquivos ainda

##############################################################
##############################################################
#Sendo pragmatico, estou desistindo dos clusters espaciais para trabalhar com os biomas
library(maptools)
library(sp)
bioma=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/biomas/bioma.shp')
municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')

##Extraindo as coordenadas dos municípios
muncoord=as.data.frame(coordinates(municipios))
colnames(muncoord)=c('longitude','latitude')
#Convertendo o data frame muncoord em um spatial points data frame
coordinates(muncoord)=~longitude+latitude

#Separando os shapes dos biomas
caatinga=bioma[bioma@data$NOME=='Caatinga',]
cerrado=bioma[bioma@data$NOME=='Cerrado',]
pantanal=bioma[bioma@data$NOME=='Pantanal',]
pampa=bioma[bioma@data$NOME=='Pampa',]
amazonia=bioma[bioma@data$NOME=='Amaz\xf4nia',]
mata_atlantica=bioma[bioma@data$NOME=='Mata Atl\xe2ntica',]

#Sobrepondo os pontos de todos os municípios do Brasil sobre cada um dos shapefiles isolados
#dos biomas. Esse comando over é do pacote sp
ptcaatinga=over(muncoord,caatinga,fn=NULL)
ptcerrado=over(muncoord,cerrado,fn=NULL)
ptpantanal=over(muncoord,pantanal,fn=NULL)
ptpampa=over(muncoord,pampa,fn=NULL)
ptamazonia=over(muncoord,amazonia,fn=NULL)
ptmata_atlantica=over(muncoord,mata_atlantica,fn=NULL)

munbiomas=cbind(municipios@data,ptcaatinga,ptcerrado,ptpantanal,ptpampa,ptamazonia,ptmata_atlantica)
munbiomas=munbiomas[,c(7,2,3,8,11,14,17,20,23)]

#Convertendo colunas de factor para numeric

#> levels(munbiomas$NOME)
#[1] "Amaz\xf4nia"       "Caatinga"          "Cerrado"           "Mata Atl\xe2ntica" "Pampa"            
#[6] "Pantanal"  

munbiomas$NOME=as.numeric(munbiomas$NOME)
munbiomas$NOME.1=as.numeric(munbiomas$NOME.1)
munbiomas$NOME.2=as.numeric(munbiomas$NOME.2)
munbiomas$NOME.3=as.numeric(munbiomas$NOME.3)
munbiomas$NOME.4=as.numeric(munbiomas$NOME.4)
munbiomas$NOME.5=as.numeric(munbiomas$NOME.5)

#Criando a coluna bioma
munbiomas$bioma=rowSums(munbiomas[,c(4:9)],na.rm=T)
munbiomas=munbiomas[,c(1:3,10)]

#33 municípios acabaram não sendo classificados em nenhum bioma.
#Vou fazer isso manualmente
coordsmunsembioma=coordinates(municipios)
coordsmunsembioma=cbind(municipios@data,coordsmunsembioma)
coordsmunsembioma=coordsmunsembioma[,c(7,8,9)]
colnames(coordsmunsembioma)[2:3]=c('longitude','latitude')

sembioma=munbiomas[munbiomas$bioma==0,]

sembioma=merge(sembioma,coordsmunsembioma,by=c('codigo_ibg'),all.x=T,all.y=F)

plot(bioma,axes=F)
points(sembioma$longitude,sembioma$latitude,pch=21,col='darkgreen',bg=adjustcolor("darkgreen",0.5),cex=1.5,lwd=0.9)


#Dos 33 municipios que ficaram sem bioma vou classificar 30 como mata atlantica, um como caatinga(CE)
#e dois como amazonia(MA, PA)

munbiomas$bioma[munbiomas$bioma==0&munbiomas$uf!='PA'&munbiomas$uf!='CE'&
                  munbiomas$uf!='MA']=4
munbiomas$bioma[munbiomas$bioma==0&munbiomas$uf=='CE']=2
munbiomas$bioma[munbiomas$bioma==0&(munbiomas$uf=='MA'|munbiomas$uf=='PA')]=1
munbiomas=munbiomas[,c(1,4)]

setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/Tabela Municipio_Bioma')
write.csv(munbiomas,file='Tabela de classificação de municípios por biomas',row.names = F)

##################################################################
##################################################################
###Vou precisar 
library(RMySQL)
con=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
#Vou usar os SPI's da tabela precipitação 6 porque o período é mais longo
pam=dbConnect(MySQL(),user='',password='''',host='',dbname='PAM')

#Para trazer os valores de 1994 a valores de 2015 multiplica-se por 4.154229

permanentes=dbGetQuery(pam,'select *from Permanentes')
permanentes$cultura=as.factor(permanentes$cultura)
permanentes$numcultura=as.numeric(permanentes$cultura)
cafe=permanentes[permanentes$numcultura==9,]
banana=permanentes[permanentes$numcultura==4,]
laranja=permanentes[permanentes$numcultura==19,]


temporarias=dbGetQuery(pam,'select *from Temporarias')
temporarias$cultura=as.factor(temporarias$cultura)
temporarias$numcultura=as.numeric(temporarias$cultura)
soja=temporarias[temporarias$numcultura==27,]
cana=temporarias[temporarias$numcultura==9,]
milho=temporarias[temporarias$numcultura==25,]


#federação
library(maptools)
library(maps)
municipios=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/municipios_2010/municipios_2010.shp')
municipios@data$ordem=1:dim(municipios)[1]
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/estados_2010/estados_2010.shp')

bioma=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/biomas/bioma.shp')
bioma@data$numbioma=as.numeric(bioma@data$NOME)
bioma@data$cor[bioma@data$numbioma==1]=adjustcolor("forestgreen",0.35)
bioma@data$cor[bioma@data$numbioma==3]=adjustcolor("gold2",0.35)
bioma@data$cor[bioma@data$numbioma==2]=adjustcolor("coral1",0.35)
bioma@data$cor[bioma@data$numbioma==4]=adjustcolor("green3",0.45)
bioma@data$cor[bioma@data$numbioma==5]=adjustcolor("royalblue2",0.35)
bioma@data$cor[bioma@data$numbioma==6]=adjustcolor("navyblue",0.40)

#Café

cafe1=cafe[cafe$Ano==1995,c(2,8)]
cafe1$Valor.da.produção=cafe1$Valor.da.produção*4.154229
cafemun=municipios
coordsmun=as.data.frame(coordinates(cafemun))
cafemun@data=cbind(cafemun@data, coordsmun)
cafemun@data=merge(cafemun@data,cafe1,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
cafemun@data=cafemun@data[order(cafemun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(cafemun,add=T,border=F)
points(cafemun$V1,cafemun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=cafemun@data$Valor.da.produção/35000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.9)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("69", "137","206","275"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(274006/35000/4,274006/35000/2,274006/35000*3/4,274006/35000))
legenda=as.character(c("Amazônia","Cerrado","Caatinga","Mata Atlântica","Pantanal","Pampa"))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)


cafe2=cafe[cafe$Ano==2015,c(2,8)]
cafemun=municipios
coordsmun=as.data.frame(coordinates(cafemun))
cafemun@data=cbind(cafemun@data, coordsmun)
cafemun@data=merge(cafemun@data,cafe2,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
cafemun@data=cafemun@data[order(cafemun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(cafemun,add=T,border=F)
points(cafemun$V1,cafemun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=cafemun@data$Valor.da.produção/35000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("69", "137","206","275"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(274006/35000/4,274006/35000/2,274006/35000*3/4,274006/35000))
legenda=as.character(c("Amazônia","Cerrado","Caatinga","Mata Atlântica","Pantanal","Pampa"))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)



#Banana
banana1=banana[banana$Ano==1995,c(2,8)]
banana1$Valor.da.produção=banana1$Valor.da.produção*4.154229
bananamun=municipios
coordsmun=as.data.frame(coordinates(bananamun))
bananamun@data=cbind(bananamun@data, coordsmun)
bananamun@data=merge(bananamun@data,banana1,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
bananamun@data=bananamun@data[order(bananamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(bananamun,add=T,border=F)
points(bananamun$V1,bananamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=bananamun@data$Valor.da.produção/30000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("59", "118","176","235"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(235000/30000/4,235000/30000/2,235000/30000*3/4,235000/30000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)



banana2=banana[banana$Ano==2015,c(2,8)]
bananamun=municipios
coordsmun=as.data.frame(coordinates(bananamun))
bananamun@data=cbind(bananamun@data, coordsmun)
bananamun@data=merge(bananamun@data,banana2,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
bananamun@data=bananamun@data[order(bananamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(bananamun,add=T,border=F)
points(bananamun$V1,bananamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=bananamun@data$Valor.da.produção/30000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("59", "118","176","235"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(235000/30000/4,235000/30000/2,235000/30000*3/4,235000/30000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)


#Laranja
laranja1=laranja[laranja$Ano==1995,c(2,8)]
laranja1$Valor.da.produção=laranja1$Valor.da.produção*4.154229
laranjamun=municipios
coordsmun=as.data.frame(coordinates(laranjamun))
laranjamun@data=cbind(laranjamun@data, coordsmun)
laranjamun@data=merge(laranjamun@data,laranja1,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
laranjamun@data=laranjamun@data[order(laranjamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(laranjamun,add=T,border=F)
points(laranjamun$V1,laranjamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=laranjamun@data$Valor.da.produção/31000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("60", "120","180","240"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(240000/31000/4,240000/31000/2,240000/31000*3/4,240000/31000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)



laranja2=laranja[laranja$Ano==2015,c(2,8)]
laranjamun=municipios
coordsmun=as.data.frame(coordinates(laranjamun))
laranjamun@data=cbind(laranjamun@data, coordsmun)
laranjamun@data=merge(laranjamun@data,laranja2,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
laranjamun@data=laranjamun@data[order(laranjamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(laranjamun,add=T,border=F)
points(laranjamun$V1,laranjamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=laranjamun@data$Valor.da.produção/31000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=1)
#text(getSpPPolygonsLabptSlots(regiao), labels=regiao@data$sigla, cex=2)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
#Ajustar legenda
legend(c("60", "120","180","240"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(240000/31000/4,240000/31000/2,240000/31000*3/4,240000/31000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)


#Soja
soja1=soja[soja$Ano==1995,c(2,9)]
soja1$Valor.da.produção=soja1$Valor.da.produção*4.154229
sojamun=municipios
coordsmun=as.data.frame(coordinates(sojamun))
sojamun@data=cbind(sojamun@data, coordsmun)
sojamun@data=merge(sojamun@data,soja1,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
sojamun@data=sojamun@data[order(sojamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(sojamun,add=T,border=F)
points(sojamun$V1,sojamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=sojamun@data$Valor.da.produção/190000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("0,43", "0,85","1,28","1,7"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Bilhões R$",
       pt.cex =c(1689638/190000/4,1689638/190000/2,1689638/190000*3/4,1689638/190000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)


soja2=soja[soja$Ano==2015,c(2,9)]
sojamun=municipios
coordsmun=as.data.frame(coordinates(sojamun))
sojamun@data=cbind(sojamun@data, coordsmun)
sojamun@data=merge(sojamun@data,soja2,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
sojamun@data=sojamun@data[order(sojamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(sojamun,add=T,border=F)
points(sojamun$V1,sojamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=sojamun@data$Valor.da.produção/190000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("0,43", "0,85","1,28","1,7"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Bilhões R$",
       pt.cex =c(1689638/190000/4,1689638/190000/2,1689638/190000*3/4,1689638/190000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)

#Cana
cana1=cana[cana$Ano==1995,c(2,9)]
cana1$Valor.da.produção=cana1$Valor.da.produção*4.154229
canamun=municipios
coordsmun=as.data.frame(coordinates(canamun))
canamun@data=cbind(canamun@data, coordsmun)
canamun@data=merge(canamun@data,cana1,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
canamun@data=canamun@data[order(canamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(canamun,add=T,border=F)
points(canamun$V1,canamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=canamun@data$Valor.da.produção/65000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("105", "209","314","418"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(418000/65000/4,418000/65000/2,418000/65000*3/4,418000/65000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)


cana2=cana[cana$Ano==2015,c(2,9)]
canamun=municipios
coordsmun=as.data.frame(coordinates(canamun))
canamun@data=cbind(canamun@data, coordsmun)
canamun@data=merge(canamun@data,cana2,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
canamun@data=canamun@data[order(canamun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(canamun,add=T,border=F)
points(canamun$V1,canamun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=canamun@data$Valor.da.produção/65000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("105", "209","314","418"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(418000/65000/4,418000/65000/2,418000/65000*3/4,418000/65000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)


#Milho
milho1=milho[milho$Ano==1995,c(2,9)]
milho1$Valor.da.produção=milho1$Valor.da.produção*4.154229
milhomun=municipios
coordsmun=as.data.frame(coordinates(milhomun))
milhomun@data=cbind(milhomun@data, coordsmun)
milhomun@data=merge(milhomun@data,milho1,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
milhomun@data=milhomun@data[order(milhomun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(milhomun,add=T,border=F)
points(milhomun$V1,milhomun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=milhomun@data$Valor.da.produção/80000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("156", "312","467","623"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(623000/80000/4,623000/80000/2,623000/80000*3/4,623000/80000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)



milho2=milho[milho$Ano==2015,c(2,9)]
milhomun=municipios
coordsmun=as.data.frame(coordinates(milhomun))
milhomun@data=cbind(milhomun@data, coordsmun)
milhomun@data=merge(milhomun@data,milho2,by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=F)
milhomun@data=milhomun@data[order(milhomun@data$ordem),]
plot(uf,lwd=1)
plot(bioma,add=TRUE,lwd=1,col=bioma@data$cor,border=F)
plot(milhomun,add=T,border=F)
points(milhomun$V1,milhomun$V2,pch=21,col="olivedrab",
       bg=adjustcolor("olivedrab",0.5),cex=milhomun@data$Valor.da.produção/80000,lwd=0.9)
map.scale(x=-49.6, y=-31.5,relwidth=0.08,metric=T,ratio=F,cex=0.8)
#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/Tese/ANA/Dados de pluviosidade Brasil - ANA/3-Dados MySQL')
source(compassRose(-44.4,-27.5))
legend(c("156", "312","467","623"),x=-70.8,y=-12, pt.bg=adjustcolor("olivedrab",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.1,title="Milhões R$",
       pt.cex =c(623000/80000/4,623000/80000/2,623000/80000*3/4,623000/80000))
cores=as.character(c(adjustcolor("forestgreen",0.35),adjustcolor("gold2",0.35),adjustcolor("coral1",0.35),
                     adjustcolor("green3",0.45),adjustcolor("navyblue",0.40),adjustcolor("royalblue2",0.35)))
legend(x=-70.8,y=-21.5, title='Biomas',legenda, fill=cores, bty="n", cex=1.1)

############################Culturas agrícolas
#Vou fazer as regressões para as variáveis físicas
#dos principais produtos agrícolas da permanentes e temporários.
#Vou fazer uma regressão para cada bioma ou bio é binária em um modelo?
#Como vou utilizar os SPI nessas regressões?

amc=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/AMCs/emancipa',
             header=T)
amc=amc[,c(1,2)]


#Codificar bioma/amc
bioma=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/Shapefiles/Tabela Municipio_Bioma/Tabela de classificação de municípios por biomas',
               header=T)
bioma=merge(bioma,amc,by.x=c('codigo_ibg'),by.y=c('UFMUNDV'))
bioma=bioma[,c(2,3)]
bioma=unique(bioma)


library(RMySQL)
#Vou usar os SPI's da tabela precipitação 6 porque o período é mais longo
#Vou tentar com SPI 12 e SPI 6 e usar os que apresentarem melhores resultados. SPI 12 justifico
#o uso pelo fato dos dados serem anuais e SPI 6 pelo fato de organização mundial de
#meteorologia indicar para caracterizar seca agrícola. Outro argumento p o spi12
#é que no caso das culturas permanentes e seme permanetes como a cana, elas ficam
# o ano todo expostas as condições naturais
prec6=dbConnect(MySQL(),user='',password='''',host='',dbname='PLUVIOSIDADE')
spi=dbGetQuery(prec6,'select `Codigo`,`Mes`,`Ano`,`spi6`,`spi12` from Precipitacao6')
spi$Ano=as.numeric(spi$Ano)
spi=spi[spi$Ano>=1990,]
spi=merge(spi,amc,by.x=c('Codigo'),by.y=c('UFMUNDV'))
spi=aggregate(spi[,c(4,5)],by=list(AMC=spi$NEW_CODE_1970_1997,Mes=spi$Mes,Ano=spi$Ano),mean,na.rm=T)
spi=spi[order(spi$AMC,spi$Ano,spi$Mes),]

#Variáveis para SPI 6
#Muito chuvoso
spi$spi6mc=0
spi$spi6mc[spi$spi6>=1.5&spi$spi6<=1.99]=1
#Extremamente chuvoso
spi$spi6ec=0
spi$spi6ec[spi$spi6>1.99]=1
#Muito ou extremanente chuvoso (as duas anteriores podem ser usadas juntas
#mas a próxima tem que ser usada isoladamente)
spi$spi6mec=0
spi$spi6mec[spi$spi6>=1.5]=1

#Severamente seco
spi$spi6ss=0
spi$spi6ss[spi$spi6>=-1.99&spi$spi6<=-1.5]=1
#Extremamente seco
spi$spi6es=0
spi$spi6es[spi$spi6<(-1.99)]=1
#Severa ou extremamente seco (as duas anteriores podem ser usadas juntas
#mas a próxima tem que ser usada isoladamente)
spi$spi6ses=0
spi$spi6ses[spi$spi6<=-1.5]=1


#Variáveis para SPI 12
#Muito chuvoso
spi$spi12mc=0
spi$spi12mc[spi$spi12>=1.5&spi$spi12<=1.99]=1
#Extremamente chuvoso
spi$spi12ec=0
spi$spi12ec[spi$spi12>1.99]=1
#Muito ou extremanente chuvoso (as duas anteriores podem ser usadas juntas
#mas a próxima tem que ser usada isoladamente)
spi$spi12mec=0
spi$spi12mec[spi$spi12>=1.5]=1

#Severamente seco
spi$spi12ss=0
spi$spi12ss[spi$spi12>=-1.99&spi$spi12<=-1.5]=1
#Extremamente seco
spi$spi12es=0
spi$spi12es[spi$spi12<(-1.99)]=1
#Severa ou extremamente seco (as duas anteriores podem ser usadas juntas
#mas a próxima tem que ser usada isoladamente)
spi$spi12ses=0
spi$spi12ses[spi$spi12<=-1.5]=1

#Agora eu tenho o número de eventos extremos por ano (muito chuvoso, extremamente chuvoso,
#muito ou extreamamente chuvos,severamente seco, extremamente seco, severa ou
#extremamente seco) e por spi (spi6 e spi 12). Lembrando que os dados não estão mais na escala municipal
#mas por AMC
spi=aggregate(spi[,c(6:17)],by=list(AMC=spi$AMC,Ano=spi$Ano),sum,na.rm=T)

#########################################################
###################Vou carregar os dados da PAM e colocar também na escala de AMCs
pam=dbConnect(MySQL(),user='',password='''',host='',dbname='PAM')

#Culturas permanentes
perm=dbGetQuery(pam,'select *from Permanentes')
perm$Municipio=as.numeric(perm$Municipio)
perm=merge(perm,amc,by.x=c('Municipio'),by.y=c('UFMUNDV'))
perm$cultura=as.factor(perm$cultura)
perm$numcultura=as.numeric(perm$cultura)
#Café, banana, laranja (àrea e produção)
perm=perm[perm$numcultura==9|perm$numcultura==4|perm$numcultura==19,]
perm=aggregate(perm[,c(5:7)],by=list(AMC=perm$NEW_CODE_1970_1997,Ano=perm$Ano,Cultura=perm$cultura),
               sum,na.rm=T)
#Os valores iguais a zero resultam da soma de NAs. Tenho que fazê-los voltar a ser NA
perm$Quantidade.produzida..Toneladas.[perm$Quantidade.produzida..Toneladas.==0]=NA
perm$Área.destinada.à.colheita..Hectares.[perm$Área.destinada.à.colheita..Hectares.==0]=NA
perm$Área.colhida..Hectares.[perm$Área.colhida..Hectares.==0]=NA
#spi e bioma
perm$Ano=as.numeric(perm$Ano)
perm=merge(perm,spi,by=c('AMC','Ano'))
perm=merge(perm,bioma,by.x=c('AMC'),by.y=c('NEW_CODE_1970_1997'))
#problema básico é que algumas amcs estão em mais de dois biomas. por isso a tabela
#perm esta com tamanho maior do que o esperado. Vou deixar passar por enquanto
# já que são poucos municípios (não resolvido)

#Culturas temporárias
temporarias=dbGetQuery(pam,'select *from Temporarias')
#Tem um problema na coluna município porque em alguns casos os códigos não foram separados
#do nome do municipio e do estado. então tenho que aplicar novamente
#tidyr e separate novamente para essa coluna
library(tidyr)
temporarias=separate(temporarias,Municipio,c('Municipio','Nome','UF'),sep='-')
temporarias$Municipio=as.numeric(temporarias$Municipio)
temporarias=merge(temporarias,amc,by.x=c('Municipio'),by.y=c('UFMUNDV'))
temporarias$cultura=as.factor(temporarias$cultura)
temporarias$numcultura=as.numeric(temporarias$cultura)
#milho, cana e soja (área e produção)
temporarias=temporarias[temporarias$numcultura==25|temporarias$numcultura==9|
                          temporarias$numcultura==27,]
temporarias=aggregate(temporarias[,c(7:9)],by=list(AMC=temporarias$NEW_CODE_1970_1997,
                          Ano=temporarias$Ano, Cultura=temporarias$cultura),
                          sum,na.rm=T)
#Os valores iguais a zero resultam da soma de NAs. Tenho que fazê-los voltar a ser NA
temporarias$Quantidade.produzida..Toneladas.[temporarias$Quantidade.produzida..Toneladas.==0]=NA
temporarias$Área.plantada..Hectares.[temporarias$Área.plantada..Hectares.==0]=NA
temporarias$Área.colhida..Hectares.[temporarias$Área.colhida..Hectares.==0]=NA
#spi e bioma
temporarias$Ano=as.numeric(temporarias$Ano)
temporarias=merge(temporarias,spi,by=c('AMC','Ano'))
temporarias=merge(temporarias,bioma,by.x=c('AMC'),by.y=c('NEW_CODE_1970_1997'))
#problema básico é que algumas amcs estão em mais de dois biomas. por isso a tabela
#perm esta com tamanho maior do que o esperado. Vou deixar passar por enquanto
# já que são poucos municípios (não resolvido)


#Calcular manualmente o valor do rendimento médio (toneladas por hectare)
#de culturas temporárias e permanentes #dividindo o valor da produção pela área colhida.
perm$rendmedio=perm$Quantidade.produzida..Toneladas./perm$Área.colhida..Hectares.
temporarias$rendmedio=temporarias$Quantidade.produzida..Toneladas./temporarias$Área.colhida..Hectares.

#Salvar as tabelas no banco de dados da pam
dbWriteTable(conn=pam,name='Temporarias_2',value = as.data.frame(temporarias))
dbWriteTable(conn=pam,name='Permanentes_2',value = as.data.frame(perm))

#> levels(munbiomas$NOME)
#[1] "Amaz\xf4nia"       "Caatinga"          "Cerrado"           "Mata Atl\xe2ntica" "Pampa"            
#[6] "Pantanal"  

####################################################################
####################################################################
####################################################################
#Hora de tentar fazer as regressões
library(RMySQL)
library(plm)
library(splm)
library(lmtest)

pam=dbConnect(MySQL(),dbname='PAM',user='',password='''',host='')

temporarias=dbGetQuery(pam,'Select *from Temporarias_2')
permanentes=dbGetQuery(pam,'Select *from Permanentes_2')
colnames(permanentes)[5]='Área.plantada..Hectares.'

culturas=rbind(temporarias,permanentes)
culturas=culturas[,c(2:21)]
culturas$Cultura=as.factor(culturas$Cultura)
culturas$numcult=as.numeric(culturas$Cultura)
culturas$tempo=culturas$Ano

culturas$rendmedio[culturas$numcult==4&culturas$rendmedio>=100&!is.na(culturas$rendmedio)]=culturas$rendmedio/10


############lin -lin
lcult=list()

for (i in 1:6){
  for (j in 1:6){
cult=culturas[culturas$numcult==i&culturas$bioma==j,]
#Aqui coloquei como critério que para rodar a regressão o
#número de NAs tem que ser menor do que o número de registros
if(sum(is.na(cult$rendmedio))<dim(cult)[1])
{
cult=plm.data(cult)

######################################
#Trabalhar de início apenas com SPI6 (resultados não ficaram legais a combinação SPI6 com área plantada)
#Tentar rendimento médio com SPI6 
x=as.matrix(cult[,c(7:8,10:11,22)])
y=as.matrix(cult[,c(20)])
#Vou fazer um looping para obter todas as regressões não espaciais de uma vez só

#Efeitos fixos
fe=plm(formula = y~x,data=cult,model='within')
#Efeitos aleatórios
re=plm(formula = y~x,data=cult,model='random')
#Hausman
haus=phtest(re, fe)

lcult[[paste(i,j,1)]]=summary(fe)
lcult[[paste(i,j,2)]]=summary(re)
lcult[[paste(i,j,3)]]=haus
}
}
}

lcult40=lcult[1:40]
lcult80=lcult[41:80]
lcult105=lcult[81:105]

#log-lin
culturas$logrenmedio=log(culturas$rendmedio)
lcult=list()

for (i in 1:6){
  for (j in 1:6){
    cult=culturas[culturas$numcult==i&culturas$bioma==j,]
    #Aqui coloquei como critério que para rodar a regressão o
    #número de NAs tem que ser menor do que o número de registros
    if(sum(is.na(cult$logrenmedio))<dim(cult)[1])
    {
      cult=plm.data(cult)
      
      ######################################
      #Trabalhar de início apenas com SPI6 (resultados não ficaram legais a combinação SPI6 com área plantada)
      #Tentar log do rendimento médio com SPI6 
      x=as.matrix(cult[,c(7:8,10:11,22)])
      y=as.matrix(cult[,c(23)])
      #Vou fazer um looping para obter todas as regressões não espaciais de uma vez só
      
      #Efeitos fixos
      fe=plm(formula = y~x,data=cult,model='within')
      #Efeitos aleatórios
      re=plm(formula = y~x,data=cult,model='random')
      #Hausman
      haus=phtest(re, fe)
      
      lcult[[paste(i,j,1)]]=summary(fe)
      lcult[[paste(i,j,2)]]=summary(re)
      lcult[[paste(i,j,3)]]=haus
    }
  }
}

lcult40=lcult[1:40]
lcult80=lcult[41:80]
lcult105=lcult[81:105]



##Robusto a heterocedasticidade
#log-lin

culturas$logrenmedio=log(culturas$rendmedio)
lcult=list()

for (i in 1:6){
  for (j in 1:6){
    cult=culturas[culturas$numcult==i&culturas$bioma==j,]
    #Aqui coloquei como critério que para rodar a regressão o
    #número de NAs tem que ser menor do que o número de registros
    if(sum(is.na(cult$logrenmedio))<dim(cult)[1])
    {
      cult=plm.data(cult)
      
      ######################################
      #Trabalhar de início apenas com SPI6 (resultados não ficaram legais a combinação SPI6 com área plantada)
      #Tentar log do rendimento médio com SPI6 
      x=as.matrix(cult[,c(7:8,10:11,22)])
      y=as.matrix(cult[,c(23)])
      #Vou fazer um looping para obter todas as regressões não espaciais de uma vez só
      
      #Efeitos fixos
      fe=plm(formula = y~x,data=cult, model='within')

      #Fazendo só efeitods fixos e robustos a heterocedasticidade, eu acho
      lcult[[paste(i,j,1)]]=summary(fe)
      lcult[[paste(i,j,2)]]=coeftest(fe, vcov.=function(x) vcovHC(x, type="HC1"))
    }
  }
}

lcult35=lcult[1:35]
lcult70=lcult[35:70]


##Robusto a heterocedasticidade
#log-lin com binárias para os anos e tempo contínuo

###Erro: Error in crossprod(t(X), beta) : argumentos não compatíveis
#não consigo ver summary do modelo ajustado.
culturas$Ano=as.character(culturas$Ano)
culturas=cbind(culturas,model.matrix(~culturas$Ano-1))

culturas$logrenmedio=log(culturas$rendmedio)
lcult=list()

for (i in 1:6){
  for (j in 1:6){
    cult=culturas[culturas$numcult==i&culturas$bioma==j,]
    #Aqui coloquei como critério que para rodar a regressão o
    #número de NAs tem que ser menor do que o número de registros
    if(sum(is.na(cult$logrenmedio))<dim(cult)[1])
    {
      cult=plm.data(cult)
      
      ######################################
      #Trabalhar de início apenas com SPI6 (resultados não ficaram legais a combinação SPI6 com área plantada)
      #Tentar log do rendimento médio com SPI6 
      x=as.matrix(cult[,c(7:8,10:11,22,24:48)])
      y=as.matrix(cult[,c(49)])
      #Vou fazer um looping para obter todas as regressões não espaciais de uma vez só
      
      #Efeitos fixos
      fe=plm(formula = y~x,data=cult, model='within')
      
      #Fazendo só efeitods fixos e robustos a heterocedasticidade, eu acho
      lcult[[paste(i,j,1)]]=summary(fe)
      lcult[[paste(i,j,2)]]=coeftest(fe, vcov.=function(x) vcovHC(x, type="HC1"))
    }
  }
}

lcult35=lcult[1:35]
lcult70=lcult[35:70]


temporarias2=cbind(temporarias2,model.matrix(~temporarias2$Ano-1))








########Exemplo
#Opção 1 = Unidades da federação
library(plm)
l2=list()
ufref=as.data.frame(table(temporarias$estado))
ufref=ufref[c(1:6,8:27),c(1)]
ufref=as.character(ufref)

temporarias2=temporarias
temporarias2=cbind(temporarias2,model.matrix(~temporarias2$Ano-1))

for (i in 1:length(ufref)){
  
  
  l1=list()
  ufref2=ufref[i]
  uf=temporarias2[temporarias2$estado==ufref2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,49,19,21,28:48)]
  uf=uf[!is.na(uf$Codigo),]
  
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  X=as.matrix(uf[,c(6:7,10:28)])
  
  
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[ufref2]]=l1
}

#Opção 2 Grupos climáticos de Koppen
library(plm)
l2=list()
koppenref=as.data.frame(table(temporarias$koppen))
koppenref=koppenref[,c(1)]
koppenref=as.character(koppenref)

for (i in 1:length(koppenref)){
  
  
  l1=list()
  koppen2=koppenref[i]
  uf=temporarias[temporarias$koppen==koppen2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,29,19,21,28)]
  uf=uf[!is.na(uf$Codigo),]
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  #melhor ajuste até o momento
  X=as.matrix(uf[,c(6:7)])
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within",effect = 'twoways')
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  #haus
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[koppen2]]=l1
}


summary(l2$Af[[1]])
summary(l2$Am[[1]])
summary(l2$As[[1]])
summary(l2$Aw[[1]])
summary(l2$BSh[[1]])
summary(l2$Cfa[[1]])
summary(l2$Cfb[[1]])
summary(l2$Cwa[[1]])
summary(l2$Cwb[[1]])


#Opção 3 Grupos climáticos de Koppen - com binárias ao invés de twoways
temporarias2=temporarias
temporarias2=cbind(temporarias2,model.matrix(~temporarias2$Ano-1))
library(plm)
l2=list()
koppenref=as.data.frame(table(temporarias2$koppen))
koppenref=koppenref[,c(1)]
koppenref=as.character(koppenref)

for (i in 1:length(koppenref)){
  
  
  l1=list()
  koppen2=koppenref[i]
  uf=temporarias2[temporarias2$koppen==koppen2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,49,19,21,28:48)]
  uf=uf[!is.na(uf$Codigo),]
  
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  #melhor ajuste até o momento
  X=as.matrix(uf[,c(6:7,10:28)])
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  #summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  #summary(re)
  
  haus=phtest(re, fe)
  #haus
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[koppen2]]=l1
}


summary(l2$Af[[1]])
summary(l2$Am[[1]])
summary(l2$As[[1]])
summary(l2$Aw[[1]])
summary(l2$BSh[[1]])
summary(l2$Cfa[[1]])
summary(l2$Cfb[[1]])
summary(l2$Cwa[[1]])
summary(l2$Cwb[[1]])

#Opção 4 . Grandes regiões brasileiras

library(plm)
l2=list()
regref=as.data.frame(table(temporarias$regiao))
regref=regref[,c(1)]
regref=as.character(regref)

for (i in 1:length(regref)){
  
  
  l1=list()
  regref2=regref[i]
  uf=temporarias[temporarias$regiao==regref2,]
  uf=unique(uf)
  uf$prec100=uf$Prectotal/100
  uf=uf[,c(1,2,6,11,31,22,23)]
  uf=uf[!is.na(uf$Codigo),]
  uf=cbind(uf,model.matrix(~uf$Ano-1))
  uf=plm.data(uf)
  uf=unique(uf)
  #vcultperm$Ano=as.numeric(vcultperm$Ano)
  Y=as.matrix(uf[,c(3)])
  X=as.matrix(uf[,c(6:7,9:27)])
  
  
  fm=Y~X
  
  
  ###################
  #Efeitos fixos e aleatórios não espacial + teste de hausmann
  fe=plm(formula=fm,data=uf,model="within")
  summary(fe)
  
  re=plm(formula=fm,data=uf,model="random")
  summary(re)
  
  haus=phtest(re, fe)
  
  l1[[1]]=fe
  l1[[2]]=re
  l1[[3]]=haus
  
  l2[[regref2]]=l1
}

library(xtable)

co=summary(l2$`Centro-Oeste`[[1]])
co=as.data.frame(co$coefficients)
co=co[c(1,2),]

ne=summary(l2$Nordeste[[1]])
ne=as.data.frame(ne$coefficients)
ne=ne[c(1,2),]

n=summary(l2$Norte[[1]])
n=as.data.frame(n$coefficients)
n=n[c(1,2),]

se=summary(l2$Sudeste[[1]])
se=as.data.frame(se$coefficients)
se=se[c(1,2),]

s=summary(l2$Sul[[1]])
s=as.data.frame(s$coefficients)
s=s[c(1,2),]

temporaria=rbind(co,ne,n,se,s)
temporaria$Estimate=round(temporaria$Estimate,4)
temporaria$`Std. Error`=round(temporaria$`Std. Error`,4)
temporaria$`t-value`=round(temporaria$`t-value`,2)
temporaria$`Pr(>|t|)`=round(temporaria$`Pr(>|t|)`,2)
colnames(temporaria)=c('Estimativa','Erro Padrão','t-valor','Pr(>|t|)')

xtable(temporaria)



library(RMySQL)
spi=dbConnect(MySQL(),dbname='PLUVIOSIDADE',user='',password='''',host='')
dbListTables(spi)
dbListFields(spi,'Precipitacao8')

spitable=dbGetQuery(spi,'Select *from Precipitacao8')

spitable$mk24SensSlope=as.numeric(spitable$mk24SensSlope)
spitable$`mk24Corrected p.value`=as.numeric(spitable$`mk24Corrected p.value`)
spi24=spitable[spitable$mk24SensSlope>0.001&spitable$`mk24Corrected p.value`<=0.05,]
View(as.data.frame(table(spi24$UF)))







library(RMySQL)

con=dbConnect(MySQL(), user='', password='''',
              dbname='PAM', host='')

dbListTables(con)

perm=dbGetQuery(con,'select *from Permanentes_2')
perm=perm[perm$Cultura==perm[4,4],]


laranja=read.csv('/home/bmiyamoto/Documentos/Pesquisa/Tese/Dados de Produção/PAM/Permanentes/culturas permanentes(laranja).csv',
                header=T, fileEncoding = 'WINDOWS-1252',sep=';',
                skip=3,na.strings = c('-','NA'))

qt=aggregate(perm[,c(7)], by=list(Ano=perm$Ano),sum, na.rm=T)
