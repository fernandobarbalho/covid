library(dplyr)
library(purrr)
library(stringr)
library(rsiconfi)
library(ggplot2)

hack_datasus <- function(sistema, modalidade, tipo_arquivo, ano, UF, mes){
  
  #Função gera dataframe a partir de ftp feita na página do datasus
  
  #sistema ex:'SIHSUS' Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
  #modalidade  'dados'
  #tipo_arquivo ex: 'RD'#Varia conforme o sistema
  #ano ex: 17 Dois últimos dígitos do ano 
  #UF ex:'AL' Sigla de UF Brasileira
  #mes ex:'12' strings entre 01 e 12
  
  
  dest_file<- paste0(tipo_arquivo,UF,ano,mes,".dbc")
  
  #geral
  #str_download <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/",sistema,"/","200508","_/",modalidade,"/",tipo_arquivo,"/", tipo_arquivo,UF,ano,mes,".dbc")
  
  #específico SIM
  #ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/DOAC2017.dbc
  #ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/DOPAC2016.dbc
  #ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DORES/DOPSP2018.dbc
  str_download<- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/",sistema,"/","CID10/DORES/DO",UF,ano,".dbc")
  download.file(str_download,destfile = dest_file, mode='wb')
  library(read.dbc)
  datasus<- read.dbc(dest_file)
  
  
}





sistema <- 'SIM' #Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
modalidade <- 'dados'
tipo_arquivo<- 'LT'#Varia conforme o sistema
ano <- 2018 #Dois últimos dígitos do ano 
UF <- 'CE' #Siglas das UFs Brasileiras
mes<- '12' #strings entre 01 e 12


ufs<-
municipios_IBGE%>%
  filter(!is.na(cod_uf)) %>%
  distinct(uf) 
  
ano<- 2014:2017


df_obitos_respiratorios_ate_2017<-
map_dfr(ano, function(a_ano){
  print(a_ano)
  map_dfr(ufs$uf, function(a_uf){
    
    print(a_uf)
    #Carrega em df_datasus os dados relativos à base de dados montada a partir dos parãmetros
    df_datasus<- hack_datasus(sistema, modalidade, tipo_arquivo, a_ano, a_uf, mes)
    
    
    df_datasus %>%
      filter(str_sub(CAUSABAS,1,1)=="J",
             str_sub(IDADE,1,1)=="4") %>%
      mutate(Codigo = CAUSABAS ) %>%
      mutate(ano_obito  = a_ano) %>%
      mutate(mes_obito = str_sub(DTOBITO ,3,4)) %>%
      mutate(IDADE = as.numeric( substr(IDADE,2,4))) %>%
      mutate(UF = a_uf) %>%
      mutate(faixa_etaria = case_when(
        IDADE <9 ~"0 a 9",
        IDADE %in% 10:19 ~ "10-19",
        IDADE %in% 20:29 ~ "20-29",
        IDADE %in% 30:39 ~ "30-39",
        IDADE %in% 40:49 ~ "40-49",
        IDADE %in% 50:59 ~ "50-59",
        IDADE %in% 60:69 ~ "60-69",
        IDADE %in% 70:79 ~ "70-79",
        IDADE>80   ~ "80 ou  mais"
        
      )) %>%
      group_by(ano_obito, mes_obito, UF, CODMUNOCOR,  Codigo,faixa_etaria) %>%
      summarise(
        quantidade = n()
      )
    
    
  })
  
  
})  


df_obitos_respiratorios %>%
  group_by(UF, ano_obito) %>%
  summarise(
    sum(quantidade)
  )
  
df_obitos_respiratorios_ate_2017 %>%
  group_by(UF, ano_obito) %>%
  summarise(
    sum(quantidade)
  )


df_obitos_respiratorios_serie <-
  df_obitos_respiratorios %>%
  bind_rows(df_obitos_respiratorios_ate_2017)


library(readxl)
library(viridis)
CID10 <- read_excel("Docs_Tabs_CID10/TABELAS/CID10.xlsx")
CID10<- CID10[,c(1,5)]
names(CID10)<- c("Codigo", "descricao") 


save(list = c("df_obitos_respiratorios_serie","CID10"), file = "DoencasRespiratorias.RData")

df_datasus<- hack_datasus(sistema, modalidade, tipo_arquivo, 2017, "SP", mes)

df_obitos_respiratorios %>%
  inner_join(CID10) %>%
  filter(!is.na(faixa_etaria)) %>%
  ggplot(aes(x= mes_obito, y= quantidade,  fill= faixa_etaria)) +
  geom_col() +
  scale_fill_viridis(discrete=TRUE) +
  
  theme_light() +
  theme(
    strip.text =  element_blank(),
    axis.text.x =  element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )+
  labs(
    title =  "Gráfico da distribuição de óbitos por mês: SP, 2018, doenças respiratórias",

    y = "Total de Óbitos",
    x=  "Mês",
    fill = "Faixa etária"
  )+
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  #facet_grid(mes_obito~.,  space = "free_y") +
  coord_flip()


  
  #Equipamentos do Ceará
  sistema <- 'CNES' #Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
modalidade <- 'dados'
tipo_arquivo<- 'EQ'#Varia conforme o sistema
ano <- 19 #Dois últimos dígitos do ano 
UF <- 'CE' #Siglas das UFs Brasileiras
mes<- '12' #strings entre 01 e 12


#Carrega em df_datasus os dados relativos à base de dados montada a partir dos parãmetros
df_equipamentos_ce<- hack_datasus(sistema, modalidade, tipo_arquivo, ano, UF, mes)


#Profissionais do Ceará
sistema <- 'CNES' #Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
modalidade <- 'dados'
tipo_arquivo<- 'PF'#Varia conforme o sistema
ano <- 19 #Dois últimos dígitos do ano 
UF <- 'CE' #Siglas das UFs Brasileiras
mes<- '12' #strings entre 01 e 12


#Carrega em df_datasus os dados relativos à base de dados montada a partir dos parãmetros
df_profissionais_ce<- hack_datasus(sistema, modalidade, tipo_arquivo, ano, UF, mes)

df_equip_52<-
  df_equipamentos_ce %>%
  filter(CODEQUIP==52) %>%
  group_by(CODUFMUN) %>%
  summarise(
    n()
  )

