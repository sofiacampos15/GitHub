# Lista 02 ###

# Obs.: os arquivos referentes à Lista 2 encontram-se na pasta 'dados', no menu 
# 'arquivos', na sala 'E04 - 05.out' de nossa disciplina.


# 1 (3 pontos) ----

# Crie um único objeto combinado, contendo informações sobre os tamanhos das bancadas 
# dos partidos (arquivo `bancadas.rds`), 
# suas respectivas coligações eleitorais para 2018 (arquivo `coligacoes.xlsx`) e 
# o grau de concordância com a agenda do Gov Temer (arquivo `governismo_temer.xlsx`). 

#Resposta:

#a) Carregando pacotes

lista.de.pacotes = c("tidyverse","haven","lubridate",
                     "janitor","readxl",
                     "stringr", "magrittr")

novos.pacotes <-
  lista.de.pacotes[!(lista.de.pacotes %in%
                       installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)

rm(lista.de.pacotes, novos.pacotes)

#b) Carregando banco de dados

bancadas <- read_rds("bancadas.rds")
coligacoes <- read_xlsx("coligacoes.xlsx")
governismo_temer <- read_xlsx("governismo_temer.xlsx")


#c) Criando objeto: juntando banco de dados pela chave comum "party" (partido)

objeto <- bancadas %>%
  select(party, size) %>%
  right_join(coligacoes, "party") %>%  #juntando banco de dados "coligacoes"
  right_join(governismo_temer, "party") #juntando banco de dados "governismo_temer"


# 2 (3 pontos) ----

# Com base no objeto criado:

# 2.1 Crie uma variável (uma coluna) unindo as variáveis departido e candidato, 
# chamada 'candidato_sigla', sem excluir as variáveis originais.

#Resposta:

#Criando variavel "coligacao" de 2018 por bancada


objeto <- objeto %>%
  select(party, size, president, partypresid, governismo) %>%
#Unindo duas colunas por meio de um separador
  unite(candidato_sigla, president, partypresid, sep = " , ", remove = T, na.rm = F) 


#juntando as variaveis originais do banco de dados "coligacao" por meio da chave comum "party"
 
objeto <- objeto %>%
  select(party, size, candidato_sigla, governismo) %>%
  right_join(coligacoes, "party")

# 2.2 Crie uma variável categórica, baseada na concordância com o governo Temer,
# indicando se o partido era governo ou oposição a Temer (por favor, explicite os critérios) 
# dica: pesquise sobre o uso da função 'case_when' em conjunto com a função 'mutate'

#Resposta:

#Criando categoria de concordancia com o governo 

#Critérios: 0 - 0,33 > Baixa concordância; 0,34 - 0,67 > Media concordância;
#0,68 - 1: Alta Concordância

#Aredito que com 3 opções para a variavel categorica levando em condiseração 
#a intensidade de concordanco com a agenda do governo temer,
#os resultados demonstram um espectro menos dicotomico, sendo possivel obervar
# diferenças menos polarizadas.


objeto <- objeto %>%
  mutate(concordancia=case_when(governismo <= 0.33 ~ "Baixa",
                                governismo > 0.33 & governismo <= 0.67 ~ "Media",
                                governismo > 0.67 ~ "Alta"))



## 3 (3 pontos) ----

# identifique:

# 3.1 qual candidato tem a COLIGAÇÃO com menor média de concordância
#com a agenda do Governo Temer e 

#Em um primeiro momento, dei a seguinte solução:

#COlando os dados em ordem decrescente 

objeto %>%
  arrange(desc(governismo))

#Pegando o último resultado
#Resposta: PSOL    6    Boulos , PSOL, 0.13 Baixa

#Depois, em debates do grupo, percebi que teria que fazer diferente

#Outra solução:

med_concor <- objeto %>%
  filter(!is.na(president), !is.na(governismo)) %>% #filtrando as NA
  group_by(president) %>%
  #Media das observacoes com valores iguais na variavel "president"
  summarise(med_concor = mean(governismo)) %>%
  arrange(desc(med_concor))


#Reposta: Boulos - 0, 13

# 3.2 qual candidato tem a COLIGAÇÃO com maior proporção total de assentos.

#Novo objeto com a proporcao de assentos

prop_assent <- objeto %>%
  filter(!is.na(president), !is.na(size)) %>% #filtrando NA
  group_by(president) %>% #agrupando as observacoes po president
  summarise(prop = sum(size/513)) #fazendo a proporcao



# 4 (1 ponto) ----

# Documente o processo (no decorrer do script) e esboce, de maneira bastante simples, 
# em separado, seu Fluxo de trabalho, incluindo:
# 1 Coleta
# 2 Análise
# 3 Comunicação