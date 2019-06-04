library(tidyverse)
library(openxlsx)
library(data.table)
library(genderBR)

#Inicializando functions
source('script/R/functions.R')

#coletando dados
dados_salarios_poa <- data.frame() #Criando data frame
nome_arquivo <- list.files('arquivos/') #Listando todos os arquivos do diretorio
lista_arquivos <- NULL #Lista auxiliar

for (i in 1:length(nome_arquivo)) { #Lendo arquivos e armazenando em uma lista
  lista_arquivos[[i]] <- read.csv2(file = paste0('arquivos/', nome_arquivo[i]),
                                   sep = ';',
                                   stringsAsFactors = FALSE)
  cat(nome_arquivo[i],'\n')
}

for (i in 1:length(lista_arquivos)) { #Unificando todos arquivos em um dataframe
  dados_salarios_poa <- rbind(dados_salarios_poa, lista_arquivos[[i]])    
}

#tratnado os dados
nomes_colunas <- ajustar_nomes(colnames(dados_salarios_poa))
names(dados_salarios_poa) <- nomes_colunas

dados_salarios_poa <- dados_salarios_poa %>% 
  filter(
    remuneracao_apos_deducoes_obrigatorias > 0.00
  ) %>% 
  mutate(
    competencia                            = paste0(substr(competencia, 4, 7), '-', substr(competencia, 1, 2)),
    remuneracao_basica_bruta               = formata_string_para_double(remuneracao_basica_bruta),
    gratificacao_natalina                  = formata_string_para_double(gratificacao_natalina),
    ferias                                 = formata_string_para_double(ferias),
    outras_remuneracoes_eventuais          = formata_string_para_double(outras_remuneracoes_eventuais),
    abate_teto                             = formata_string_para_double(abate_teto),
    imposto_de_renda_retido_na_fonte       = formata_string_para_double(imposto_de_renda_retido_na_fonte),
    previdencia_oficial                    = formata_string_para_double(previdencia_oficial),
    remuneracao_apos_deducoes_obrigatorias = formata_string_para_double(remuneracao_apos_deducoes_obrigatorias),
    demais_deducoes                        = formata_string_para_double(demais_deducoes),
    diarias                                = formata_string_para_double(diarias),
    demais_verbas_indenizatorias           = formata_string_para_double(demais_verbas_indenizatorias),
    grupo_salario                          = case_when(between(remuneracao_apos_deducoes_obrigatorias, 0, 1000) ~ 'R$ 0 - R$ 1.000',
                                                       between(remuneracao_apos_deducoes_obrigatorias, 1001, 3000) ~ 'R$ 1.001 - R$ 3.000',
                                                       between(remuneracao_apos_deducoes_obrigatorias, 3001, 6000) ~ 'R$ 3.001 - R$ 6.000',
                                                       between(remuneracao_apos_deducoes_obrigatorias, 6001, 9000) ~ 'R$ 6.001 - R$ 9.000',
                                                       between(remuneracao_apos_deducoes_obrigatorias, 9001, 12000) ~ 'R$ 9.001 - R$ 12.000',
                                                       between(remuneracao_apos_deducoes_obrigatorias, 12001, 15000) ~ 'R$ 12.001 - R$ 15.000',
                                                       TRUE ~ '> R$ 15.000'),
    genero                                 = get_gender(nome)
  )

#Analisando dados
database_salarios <- dados_salarios_poa %>% 
  group_by(
    competencia,
    genero,
    orgao,
    orgao_de_exercicio,
    tipo_de_folha,
    cargo,
    grupo_salario
  ) %>% 
  summarise(
    salario_bruto   = sum(remuneracao_basica_bruta, na.rm = TRUE),
    salario_liquido = sum(remuneracao_apos_deducoes_obrigatorias, na.rm = TRUE),
    qt_funcionarios = n_distinct(nome)
  ) %>% 
  ungroup()

GG1 <- database_salarios %>% 
  group_by(
    competencia
  ) %>% 
  summarise(
    qt_func = sum(qt_funcionarios, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = competencia, y = qt_func)) +
  geom_bar(stat = 'identity', colour = 'black') +
  geom_label(aes(label = qt_func, fontface = 'bold')) +
  labs(title = 'Total de Funcionários PREF POA')


GG2 <- database_salarios %>% 
  group_by(
    competencia,
    grupo_salario
  ) %>% 
  summarise(
    count = n(),
    salario = sum(salario_liquido)
  ) %>% 
  mutate(
    percent = count/sum(count)
  ) %>% 
  ggplot(aes(x = competencia, y = percent, fill = grupo_salario)) +
  geom_bar(stat = 'identity', colour = 'black') +
  scale_y_continuous(labels=scales::percent) +
  labs(title = 'Faixa Salarial Funcionários PREF POA')

GG3 <- database_salarios %>% 
  group_by(
    competencia,
    orgao,
    grupo_salario
  ) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    percent = count/sum(count)
  ) %>% 
  ggplot(aes(x = competencia, y = percent, fill = grupo_salario)) +
  geom_bar(stat = 'identity', colour = 'black') +
  scale_y_continuous(labels=scales::percent) +
  labs(title = 'Faixa Salarial Funcionários PREF POA') +
  facet_wrap(~ orgao, ncol = 3, scale = 'free_y')


GG4 <- database_salarios %>%
  filter(
    !is.na(genero)
  ) %>% 
  group_by(
    competencia,
    genero
  ) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    percent = count/sum(count)
  ) %>% 
  ggplot(aes(x = competencia, y = percent, fill = genero)) +
  geom_bar(stat = 'identity', colour = 'black') +
  scale_y_continuous(labels=scales::percent) +
  labs(title = 'Distribuição Funcionários PREF POA - Gênero')

GG5 <- database_salarios %>%
  filter(
    !is.na(genero)
  ) %>% 
  group_by(
    competencia,
    genero
  ) %>% 
  summarise(
    count = n(),
    media_salario = mean(salario_liquido, na.rm = TRUE)
  ) %>%  
  ggplot(aes(x = competencia, y = media_salario)) +
  geom_bar(stat = 'identity', colour = 'black') +
  labs(title = 'Média Salarial Funcioários PREF POA - Gênero') +
  facet_wrap(~ genero)

GG6 <-database_salarios %>% 
  filter(
    !is.na(genero)
  ) %>% 
  group_by(
    competencia,
    genero,
    grupo_salario
  ) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    percent = count/sum(count)
  ) %>% 
  ggplot(aes(x = competencia, y = percent, fill = grupo_salario)) +
  geom_bar(stat = 'identity', colour = 'black') +
  scale_y_continuous(labels=scales::percent) +
  labs(title = 'Faixa Salarial Funcionários PREF POA - Gênero') +
  facet_wrap(~ genero)

# dados_salarios_poa %>%
#   filter(
#     !is.na(genero)
#   ) %>%  
#   ggplot(aes(y = remuneracao_apos_deducoes_obrigatorias, x = orgao)) +
#   geom_boxplot()

ggsave(filename = 'arquivos/ggplot1.png',
       plot = GG1,
       width = 11,
       height = 6 )

ggsave(filename = 'arquivos/ggplot2.png',
       plot = GG2,
       width = 11,
       height = 6 )

ggsave(filename = 'arquivos/ggplot3.png',
       plot = GG3,
       width = 35,
       height = 20 )

ggsave(filename = 'arquivos/ggplot4.png',
       plot = GG4,
       width = 11,
       height = 6 )

ggsave(filename = 'arquivos/ggplot5.png',
       plot = GG5,
       width = 20,
       height = 6 )

ggsave(filename = 'arquivos/ggplot6.png',
       plot = GG6,
       width = 25,
       height = 12 )






