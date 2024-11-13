#Project: analysing data from Brazil's Central Bank IF.Data system
#Source: https://www3.bcb.gov.br/ifdata/#

#Load main packages
library(tidyverse)
library(tidylog)
library(janitor)
library(data.table)
library(beepr)
beep()

#Don't show numbers in scientific notation
options(scipen = 999)

#Load databases
raw.files <- data_frame(filename = list.files('raw/'))

raw.file.paths <- raw.files |>
  mutate(filepath = paste0("raw/", filename))

raw.data <- raw.file.paths |>
  # 'do' the function for each row in turn
  rowwise() |>
  do(.,
     read_csv2(file=.$filepath))

#Create function to add filepath to each file
read.file.add.filename <- function(filepath){
  read_csv2(filepath) |>
    mutate(filepath = filepath)
}

#Create dataframe with all of the files and their filepath
raw.data.with.paths <- raw.file.paths |>
  rowwise() |>
  do(.,
     read.file.add.filename(file=.$filepath))

raw <- raw.data.with.paths |>
  mutate(ano = str_sub(filepath, start = 11, end = 14))

#Remove files we no longer need
rm(list = c("raw.data",
            "raw.data.with.paths",
            "raw.file.paths",
            "raw.files",
            "test",
            "test_year",
            "files"))

#Create a tidy dataframe
df <- raw |>
  mutate(`Conglomerado Financeiro` = as.numeric(`Conglomerado Financeiro`),
         `Conglomerado Financeiro...4` = as.numeric(`Conglomerado Financeiro...4`),
         conglomerado_financeiro_cod = case_when(
    !is.na(`Conglomerado Financeiro`) ~ `Conglomerado Financeiro`,
    !is.na(`Conglomerado Financeiro...4`) ~ `Conglomerado Financeiro...4`),
    `Conglomerado Prudencial` = as.numeric(`Conglomerado Prudencial`),
    `Conglomerado Prudencial...5` = as.numeric(`Conglomerado Prudencial...5`),
    conglomerado_prudencial_cod = case_when(
      !is.na(`Conglomerado Prudencial`) ~ `Conglomerado Prudencial`,
      !is.na(`Conglomerado Prudencial...5`) ~ `Conglomerado Prudencial...5`)) |>
    rename(instituicao = Instituição,
           instituicao_cod = Código,
           conglomerado_financeiro = `Conglomerado Financeiro...3`,
           conglomerado_prudencial = `Conglomerado Prudencial...5`,
           tcb = TCB,
           tc = TC,
           ti = TI,
           cidade = Cidade,
           uf = UF,
           mes_ano = Data,
           ativo_total = `Ativo Total`,
           carteira_credito_classificada = `Carteira de Crédito Classificada`,
           passivo_circulante = `Passivo Circulante e Exigível a Longo Prazo e Resultados de Exercícios Futuros`,
           captacoes = Captações,
           patrimonio_liquido = `Patrimônio Líquido`,
           lucro_liquido = `Lucro Líquido`,
           agencias = `Número de Agências`,
           postos_atendimento = `Número de Postos de Atendimento`) |>
  select(instituicao,
         instituicao_cod,
         conglomerado_financeiro,
         conglomerado_financeiro_cod,
         conglomerado_prudencial,
         conglomerado_prudencial_cod,
         tcb,
         tc,
         ti,
         cidade,
         uf,
         mes_ano,
         ano,
         ativo_total,
         carteira_credito_classificada,
         passivo_circulante,
         captacoes,
         patrimonio_liquido,
         lucro_liquido,
         agencias,
         postos_atendimento,
         filepath) |>
  mutate(proporcao = case_when(
    str_detect(ativo_total, "%") ~ TRUE,
    T ~ FALSE))

#Compare proportion of credit by type of control over time
historico_carteira_pct <- df |>
  filter(proporcao == TRUE &
           (instituicao == "1 - Público" |
              instituicao == "2 - Privado Nacional" |
              instituicao == "3 - Privado com Controle Estrangeiro")) |>
  select(ano, instituicao, carteira_credito_classificada) |>
  pivot_wider(names_from = instituicao,
              values_from = carteira_credito_classificada)

#Compare volume of credit by type of control over time
historico_carteira <- df |>
  filter(proporcao == FALSE &
           (instituicao == "1 - Público" |
              instituicao == "2 - Privado Nacional" |
              instituicao == "3 - Privado com Controle Estrangeiro")) |>
  mutate(banco = case_when(
    instituicao == "1 - Público" ~ "banco_publico",
      instituicao == "2 - Privado Nacional" ~ "banco_privado_nacional",
      instituicao == "3 - Privado com Controle Estrangeiro" ~ "banco_privado_estrangeiro")) |>
  select(ano, banco, carteira_credito_classificada) |>
  pivot_wider(names_from = banco,
              values_from = carteira_credito_classificada) |>
  mutate(data = str_c(ano, "-12-31"),
         data = case_when(
           ano == 2024 ~ "2024-06-30",
           T ~ data),
         data = as.Date(data),
         banco_publico = as.numeric(gsub("\\.", "", banco_publico)),
         banco_privado_nacional = as.numeric(gsub("\\.", "", banco_privado_nacional)),
         banco_privado_estrangeiro = as.numeric(gsub("\\.", "", banco_privado_estrangeiro)),
         bancos_publicos_real = round((deflateBR::deflate(banco_publico, data, "06/2024", index = "ipca")),0),
         bancos_privados_nacionais_real = round((deflateBR::deflate(banco_privado_nacional, data, "06/2024", index = "ipca")),0),
         bancos_privados_estrangeiros_real = round((deflateBR::deflate(banco_privado_estrangeiro, data, "06/2024", index = "ipca")),0),
         bancos_publicos_real = round((bancos_publicos_real*1000),0),
         bancos_privados_nacionais_real = round((bancos_privados_nacionais_real*1000),0),
         bancos_privados_estrangeiros_real = round((bancos_privados_estrangeiros_real*1000),0))

#Save files in CSV
write.csv(historico_carteira, "data/historico_carteira.csv", row.names = F)
write.csv(historico_carteira_pct, "data/historico_carteira_pct.csv", row.names = F)
