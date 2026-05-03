
# RELATÓRIO DE PÓS-DOC -----------------------------------
# Autor: Daniel Leonel da Rocha
# Título: Democracia mobilizada

# ---------------------------------------------------------
# 1. LIMPAR AMBIENTE --------------------------------------
# ---------------------------------------------------------
rm(list = ls())

# ---------------------------------------------------------
# 2. CARREGAR PACOTES -------------------------------------
# ---------------------------------------------------------
library(tidyverse)  # Manipulação, transformação e visualização de dados (dplyr, ggplot2, etc.)
library(rio)        # Importação de arquivos (ex: .sav, .dta, .csv)
library(survey)     # Modelos com dados amostrais complexos (pesos, svyglm)
library(broom)      # Organização dos resultados dos modelos em formato tidy
library(ggeffects)  # Cálculo de predições marginais (especialmente para interações)
library(car)        # Diagnósticos de modelos (ex: VIF para multicolinearidade)
library(lmtest)     # Testes estatísticos (ex: Breusch-Pagan para heterocedasticidade)

# ---------------------------------------------------------
# 3. IMPORTAR BASE DE DADOS -------------------------------
# ---------------------------------------------------------
redem <- import("~/Assessorias/ReDem - Base/INCT_ReDem.sav")

# ---------------------------------------------------------
# 4. TRATAMENTO DE MISSING --------------------------------
# ---------------------------------------------------------
redem <- redem %>%
  mutate(across(P02A:P22, ~ ifelse(. > 10, NA, .)))

# ---------------------------------------------------------
# 5. RECODIFICAÇÃO DE VARIÁVEIS ---------------------------
# ---------------------------------------------------------
# Vetor com os itens de democracia utilizados no artigo
vars_all <- c("P09","P03",
              "P13","P17","P18","P19",
              "P02A","P05","P06","P08","P10","P11","P12",
              "P15","P20","P21","P22")

redem <- redem %>%
  mutate(
    # Variável principal: engajamento em protesto
    grupo_protesto = case_when(
      P162 == 1 ~ "Já protestou",
      P162 == 2 ~ "Pensou em protestar",
      P162 == 3 ~ "Não protestou"
    ),
    
    grupo_protesto = factor(grupo_protesto,
                            levels = c("Não protestou",
                                       "Pensou em protestar",
                                       "Já protestou")),
    
    # Controles sociodemográficos
    idade = as.numeric(IDADE_EX),
    renda = ifelse(RENDA_1 > 6, NA, 7 - RENDA_1),
    educação = as.numeric(ESCOLARIDADE),
    ideologia = ifelse(P63 > 10, NA, P63),
    sexo = ifelse(SEXO == 1, "Masculino", "Feminino")
  ) %>%
  select(
    # Itens de democracia
    all_of(vars_all),
    
    # Variável principal
    grupo_protesto,
    
    # Controles
    idade, renda, educação, ideologia, sexo,
    
    # Peso amostral
    FATOR_POND
  )

# ---------------------------------------------------------
# 6. DEFINIÇÃO DO DESENHO AMOSTRAL ------------------------
# ---------------------------------------------------------
desenho <- svydesign(
  ids = ~1,
  weights = ~FATOR_POND,
  data = redem
)

# ---------------------------------------------------------
# 7. MODELOS PRINCIPAIS (SEM INTERAÇÃO) -------------------
# ---------------------------------------------------------

modelos <- setNames(
  lapply(vars_all, function(v){
    formula <- as.formula(
      paste(v, "~ grupo_protesto + idade + sexo + educação + renda + ideologia")
    )
    svyglm(formula, design = desenho)
  }),
  vars_all
)

# ---------------------------------------------------------
# 8. EXTRAÇÃO DOS RESULTADOS ------------------------------
# ---------------------------------------------------------

resultados <- lapply(names(modelos), function(v){
  tidy(modelos[[v]]) %>%
    filter(str_detect(term, "grupo_protesto")) %>%
    mutate(item = v)
}) %>%
  bind_rows() %>%
  mutate(
    hipotese = case_when(
      item %in% c("P09","P03") ~ "Participativa",
      item %in% c("P13","P17","P18","P19") ~ "Social",
      item %in% c("P02A","P05","P06","P08","P10","P11","P12") ~ "Liberal",
      item %in% c("P15","P20","P21","P22") ~ "Majoritária-Autoritária"
    ),
    grupo = case_when(
      term == "grupo_protestoPensou em protestar" ~ "Pensou",
      term == "grupo_protestoJá protestou" ~ "Protestou"
    )
  )

# ---------------------------------------------------------
# 9. VISUALIZAÇÃO DOS COEFICIENTES ------------------------
# ---------------------------------------------------------

plot_hipotese <- function(nome_hipotese){
  
  dados <- resultados %>%
    filter(hipotese == nome_hipotese)
  
  ggplot(dados, aes(x = estimate, y = item, shape = grupo)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                       xmax = estimate + 1.96*std.error),
                   height = 0.2,
                   position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Efeito do protesto -", nome_hipotese),
      x = "Coeficiente (ref.: não protestou)",
      y = "Itens"
    ) +
    theme_minimal()
}

# Exportação
g_part <- plot_hipotese("Participativa")
g_soc  <- plot_hipotese("Social")
g_lib  <- plot_hipotese("Liberal")
g_maj  <- plot_hipotese("Majoritária-Autoritária")

ggsave("grafico_hipo_participativa.png", g_part, 10, 6, dpi = 600)
ggsave("grafico_hipo_social.png", g_soc, 10, 6, dpi = 600)
ggsave("grafico_hipo_liberal.png", g_lib, 10, 6, dpi = 600)
ggsave("grafico_hipo_majoritaria.png", g_maj, 10, 6, dpi = 600)

# ---------------------------------------------------------
# 10. MODELOS COM INTERAÇÃO -------------------------------
# ---------------------------------------------------------

modelos_int <- setNames(
  lapply(vars_all, function(v){
    formula <- as.formula(
      paste(v, "~ grupo_protesto * ideologia + idade + sexo + educação + renda")
    )
    svyglm(formula, design = desenho)
  }),
  vars_all
)

# ---------------------------------------------------------
# 11. PREDIÇÕES MARGINAIS ---------------------------------
# ---------------------------------------------------------

resultados_int <- lapply(names(modelos_int), function(v){
  
  pred <- ggpredict(modelos_int[[v]],
                    terms = c("ideologia [0,5,10]", "grupo_protesto"))
  
  as.data.frame(pred) %>%
    mutate(item = v)
  
}) %>%
  bind_rows() %>%
  mutate(
    hipotese = case_when(
      item %in% c("P09","P03") ~ "Participativa",
      item %in% c("P13","P17","P18","P19") ~ "Social",
      item %in% c("P02A","P05","P06","P08","P10","P11","P12") ~ "Liberal",
      item %in% c("P15","P20","P21","P22") ~ "Majoritária-Autoritária"
    ),
    grupo = group,
    ideologia = x
  )

# ---------------------------------------------------------
# 12. VISUALIZAÇÃO DAS INTERAÇÕES -------------------------
# ---------------------------------------------------------

plot_interacao <- function(nome_hipotese){
  
  dados <- resultados_int %>%
    filter(hipotese == nome_hipotese)
  
  ggplot(dados, aes(x = ideologia, y = predicted, color = grupo)) +
    geom_line(size = 1) +
    facet_wrap(~item) +
    labs(
      title = paste("Efeito do protesto por ideologia -", nome_hipotese),
      x = "Ideologia (0 = esquerda, 10 = direita)",
      y = "Valor predito"
    ) +
    theme_bw()
}

# Exportação
g_part <- plot_interacao("Participativa")
g_soc  <- plot_interacao("Social")
g_lib  <- plot_interacao("Liberal")
g_maj  <- plot_interacao("Majoritária-Autoritária")

ggsave("grafico_int_participativa.png", g_part, 10, 6, dpi = 600)
ggsave("grafico_int_social.png", g_soc, 10, 6, dpi = 600)
ggsave("grafico_int_liberal.png", g_lib, 10, 6, dpi = 600)
ggsave("grafico_int_majoritaria.png", g_maj, 10, 6, dpi = 600)

# ---------------------------------------------------------
# 13. DIAGNÓSTICOS ----------------------------------------
# ---------------------------------------------------------

# 13.1 VIF
vif_int <- lapply(vars_all, function(v){
  f <- as.formula(
    paste(v, "~ grupo_protesto * ideologia + idade + sexo + educação + renda")
  )
  m <- lm(f, data = desenho$variables)
  vif(m)
})

# 13.2 Breusch-Pagan
bp_results <- lapply(vars_all, function(v){
  f <- as.formula(
    paste(v, "~ grupo_protesto * ideologia + idade + sexo + educação + renda")
  )
  m <- lm(f, data = desenho$variables)
  bptest(m)
})

# ---------------------------------------------------------
# 14. SUMÁRIO DOS DIAGNÓSTICOS ----------------------------
# ---------------------------------------------------------

bp_table <- data.frame(
  modelo = vars_all,
  p_value = sapply(bp_results, function(x) x$p.value)
)

bp_table
