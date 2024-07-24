  
  ########################################################
  ################                        ################
  ################      Open Co Case      ################
  ################       24/07/2024       ################
  ################                        ################
  ########################################################
  
  {
    if (!require("pacman")) install.packages("pacman") else library(pacman)
    pacman::p_load(stats, tidyverse, magrittr, 
                   usethis, extrafont, gt, kableExtra, ggiraph, geobr,
                   shiny, bslib, scales, glue)
    
    extrafont::loadfonts(device = "win")
    options(timeout = max(1000, getOption("timeout")))
  }
    
  # -------------------------------------------------------
  
  setwd("E:/OpenCo")
  
  ## Intermediários
  
  color_bg = "#046c3c"
  color_bg2 = alpha("#2dcc4d", 0)
  
  tema <- theme(plot.title = element_text(size = 17, colour = "white", face = "bold", hjust = 0, margin = margin(0,0,15,0)),
                
                text = element_text(color = "white"),
                
                panel.grid = element_blank(),
                panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
                
                plot.background = element_rect(fill = color_bg, color = NA),
                panel.background = element_rect(fill = color_bg, color = NA),
                
                legend.background = element_rect(fill = color_bg, color = NA),
                legend.box.background = element_rect(fill = color_bg),
                
                axis.line = element_line(colour = "white", arrow = grid::arrow(length = unit(0.3, "cm"), type = "closed")))
  
  
  ## Funções Intermediárias
  
  # EDA
  
  eda <- function(df, feature, titulo = "Figura x. Título", plot = TRUE, bad_filter = "bad %in% c(0,1)") {
    
    df %>% 
      filter(eval(parse(text = bad_filter))) %>% 
      summarise(n = n(), .by = feature) %>%
      rename(feature_name = feature) %>%
      arrange(desc(n)) %>% 
      mutate(percent = n / sum(n) * 100,
             cum_percent = cumsum(percent),
             rank = rank(-percent, ties.method = "min"),
             rank8 = ifelse(rank <= 8, 1, 0),
             tooltip = glue('<b> Acumulado: {format(cum_percent, digits = 3)}% </b> \n {feature_name} ({rank}º) \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'), 
             tooltip2 = glue('{feature_name} \n Operações: {n} \n Individual: {format(percent, digits = 2)}%')) %>% 
      
      if(plot) { 
        {ggplot(., aes(x = reorder(feature_name, cum_percent), y = cum_percent, group = 1)) +
            geom_line(size = .75, colour = "#6C0434") +
            geom_point_interactive(aes(tooltip = tooltip, data_id = feature_name), color = "#6C0434", size = 3.5) +
            scale_y_continuous(limits = c(0,100), breaks = seq(25, 100, 25)) +
            labs(title = titulo, x = NULL, y = NULL) +
            tema +
            theme(axis.text.x = element_text(angle = 90),
                  panel.grid.major.y = element_blank(),
                  plot.background = element_rect(fill = "#046c3c"),
                  panel.background = element_rect(fill = "#046c3c"))} %>%
          
          girafe(ggobj = .)
        
      } else .
  }
  
  
  # Bons e Maus Pagadores
  
  bmp_plot <- function(df, feature, plot_title = NULL, plot_subtitle = NULL) {
    
    bind_rows(summarise(df, n = n(), .by = c(feature)),
              summarise(df, n = n(), .by = c(feature, 'bad'))) %>% 
      rename(feature_name = feature) %>%
      arrange(desc(n)) %>% 
      mutate(percent = n / sum(n) * 100,
             cum_percent = cumsum(percent),
             rank = rank(-percent, ties.method = "min"),
             rank8 = ifelse(rank <= 8, 1, 0),
             bad = replace_na(as.character(bad), "Total"),
             tooltip = glue('<b> Acumulado: {format(cum_percent, digits = 3)}% </b> \n {feature_name} ({rank}º) \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'), 
             tooltip2 = glue('{feature_name} \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'), 
             .by = bad) %>%
      filter(bad != "Total") %>% 
      
      {
        {if(feature == 'setor') {ggplot(., aes(x = reorder(abbreviate(feature_name), -n), y = percent))} else {ggplot(., aes(x = reorder(feature_name, -n), y = percent))}} +
          geom_point_interactive(aes(color = bad, shape = bad, size = bad, fill = bad, alpha = bad, tooltip = tooltip2, data_id = bad), show.legend = F) +
          labs(title = plot_title,
               subtitle = plot_subtitle,
               x = "", y = "") +
          {if(feature == 'setor') scale_x_discrete(guide = guide_axis(n.dodge = 2), label = label_wrap(10))} +
          scale_size_manual(values = c(6, 6 , 4)) +
          scale_shape_manual(values = c(23, 23, 21)) +
          scale_color_manual(values = c('#114264', '#aa2e25', '#c90076')) +
          scale_fill_manual(values = c('#447597', '#f77b72', '#c90076')) +
          scale_alpha_manual(values = c(.8,.8,.4)) +
          tema + 
          theme(plot.title = element_text(margin = margin(0,0,5,0)),
                plot.subtitle = element_text(size = 10, color = "white"),
                axis.text.x = element_text(angle = 90),
                legend.background = element_rect(color = "#046c3c"),
                legend.position = "top") + 
          {if(feature == 'setor') theme(axis.text.x = element_text(size = 10, angle = 0))} 
      } %>% 
      
      girafe(ggobj = .)
    
  }
  
  
  
  # Loss Quantile Plot
  
  loss_qtl_plot <- function(df, feature, qtl_filter = "4", 
                            plot_title = NULL, plot_subtitle = NULL) {
    
    df %>% 
      filter(!is.na(loss_qtl)) %>%
      summarise(n = n(), .by = c(feature, 'loss_qtl')) %>% 
      rename(feature_name = feature) %>%
      arrange(desc(n)) %>%
      mutate(loss_qtl = as.character(loss_qtl)) %>% 
      mutate(percent = n / sum(n) * 100,
             cum_percent = cumsum(percent),
             rank = rank(-percent, ties.method = "min"),
             rank8 = ifelse(rank <= 8, 1, 0),
             tooltip = glue('<b> Acumulado: {format(cum_percent, digits = 3)}% </b> \n {feature_name} ({rank}º) \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'), 
             tooltip2 = glue('{feature_name} \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'),
             .by = loss_qtl) %>% 
      filter(loss_qtl == qtl_filter) %>% 
      
      {
        {if(feature == 'setor') {ggplot(., aes(x = reorder(abbreviate(feature_name), -n), y = percent))} else {ggplot(., aes(x = reorder(feature_name, -n), y = percent))}} +
          geom_point_interactive(aes(size = 6, alpha = .8, tooltip = tooltip2, data_id = feature_name), 
                                 color = "#aa2e25", fill = '#f77b72', shape = 23, alpha = 0.8,
                                 show.legend = F) +
          labs(title = plot_title, subtitle = plot_subtitle,
               x = "", y = "") +
          {if(feature == 'setor') scale_x_discrete(guide = guide_axis(n.dodge = 2), label = label_wrap(10))} +
          tema + 
          theme(plot.title = element_text(margin = margin(0,0,15,0)),
                plot.subtitle = element_text(size = 10, color = "white"),
                axis.text.x = element_text(angle = 90),
                legend.background = element_rect(color = "#046c3c")) +
          {if(feature == 'setor') theme(axis.text.x = element_text(size = 10, angle = 0))}
        
      } %>% 
      
      girafe(ggobj = .)
    
  }
  
  
  ## Importação + Tratamento Inicial
  dados = read_csv2('./openco_dataset.csv') %>%
    
          # removendo observações sem nenhum valor
          filter(if_all(everything(), ~ !is.na(.))) %>%
          
          # corrigindo nomes dos setores e criando variáveis 'bad' e 'loss'
          mutate(setor = case_when(setor == "ATACADO" ~ "Atacado",
                                   setor == "VAREJO" ~ "Varejo",
                                   setor == "INDUSTRIA DA CONSTRUCAO" ~ "Indústria da Construção",
                                   setor == "SERVICOS DE ALOJAMENTO/ALIMENTACAO" ~ "Serviços de Alojamento/Alimentação",
                                   setor == "SIDERURGICA-METALURGIA" ~ "Siderúrgica-Metalurgia",
                                   setor == "SERVICOS ADMINISTRATIVOS" ~ "Serviços Administrativos",
                                   setor == "SERVICOS PROFISSIONAIS E TECNICOS" ~ "Serviços Profissionais e Técnicos",
                                   setor == "BENS DE CONSUMO" ~ "Bens de Consumo",
                                   setor == "TRANSPORTE" ~ "Transporte",
                                   setor == "TEXTEIS" ~ "Têxteis",
                                   setor == "SERVICOS DE EDUCACAO" ~ "Serviços de Educação",
                                   setor == "TELECOM" ~ "Telecom",
                                   setor == "BENS DE CAPITAL" ~ "Bens de Capital",
                                   setor == "QUIMICA-PETROQUIMICA" ~ "Química-Petroquímica",
                                   setor == "DIVERSOS" ~ "Diversos",
                                   setor == "SERVICOS DIVERSOS" ~ "Serviços Diversos",
                                   setor == "SERVICOS DE SAUDE" ~ "Serviços de Saúde",
                                   setor == "INDUSTRIA DIGITAL" ~ "Indústria Digital",
                                   setor == "PAPEL E CELULOSE" ~ "Papel e Celulose",
                                   setor == "MINERACAO" ~ "Mineração",
                                   setor == "PRODUTOS DE AGROPECUARIA" ~ "Produtos de Agropecuária",
                                   setor == "SERVICOS DE SANEAMENTO BASICO" ~ "Serviços de Saneamento Básico",
                                   setor == "INDUSTRIA AUTOMOTIVA" ~ "Indústria Automotiva",
                                   setor == "ELETROELETRONICOS" ~ "Eletroeletrônicos",
                                   setor == "ENERGIA" ~ "Energia",
                                   TRUE ~ setor),
                 bad = ifelse(atraso_corrente > 180, 1, 0),
                 loss = ifelse(bad == 1, (valor_em_aberto / valor_contrato_mais_juros) %>% round(2), NA),
                 loss_qtl = cut(x = loss, breaks = quantile(loss, probs = 0:4/4, na.rm = T), include.lowest = TRUE, right = FALSE, labels = FALSE),
                 loss_4qtl = ifelse(loss_qtl != 4 | is.na(loss_qtl), 0, 1),
                 loss_lr = ifelse(is.na(loss), 0, 1))
                  
  
  # Distribuição das Operações por UF e SE

  uf_total <- dados %>% 
                summarise(n = n(), .by = 'estado') %>%
                rename(feature_name = 'estado') %>%
                arrange(desc(n)) %>% 
                mutate(percent = n / sum(n) * 100,
                       cum_percent = cumsum(percent),
                       rank = rank(-percent, ties.method = "min"),
                       rank8 = ifelse(rank <= 8, 1, 0))
  
  se_total <- dados %>% 
                summarise(n = n(), .by = 'setor') %>%
                rename(feature_name = 'setor') %>%
                arrange(desc(n)) %>% 
                mutate(percent = n / sum(n) * 100,
                       cum_percent = cumsum(percent),
                       rank = rank(-percent, ties.method = "min"),
                       rank8 = ifelse(rank <= 8, 1, 0))
  
  
  ## Questão 1
  q1 = dados %>%  
       summarise(ticket_medio = mean(valor_contrato),
                 prazo_medio = weighted.mean(prazo, w = valor_contrato / sum(valor_contrato)),
                 taxa_media = weighted.mean(taxa, w = valor_contrato / sum(valor_contrato)))
  
  
  
  ## Questão 2 e 3
  q23 = list(
    
    `2_uf` = bind_rows(summarise(dados, n = n(), .by = c('estado')),
                       summarise(dados, n = n(), .by = c('estado', 'bad'))) %>% 
             rename(feature_name = 'estado') %>%
             arrange(desc(n)) %>% 
             mutate(percent = n / sum(n) * 100,
                    cum_percent = cumsum(percent),
                    rank = rank(-percent, ties.method = "min"),
                    rank8 = ifelse(rank <= 8, 1, 0),
                    bad = replace_na(as.character(bad), "Total"),
                    .by = bad) %>%
             filter(bad != "Total"),
    
    `2_se` = bind_rows(summarise(dados, n = n(), .by = c('setor')),
                       summarise(dados, n = n(), .by = c('setor', 'bad'))) %>% 
              rename(feature_name = 'setor') %>%
              arrange(desc(n)) %>% 
              mutate(percent = n / sum(n) * 100,
                     cum_percent = cumsum(percent),
                     rank = rank(-percent, ties.method = "min"),
                     rank8 = ifelse(rank <= 8, 1, 0),
                     bad = replace_na(as.character(bad), "Total"),
                     .by = bad) %>%
              filter(bad != "Total"),
    
    `3_uf` =  dados %>% 
                filter(!is.na(loss_qtl)) %>%
                summarise(n = n(), .by = c('estado', 'loss_qtl')) %>% 
                rename(feature_name = 'estado') %>%
                arrange(desc(n)) %>%
                mutate(loss_qtl = as.character(loss_qtl)) %>% 
                mutate(percent = n / sum(n) * 100,
                       cum_percent = cumsum(percent),
                       rank = rank(-percent, ties.method = "min"),
                       rank8 = ifelse(rank <= 8, 1, 0),
                       tooltip = glue('<b> Acumulado: {format(cum_percent, digits = 3)}% </b> \n {feature_name} ({rank}º) \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'), 
                       tooltip2 = glue('{feature_name} \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'),
                       .by = loss_qtl) %>% 
                filter(loss_qtl == 4),
      
    `3_se` =  dados %>% 
                filter(!is.na(loss_qtl)) %>%
                summarise(n = n(), .by = c('setor', 'loss_qtl')) %>% 
                rename(feature_name = 'setor') %>%
                arrange(desc(n)) %>%
                mutate(loss_qtl = as.character(loss_qtl)) %>% 
                mutate(percent = n / sum(n) * 100,
                       cum_percent = cumsum(percent),
                       rank = rank(-percent, ties.method = "min"),
                       rank8 = ifelse(rank <= 8, 1, 0),
                       tooltip = glue('<b> Acumulado: {format(cum_percent, digits = 3)}% </b> \n {feature_name} ({rank}º) \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'), 
                       tooltip2 = glue('{feature_name} \n Operações: {n} \n Individual: {format(percent, digits = 2)}%'),
                       .by = loss_qtl) %>% 
                filter(loss_qtl == 4)
    
  )
  

  
  ## Questão 4
  model <- glm(loss_4qtl ~  estado + setor, 
               family = quasibinomial, data = dados)
  dados$new_score <- predict(model, new_data = dados, type = 'response')
  
  
  
     
  
  