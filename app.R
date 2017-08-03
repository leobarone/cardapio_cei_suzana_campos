library(shiny)
library(pdftools)
library(stringr)
library(purrr)
library(dplyr)
library(rvest)
library(lubridate)

vetor_dia_semana <- function(lista_cardapio, dia_semana){
  vetor <- c()
  for (i in lista_cardapio){
    k = NA
    for (j in i){
      if(j[1] == dia_semana) {
        k <- j[2]
      }
    }
    if (!is.na(k)){
      vetor <- c(vetor, k)
    } else {
      vetor <- c(vetor, "Não Disponível")
    }
  }
  return(vetor)
}

url_sopa <-"https://www.dropbox.com/s/pubxuna75aa66ji/sopa.png"
download.file(url_sopa, "sopa.png")


cardapio <- function(){
  
  url_sme <- "http://portal.sme.prefeitura.sp.gov.br//Main/Noticia/Visualizar/PortalSMESP/Cardapios-das-Unidades-com-Gestao-Terceirizada"
  url_cardapio <- url_sme %>%
    read_html() %>%
    html_nodes(xpath = "//span/a") %>%
    html_attr(name = "href")
  
  url_cardapio <- url_cardapio[1]
  
  cardapio_txt_pag <- pdf_text(url_cardapio)
  
  cardapio_txt_linhas <- map(cardapio_txt_pag, function(x) {str_split(x, pattern = "\n")}) %>%
    reduce(c) %>%
    reduce(c) 
  
  dias_refeicao <- str_which(cardapio_txt_linhas, "SEMANA DE")
  dias_refeicao <- c(dias_refeicao, str_which(cardapio_txt_linhas, "Segunda-feira"))
  dias_refeicao <- c(dias_refeicao, str_which(cardapio_txt_linhas, "Terça-feira"))
  dias_refeicao <- c(dias_refeicao, str_which(cardapio_txt_linhas, "Quarta-feira"))
  dias_refeicao <- c(dias_refeicao, str_which(cardapio_txt_linhas, "Quinta-feira"))
  dias_refeicao <- c(dias_refeicao, str_which(cardapio_txt_linhas, "Sexta-feira"))
  dias_refeicao <- dias_refeicao[order(dias_refeicao)]
  
  cardapio_reduzido <- cardapio_txt_linhas[dias_refeicao]
  
  semana <- str_which(cardapio_reduzido, "SEMANA DE")
  semana <- c(semana, length(cardapio_reduzido) + 1)
  
  cardapio_reduzido <- str_replace(cardapio_reduzido, "SEMANA DE ", "")
  cardapio_reduzido <- str_replace(cardapio_reduzido, " a ", ": ")
  
  lista_cardapio <- list()
  
  for (i in 1:(length(semana) - 1)){
    lista_cardapio[[i]] <- cardapio_reduzido[semana[i]:(semana[i + 1] - 1)]
  }
  
  lista_cardapio <- map(lista_cardapio, function(x) {str_split(x, ": ")})
  
  datas_iniciais <- map_chr(lista_cardapio, function(x) {x[[1]][1]})
  datas_iniciais <- str_sub(datas_iniciais, 1, 5)
  datas_iniciais <- paste0(datas_iniciais, "/2017")
  datas_iniciais <- dmy(datas_iniciais)
  datas_iniciais[datas_iniciais == "2017-08-10"] <- "2017-08-07"
  datas_iniciais[datas_iniciais == "2017-08-01"] <- "2017-07-31"
  
  datas_finais <- map_chr(lista_cardapio, function(x) {x[[1]][2]})
  datas_finais <- dmy(datas_finais)
  datas_finais[datas_finais == "2017-08-14"] <- "2017-08-11"
  
  dias <- c("Segunda-feira", "Terça-feira", "Quarta-feira", "Quinta-feira", "Sexta-feira")
  
  lista_cardapio_organizada <- map(dias, function(x) {vetor_dia_semana(lista_cardapio, x)})
  names(lista_cardapio_organizada) <- str_replace(dias, "-","_")
  df <- as.data.frame(lista_cardapio_organizada)
  
  df$data_inicial <- datas_iniciais
  df$data_final <- datas_finais
  
refeicao <- c(rep("Colação", 5), rep("Almoço", 5), rep("Refeição da tarde", 5),
                rep("Colação", 5), rep("Almoço", 5), rep("Refeição da tarde", 5),
                rep("Desjejum", 5), rep("Colação", 5), rep("Almoço", 5), rep("Refeição da tarde", 5),
                rep("Desjejum", 5), rep("Colação", 5), rep("Almoço", 5), rep("Lanche", 5), rep("Refeição da tarde", 5),
                rep("Desjejum", 5), rep("Colação", 5), rep("Almoço", 5), rep("Lanche", 5), rep("Refeição da tarde", 5))
  idade <- c(rep("6 meses", 5), rep("6 meses", 5), rep("6 meses", 5), 
             rep("7 meses", 5), rep("7 meses", 5), rep("7 meses", 5),
             rep("8 a 11 meses", 5), rep("8 a 11 meses", 5), rep("8 a 11 meses", 5), rep("8 a 11 meses", 5),
             rep("1 ano a 1 ano e 11 meses", 5), rep("1 ano a 1 ano e 11 meses", 5), rep("1 ano a 1 ano e 11 meses", 5), rep("1 ano a 1 ano e 11 meses", 5), rep("1 ano a 1 ano e 11 meses", 5),
             rep("2 a 6 anos", 5), rep("2 a 6 anos", 5), rep("2 a 6 anos", 5), rep("2 a 6 anos", 5), rep("2 a 6 anos", 5))  
  
  df$Refeição <- refeicao
  df$idade <- idade
  
  rep(unique(datas_iniciais), 5)
  
  data_inicial_complementar <- rep(unique(datas_iniciais), 5)
  data_inicial_complementar <- data_inicial_complementar[order(data_inicial_complementar)]
  data_final_complementar <- rep(unique(datas_finais), 5)
  data_final_complementar <- data_final_complementar[order(data_final_complementar)]
  
  df_complementar <- data.frame(Segunda_feira = "Mamadeira preparada com Fórmula Láctea Infantil",
                                Terça_feira = "Mamadeira preparada com Fórmula Láctea Infantil",
                                Quarta_feira = "Mamadeira preparada com Fórmula Láctea Infantil",
                                Quinta_feira = "Mamadeira preparada com Fórmula Láctea Infantil",
                                Sexta_feira = "Mamadeira preparada com Fórmula Láctea Infantil",
                                data_inicial = data_inicial_complementar,
                                data_final = data_final_complementar,
                                Refeição = rep(c("Desjejum", "Lanche", "Desjejum", "Lanche", "Lanche"), 5),
                                idade = rep(c("6 meses", "6 meses", "7 meses", "7 meses", "8 a 11 meses"), 5))
  
  df <- rbind(df, df_complementar)
  
  df$semana <- paste0(day(df$data_inicial), "/", month(df$data_inicial), " a ", 
                     day(df$data_final), "/", month(df$data_final))
  
  df$Refeição <- factor(df$Refeição, 
                        levels = c("Desjejum", "Colação", "Almoço", "Lanche", "Refeição da tarde"),
                        ordered = T)
  df$idade <- factor(df$idade, 
                     levels = c( "6 meses", "7 meses", "8 a 11 meses", "1 ano a 1 ano e 11 meses", "2 a 6 anos"),
                     ordered = T)
  
  df <- df[order(df$data_inicial, df$idade, df$Refeição),]
 
  return(df)
}

dados <- cardapio()

# Define UI for application that draws a histogram
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Cardápio CEI Suzana Campos Tauil"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("semana_mes", "Semana do Mês:", 
                  choices = unique(dados$semana)),
      hr(),
      selectInput("dia_semana", "Dia da Semana:", 
            choices = str_replace(colnames(dados[1:5]), "_", "-")),
      hr(),
      selectInput("idade", "Idade da Criança:", 
                  choices = unique(dados$idade))
    ),
    
    # Create a spot for the barplot
    mainPanel(
      tableOutput("tableOutput"),
      hr(),
      hr(),
      hr(),
      hr(),
      imageOutput("preImage")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tableOutput <- renderTable({ 
    dados <- dados[dados$idade == input$idade &
            dados$semana == input$semana_mes, 
          c("Refeição", str_replace(input$dia_semana, "-", "_"))]
    names(dados)[2] <- "Cardápio"
    dados
  })
  
  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "sopa.png"
    
    # Return a list containing the filename and alt text
    list(src = filename)
    
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

