library(RSelenium)
library("rvest")
library("tidyverse")

ExtractStaticId <- function(remDr, name, date_flag = FALSE) {
  address <- "ctl00_Contenuto_ucGestioneSondaggio_ucDatiSondaggioReadOnly"
  field <- remDr$findElements("id", 
                              glue::glue("{address}_{name}"))[[1]]$getElementText() |> 
    unlist()
  if (date_flag) {
    field <- as.Date(field, format = "%d/%m/%Y")
  }
  return(field)
}

CalculateIterationsRequired <- function(remDr, table.size) {
  iterations <- remDr$getPageSource()[[1]] |> 
    read_html() |> 
    html_nodes("table.lista") |> 
    {\(x) x[1]}()|> 
    {\(x) x[[1]]}() |> 
    html_nodes("tfoot") |> 
    html_nodes("span") |> 
    {\(x) x[1]}() |> 
    html_text() |> 
    stringr::str_extract("(?<=\\()\\d+") |> 
    as.numeric() |> 
    {\(x) c(rep(table.size, floor(x/table.size)), x%%table.size)}()
  
  if (tail(iterations, 1) == 0) {
    iterations <- head(iterations, -1)
  }
  
  return(iterations)
}

rD <- rsDriver(browser="firefox", port=5858L, verbose=F)
remDr <- rD[["client"]]

lista_sondaggi <- list()

remDr$navigate("http://sondaggipoliticoelettorali.it/ListaSondaggi.aspx?st=SONDAGGI")
remDr$navigate("http://sondaggipoliticoelettorali.it/ListaSondaggi.aspx?st=SONDAGGI")

totale.numero.sondaggi <- CalculateIterationsRequired(remDr, 13)

for (n.pagina in 1:5) {
  numero.sondaggi <- totale.numero.sondaggi[n.pagina]
  for (riga in 1:numero.sondaggi) {
    if (n.pagina > 1) {
      remDr$findElement(using = "id", value = "ctl00_Contenuto_dgSondaggi_VaiAPaginaTextBox")$clearElement()
      remDr$findElement(using = "id", value = "ctl00_Contenuto_dgSondaggi_VaiAPaginaTextBox")$sendKeysToElement(list(as.character(n.pagina)))
      remDr$findElements("id", 
                         "ctl00_Contenuto_dgSondaggi_VaiAPaginaBottone")[[1]]$clickElement()
    }
    remDr$findElements("name", glue::glue("ctl00$Contenuto$dgSondaggi_Row{riga}_Realizzatore"))[[1]]$clickElement()
    
    
    campi <- c("Titolo", "Realizzatore", "Committente", "Mezzo_Comunic_massa",
               "Estensione_Territoriale", "DataRealizzazioneDa", 
               "DataRealizzazioneA", "Data_Pubblicazione", "Metodo_Campionamento",
               "Campione_Intervistati", "Rappresentativa_Campione",
               "Metodo_Raccolta_Informazioni")
    
    date.flag <- c(rep(FALSE, 5), rep(TRUE, 3), rep(FALSE, 4))
    
    sondaggio <- map2(campi, date.flag, ~ExtractStaticId(remDr, .x, .y)) |> 
      set_names(campi)
    
    sondaggio$Domande <- list()
    
    remDr$findElements("name", "ctl00$Titolo$TabSondaggio$DomandeRisposte")[[1]]$clickElement()
    
    totale.numero.domande <- CalculateIterationsRequired(remDr, 5)
    
    for (i in 1:length(totale.numero.domande)) {
      
      numero.domande <- totale.numero.domande[i]
      
      for (j in 1:numero.domande) {
        
        domanda <- list()
        remDr$findElements("name", 
                           glue::glue("ctl00$Contenuto$ucGestioneDomande$ucLis\\taDomande$dgDomande_Row{j}_Domanda"))[[1]]$clickElement()
        
        domanda$Testo <- remDr$findElements("id", "ctl00_Contenuto_ucGestioneDomande_ucSchedaDomandaReadOnly_Domanda")[[1]]$getElementText() |> 
          unlist()
        
        risposta <- remDr$findElements("id", "ctl00_Contenuto_ucGestioneDomande_ucSchedaDomandaReadOnly_Risposta")[[1]]$getElementText() |> 
          unlist() |> 
          stringr::str_trim()
        
        n.tables <- remDr$getPageSource()[[1]] |> 
          read_html() |> 
          html_nodes("table.lista")
        
        if (risposta == "" & length(n.tables) > 1) {
          domanda$Risposta <- n.tables %>% 
            .[2] %>% 
            .[[1]] %>% # keep the first element of this list
            html_table(fill=T)
        } else {
          domanda$Risposta <- risposta
        }
        sondaggio$Domande <- c(sondaggio$Domande, list(domanda))
      }
      if (length(totale.numero.domande) > 1 & i != length(totale.numero.domande)) {
        remDr$findElements("name", 
                           "ctl00$Contenuto$ucGestioneDomande$ucListaDomande$dgDomande_PaginaSuccessiva")[[1]]$clickElement()
      }
    }
    lista_sondaggi <- c(lista_sondaggi, list(sondaggio))
    remDr$navigate("http://sondaggipoliticoelettorali.it/ListaSondaggi.aspx?st=SONDAGGI")
  }
}
