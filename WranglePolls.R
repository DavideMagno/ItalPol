GetColumnsFromStrings <- function(string) {
  wrangled.text <- string |> 
    stringr::str_split("\n") |> 
    unlist()
  
  if (length(wrangled.text) > 1) {
    wrangled.text <- wrangled.text |> 
      tail(-1)
  }
  wrangled.text |> 
    stringr::str_remove_all(stringr::fixed("movimento 5 stelle", 
                                           ignore_case = TRUE)) |> 
    stringr::str_remove_all(stringr::fixed("m5s", 
                                           ignore_case = TRUE)) |> 
    stringr::str_remove_all(stringr::fixed("articolo 1", 
                                           ignore_case = TRUE)) |> 
    stringr::str_remove_all(stringr::fixed("art1", 
                                           ignore_case = TRUE)) |> 
    stringr::str_extract_all("\\d+[(\\,\\d+)]*") |> 
    purrr::map_dbl(length) |> 
    unique() |> 
    max()
}

WrangleAnswers <- function(domanda) {
  if (tibble::is_tibble(domanda$Risposta)) {

    test.flag <- ifelse(ncol(domanda$Risposta) > 2, FALSE, TRUE)
    test <- domanda$Risposta |> 
      dplyr::pull("X1") |> 
      paste(collapse = "-")
  } else {
    test <- domanda$Risposta 
    
    number.variables <- GetColumnsFromStrings(test)

    test.flag <- ifelse(number.variables > 1, FALSE, TRUE)
  }
  
  test <- grepl("\\bPartito Democratico\\b|\\bPD\\b", 
                x = test, ignore.case = TRUE) & 
    grepl("\\bForza Italia\\b|\\bFI\\b", 
          x = test, ignore.case = TRUE) &
    !grepl("Elet", x = test, ignore.case = TRUE)
  
  if (test) {
    test <- !grepl("fiducia|coalizion|gradimento|elettor|simpatizzant|favorevole|legge|analisi|Draghi", 
                   domanda$Testo, 
                   ignore.case = TRUE) 
  } else {
    test <- FALSE
  }
  
  test <- test & test.flag
  
  return(test)
}

FindElectoralPolls <- function(polls) {
  purrr::pluck(polls,  "Domande") |>
    purrr::map_lgl(WrangleAnswers)
}

ExtractElectoralPoll <- function(poll, sondaggi.partiti) {
  if(any(sondaggi.partiti)) {
    domanda <- poll$Domande[min(which(sondaggi.partiti))] |> 
      purrr::flatten()
    poll$Domande <- NULL
    poll <- c(poll, domanda)
    
    
  } else {
    poll <- NULL
  }
  return(poll)
}

sondaggi.finali <- purrr::map2(lista_nazionale,
                               sondaggi.partiti,
                               ExtractElectoralPoll) |> 
  {\(x) x[purrr::map_lgl(x, ~!is.null(.x))]}()


# ExtractNationalPolls <- function(polls) {
#   index.nationals <- purrr::map_chr(polls,
#                                     purrr::pluck("Estensione_Territoriale")) |>
#     grep("italia|nazionale", x = _, ignore.case = TRUE)
#   
#   polls[index.nationals]
# }
# 
# lista_nazionale <- ExtractNationalPolls(lista_sondaggi)

sondaggi.partiti <- purrr::map(lista_nazionale, FindElectoralPolls)

purrr::map_dbl(sondaggi.partiti, sum)

purrr::map_dbl(sondaggi.partiti, sum) |> 
  {\(x) x[x >1]}()

purrr::map_dbl(sondaggi.partiti, sum) |> 
        {\(x) which(x == 3)}()

