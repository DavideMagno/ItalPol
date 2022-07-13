WrangleAnswers <- function(domanda) {
  if (tibble::is_tibble(domanda$Risposta)) {
    test <- domanda$Risposta |> 
      dplyr::pull("X1") |> 
      paste(collapse = "-")
  } else {
    test <- domanda$Risposta 
  }

  test <- grepl("\\bPartito Democratico\\b|\\bPD\\b", 
         x = test, ignore.case = TRUE) & 
    grepl("\\bForza Italia\\b|\\bFI\\b", 
         x = test, ignore.case = TRUE)
  
  if (test) {
    test <- !grepl("fiducia|coalizion|gradimento", domanda$Testo, 
                   ignore.case = TRUE) 
  } else {
    test <- FALSE
  }
  
  return(test)
}


FindElectoralPolls <- function(polls) {
  purrr::pluck(polls,  "Domande") |>
    purrr::map_lgl(WrangleAnswers)
}

a <- purrr::map(lista_nazionale, FindElectoralPolls)

purrr::map(a, sum)

# lista_nazionale[[1]] |>
#   purrr::pluck("Domande") |>
#   purrr::map(purrr::pluck("Risposta")) |>
#   purrr::map_lgl(WrangleAnswers)
# 
# # 
# # 
# # 
# ExtractNationalPolls <- function(polls) {
#   index.nationals <- purrr::map_chr(polls,
#                                     purrr::pluck("Estensione_Territoriale")) |>
#     grep("italia|nazionale", x = _, ignore.case = TRUE)
# 
#   polls[index.nationals]
# }
# 
# lista_nazionale <- ExtractNationalPolls(lista_sondaggi)
# 
# realizzatori <- purrr::map_chr(lista_nazionale, purrr::pluck("Realizzatore"))
# 
# lista_realizzatori <- realizzatori |> 
#   unique()
# 
# lista_nazionale[realizzatori %in% lista_realizzatori[1]] |> 
#   purrr::map(purrr::pluck("Domande")) |> 
#   purrr::map(~purrr::map(.x, purrr::pluck("Testo"))) |> 
#   purrr::flatten() |> 
#   unlist()
