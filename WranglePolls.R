WrangleAnswers <- function(answer) {
  browser()
  if (tibble::is_tibble(answer)) {
    test <- answer |> 
      dplyr::pull("X1") |> 
      paste(collapse = "-")
  } else {
    test <- answer 
  }
  test <- test |> 
    grep("\\bPartito Democratico\\b|\\bPD\\b", 
         x = _, ignore.case = TRUE,
         value = TRUE) 
  
  if (length(test) == 1) {
    test <- test |>
      stringr::str_detect("^((?!Berlusconi).)*$")
  } else {
    test <- FALSE
  }
  
  return(test)
}


FindElectoralPolls <- function(polls) {
  purrr::pluck(polls,  "Domande") |>
    purrr::map(purrr::pluck("Risposta")) |> 
    purrr::map_lgl(WrangleAnswers)
}

# a <- purrr::map(lista_nazionale, FindElectoralPolls)

lista_nazionale[[2]] |>
  purrr::pluck("Domande") |>
  purrr::map(purrr::pluck("Risposta")) |>
  purrr::map_lgl(WrangleAnswers)

# 
# 
# 
# ExtractNationalPolls <- function(polls) {
#   browser()
#   index.nationals <- purrr::map_chr(polls, 
#                                     purrr::pluck("Estensione_Territoriale")) |> 
#     grep("italia|nazionale", x = _, ignore.case = TRUE)
#   
#   polls[index.nationals]
# }
# 
# a <- ExtractNationalPolls(lista_sondaggi)
