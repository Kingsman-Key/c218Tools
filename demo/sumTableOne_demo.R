# example code

df <- iris %>%
  dplyr::mutate(
    Petal.Length.2f = case_when(
      Petal.Length < 2 ~ "small",
      Petal.Length >= 2 ~ "large",
      TRUE ~ NA_character_
    )
  )
table1 <- tableone::CreateTableOne(vars = c("Sepal.Length", "Sepal.Width", "Petal.Length.2f"), strata = "Species", data = iris) %>%
  print(showAllLevels = T)

res <- c218Tools::sumTableOne(table1)
