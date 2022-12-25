
Sys.setlocale("LC_TIME", "Spanish")

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(reactable)


# Preparing data ----------------------------------------------------------

aportes <- read_excel("../fondo/aportes.xlsx", sheet = "Ingreso") |>
  janitor::clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    label_fecha = factor(format(fecha, "%d %b %Y", locale = "spanish")),
    label_fecha = forcats::fct_reorder(label_fecha, fecha),
    month = factor(format(fecha, "%b %Y", locale = "spanish")),
    month = forcats::fct_reorder(label_fecha, fecha)
    )

gastos <- read_excel("../fondo/aportes.xlsx", sheet = "Gastos") |>
  janitor::clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    label_fecha = factor(format(fecha, "%b %Y", locale = "spanish")),
    label_fecha = forcats::fct_reorder(label_fecha, fecha)
  )

dates_order <- aportes |>
  distinct(label_fecha) |>
  pull(label_fecha) %>%
  as.character() %>%
  c("miembro", "total", .)

aportes_table <- aportes |>
  group_by(miembro, label_fecha) |>
  summarise(aporte = sum(aporte)) |> 
  pivot_wider(names_from = label_fecha, values_from = aporte, values_fill = 0) |>
  rowwise() |>
  mutate(total = sum(c_across(where(is.numeric)))) |>
  select(all_of(dates_order))



# html table --------------------------------------------------------------

aportes_table |>
  setNames(str_to_sentence(names(aportes_table))) |> 
  reactable(
    defaultColDef = colDef(width = 110),
    columns = list(
      Total = colDef(
        format = colFormat(separators = TRUE),
        style = list(position = "sticky", left = 90, background = "#fff", zIndex = 1),
        headerStyle = list(position = "sticky", left = 90, background = "#fff", zIndex = 1)
        ),
      Miembro = colDef(
        width = 90,
        style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
        headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
      )
    )
  )








