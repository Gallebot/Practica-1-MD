library(dplyr)
library(plotly)
library(readr)   # parse_number
library(stringr) # str_detect, str_extract

# ========= 0) Lista de datasets =========
rutas <- c(
  "/media/sf_iCloudDrive/IPN/6to Semestre/Minería de Datos/Prácticas Hechas/dataset 1 filtrado.csv",
  "/media/sf_iCloudDrive/IPN/6to Semestre/Minería de Datos/Prácticas Hechas/dataset 2 filtrado.csv",
  "/media/sf_iCloudDrive/IPN/6to Semestre/Minería de Datos/Prácticas Hechas/dataset 3 filtrado.csv"
)

# 1) Parser de duración
# H.MM -> minutos
parse_duration_hmm <- function(x) {
  if (length(x) == 0) return(numeric())
  s <- trimws(as.character(x))
  s[s == ""] <- NA_character_
  s <- gsub(",", ".", s, fixed = TRUE)
  v <- suppressWarnings(as.numeric(s))
  v[!is.finite(v)] <- NA_real_
  horas <- floor(v)
  mins  <- round((v - horas) * 100)
  bad <- is.na(v) | mins < 0 | mins > 59
  out <- horas * 60 + mins
  out[bad] <- NA_real_
  out
}

# 2) Función para cargar y limpiar UN dataset
carga_y_limpia <- function(path, nombre_dataset = NULL) {
  if (is.null(nombre_dataset)) nombre_dataset <- basename(path)
  stopifnot(file.exists(path))
  df <- read_csv(path, show_col_types = FALSE)
  

  req_cols <- c("airline", "class", "price", "duration")
  faltan <- setdiff(req_cols, names(df))
  if (length(faltan)) {
    stop(paste("Faltan columnas en", path, ":", paste(faltan, collapse = ", ")))
  }
  
  df %>%
    mutate(
      dataset  = nombre_dataset,
      price_num = readr::parse_number(as.character(price)),
      dur_min   = parse_duration_any(duration)
    ) %>%
    filter(!is.na(airline), !is.na(class), !is.na(price_num), !is.na(dur_min))
}

# 3) Cargar, unir y muestrear
lista_limpia <- Map(carga_y_limpia, rutas, basename(rutas))
df_all <- bind_rows(lista_limpia)

# Tamaño de muestra para el cubo
N_SAMPLE <- min(1500, nrow(df_all))
set.seed(1)
df_samp <- df_all %>%
  select(airline, class, price_num, dur_min, dataset) %>%
  slice_sample(n = N_SAMPLE)

# 4) Airline a eje X
air_levels <- df_samp %>%
  count(airline, sort = TRUE) %>% pull(airline)

plot_df <- df_samp %>%
  mutate(
    x_air = as.integer(factor(airline, levels = air_levels)) - 1L,  # 0..k-1
    y_prc = price_num,
    z_dur = dur_min
  )

# 5) Graficar el cubo (X=airline, Y=price, Z=duration; color=class)
fig <- plot_ly(
  plot_df,
  x = ~x_air, y = ~y_prc, z = ~z_dur,
  type = "scatter3d", mode = "markers",
  color = ~class,
  text = ~paste0(
    "<b>Aerolínea:</b> ", airline,
    "<br><b>Clase:</b> ", class,
    "<br><b>Precio:</b> ", round(y_prc, 2),
    "<br><b>Duración (min):</b> ", round(z_dur, 0),
    "<br><b>Dataset:</b> ", dataset
  ),
  hoverinfo = "text",
  marker = list(size = 6)
) %>%
  layout(
    title = paste0("Cubo OLAP Vuelos en India"),
    legend = list(title = list(text = "class")),
    scene = list(
      xaxis = list(
        title = "airline",
        tickvals = 0:(length(air_levels) - 1),
        ticktext = air_levels
      ),
      yaxis = list(title = "price"),
      zaxis = list(title = "duration (min)"),
      aspectmode = "cube"
    )
  )

fig
