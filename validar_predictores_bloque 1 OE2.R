###############################################################################
#  OBJETIVO ESPECÍFICO 2 - VALIDADOR INDEPENDIENTE DE PREDICTORES
#  validar_predictores_OE2.R
#
#  Verifica, de forma AJENA al script de construcción, que la matriz de
#  modelado producida en el Bloque 1 respeta el enfoque one-step-ahead y no
#  incorpora fuga temporal de información. Recalcula rezagos y media histórica
#  directamente desde el dataset original y contrasta contra la matriz.
#
#  NO modifica datos. Emite un dictamen de aptitud (APTO / NO APTO).
#  Equivalente, para el OE2, al 'validar_dataset.R' del OE1.
###############################################################################

## ---------------------------------------------------------------------------
## 0. PARÁMETROS (deben coincidir con los del Bloque 1)
## ---------------------------------------------------------------------------
RUTA_CSV     <- "dataset_inventarios_ferreteria.csv"      # dataset original
if (!file.exists(RUTA_CSV)) {
  alt <- c("dataset_inventarios_ferreteria-usar.csv")
  hit <- alt[file.exists(alt)]; if (length(hit)) RUTA_CSV <- hit[1]
}
RUTA_MATRIZ  <- "matriz_modelado_OE2.csv"                 # salida del Bloque 1
N_LAGS       <- 4
FASE_PHI     <- 30
SEMANA_CORTE <- 42
EST_A        <- c(FER = 0.15, EPP = 0.20, HER = 0.25)
N_SEMANAS    <- 52

# Variables que NUNCA deben aparecer como predictoras (dependen de t o de la demanda)
VARS_PROHIBIDAS <- c("ventas","inventario_final","inventario_inicial",
  "demanda_insatisfecha","quiebre_stock","unidades_recibidas","devoluciones",
  "posicion_inventario","costo_unitario","precio_venta","margen_bruto",
  "valor_inventario_final","factor_estacional")

## ---------------------------------------------------------------------------
## 1. INFRAESTRUCTURA DE HALLAZGOS
## ---------------------------------------------------------------------------
.h <- data.frame(sev=character(), id=character(), desc=character(),
                 viol=integer(), evaluadas=integer())
check <- function(id, desc, ok, sev="CRITICO") {
  ok <- ok & !is.na(ok)
  assign(".h", rbind(get(".h",envir=.GlobalEnv),
    data.frame(sev=sev,id=id,desc=desc,viol=sum(!ok),evaluadas=length(ok))),
    envir=.GlobalEnv)
  invisible(sum(!ok)==0)
}
lin <- function() cat(strrep("=",74),"\n")

## ---------------------------------------------------------------------------
## 2. LECTURA
## ---------------------------------------------------------------------------
lin(); cat("VALIDADOR INDEPENDIENTE DE PREDICTORES - OE2\n"); lin()
stopifnot(file.exists(RUTA_CSV), file.exists(RUTA_MATRIZ))

d  <- read.csv(RUTA_CSV, encoding="UTF-8", stringsAsFactors=FALSE)
d  <- d[order(d$SKU, d$semana), ]; rownames(d) <- NULL
m  <- read.csv(RUTA_MATRIZ, encoding="UTF-8", stringsAsFactors=FALSE)
m  <- m[order(m$SKU, m$semana), ]; rownames(m) <- NULL
cat("Dataset original:", nrow(d), "filas | Matriz de modelado:", nrow(m), "filas\n\n")

## ---------------------------------------------------------------------------
## 3. RECONSTRUCCIÓN INDEPENDIENTE (por un camino distinto al del Bloque 1)
##    Se indexa la demanda por (SKU, semana) y se recalcula todo desde cero.
## ---------------------------------------------------------------------------
clave <- paste(d$SKU, d$semana, sep="#")
dem_de <- setNames(d$demanda_semanal, clave)          # demanda por SKU#semana

# 3.1 Rezago esperado: demanda(SKU, semana-k)
lag_esp <- function(k) dem_de[paste(m$SKU, m$semana - k, sep="#")]

# 3.2 Media histórica esperada: media de demanda de semanas 1..(t-1) del SKU
mh_esp <- sapply(seq_len(nrow(m)), function(i) {
  prev <- d$demanda_semanal[d$SKU==m$SKU[i] & d$semana < m$semana[i]]
  if (length(prev)==0) NA_real_ else mean(prev)
})

# 3.3 Estacionalidad determinística esperada
est_esp <- 1 + EST_A[ d$Codigo_familia[match(m$SKU, d$SKU)] ] *
           sin(2*pi*(m$semana - FASE_PHI)/N_SEMANAS)

## ---------------------------------------------------------------------------
## 4. VERIFICACIONES
## ---------------------------------------------------------------------------
cat(">> Verificaciones\n")

# (1) Rezagos correctamente desplazados (por SKU, sin cruce entre referencias)
for (k in 1:N_LAGS) {
  col <- paste0("lag_", k)
  esp <- as.numeric(lag_esp(k))
  ok  <- (is.na(esp) & is.na(m[[col]])) | (!is.na(esp) & m[[col]]==esp)
  check(paste0("LAG-",k), paste0("lag_",k,"(t) = demanda(t-",k,") del mismo SKU"), ok)
}

# (2) Media histórica sin información actual ni futura
tol <- 1e-6
ok_mh <- (is.na(mh_esp) & is.na(m$media_historica)) |
         (abs(m$media_historica - mh_esp) < tol)
check("MEDIA-HIST", "media_historica = promedio de semanas < t (sin fuga)", ok_mh)

# (3) Estacionalidad determinística coincide (no proviene del factor con ruido)
check("ESTACIONAL", "estacional_det = fórmula determinística de la semana t",
      abs(m$estacional_det - as.numeric(est_esp)) < 1e-6)

# (4) Filas eliminadas = exactamente las primeras N_LAGS semanas de cada SKU
semanas_presentes <- sort(unique(m$semana))
check("DESCARTE-SEM", paste0("La matriz contiene solo semanas ", N_LAGS+1, "-", N_SEMANAS),
      all(semanas_presentes == (N_LAGS+1):N_SEMANAS))
n_por_sku <- table(m$SKU)
check("DESCARTE-N", paste0("Cada SKU conserva ", N_SEMANAS-N_LAGS, " semanas"),
      all(n_por_sku == (N_SEMANAS - N_LAGS)))
check("TAMANO", paste0("Matriz = 120 x ", N_SEMANAS-N_LAGS, " = ", 120*(N_SEMANAS-N_LAGS), " filas"),
      nrow(m) == 120*(N_SEMANAS-N_LAGS))

# (5) Coherencia temporal de la partición: max(train) < min(valid) por SKU
if ("conjunto" %in% names(m)) {
  spl <- split(m, m$SKU)
  ok_orden <- sapply(spl, function(s) {
    tr <- s$semana[s$conjunto=="entrenamiento"]
    va <- s$semana[s$conjunto=="validacion"]
    if (length(tr)==0 || length(va)==0) return(FALSE)
    max(tr) < min(va)
  })
  check("PART-ORDEN", "En cada SKU, toda semana de validación es posterior al entrenamiento",
        ok_orden)

  # Sin solapamiento de semanas entre conjuntos dentro del SKU
  ok_disj <- sapply(spl, function(s)
    length(intersect(s$semana[s$conjunto=="entrenamiento"],
                     s$semana[s$conjunto=="validacion"]))==0)
  check("PART-DISJUNTO", "Entrenamiento y validación no comparten semanas por SKU", ok_disj)

  # Corte en la semana esperada
  check("PART-CORTE", paste0("Entrenamiento = semanas <= ", SEMANA_CORTE),
        all(m$semana[m$conjunto=="entrenamiento"] <= SEMANA_CORTE) &
        all(m$semana[m$conjunto=="validacion"]   >  SEMANA_CORTE))
} else {
  check("PART-EXISTE", "La matriz contiene la columna 'conjunto'", FALSE)
}

# (6) Ninguna variable prohibida quedó como columna de la matriz
presentes <- intersect(VARS_PROHIBIDAS, names(m))
check("NO-FUGA-COLS", "Ninguna variable dependiente de t figura en la matriz",
      length(presentes)==0)
if (length(presentes)>0) cat("   -> columnas indebidas:", paste(presentes, collapse=", "), "\n")

## ---------------------------------------------------------------------------
## 5. DICTAMEN
## ---------------------------------------------------------------------------
cat("\n"); lin(); cat("RESUMEN\n"); lin()
h <- .h
for (i in seq_len(nrow(h)))
  cat(sprintf("  [%s] %-14s %s  (%d/%d violaciones)\n",
      ifelse(h$viol[i]==0,"OK ","!! "), h$id[i], h$desc[i], h$viol[i], h$evaluadas[i]))
ncrit <- sum(h$sev=="CRITICO" & h$viol>0)
cat(sprintf("\nVerificaciones: %d | superadas: %d | críticas con hallazgos: %d\n",
    nrow(h), sum(h$viol==0), ncrit))

lin(); cat("DICTAMEN: ")
if (ncrit==0) {
  cat("MATRIZ DE PREDICTORES APTA PARA EL ENTRENAMIENTO.\n")
  cat("No se detectó fuga temporal de información; la partición es coherente.\n")
} else {
  cat("NO APTA.\n")
  cat("Se detectaron hallazgos críticos que deben corregirse antes de modelar.\n")
}
lin()
