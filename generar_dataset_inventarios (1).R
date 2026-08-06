###############################################################################
#  GENERACIÓN DE DATASET SINTÉTICO DE INVENTARIOS
#  PyME sector ferretero · 120 SKUs · 52 semanas
#  Proyecto de investigación - Objetivo 1
#
#  Implementa ÍNTEGRAMENTE la Tabla de Parámetros aprobada
#  (Tabla_Parametros_Dataset_Ferreteria.xlsx). No modifica variables,
#  distribuciones ni parámetros.
#
#  Lenguaje: R base (sin dependencias de CRAN)
#  Semilla oficial única del proyecto: 123
###############################################################################

set.seed(123)   # <-- SEMILLA OFICIAL. NO CAMBIAR. Reproduce el dataset exacto.

# Locale UTF-8 para lectura correcta de acentos (robusto en Linux/Win/Mac)
suppressWarnings(try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE))

## ---------------------------------------------------------------------------
## 0. PARÁMETROS GLOBALES
## ---------------------------------------------------------------------------
N_SEMANAS <- 52
R_REVISION <- 1          # periodo de revisión (semanas)
FASE_PHI   <- 30         # semana pico estacional
SIGMA_RUIDO_EST <- 0.05  # ruido lognormal del factor estacional
TENDENCIA <- 1.0         # sin tendencia (no se definió tasa en la tabla aprobada)
TASA_MANTENER_ANUAL <- 0.25

# Parámetros por familia (de la hoja Parametros_R)
par_fam <- list(
  FER = list(lam_shape=4.0, lam_scale=15, nb_size=8, est_A=0.15,
             lt_shape=4, lt_scale=0.25, marg_a=16, marg_b=24,
             p_dev=0.01, costo_ordenar=20000, ruido_fx=0),
  EPP = list(lam_shape=3.0, lam_scale=4,  nb_size=5, est_A=0.20,
             lt_shape=4, lt_scale=0.50, marg_a=12, marg_b=28,
             p_dev=0.02, costo_ordenar=30000, ruido_fx=0),
  HER = list(lam_shape=1.5, lam_scale=1,  nb_size=2, est_A=0.25,
             lt_shape=4, lt_scale=1.00, marg_a=8,  marg_b=32,
             p_dev=0.03, costo_ordenar=60000, ruido_fx=0.03)  # FX solo HER
)

# Cumplimiento del proveedor (Beta, igual para todas las familias)
CUMP_A <- 47.5; CUMP_B <- 2.5

# Niveles de servicio por clase ABC -> z
z_abc <- c(A = 2.05, B = 1.64, C = 1.28)  # 98% / 95% / 90%

## ---------------------------------------------------------------------------
## 1. FUNCIÓN AUXILIAR: distribución Triangular (base R, transformada inversa)
##    No se usa el paquete 'triangle' para no depender de CRAN.
## ---------------------------------------------------------------------------
rtriangular <- function(n, a, c, b) {
  # a=min, c=moda, b=max
  u  <- runif(n)
  fc <- (c - a) / (b - a)
  ifelse(u < fc,
         a + sqrt(u * (b - a) * (c - a)),
         b - sqrt((1 - u) * (b - a) * (b - c)))
}

## ---------------------------------------------------------------------------
## 2. LECTURA DEL CATÁLOGO MAESTRO
## ---------------------------------------------------------------------------
cat_maestro <- read.csv("Catalogo_Maestro_Obj_1.csv",
                        stringsAsFactors = FALSE, encoding = "UTF-8")
n_sku <- nrow(cat_maestro)   # 120

## ---------------------------------------------------------------------------
## 3. ATRIBUTOS ESTÁTICOS POR SKU (se sortean UNA sola vez)
##    - lambda_sku  ~ Gamma(shape, scale)   [tasa base de rotación]
##    - costo base  ~ Triangular(min, base, max)
##    - margen      ~ Beta(a, b)
## ---------------------------------------------------------------------------
lambda_sku  <- numeric(n_sku)
costo_base  <- numeric(n_sku)   # costo "ancla" del SKU (moda del triangular)
margen_sku  <- numeric(n_sku)

for (i in seq_len(n_sku)) {
  f  <- cat_maestro$Codigo_familia[i]
  pf <- par_fam[[f]]
  lambda_sku[i] <- rgamma(1, shape = pf$lam_shape, scale = pf$lam_scale)
  costo_base[i] <- rtriangular(1,
                               a = cat_maestro$Costo_minimo[i],
                               c = cat_maestro$Costo_base[i],
                               b = cat_maestro$Costo_maximo[i])
  margen_sku[i] <- rbeta(1, pf$marg_a, pf$marg_b)
}

## ---------------------------------------------------------------------------
## 4. CLASIFICACIÓN ABC (Pareto sobre valor de consumo anual esperado)
##    valor = costo_base * demanda_anual_esperada (= lambda_sku * 52)
## ---------------------------------------------------------------------------
valor_consumo <- costo_base * lambda_sku * N_SEMANAS
ord <- order(valor_consumo, decreasing = TRUE)
cum_pct <- cumsum(valor_consumo[ord]) / sum(valor_consumo)
clase <- character(n_sku)
clase[ord] <- ifelse(cum_pct <= 0.80, "A",
                     ifelse(cum_pct <= 0.95, "B", "C"))

## ---------------------------------------------------------------------------
## 5. POLÍTICA DE INVENTARIO POR SKU  (order-up-to con revisión periódica)
##    mu_LT   = lambda * E[LT]
##    Var_D   = lambda + lambda^2 / k        (varianza semanal NB)
##    Var_LT  = shape * scale^2
##    sigma_LTD = sqrt(E[LT]*Var_D + lambda^2 * Var_LT)
##    SS  = z(clase) * sigma_LTD
##    ROP = mu_LT + SS
##    S   = lambda*(E[LT] + R) + SS
## ---------------------------------------------------------------------------
E_LT <- numeric(n_sku); SS <- numeric(n_sku)
ROP  <- numeric(n_sku); S_up <- numeric(n_sku)

for (i in seq_len(n_sku)) {
  f  <- cat_maestro$Codigo_familia[i]; pf <- par_fam[[f]]
  ELT   <- pf$lt_shape * pf$lt_scale
  VarLT <- pf$lt_shape * pf$lt_scale^2
  VarD  <- lambda_sku[i] + lambda_sku[i]^2 / pf$nb_size
  sigmaLTD <- sqrt(ELT * VarD + lambda_sku[i]^2 * VarLT)
  z <- z_abc[[ clase[i] ]]
  E_LT[i] <- ELT
  SS[i]   <- z * sigmaLTD
  ROP[i]  <- lambda_sku[i] * ELT + SS[i]
  S_up[i] <- lambda_sku[i] * (ELT + R_REVISION) + SS[i]
}

## ---------------------------------------------------------------------------
## 6. FACTOR ESTACIONAL (determinístico + ruido lognormal), por familia y semana
## ---------------------------------------------------------------------------
semanas <- 1:N_SEMANAS
# matriz [semana x familia] del componente determinístico
fam_codes <- c("FER","EPP","HER")
estacional_det <- sapply(fam_codes, function(f)
  1 + par_fam[[f]]$est_A * sin(2*pi*(semanas - FASE_PHI)/N_SEMANAS))
colnames(estacional_det) <- fam_codes

## ---------------------------------------------------------------------------
## 7. SIMULACIÓN DINÁMICA SEMANA A SEMANA
##    Orden de cálculo exacto de la hoja "Identidades_y_Política".
##    Todas las variables derivadas se calculan por identidad contable.
## ---------------------------------------------------------------------------
# Reservamos el data.frame de salida (120 * 52 filas)
n_filas <- n_sku * N_SEMANAS
out <- data.frame(
  semana=integer(n_filas), SKU=character(n_filas), Descripcion=character(n_filas),
  Familia=character(n_filas), Codigo_familia=character(n_filas),
  Subfamilia=character(n_filas), Unidad_medida=character(n_filas),
  clasificacion_ABC=character(n_filas), nivel_servicio_objetivo=numeric(n_filas),
  lambda_sku=numeric(n_filas), costo_unitario=numeric(n_filas),
  margen_bruto=numeric(n_filas), precio_venta=numeric(n_filas),
  factor_estacional=numeric(n_filas), demanda_semanal=integer(n_filas),
  inventario_inicial=integer(n_filas), unidades_recibidas=integer(n_filas),
  ventas=integer(n_filas), demanda_insatisfecha=integer(n_filas),
  quiebre_stock=integer(n_filas), devoluciones=integer(n_filas),
  inventario_final=integer(n_filas), posicion_inventario=numeric(n_filas),
  punto_reorden=numeric(n_filas), stock_seguridad=numeric(n_filas),
  pedido_realizado=integer(n_filas), cantidad_pedida=integer(n_filas),
  lead_time_pedido=numeric(n_filas), valor_inventario_final=numeric(n_filas),
  costo_mantener_sem=numeric(n_filas), costo_faltante_sem=numeric(n_filas),
  costo_ordenar_sem=numeric(n_filas),
  stringsAsFactors = FALSE
)

serv_map <- c(A=0.98, B=0.95, C=0.90)
fila <- 0

for (i in seq_len(n_sku)) {
  f  <- cat_maestro$Codigo_familia[i]; pf <- par_fam[[f]]
  # estado inicial
  inv_ini   <- round(S_up[i])            # semilla = nivel de reposición
  transito  <- rep(0, N_SEMANAS + 60)    # unidades que llegan en semana t (buffer)
  dev_prev  <- 0                          # devoluciones de la semana anterior

  for (t in seq_len(N_SEMANAS)) {
    # (1) factor estacional del periodo
    fest <- estacional_det[t, f] * rlnorm(1, meanlog = 0, sdlog = SIGMA_RUIDO_EST)

    # costo y precio del periodo (HER agrega ruido FX; resto constante)
    if (pf$ruido_fx > 0) {
      costo_t <- costo_base[i] * rlnorm(1, 0, pf$ruido_fx)
    } else {
      costo_t <- costo_base[i]
    }
    precio_t <- costo_t / (1 - margen_sku[i])

    # (2) demanda latente ~ NegBinom(size=k, mu=lambda*estacional*tendencia)
    mu_it <- lambda_sku[i] * fest * TENDENCIA
    dem   <- rnbinom(1, size = pf$nb_size, mu = mu_it)

    # (3) entradas de la semana: llegadas programadas + devoluciones previas
    recibido <- transito[t] + dev_prev
    disponible <- inv_ini + recibido

    # (4) racionamiento: ventas e insatisfecha (identidad contable)
    ventas   <- min(dem, disponible)
    insatis  <- max(0, dem - disponible)
    quiebre  <- as.integer(insatis > 0)

    # (5) devoluciones ~ Binomial(ventas, p) -> reingresan la semana siguiente
    dev_t <- rbinom(1, size = ventas, prob = pf$p_dev)

    # (6) inventario final
    inv_fin <- disponible - ventas

    # (7) posición de inventario = físico + en tránsito futuro
    en_transito_futuro <- sum(transito[(t+1):length(transito)])
    pos_inv <- inv_fin + en_transito_futuro

    # (8) política: si posición <= ROP, ordenar hasta S
    pedido <- as.integer(pos_inv <= ROP[i])
    cant_ped <- 0L
    lt_ped <- NA_real_
    if (pedido == 1) {
      cant_ped <- max(0L, as.integer(round(S_up[i] - pos_inv)))
      if (cant_ped > 0) {
        lt_ped  <- rgamma(1, shape = pf$lt_shape, scale = pf$lt_scale)
        L       <- max(1L, as.integer(round(lt_ped)))    # entero de semanas, min 1
        cumpl   <- rbeta(1, CUMP_A, CUMP_B)              # fill rate del proveedor
        recibe  <- as.integer(round(cant_ped * cumpl))
        if ((t + L) <= length(transito)) transito[t + L] <- transito[t + L] + recibe
      } else {
        pedido <- 0L
      }
    }

    # (9) volcado de la fila
    fila <- fila + 1
    out$semana[fila] <- t
    out$SKU[fila] <- cat_maestro$SKU[i]
    out$Descripcion[fila] <- cat_maestro$Descripcion[i]
    out$Familia[fila] <- cat_maestro$Familia[i]
    out$Codigo_familia[fila] <- f
    out$Subfamilia[fila] <- cat_maestro$Subfamilia[i]
    out$Unidad_medida[fila] <- cat_maestro$Unidad_medida[i]
    out$clasificacion_ABC[fila] <- clase[i]
    out$nivel_servicio_objetivo[fila] <- serv_map[[ clase[i] ]]
    out$lambda_sku[fila] <- round(lambda_sku[i], 4)
    out$costo_unitario[fila] <- round(costo_t, 2)
    out$margen_bruto[fila] <- round(margen_sku[i], 4)
    out$precio_venta[fila] <- round(precio_t, 2)
    out$factor_estacional[fila] <- round(fest, 4)
    out$demanda_semanal[fila] <- dem
    out$inventario_inicial[fila] <- inv_ini
    out$unidades_recibidas[fila] <- recibido
    out$ventas[fila] <- ventas
    out$demanda_insatisfecha[fila] <- insatis
    out$quiebre_stock[fila] <- quiebre
    out$devoluciones[fila] <- dev_t
    out$inventario_final[fila] <- inv_fin
    out$posicion_inventario[fila] <- round(pos_inv, 2)
    out$punto_reorden[fila] <- round(ROP[i], 2)
    out$stock_seguridad[fila] <- round(SS[i], 2)
    out$pedido_realizado[fila] <- pedido
    out$cantidad_pedida[fila] <- cant_ped
    out$lead_time_pedido[fila] <- if (is.na(lt_ped)) NA else round(lt_ped, 3)
    out$valor_inventario_final[fila] <- round(inv_fin * costo_t, 2)
    out$costo_mantener_sem[fila] <- round(inv_fin * costo_t * (TASA_MANTENER_ANUAL/52), 2)
    out$costo_faltante_sem[fila] <- round(insatis * (precio_t - costo_t), 2)
    out$costo_ordenar_sem[fila]  <- if (pedido == 1) pf$costo_ordenar else 0

    # (10) transición al siguiente periodo
    inv_ini  <- inv_fin
    dev_prev <- dev_t
  }
}

## ---------------------------------------------------------------------------
## 8. EXPORTAR CSV OFICIAL
## ---------------------------------------------------------------------------
write.csv(out, "dataset_inventarios_ferreteria.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Dataset generado:", nrow(out), "filas x", ncol(out), "columnas\n")
cat("SKUs:", n_sku, "| Semanas:", N_SEMANAS, "| Semilla: 123\n")
cat("Distribución ABC:\n"); print(table(clase))
cat("Tasa de quiebre global:",
    round(mean(out$quiebre_stock)*100, 2), "%\n")


