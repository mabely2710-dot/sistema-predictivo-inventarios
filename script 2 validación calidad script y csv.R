###############################################################################
#  VALIDACIÓN DEL DATASET SINTÉTICO DE INVENTARIOS
#  PyME sector ferretero · 120 SKUs · 52 semanas
#  Proyecto de investigación - Auditoría del Objetivo 1
#
#  Propósito: verificar que 'dataset_inventarios_ferreteria.csv' es COHERENTE
#  con la lógica de 'generar_dataset_inventarios.R'. Las reglas NO se toman de
#  fórmulas genéricas de inventarios: se derivan de las IDENTIDADES CONTABLES y
#  de POLÍTICA que el propio simulador impone fila a fila y entre semanas.
#
#  IMPORTANTE (metodología): el dataset NO es reproducible bit a bit porque
#  (a) falta 'Catalogo_Maestro_Obj_1.csv' y (b) la corriente RNG de set.seed(123)
#  se consume en un orden entrelazado imposible de replicar sin re-ejecutar el
#  generador completo. Por eso NO re-simulamos: validamos identidades exactas
#  (independientes del RNG) y contrastamos lo estocástico de forma distribucional.
#
#  Lenguaje: R base (sin dependencias de CRAN).  Compatible con RStudio.
###############################################################################

## ===========================================================================
## 0. CONFIGURACIÓN
## ===========================================================================
options(stringsAsFactors = FALSE)

# Ruta del dataset a validar. Ajuste si es necesario.
RUTA_CSV <- "dataset_inventarios_ferreteria.csv"
if (!file.exists(RUTA_CSV)) {
  cand <- c("dataset_inventarios_ferreteria-usar.csv",
            "dataset_inventarios_ferreteria_claude.csv")
  hit <- cand[file.exists(cand)]
  if (length(hit)) RUTA_CSV <- hit[1]
}
GENERAR_GRAFICOS <- TRUE          # PNG de validación en el directorio de trabajo
PREFIJO_PNG      <- "val_"

# --- Parámetros del generador (copiados textualmente de su código, para poder
#     RECONSTRUIR las variables de política e identificar la familia). ---------
par_fam <- list(
  FER = list(lam_shape=4.0, lam_scale=15, nb_size=8, est_A=0.15,
             lt_shape=4, lt_scale=0.25, marg_a=16, marg_b=24,
             p_dev=0.01, costo_ordenar=20000, ruido_fx=0),
  EPP = list(lam_shape=3.0, lam_scale=4,  nb_size=5, est_A=0.20,
             lt_shape=4, lt_scale=0.50, marg_a=12, marg_b=28,
             p_dev=0.02, costo_ordenar=30000, ruido_fx=0),
  HER = list(lam_shape=1.5, lam_scale=1,  nb_size=2, est_A=0.25,
             lt_shape=4, lt_scale=1.00, marg_a=8,  marg_b=32,
             p_dev=0.03, costo_ordenar=60000, ruido_fx=0.03)
)
z_abc               <- c(A = 2.05, B = 1.64, C = 1.28)   # 98/95/90 %
serv_map            <- c(A = 0.98, B = 0.95, C = 0.90)
N_SEMANAS           <- 52
R_REVISION          <- 1
FASE_PHI            <- 30
TASA_MANTENER_ANUAL <- 0.25

# Tolerancias (derivadas del redondeo que aplica el generador)
TOL_DINERO   <- 2.0     # $ (redondeos compuestos costo/precio/valores)
TOL_UNIDAD   <- 1.0     # unidades (reconstrucción de SS/ROP/S con lambda redondeada a 4)
TOL_RATIO    <- 5e-3    # identidad precio = costo/(1-margen)

## ===========================================================================
## 1. INFRAESTRUCTURA DE HALLAZGOS
## ===========================================================================
# Cada check registra: severidad, id, descripción, nº de violaciones, evidencia.
.hallazgos <- data.frame(sev=character(), id=character(), desc=character(),
                         n_viol=integer(), n_eval=integer(), evidencia=character())

registrar <- function(sev, id, desc, n_viol, n_eval, evidencia = "") {
  .hallazgos[[1]]  # touch
  assign(".hallazgos",
         rbind(get(".hallazgos", envir=.GlobalEnv),
               data.frame(sev=sev, id=id, desc=desc,
                          n_viol=as.integer(n_viol), n_eval=as.integer(n_eval),
                          evidencia=evidencia)),
         envir = .GlobalEnv)
}

# check(): evalúa un vector lógico 'ok' (TRUE = cumple). Registra según severidad
# si hay al menos una violación. sev en {"CRITICO","ADVERTENCIA","INFO"}.
check <- function(id, desc, ok, sev = "CRITICO", idx = NULL, ejemplos = 3) {
  ok <- ok & !is.na(ok)            # NA en la condición cuenta como violación
  n_eval <- length(ok)
  viol   <- which(!ok)
  n_viol <- length(viol)
  ev <- ""
  if (n_viol > 0 && !is.null(idx)) {
    muestra <- head(viol, ejemplos)
    ev <- paste0("filas: ", paste(idx[muestra], collapse=", "),
                 if (n_viol > ejemplos) " ..." else "")
  }
  registrar(sev, id, desc, n_viol, n_eval, ev)
  invisible(n_viol == 0)
}

banner <- function(txt) {
  cat("\n", strrep("=", 78), "\n", txt, "\n", strrep("=", 78), "\n", sep="")
}

## ===========================================================================
## 2. CARGA Y ORDENAMIENTO
## ===========================================================================
banner("VALIDACIÓN DEL DATASET SINTÉTICO DE INVENTARIOS")
cat("Archivo:", RUTA_CSV, "\n")
stopifnot(file.exists(RUTA_CSV))

d <- read.csv(RUTA_CSV, encoding = "UTF-8", check.names = FALSE)
# lead_time_pedido viene con "NA" textual -> numérico con NA real
if (is.character(d$lead_time_pedido))
  d$lead_time_pedido <- suppressWarnings(as.numeric(d$lead_time_pedido))

# Orden canónico: por SKU y semana (imprescindible para checks temporales)
d <- d[order(d$SKU, d$semana), ]
rownames(d) <- NULL
d$.fila <- seq_len(nrow(d))       # índice para reportar evidencia
idx <- d$.fila

cat("Filas:", nrow(d), "| Columnas:", ncol(d)-1, "\n")

## ===========================================================================
## 3. INTEGRIDAD, COMPLETITUD Y UNICIDAD
## ===========================================================================
banner("3. INTEGRIDAD, COMPLETITUD Y UNICIDAD")

col_esperadas <- c("semana","SKU","Descripcion","Familia","Codigo_familia",
  "Subfamilia","Unidad_medida","clasificacion_ABC","nivel_servicio_objetivo",
  "lambda_sku","costo_unitario","margen_bruto","precio_venta","factor_estacional",
  "demanda_semanal","inventario_inicial","unidades_recibidas","ventas",
  "demanda_insatisfecha","quiebre_stock","devoluciones","inventario_final",
  "posicion_inventario","punto_reorden","stock_seguridad","pedido_realizado",
  "cantidad_pedida","lead_time_pedido","valor_inventario_final",
  "costo_mantener_sem","costo_faltante_sem","costo_ordenar_sem")

check("EST-COLS", "Están presentes las 32 columnas esperadas",
      all(col_esperadas %in% names(d)), "CRITICO")

# Unicidad SKU-semana
dup <- duplicated(d[, c("SKU","semana")])
check("EST-DUP", "No hay pares (SKU, semana) duplicados", !dup, "CRITICO", idx)

# 120 SKU x 52 semanas completas
n_sku <- length(unique(d$SKU))
check("EST-NSKU", "Hay exactamente 120 SKU", n_sku == 120, "CRITICO")

sem_por_sku <- tapply(d$semana, d$SKU, function(s)
  length(s) == N_SEMANAS && all(sort(s) == 1:N_SEMANAS))
check("EST-SEM", "Cada SKU tiene las 52 semanas 1..52 completas",
      sem_por_sku, "CRITICO")

# Nulos en columnas donde NO se admiten (todas menos lead_time_pedido)
cols_no_na <- setdiff(col_esperadas, "lead_time_pedido")
na_por_col <- sapply(cols_no_na, function(c) sum(is.na(d[[c]])))
check("EST-NA", "Sin valores nulos en columnas obligatorias",
      all(na_por_col == 0), "CRITICO",
      idx = NULL)
if (any(na_por_col > 0))
  cat("   -> columnas con NA:",
      paste(names(na_por_col)[na_por_col>0], collapse=", "), "\n")

## ===========================================================================
## 4. TIPOS, DOMINIOS Y RANGOS
## ===========================================================================
banner("4. TIPOS, DOMINIOS Y RANGOS")

enteros <- c("semana","demanda_semanal","inventario_inicial","unidades_recibidas",
  "ventas","demanda_insatisfecha","quiebre_stock","devoluciones",
  "inventario_final","pedido_realizado","cantidad_pedida","costo_ordenar_sem")
for (c in enteros) {
  v <- d[[c]]
  check(paste0("DOM-INT-", c), paste("Columna", c, "es entera"),
        abs(v - round(v)) < 1e-9, "CRITICO", idx)
}

# No negatividad de magnitudes físicas / monetarias
no_neg <- c("demanda_semanal","inventario_inicial","unidades_recibidas","ventas",
  "demanda_insatisfecha","devoluciones","inventario_final","cantidad_pedida",
  "posicion_inventario","punto_reorden","stock_seguridad","costo_unitario",
  "precio_venta","valor_inventario_final","costo_mantener_sem",
  "costo_faltante_sem","costo_ordenar_sem","lambda_sku","factor_estacional")
for (c in no_neg)
  check(paste0("DOM-POS-", c), paste("Columna", c, ">= 0"),
        d[[c]] >= 0, "CRITICO", idx)

check("DOM-BIN-quiebre", "quiebre_stock es binaria {0,1}",
      d$quiebre_stock %in% c(0,1), "CRITICO", idx)
check("DOM-BIN-pedido", "pedido_realizado es binaria {0,1}",
      d$pedido_realizado %in% c(0,1), "CRITICO", idx)
check("DOM-MARGEN", "margen_bruto en (0,1)",
      d$margen_bruto > 0 & d$margen_bruto < 1, "CRITICO", idx)
check("DOM-FEST", "factor_estacional > 0",
      d$factor_estacional > 0, "CRITICO", idx)
check("DOM-ABC", "clasificacion_ABC en {A,B,C}",
      d$clasificacion_ABC %in% c("A","B","C"), "CRITICO", idx)
check("DOM-FAM", "Codigo_familia en {FER,EPP,HER}",
      d$Codigo_familia %in% c("FER","EPP","HER"), "CRITICO", idx)

## ===========================================================================
## 5. IDENTIDADES CONTABLES POR FILA  (derivadas del código, exactas)
## ===========================================================================
banner("5. IDENTIDADES CONTABLES POR FILA")

disponible <- d$inventario_inicial + d$unidades_recibidas

check("ID-DISP", "disponible = inv_inicial + unidades_recibidas (implícito)",
      rep(TRUE, nrow(d)), "INFO")   # traza; 'disponible' se usa abajo

check("ID-VENTAS", "ventas = min(demanda, disponible)",
      d$ventas == pmin(d$demanda_semanal, disponible), "CRITICO", idx)

check("ID-INSAT", "demanda_insatisfecha = max(0, demanda - disponible)",
      d$demanda_insatisfecha == pmax(0, d$demanda_semanal - disponible),
      "CRITICO", idx)

check("ID-BAL-DEM", "demanda = ventas + demanda_insatisfecha",
      d$demanda_semanal == d$ventas + d$demanda_insatisfecha, "CRITICO", idx)

check("ID-INVFIN", "inv_final = inv_inicial + recibidas - ventas",
      d$inventario_final == d$inventario_inicial + d$unidades_recibidas - d$ventas,
      "CRITICO", idx)

check("ID-QUIEBRE", "quiebre = 1 <=> demanda_insatisfecha > 0",
      (d$quiebre_stock == 1) == (d$demanda_insatisfecha > 0), "CRITICO", idx)

check("ID-QUIEBRE-INV", "quiebre = 1 => inv_final = 0",
      !(d$quiebre_stock == 1) | (d$inventario_final == 0), "CRITICO", idx)

check("ID-DEV-VENTAS", "devoluciones <= ventas (Binomial n=ventas)",
      d$devoluciones <= d$ventas, "CRITICO", idx)

check("ID-POS-GE-INV", "posicion_inventario >= inventario_final",
      d$posicion_inventario >= d$inventario_final - 1e-9, "CRITICO", idx)

check("ID-POS-INT", "posicion_inventario es entera (inv_final + tránsito entero)",
      abs(d$posicion_inventario - round(d$posicion_inventario)) < 1e-6,
      "CRITICO", idx)

## ===========================================================================
## 6. COHERENCIA TEMPORAL Y DE FLUJO (entre semanas, por SKU)
## ===========================================================================
banner("6. COHERENCIA TEMPORAL Y DE FLUJO")

# Vectores desplazados dentro de cada SKU
sku       <- d$SKU
prev_sku  <- c(NA, sku[-nrow(d)])
mismo_sku <- !is.na(prev_sku) & prev_sku == sku      # fila t con t-1 del mismo SKU
inv_fin_prev <- c(NA, d$inventario_final[-nrow(d)])
dev_prev     <- c(NA, d$devoluciones[-nrow(d)])

# 6.1 inv_inicial[t] = inv_final[t-1]
ok_enlace <- !mismo_sku | (d$inventario_inicial == inv_fin_prev)
check("TMP-ENLACE", "inv_inicial(t) = inv_final(t-1) dentro de cada SKU",
      ok_enlace, "CRITICO", idx)

# 6.2 unidades_recibidas[t] >= devoluciones[t-1]  (transito = recibidas-devprev >=0)
ok_trans <- !mismo_sku | (d$unidades_recibidas >= dev_prev)
check("TMP-TRANSITO", "unidades_recibidas(t) >= devoluciones(t-1)  [tránsito>=0]",
      ok_trans, "CRITICO", idx)

# 6.3 Semana 1: sin recepciones (buffer de tránsito arranca en cero)
es_sem1 <- d$semana == 1
check("TMP-SEM1-REC", "En la semana 1 unidades_recibidas = 0",
      !es_sem1 | (d$unidades_recibidas == 0), "CRITICO", idx)

## ===========================================================================
## 7. RECONSTRUCCIÓN DE LA POLÍTICA (s,S) CON REVISIÓN R=1
##    SS, ROP y S se recomputan desde lambda + parámetros de familia y clase.
##    (Independiente del RNG; usa lambda redondeada -> tolerancia TOL_UNIDAD.)
## ===========================================================================
banner("7. POLÍTICA DE INVENTARIO (s,S): SS, ROP, S, pedidos")

fam <- d$Codigo_familia
ELT   <- sapply(fam, function(f) par_fam[[f]]$lt_shape * par_fam[[f]]$lt_scale)
VarLT <- sapply(fam, function(f) par_fam[[f]]$lt_shape * par_fam[[f]]$lt_scale^2)
ksz   <- sapply(fam, function(f) par_fam[[f]]$nb_size)
lam   <- d$lambda_sku
VarD  <- lam + lam^2 / ksz
sigmaLTD <- sqrt(ELT * VarD + lam^2 * VarLT)
z_row <- z_abc[d$clasificacion_ABC]

SS_rec  <- z_row * sigmaLTD
ROP_rec <- lam * ELT + SS_rec
S_rec   <- lam * (ELT + R_REVISION) + SS_rec

check("POL-SS", "stock_seguridad = z_clase * sigma_LTD (reconstruido)",
      abs(d$stock_seguridad - SS_rec) <= TOL_UNIDAD, "CRITICO", idx)

check("POL-ROP", "punto_reorden = lambda*E[LT] + SS (reconstruido)",
      abs(d$punto_reorden - ROP_rec) <= TOL_UNIDAD, "CRITICO", idx)

# Estáticos constantes por SKU
const_por_sku <- function(col) {
  v <- d[[col]]
  key <- if (is.numeric(v)) round(v, 6) else v
  ag <- tapply(key, d$SKU, function(x) length(unique(x)) == 1)
  all(ag)
}
for (c in c("lambda_sku","margen_bruto","stock_seguridad","punto_reorden",
            "clasificacion_ABC","nivel_servicio_objetivo",
            "Codigo_familia","Descripcion")) {
  check(paste0("POL-CONST-", c), paste(c, "constante por SKU"),
        const_por_sku(c), "CRITICO")
}

# costo_unitario: constante en FER/EPP (ruido_fx=0), variable en HER
cu_var <- tapply(seq_len(nrow(d)), d$SKU, function(r) {
  f <- d$Codigo_familia[r[1]]
  nuni <- length(unique(round(d$costo_unitario[r], 4)))
  c(fam = f, varia = nuni > 1)
})
fam_sku <- sapply(cu_var, function(x) x["fam"])
var_sku <- sapply(cu_var, function(x) as.logical(x["varia"]))
check("POL-CU-FEREPP", "costo_unitario constante por SKU en FER y EPP",
      !( fam_sku %in% c("FER","EPP") & var_sku ), "CRITICO")
# HER debería variar (informativo: no invalida si algún SKU HER no varía por azar)
her_varia <- var_sku[fam_sku == "HER"]
check("POL-CU-HER", "costo_unitario varía en HER (ruido FX 0.03)",
      her_varia, "INFO")

# Lógica de ordenamiento EXACTA del generador:
#   pedido = 1  <=>  (posicion <= ROP)  Y  round(S - posicion) > 0
# (a) Nunca se ordena por encima del ROP  [dirección robusta -> CRÍTICO]
check("POL-PEDIDO-A", "pedido = 1 => posicion_inventario <= ROP (nunca ordena sobre ROP)",
      !(d$pedido_realizado == 1) | (d$posicion_inventario <= d$punto_reorden + 1e-6),
      "CRITICO", idx)
# (b) Si posicion < ROP y NO se ordenó, debe explicarse por cancelación
#     round(S - posicion) <= 0 (el pedido resultaría nulo). Tolerancia por la
#     reconstrucción de S con lambda redondeada.  [ADVERTENCIA]
bajo_rop  <- d$posicion_inventario <= d$punto_reorden - 1e-6
sin_ped   <- d$pedido_realizado == 0
explicado <- (S_rec - d$posicion_inventario) < TOL_UNIDAD   # round(S-pos)<=0 aprox
check("POL-PEDIDO-B", "pos<ROP sin pedido se explica por cancelacion round(S-pos)<=0",
      !(bajo_rop & sin_ped) | explicado, "ADVERTENCIA", idx)

check("POL-PED-CANT", "pedido = 1 <=> cantidad_pedida > 0",
      (d$pedido_realizado == 1) == (d$cantidad_pedida > 0), "CRITICO", idx)

check("POL-PED-LT", "pedido = 1 <=> lead_time_pedido != NA",
      (d$pedido_realizado == 1) == (!is.na(d$lead_time_pedido)), "CRITICO", idx)

# cantidad_pedida ~ round(S - posicion) cuando se ordena
con_pedido <- d$pedido_realizado == 1
cant_rec <- round(S_rec - d$posicion_inventario)
check("POL-CANT-S", "cantidad_pedida = round(S - posicion) al ordenar",
      !con_pedido | (abs(d$cantidad_pedida - cant_rec) <= TOL_UNIDAD),
      "ADVERTENCIA", idx)

# inv_inicial semana 1 = round(S)
ok_s1 <- !es_sem1 | (abs(d$inventario_inicial - round(S_rec)) <= TOL_UNIDAD)
check("POL-INI-S", "inv_inicial(semana 1) = round(S) reconstruido",
      ok_s1, "ADVERTENCIA", idx)

## ===========================================================================
## 8. IDENTIDADES DE COSTOS Y PRECIOS
## ===========================================================================
banner("8. IDENTIDADES DE COSTOS Y PRECIOS")

precio_rec <- d$costo_unitario / (1 - d$margen_bruto)
check("CST-PRECIO", "precio_venta = costo/(1 - margen)",
      abs(d$precio_venta - precio_rec) <= pmax(TOL_DINERO, TOL_RATIO*precio_rec),
      "CRITICO", idx)

check("CST-VALINV", "valor_inventario_final = inv_final * costo_unitario",
      abs(d$valor_inventario_final - d$inventario_final * d$costo_unitario) <= TOL_DINERO,
      "CRITICO", idx)

cm_rec <- d$inventario_final * d$costo_unitario * (TASA_MANTENER_ANUAL/52)
check("CST-MANTENER", "costo_mantener = inv_final*costo*(0.25/52)",
      abs(d$costo_mantener_sem - cm_rec) <= TOL_DINERO, "CRITICO", idx)

cf_rec <- d$demanda_insatisfecha * (d$precio_venta - d$costo_unitario)
check("CST-FALTANTE", "costo_faltante = insatisfecha*(precio - costo)",
      abs(d$costo_faltante_sem - cf_rec) <= TOL_DINERO, "CRITICO", idx)

co_fam <- sapply(fam, function(f) par_fam[[f]]$costo_ordenar)
co_esp <- ifelse(d$pedido_realizado == 1, co_fam, 0)
check("CST-ORDENAR", "costo_ordenar = pedido * costo_ordenar(familia)",
      d$costo_ordenar_sem == co_esp, "CRITICO", idx)

check("CST-NS", "nivel_servicio_objetivo = mapa(clase) {A .98,B .95,C .90}",
      abs(d$nivel_servicio_objetivo - serv_map[d$clasificacion_ABC]) < 1e-9,
      "CRITICO", idx)

## ===========================================================================
## 9. CLASIFICACIÓN ABC (ordenamiento de Pareto por valor de consumo)
## ===========================================================================
banner("9. CLASIFICACIÓN ABC")

# valor_consumo = costo_base * lambda * 52.  costo_base = costo_unitario en
# FER/EPP; en HER se estima con la mediana del costo semanal (deshace ruido FX).
sku_ids  <- unique(d$SKU)
per_sku  <- data.frame(SKU = sku_ids)
per_sku$fam   <- d$Codigo_familia[match(sku_ids, d$SKU)]
per_sku$clase <- d$clasificacion_ABC[match(sku_ids, d$SKU)]
per_sku$lam   <- d$lambda_sku[match(sku_ids, d$SKU)]
per_sku$cbase <- tapply(d$costo_unitario, d$SKU, median)[sku_ids]
per_sku$valor <- per_sku$cbase * per_sku$lam * N_SEMANAS

# Conteo por clase (informativo)
cat("Conteo de SKU por clase:\n"); print(table(per_sku$clase))

# Separación de Pareto: el valor mínimo de A debe superar (aprox.) al máx de B,
# y el mín de B al máx de C. Se admite un solapamiento leve por el redondeo de
# lambda y la estimación de costo_base en HER (tolerancia 5%).
vA <- per_sku$valor[per_sku$clase=="A"]
vB <- per_sku$valor[per_sku$clase=="B"]
vC <- per_sku$valor[per_sku$clase=="C"]
sep_AB <- if(length(vA)&&length(vB)) min(vA) >= max(vB)*0.95 else TRUE
sep_BC <- if(length(vB)&&length(vC)) min(vB) >= max(vC)*0.95 else TRUE
check("ABC-ORDEN", "Clases A>B>C ordenadas por valor de consumo (Pareto)",
      sep_AB && sep_BC, "ADVERTENCIA")

# Participación acumulada del valor (debe rondar 80% en A, 95% en A+B)
ord <- order(per_sku$valor, decreasing=TRUE)
cum <- cumsum(per_sku$valor[ord])/sum(per_sku$valor)
part_A  <- sum(per_sku$valor[per_sku$clase=="A"]) / sum(per_sku$valor)
part_AB <- sum(per_sku$valor[per_sku$clase %in% c("A","B")]) / sum(per_sku$valor)
cat(sprintf("Participación de valor -> A: %.1f%%   A+B: %.1f%%\n",
            100*part_A, 100*part_AB))
check("ABC-PARETO80", "Participación de A cercana a 80% (70-88%)",
      part_A > 0.70 & part_A < 0.88, "INFO")

## ===========================================================================
## 10. CONTRASTES DISTRIBUCIONALES (estocásticos -> no invalidan por sí solos)
## ===========================================================================
banner("10. DISTRIBUCIONES Y COMPORTAMIENTO ESTOCÁSTICO")

resumen_fam <- function(col, fun=mean) tapply(d[[col]], d$Codigo_familia, fun)

cat("Demanda semanal media por familia (obs):\n")
print(round(resumen_fam("demanda_semanal"),3))
cat("Lambda media por familia (esperado ~ shape*scale: FER 60, EPP 12, HER 1.5):\n")
lam_teo <- sapply(par_fam, function(p) p$lam_shape*p$lam_scale)
lam_obs <- tapply(d$lambda_sku[match(sku_ids,d$SKU)], per_sku$fam, mean)
print(round(rbind(obs=lam_obs, teorico=lam_teo[names(lam_obs)]),2))

# Lead time por familia: media esperada = shape*scale (FER 1, EPP 2, HER 4)
lt_obs <- tapply(d$lead_time_pedido, d$Codigo_familia,
                 function(x) mean(x, na.rm=TRUE))
lt_teo <- sapply(par_fam, function(p) p$lt_shape*p$lt_scale)
cat("Lead time medio por familia (obs vs teórico):\n")
print(round(rbind(obs=lt_obs, teorico=lt_teo[names(lt_obs)]),3))
# Advertencia si la media de LT se desvía >25% del teórico
lt_ok <- all(abs(lt_obs - lt_teo[names(lt_obs)]) <= 0.25*lt_teo[names(lt_obs)],
             na.rm=TRUE)
check("DIS-LT", "Lead time medio por familia coherente con Gamma teórica (±25%)",
      lt_ok, "ADVERTENCIA")

# Tasa de devolución observada vs p_dev
dev_rate <- tapply(seq_len(nrow(d)), d$Codigo_familia,
                   function(r) sum(d$devoluciones[r]) / max(1,sum(d$ventas[r])))
p_dev_teo <- sapply(par_fam, function(p) p$p_dev)
cat("Tasa de devolución obs vs p_dev teórico por familia:\n")
print(round(rbind(obs=dev_rate, teorico=p_dev_teo[names(dev_rate)]),4))

# Estacionalidad: correlación entre factor medio por semana y curva teórica FER
fest_sem <- tapply(d$factor_estacional[d$Codigo_familia=="FER"],
                   d$semana[d$Codigo_familia=="FER"], mean)
curva_teo <- 1 + par_fam$FER$est_A * sin(2*pi*((1:N_SEMANAS)-FASE_PHI)/N_SEMANAS)
cor_est <- suppressWarnings(cor(as.numeric(fest_sem), curva_teo))
cat(sprintf("Correlación estacional observada vs teórica (FER): %.3f\n", cor_est))
check("DIS-EST", "Patrón estacional FER correlaciona con curva teórica (r>0.6)",
      !is.na(cor_est) && cor_est > 0.6, "ADVERTENCIA")

# Tasa de quiebre global (informativa)
q_glob <- mean(d$quiebre_stock)
cat(sprintf("Tasa de quiebre global: %.2f%%\n", 100*q_glob))
q_clase <- tapply(d$quiebre_stock, d$clasificacion_ABC, mean)
cat("Tasa de quiebre por clase:\n"); print(round(q_clase,4))

## ===========================================================================
## 11. ATÍPICOS (informativo; en simulaciones lumpy son esperables)
## ===========================================================================
banner("11. VALORES ATÍPICOS (informativo)")
atip <- function(x) {
  q <- quantile(x, c(.25,.75), na.rm=TRUE); ri <- q[2]-q[1]
  sum(x > q[2] + 3*ri, na.rm=TRUE)
}
for (c in c("demanda_semanal","cantidad_pedida","valor_inventario_final")) {
  n <- atip(d[[c]])
  cat(sprintf("  %-24s outliers (>Q3+3·RIC): %d (%.2f%%)\n",
              c, n, 100*n/nrow(d)))
}
registrar("INFO","OUT-INFO","Outliers reportados (esperables en demanda lumpy)",
          0, nrow(d), "")

## ===========================================================================
## 12. GRÁFICOS DE VALIDACIÓN
## ===========================================================================
if (GENERAR_GRAFICOS) {
  banner("12. GRÁFICOS DE VALIDACIÓN (PNG)")
  gp <- function(nombre) file.path(getwd(), paste0(PREFIJO_PNG, nombre, ".png"))
  ok_png <- TRUE
  tryCatch({
    png(gp("demanda_por_familia"), 900, 600)
    boxplot(demanda_semanal ~ Codigo_familia, data=d,
            main="Demanda semanal por familia", ylab="unidades", col="grey85")
    dev.off()

    png(gp("estacionalidad_FER"), 900, 600)
    plot(1:N_SEMANAS, as.numeric(fest_sem), type="b", pch=19,
         xlab="semana", ylab="factor estacional medio",
         main="Estacionalidad FER: observada vs teórica")
    lines(1:N_SEMANAS, curva_teo, col="red", lwd=2)
    legend("topright", c("observada","teórica"), col=c("black","red"),
           lty=1, pch=c(19,NA), bty="n")
    dev.off()

    png(gp("lead_time"), 900, 600)
    hist(d$lead_time_pedido, breaks=30, col="grey85",
         main="Lead time de pedidos", xlab="semanas")
    dev.off()

    png(gp("pareto_ABC"), 900, 600)
    plot(seq_along(cum), 100*cum, type="l", lwd=2,
         xlab="SKU (ordenados por valor)", ylab="valor acumulado (%)",
         main="Curva de Pareto - clasificación ABC")
    abline(h=c(80,95), col="red", lty=2)
    dev.off()

    # Trayectoria de inventario de un SKU de clase A
    sku_A <- per_sku$SKU[per_sku$clase=="A"][1]
    dd <- d[d$SKU==sku_A, ]
    png(gp("trayectoria_SKU_A"), 900, 600)
    plot(dd$semana, dd$inventario_final, type="s", lwd=2,
         xlab="semana", ylab="inventario final",
         main=paste("Trayectoria de inventario -", sku_A))
    abline(h=dd$punto_reorden[1], col="red", lty=2)
    legend("topright","punto de reorden", col="red", lty=2, bty="n")
    dev.off()
    cat("Gráficos guardados en:", getwd(), "\n")
  }, error=function(e){ ok_png<<-FALSE; cat("   (Gráficos omitidos:", conditionMessage(e),")\n")})
}

## ===========================================================================
## 13. REPORTE FINAL Y DICTAMEN
## ===========================================================================
banner("RESUMEN DE HALLAZGOS")

h <- .hallazgos
# Solo se listan los checks CON violaciones + un conteo global
for (s in c("CRITICO","ADVERTENCIA","INFO")) {
  hs <- h[h$sev==s & h$n_viol>0, ]
  cat(sprintf("\n[%s]  %d check(s) con hallazgos\n", s, nrow(hs)))
  if (nrow(hs)) {
    for (i in seq_len(nrow(hs)))
      cat(sprintf("  - %-16s %s  (%d/%d viol.) %s\n",
                  hs$id[i], hs$desc[i], hs$n_viol[i], hs$n_eval[i], hs$evidencia[i]))
  }
}

n_crit <- sum(h$sev=="CRITICO"    & h$n_viol>0)
n_adv  <- sum(h$sev=="ADVERTENCIA" & h$n_viol>0)
n_ok   <- sum(h$n_viol==0)
cat(sprintf("\nChecks ejecutados: %d | superados: %d | CRÍTICOS: %d | ADVERTENCIAS: %d\n",
            nrow(h), n_ok, n_crit, n_adv))

banner("DICTAMEN DE APTITUD")
if (n_crit == 0) {
  cat("APTO PARA MODELADO PREDICTIVO.\n")
  cat("No se detectaron hallazgos críticos: el dataset es consistente con la\n")
  cat("lógica del generador (identidades contables, política (s,S) y costos).\n")
  if (n_adv > 0)
    cat(sprintf("Nota: %d advertencia(s) a revisar; no impiden el modelado.\n", n_adv))
} else {
  cat("NO APTO.\n")
  cat(sprintf("Se detectaron %d check(s) CRÍTICO(s) que rompen la consistencia\n", n_crit))
  cat("entre el algoritmo y el dataset. Corrija antes de modelar.\n")
}
cat(strrep("=", 78), "\n")
