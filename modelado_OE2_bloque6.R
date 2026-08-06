###############################################################################
#  OE2 - MODELADO PREDICTIVO · BLOQUE 6
#  Figuras de resultados a 300 dpi -> carpeta 'Figuras_OE2/'
###############################################################################
library(rpart)
tiene_rpartplot <- requireNamespace("rpart.plot", quietly = TRUE)
DIR <- "Figuras_OE2"; if (!dir.exists(DIR)) dir.create(DIR)
png2 <- function(f,w=9,h=6) png(file.path(DIR,f), width=w, height=h, units="in", res=300)
AZUL<-"#1F4E79"; NARANJA<-"#C57B2C"; GRIS<-"grey55"

if (!exists("dm"))   dm   <- { x<-read.csv("matriz_modelado_OE2.csv",encoding="UTF-8",
                               stringsAsFactors=FALSE); x$familia<-factor(x$familia,
                               levels=c("FER","EPP","HER")); x }
if (!exists("mlr"))   mlr   <- readRDS("mlr_OE2.rds")
if (!exists("arbol")) arbol <- readRDS("cart_OE2.rds")
if (!exists("res"))   res   <- read.csv("predicciones_OE2.csv",encoding="UTF-8",
                                        stringsAsFactors=FALSE)

## Figura 1: Árbol podado (rpart.plot si está disponible; si no, base R)
png2("Fig_OE2_1_arbol.png", 11, 7)
if (tiene_rpartplot) {
  rpart.plot::rpart.plot(arbol, type=2, extra=101, fallen.leaves=TRUE,
    box.palette="Blues", main="Árbol de decisión para regresión (CART) podado", cex=0.7)
} else {
  par(mar=c(1,1,3,1))
  plot(arbol, uniform=TRUE, margin=0.08,
       main="Árbol de decisión para regresión (CART) podado")
  text(arbol, use.n=TRUE, cex=0.6)
}
invisible(dev.off())

## Figura 2: WAPE por clase ABC, MLR vs CART
wape<-function(y,p)100*sum(abs(y-p))/sum(y)
wa <- sapply(c("A","B","C"), function(cl){ s<-res[res$clasificacion_ABC==cl,]
  c(MLR=wape(s$y,s$pred_mlr), CART=wape(s$y,s$pred_cart)) })
png2("Fig_OE2_2_wape_clase.png", 8, 5.5)
op<-par(mar=c(4.5,4.5,3,1))
bp<-barplot(wa, beside=TRUE, col=c(AZUL,NARANJA), border=NA, ylim=c(0,max(wa)*1.15),
        ylab="WAPE (%)", xlab="Clase ABC", main="Error de pronóstico (WAPE) por clase ABC")
legend("topright", c("MLR","CART"), fill=c(AZUL,NARANJA), border=NA, bty="n")
text(bp, wa, sprintf("%.1f", wa), pos=3, cex=0.8)
par(op); invisible(dev.off())

## Figura 3: Predicho vs observado
png2("Fig_OE2_3_pred_obs.png", 11, 5.5)
op<-par(mfrow=c(1,2), mar=c(4.5,4.5,3,1))
lim<-c(0, max(res$y, res$pred_mlr, res$pred_cart))
plot(res$y, res$pred_mlr, pch=16, col=rgb(0.12,0.31,0.47,0.35), xlim=lim, ylim=lim,
     xlab="Demanda observada", ylab="Demanda predicha", main="MLR"); abline(0,1,col="red",lwd=2)
plot(res$y, res$pred_cart, pch=16, col=rgb(0.77,0.48,0.17,0.35), xlim=lim, ylim=lim,
     xlab="Demanda observada", ylab="Demanda predicha", main="CART"); abline(0,1,col="red",lwd=2)
par(op); invisible(dev.off())

## Figura 4: Diagnóstico de residuales de la MLR
r <- residuals(mlr); fit <- fitted(mlr)
png2("Fig_OE2_4_diag_mlr.png", 11, 5.5)
op<-par(mfrow=c(1,2), mar=c(4.5,4.5,3,1))
plot(fit, r, pch=16, col=rgb(0.3,0.3,0.3,0.3), xlab="Valores ajustados",
     ylab="Residuales", main="Residuales vs ajustados (MLR)"); abline(h=0,col="red",lwd=2)
qqnorm(r, pch=16, col=rgb(0.3,0.3,0.3,0.3), main="Q-Q normal de residuales (MLR)")
qqline(r, col="red", lwd=2)
par(op); invisible(dev.off())

## Figura 5: Importancia de variables del CART
imp <- sort(100*arbol$variable.importance/sum(arbol$variable.importance))
png2("Fig_OE2_5_importancia.png", 8, 5.5)
op<-par(mar=c(4.5,8,3,1))
bp<-barplot(imp, horiz=TRUE, las=1, col=GRIS, border=NA, xlim=c(0,max(imp)*1.15),
        xlab="Importancia relativa (%)", main="Importancia de variables (CART)")
text(imp, bp, sprintf("%.1f", imp), pos=4, cex=0.8)
par(op); invisible(dev.off())

cat("rpart.plot disponible:", tiene_rpartplot, "\n")
cat("Figuras generadas en:", normalizePath(DIR), "\n")
cat(paste0("  - ", list.files(DIR)), sep="\n"); cat("\n")
