---
title: "Interpolaci?n Mano Spline"
author: "Eddy Herrera Daza"
date: "31 de agosto de 2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Datos o Nodos

```{r,echo=T}
x=c(14.6, 14.7, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.5, 17.3, 16.8, 15.4, 14.8, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,              9.00, 9.30, 10, 10.2, 10.3, 10.0, 9.50)
y=c(14.7, 14.0, 13.4, 12.3, 11.0, 10.5, 10.2, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.50, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,              13.9, 14.9, 16, 16.4, 16.8, 10.7, 11.0)
plot(x,y, pch=19, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Diagrama de Nodos ")

x=c(14.6, 14.6, 14.8, 15.2, 15.7,17.0, 17.6, 17.3, 16.8, 15.4,14.4, 15.0, 14.9, 14.3, 14.0, 13.8, 13.1, 13.0, 13.3, 13.2, 12.4, 11.9, 11.7, 11.6, 11.3, 10.6, 10.1,  9.7, 9.4,  9.3,  9.9, 10.2, 10.0, 9.50,8.6,  7.5,  6.7,  6.6, 7.70, 8.00, 9.00, 10.3)
y=c(14.7, 13.4, 12.3, 11.0, 10.2,8.20, 7.10, 6.60, 6.80, 8.30,9.30, 6.30, 4.70, 4.50, 4.90, 5.80, 8.20, 7.60, 5.80, 4.50, 4.20, 5.70, 7.00, 7.90, 8.20,5.50, 4.60,  4.7, 5.00,  5.5,  7.8, 9.40, 10.7, 11.0, 9.90,  9.0, 9.30, 9.70, 11.7, 12.3, 13.9, 16.8)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama ")
x
plot(x,y,main = "Interpolación",asp=1)
vx1 = c(x[1:4])
vy1 = c(y[1:4])
splines = splinefun(vx1,vy1, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = vx1[1], to = vx1[length(vx1)])
vx2 = c(x[4:7])
vy2 = c(y[4:7])
splines = splinefun(vx2,vy2, method = "fmm")
curve(splines(x), add = TRUE, col = "green", from = vx2[1], to = vx2[length(vx2)])
vx3 = c(x[7:12])
vy3 = c(y[7:12])
splines = splinefun(vx3,vy3, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx3[1], to = vx3[length(vx3)])
vx4 = c(x[12:13])
vy4 = c(y[12:13])
splines = splinefun(vx4,vy4, method = "fmm")
curve(splines(x), add = TRUE, col = "green", from = vx4[1], to = vx4[length(vx4)])
vx5 = c(x[13:18])
vy5 = c(y[13:18])
splines = splinefun(vx5,vy5, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = vx5[1], to = vx5[length(vx5)])
vx6 = c(x[18:25])
vy6 = c(y[18:25])
splines = splinefun(vx6,vy6, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx6[1], to = vx6[length(vx6)])
vx7 = c(x[25:26])
vy7 = c(y[25:26])
splines = splinefun(vx7,vy7, method = "fmm")
curve(splines(x), add = TRUE, col = "green", from = vx7[1], to = vx7[length(vx7)])
vx8 = c(x[26:28])
vy8 = c(y[26:28])
splines = splinefun(vx8,vy8, method = "fmm")
curve(splines(x), add = TRUE, col = "red", from = vx8[1], to = vx8[length(vx8)])
vx9 = c(x[28:30])
vy9 = c(y[28:30])
splines = splinefun(vx9,vy9, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx9[1], to = vx9[length(vx9)])
vx10 = c(x[30:32])
vy10 = c(y[30:32])
splines = splinefun(vx10,vy10, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx10[1], to = vx10[length(vx10)])
vx11 = c(x[32:34])
vy11 = c(y[32:34])
splines = splinefun(vx11,vy11, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx11[1], to = vx11[length(vx11)])
vx12 = c(x[34:36])
vy12 = c(y[34:36])
splines = splinefun(vx12,vy12, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx12[1], to = vx12[length(vx12)])
vx13 = c(x[36:38])
vy13 = c(y[36:38])
splines = splinefun(vx13,vy13, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx13[1], to = vx13[length(vx13)])
vx14 = c(x[38:40])
vy14 = c(y[38:40])
splines = splinefun(vx14,vy14, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx14[1], to = vx14[length(vx14)])
vx15 = c(x[40:42])
vy15 = c(y[40:42])
splines = splinefun(vx15,vy15, method = "fmm")
curve(splines(x), add = TRUE, col = "blue", from = vx15[1], to = vx15[length(vx15)])


