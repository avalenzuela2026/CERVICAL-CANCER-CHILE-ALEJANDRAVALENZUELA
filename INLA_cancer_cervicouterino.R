rm(list = ls())

# =========================================================
library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(stringi)
library(spdep)
library(Matrix)
library(INLA)
library(readxl)
library(ggplot2)

data_panel <- read_excel(
  "/Users/alevalenzuelasaldia/Desktop/BASE_INLA_COMPLETA.xlsx",
  sheet = "Sheet1")

# =========================
# 1) Parseo base y variables clave (SIN armar aún el panel)
# =========================
df_raw <- data_panel %>%
  mutate(
    ID = as.integer(IdRegion),
    AÑO = as.integer(AÑO),
    Semestre = factor(Semestre, levels = c("S1","S2"), ordered = TRUE),
    
    # binaria 0/1
    GrupoEdad_bin = case_when(
      GrupoEdad == "<25" ~ 0L,
      GrupoEdad == "25+" ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    Dx = as.integer(Dx),
    
    # numéricos (por si vienen como texto con coma)
    Ex        = as.numeric(str_replace(as.character(Ex), ",", ".")),
    Esperadas = as.numeric(str_replace(as.character(Esperadas), ",", ".")),
    
    PAP_total       = as.numeric(PAP_total),
    PAP_negativos   = as.numeric(PAP_negativos),
    PAP_inadecuados = as.numeric(PAP_inadecuados),
    PAP_positivos   = as.numeric(PAP_positivos)
  ) %>%
  filter(ID %in% 1:15) %>%
  filter(!is.na(ID), !is.na(AÑO), !is.na(Semestre), !is.na(GrupoEdad_bin))

stopifnot(all(df_raw$GrupoEdad_bin %in% c(0L, 1L)))

# =========================
# 2) Construir panel completo (16 semestres por ID y grupo)
# =========================
all_periods <- tidyr::expand_grid(
  AÑO = 2011:2018,
  Semestre = factor(c("S1","S2"), levels = c("S1","S2"), ordered = TRUE)
) %>%
  mutate(TIME = row_number())   # 1..16

skeleton <- tidyr::expand_grid(
  ID = sort(unique(df_raw$ID)),
  GrupoEdad_bin = sort(unique(df_raw$GrupoEdad_bin)),
  all_periods
)

df <- skeleton %>%
  left_join(df_raw, by = c("ID","GrupoEdad_bin","AÑO","Semestre")) %>%
  mutate(
    Dx = replace_na(Dx, 0L),
    
    # PAP faltante -> 0
    PAP_total       = replace_na(PAP_total, 0),
    PAP_negativos   = replace_na(PAP_negativos, 0),
    PAP_inadecuados = replace_na(PAP_inadecuados, 0),
    PAP_positivos   = replace_na(PAP_positivos, 0),
    
    # Ex inválido -> NA (si luego lo necesitas)
    Ex = if_else(is.na(Ex) | Ex <= 0, NA_real_, Ex)
    
    # Esperadas: NO se toca aquí (queda como venga: puede ser 0 o NA)
  ) %>%
  arrange(ID, GrupoEdad_bin, TIME)

# =========================
# 3) Covariable PAP (coherente con tu df0 actual)
#    tasa_pos = positivos / total PAP (0-1)
# =========================
df <- df %>%
  mutate(
    tasa_pos = if_else(PAP_total > 0, PAP_positivos / PAP_total, NA_real_)
  )
# =========================
# 4) IDTIME + chequeos panel
# =========================
stopifnot(n_distinct(df$TIME) == 16)
chk <- df %>% count(ID, GrupoEdad_bin)
stopifnot(all(chk$n == 16))

sp <- n_distinct(df$ID)
tm <- n_distinct(df$TIME)

df <- df %>%
  mutate(
    IDTIME = (ID - 1L) * tm + TIME
  )
stopifnot(all(df$IDTIME %in% 1:(sp * tm)))

cat("NA Esperadas:", sum(is.na(df$Esperadas)), "\n")
cat("Esperadas <= 0:", sum(df$Esperadas <= 0, na.rm = TRUE), "\n")

# =========================
# 5) df0 FINAL (el mismo formato que estabas usando)
# =========================
df0 <- df %>%
  mutate(
    tasa_pos = as.numeric(tasa_pos),              # 0-1
    tasa_pos_1pp = 100 * tasa_pos,                # 0-100
    GrupoEdad_bin = as.integer(GrupoEdad_bin),     # 0/1
    tasa_x_edad = tasa_pos_1pp * GrupoEdad_bin,
    GrupoEdad_lab = if_else(GrupoEdad_bin == 0, "<25", "25+")
  )

stopifnot(all(df0$GrupoEdad_bin %in% c(0L, 1L)))

View(df0)
# =========================================================
# 2) Shapefile -> IdRegion y matriz de vecindad (H)
# =========================================================
regiones_shp <- st_read(
  "/Users/alevalenzuelasaldia/Desktop/gadm41_CHL_shp(2)/gadm41_CHL_1.shp"
)

norm_nom <- function(x){
  x %>%
    str_squish() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper()
}

regiones_shp <- regiones_shp %>%
  mutate(
    REGION_STD = norm_nom(NAME_1),
    REGION_STD = str_replace_all(REGION_STD, "BIO-BIO", "BIOBIO"),
    REGION_STD = str_replace_all(REGION_STD, "^ARAUCANIA$", "LA ARAUCANIA"),
    REGION_STD = str_replace_all(REGION_STD, "^SANTIAGO METROPOLITAN$", "METROPOLITANA"),
    REGION_STD = str_replace_all(REGION_STD, "^AYSEN.*", "AYSEN"),
    REGION_STD = str_replace_all(REGION_STD, "^MAGALLANES.*", "MAGALLANES"),
    REGION_STD = str_replace_all(REGION_STD, "^LIBERTADOR.*O'HI.*", "O'HIGGINS")
  )

region_lookup <- tibble(
  REGION_STD = c("TARAPACA","ANTOFAGASTA","ATACAMA","COQUIMBO","VALPARAISO",
                 "O'HIGGINS","MAULE","BIOBIO","LA ARAUCANIA","LOS LAGOS",
                 "AYSEN","MAGALLANES","METROPOLITANA","LOS RIOS",
                 "ARICA Y PARINACOTA","NUBLE"),
  IdRegion = 1:16
)

regiones_shp <- regiones_shp %>%
  left_join(region_lookup, by = "REGION_STD")

if (any(is.na(regiones_shp$IdRegion))) {
  print(regiones_shp %>%
          filter(is.na(IdRegion)) %>%
          st_drop_geometry() %>%
          select(NAME_1, REGION_STD))
  stop("Aún quedan regiones sin mapear.")
}

# Colapsar Ñuble (16) -> Biobío (8) para trabajar con 15 regiones
regiones_shp_15 <- regiones_shp %>%
  mutate(IdRegion = if_else(IdRegion == 16L, 8L, IdRegion)) %>%
  group_by(IdRegion) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  arrange(IdRegion)

reg_sp <- as(regiones_shp_15, "Spatial")
nb <- poly2nb(reg_sp, queen = TRUE)

#nb2INLA("reg_ant.adj", nb)
H <- inla.read.graph("reg_ant.adj")
#inla.write.graph(H, file = "reg_ant.graph")
stopifnot(H$n == 15)

# =========================================================
# 3) Visualización opcional de la matriz de vecindad W
# =========================================================
W <- as.matrix(inla.graph2matrix(H))
diag(W) <- 0

dfW <- as.data.frame(W) %>%
  mutate(i = row_number()) %>%
  pivot_longer(-i, names_to = "j", values_to = "adj") %>%
  mutate(j = as.integer(gsub("V", "", j)))

vecinos_tab <- region_lookup %>%
  filter(IdRegion <= 15) %>%
  arrange(IdRegion) %>%
  mutate(n_vecinos = sapply(nb, length)) %>%
  select(IdRegion, REGION_STD, n_vecinos)

vecinos_tab


### GRAFICO DE LA MATRIZ CON LAS VECINDADES
W <- as.matrix(inla.graph2matrix(H))

diag(W) <- 0
n_vecinos <- rowSums(W)
W_deg <- W
colnames(W) <- paste0("R", 1:15)
rownames(W) <- paste0("R", 1:15)
diag(W_deg) <- n_vecinos
dfW_deg <- as.data.frame(W_deg) %>%
  tibble::rownames_to_column("i") %>%
  pivot_longer(-i, names_to = "j", values_to = "valor")


dfW_deg <- dfW_deg %>%
  mutate(
    i = factor(i, levels = rev(unique(i))),
    j = factor(j, levels = unique(j))
  )


ggplot(dfW_deg, aes(x = j, y = i, fill = factor(valor))) +
  geom_tile(color = "grey80") +
  coord_equal() +
  scale_fill_manual(
    name = "N° de vecinas",
    values = c(
      "0" = "#E0FFFF",
      "1" = "#99EDFF",
      "2" = "#4CC3FF",
      "3" = "#007FFF"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Matriz de vecindad con número de regiones vecinas",
    x = "Región j",
    y = "Región i"
  )
names(df0)

# =========================================================
# 4) Modelos base INLA (Poisson con offset E=Ex)
# =========================================================
m0 <- inla(
  Dx ~ 1,
  family = "poisson",
  data = df0, E = Esperadas,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

mT <- inla(
  Dx ~ 1 + f(TIME, model = "rw1"),
  family = "poisson",
  data = df0, E = Esperadas,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE),
  control.inla      = list(strategy = "laplace")
)

mS <- inla(
  Dx ~ 1 +
    f(ID, model = "bym2", graph = H, scale.model = TRUE,
      hyper = list(
        phi  = list(prior = "pc",      param = c(0.5, 2/3)),
        prec = list(prior = "pc.prec", param = c(0.75, 0.05))
      )
    ),
  family = "poisson",
  data = df0, E = Esperadas,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE),
  control.inla      = list(strategy = "laplace")
)

mST <- inla(
  Dx ~ 1 +
    GrupoEdad +
    tasa_pos_1pp +
    f(ID, model = "bym2", graph = H, scale.model = TRUE,
      hyper = list(
        phi  = list(prior = "pc",      param = c(0.5, 2/3)),
        prec = list(prior = "pc.prec", param = c(0.75, 0.05))
      )
    ) +
    f(TIME, model = "rw1"),
  family = "poisson",
  data = df0, E = Esperadas,
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE,
                           return.marginals.predictor = TRUE),
  control.inla      = list(strategy = "laplace")
)

cmp <- tibble(
  modelo = c("Nulo", "Tiempo (RW1)", "Espacio (BYM2)", "Espacio+Tiempo+Cov"),
  DIC  = c(m0$dic$dic, mT$dic$dic, mS$dic$dic, mST$dic$dic),
  WAIC = c(m0$waic$waic, mT$waic$waic, mS$waic$waic, mST$waic$waic)
)

fix <- mST$summary.fixed %>%
  mutate(RR = exp(mean),
         RR_low = exp(`0.025quant`),
         RR_high = exp(`0.975quant`))

# =========================================================
# 5) Interacción espacio-tiempo (Knorr-Held I–IV) via generic0
# =========================================================
sp <- n_distinct(df$ID)
tm <- n_distinct(df$TIME)
stopifnot(identical(sort(unique(df$ID)),   1:sp))
stopifnot(identical(sort(unique(df$TIME)), 1:tm))

df0 <- df0 %>% mutate(IDTIME = (ID - 1L) * tm + TIME)
stopifnot(all(df$IDTIME %in% 1:(sp * tm)))

NSp <- Diagonal(sp)
NTm <- Diagonal(tm)

Wg  <- inla.graph2matrix(H)
Qs <- Diagonal(nrow(Wg), Matrix::colSums(Wg)) - Wg
Qs <- as(as(Qs, "generalMatrix"), "CsparseMatrix")

Dt <- diff(diag(tm), differences = 1)
Qt <- t(Dt) %*% Dt
Qt <- as(as(Matrix(Qt), "generalMatrix"), "CsparseMatrix")

C1 <- as(kronecker(NSp, NTm), "CsparseMatrix")
C2 <- as(kronecker(NSp, Qt),  "CsparseMatrix")
C3 <- as(kronecker(Qs,  NTm), "CsparseMatrix")
C4 <- as(kronecker(Qs,  Qt),  "CsparseMatrix")

make_A_inla <- function(R, tol = 1e-10){
  eg <- eigen(as.matrix(R), symmetric = TRUE)
  idx0 <- abs(eg$values) < tol
  t(eg$vectors[, idx0, drop = FALSE])
}

A2 <- make_A_inla(C2)
A3 <- make_A_inla(C3)
A4 <- make_A_inla(C4)

base_fixed <- Dx ~ 1 + GrupoEdad + tasa_pos_1pp +
  f(ID, model = "bym2", graph = H, scale.model = TRUE,
    hyper = list(
      phi  = list(prior = "pc",      param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05))
    )
  ) +
  f(TIME, model = "rw1")

mI <- inla(
  update(base_fixed, . ~ . + f(IDTIME, model = "generic0", Cmatrix = C1)),
  family = "poisson", data = df0, E = Esperadas,
  control.fixed = list(mean = 0, prec = 0.0001, mean.intercept = 0, prec.intercept = 0.0001),
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE),
  control.inla      = list(strategy = "laplace")
)

mII <- inla(
  Dx ~ 1 + GrupoEdad + tasa_pos_1pp +
    f(ID, model = "bym2", graph = H, scale.model = TRUE,
      hyper = list(
        phi  = list(prior = "pc",      param = c(0.5, 2/3)),
        prec = list(prior = "pc.prec", param = c(0.75, 0.05))
      )
    ) +
    f(TIME, model = "rw1") +
    f(IDTIME, model = "generic0", Cmatrix = C2,
      constr = TRUE,
      extraconstr = list(A = A2, e = rep(1e-5, nrow(A2)))
    ),
  family = "poisson",
  data   = df0,
  E      = Esperadas,
  control.fixed = list(mean = 0, prec = 0.0001, mean.intercept = 0, prec.intercept = 0.0001),
  control.predictor = list(compute = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.inla      = list(strategy = "laplace")
)

mIII <- inla(
  update(base_fixed, . ~ . +
           f(IDTIME, model="generic0", Cmatrix=C3, constr=TRUE,
             extraconstr=list(A=A3, e=rep(1e-5, nrow(A3)))
           )),
  family="poisson", data=df0, E=Esperadas,
  control.fixed = list(mean = 0, prec = 0.0001, mean.intercept = 0, prec.intercept = 0.0001),
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE),
  control.inla=list(strategy="laplace")
)

mIV <- inla(
  update(base_fixed, . ~ . +
           f(IDTIME, model="generic0", Cmatrix=C4, constr=TRUE,
             extraconstr=list(A=A4, e=rep(1e-5, nrow(A4)))
           )),
  family="poisson", data=df0, E=Esperadas,
  control.fixed = list(mean = 0, prec = 0.0001, mean.intercept = 0, prec.intercept = 0.0001),
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE),
  control.inla=list(strategy="laplace")
)

cmp2 <- tibble(
  modelo = c("mST (sin interaccion)", "Knorr-Held I", "Knorr-Held II", "Knorr-Held III", "Knorr-Held IV"),
  DIC  = c(mST$dic$dic, mI$dic$dic, mII$dic$dic, mIII$dic$dic, mIV$dic$dic),
  WAIC = c(mST$waic$waic, mI$waic$waic, mII$waic$waic, mIII$waic$waic, mIV$waic$waic)
)

summary(mIV)
# =========================================================
# 7) Hiperparámetros BYM2 y posterior (phi, precision)
# =========================================================
phi_marg  <- mIV$marginals.hyperpar[["Phi for ID"]]
prec_marg <- mIV$marginals.hyperpar[["Precision for ID"]]

plot(phi_marg, type="l",
     main="Posterior phi (BYM2) – Modelo KH IV",
     xlab="phi", ylab="densidad")

plot(prec_marg, type="l",
     main="Posterior Precision(ID) – Modelo KH IV",
     xlab="precisión", ylab="densidad")

phi_sum <- mIV$summary.hyperpar["Phi for ID", c("mean","0.025quant","0.5quant","0.975quant")]
phi_sum


# =========================================================
#Sensibilidad de priors (phi/prec) – MODELO FINAL KH IV

stopifnot(exists("df0"), exists("H"), exists("C4"), exists("A4"))
stopifnot(all(c("Dx","Esperadas","ID","TIME","IDTIME","GrupoEdad","tasa_pos_1pp") %in% names(df0)))


# (A) BYM2: phi + precision espacial (PC priors)
hyper_base <- list(
  phi  = list(prior="pc",      param=c(0.5, 2/3)),
  prec = list(prior="pc.prec", param=c(0.75, 0.05))
)
hyper_lowphi <- list(
  phi  = list(prior="pc",      param=c(0.2, 2/3)),
  prec = list(prior="pc.prec", param=c(0.75, 0.05))
)
hyper_highphi <- list(
  phi  = list(prior="pc",      param=c(0.8, 2/3)),
  prec = list(prior="pc.prec", param=c(0.75, 0.05))
)

hyper_moreprec <- list(
  phi  = list(prior="pc",      param=c(0.5, 2/3)),
  prec = list(prior="pc.prec", param=c(1.0, 0.05))
)
hyper_lessprec <- list(
  phi  = list(prior="pc",      param=c(0.5, 2/3)),
  prec = list(prior="pc.prec", param=c(0.5, 0.05))
)

# (B) RW1 TIME: precision prior
rw_base <- list(prec = list(prior="pc.prec", param=c(1.0, 0.05)))
rw_more <- list(prec = list(prior="pc.prec", param=c(2.0, 0.05)))
rw_less <- list(prec = list(prior="pc.prec", param=c(0.5, 0.05)))

# (C) IDTIME (generic0): precision prior
idtime_base <- list(prec = list(prior="pc.prec", param=c(1.0, 0.05)))
idtime_more <- list(prec = list(prior="pc.prec", param=c(2.0, 0.05)))
idtime_less <- list(prec = list(prior="pc.prec", param=c(0.5, 0.05)))

# (D) Betas (incluye intercepto): Normal(0, var)
beta_vars <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)

make_fixed <- function(var){
  list(
    mean = 0,
    prec = 1/var,
    mean.intercept = 0,
    prec.intercept = 1/var
  )
}

fixed_list <- setNames(
  lapply(beta_vars, make_fixed),
  paste0("BETA_var_", format(beta_vars, scientific = FALSE))
)

fixed_lab <- setNames(
  paste0("β: N(0, var=", format(beta_vars, scientific = FALSE), ")"),
  names(fixed_list)
)

# -------------------------------
# 2) Función: ajustar el MISMO modelo final KH IV
# -------------------------------
fit_mIV <- function(hyp_bym2, hyp_rw1, hyp_idtime, fixed_ctrl){
  inla(
    Dx ~ 1 + GrupoEdad + tasa_pos_1pp +  tasa_x_edad + 
      f(ID, model="bym2", graph=H, scale.model=TRUE, hyper=hyp_bym2) +
      f(TIME, model="rw1", hyper=hyp_rw1) +
      f(IDTIME, model="generic0", Cmatrix=C4, hyper=hyp_idtime,
        constr=TRUE,
        extraconstr=list(A=A4, e=rep(1e-5, nrow(A4)))
      ),
    family="poisson", data=df0, E=Esperadas,
    control.fixed = fixed_ctrl,
    control.predictor = list(compute=TRUE),
    control.compute   = list(dic=TRUE, waic=TRUE),
    control.inla      = list(strategy="laplace")
  )
}

# -------------------------------
# 3) Ajuste BASE + escenarios
# -------------------------------

# Base: betas difusas (var=1000)
m_base <- fit_mIV(hyper_base, rw_base, idtime_base, fixed_list[["BETA_var_1000"]])

# --- Sensibilidad BETAS: solo cambia var(betas), resto fijo
beta_models <- lapply(names(fixed_list), function(tag){
  fit_mIV(hyper_base, rw_base, idtime_base, fixed_list[[tag]])
})
names(beta_models) <- names(fixed_list)

# --- Sensibilidad hiperparámetros (mantengo betas var=1000)
m_phi_low   <- fit_mIV(hyper_lowphi,  rw_base, idtime_base, fixed_list[["BETA_var_1000"]])
m_phi_high  <- fit_mIV(hyper_highphi, rw_base, idtime_base, fixed_list[["BETA_var_1000"]])

m_prec_more <- fit_mIV(hyper_moreprec, rw_base, idtime_base, fixed_list[["BETA_var_1000"]])
m_prec_less <- fit_mIV(hyper_lessprec, rw_base, idtime_base, fixed_list[["BETA_var_1000"]])

m_time_more <- fit_mIV(hyper_base, rw_more, idtime_base, fixed_list[["BETA_var_1000"]])
m_time_less <- fit_mIV(hyper_base, rw_less, idtime_base, fixed_list[["BETA_var_1000"]])

m_it_more   <- fit_mIV(hyper_base, rw_base, idtime_more, fixed_list[["BETA_var_1000"]])
m_it_less   <- fit_mIV(hyper_base, rw_base, idtime_less, fixed_list[["BETA_var_1000"]])

# Unimos todo
models <- c(
  list(BASE = m_base),
  beta_models,
  list(
    PHI_low     = m_phi_low,
    PHI_high    = m_phi_high,
    PREC_more   = m_prec_more,
    PREC_less   = m_prec_less,
    TIME_more   = m_time_more,
    TIME_less   = m_time_less,
    IDTIME_more = m_it_more,
    IDTIME_less = m_it_less
  )
)

lab <- c(
  BASE = "Base (β var=1000)",
  fixed_lab,
  PHI_low     = "φ: PC(U=0.2, α=2/3)",
  PHI_high    = "φ: PC(U=0.8, α=2/3)",
  PREC_more   = "prec(ID): PCprec(U=1.0, α=0.05)",
  PREC_less   = "prec(ID): PCprec(U=0.5, α=0.05)",
  TIME_more   = "prec(TIME): PCprec(U=2.0, α=0.05)",
  TIME_less   = "prec(TIME): PCprec(U=0.5, α=0.05)",
  IDTIME_more = "prec(IDTIME): PCprec(U=2.0, α=0.05)",
  IDTIME_less = "prec(IDTIME): PCprec(U=0.5, α=0.05)"
)

# -------------------------------
# 4) Tabla DIC/WAIC
# -------------------------------
fit_cmp <- tibble(
  escenario = names(models),
  Prior = unname(lab[names(models)]),
  DIC  = sapply(models, \(m) m$dic$dic),
  WAIC = sapply(models, \(m) m$waic$waic)
)
fit_cmp

# -------------------------------
# 5) Extraer densidades posteriores (fixed + hyper)
# -------------------------------
marg_to_df <- function(marg, escenario, parametro){
  as.data.frame(marg) %>%
    setNames(c("x","dens")) %>%
    mutate(
      escenario = escenario,
      Prior = unname(lab[escenario]),
      parametro = parametro
    )
}

# NOMBRES actualizados según tu summary: GrupoEdad25+ y tasa_pos_1pp
fixed_targets <- c("(Intercept)", "GrupoEdad25+", "tasa_pos_1pp")
hyper_targets <- c("Phi for ID", "Precision for ID", "Precision for TIME", "Precision for IDTIME")

fixed_dens_df <- bind_rows(lapply(names(models), function(tag){
  m <- models[[tag]]
  bind_rows(lapply(fixed_targets, function(pn){
    marg <- m$marginals.fixed[[pn]]
    if(is.null(marg)) return(NULL)
    marg_to_df(marg, tag, pn)
  }))
}))

hyper_dens_df <- bind_rows(lapply(names(models), function(tag){
  m <- models[[tag]]
  bind_rows(lapply(hyper_targets, function(hn){
    marg <- m$marginals.hyperpar[[hn]]
    if(is.null(marg)) return(NULL)
    marg_to_df(marg, tag, hn)
  }))
}))

# -------------------------------
# 6) Función para plotear densidades posteriores superpuestas
# -------------------------------
plot_post <- function(df_plot, param_name, keep_tags, title, xlab=NULL){
  df2 <- df_plot %>% filter(parametro == param_name, escenario %in% keep_tags)
  ggplot(df2, aes(x = x, y = dens, color = Prior, linetype = Prior)) +
    geom_line(linewidth = 0.9) +
    theme_minimal() +
    labs(title = title, x = xlab, y = "Densidad Posterior",
         color="Prior", linetype="Prior")
}

# -------------------------------
# 7) GRÁFICOS
# -------------------------------

# (A) Betas: sensibilidad por varianza
keep_beta <- names(fixed_list)

print(plot_post(
  fixed_dens_df, "(Intercept)", keep_beta,
  "Sensibilidad posterior de b0 (Intercepto) a var(β)",
  xlab="B0"
))

print(plot_post(
  fixed_dens_df, "GrupoEdad25+", keep_beta,
  "Sensibilidad posterior de β(GrupoEdad25+) a var(β)",
  xlab="β edad"
))

print(plot_post(
  fixed_dens_df, "tasa_pos_1pp", keep_beta,
  "Sensibilidad posterior de β(PAP positivos por 1pp) a var(β)",
  xlab="β PAP (por 1pp)"
))

# (B) φ
keep_phi <- c("BASE","PHI_low","PHI_high")
print(plot_post(
  hyper_dens_df, "Phi for ID", keep_phi,
  "Sensibilidad posterior de φ (BYM2) al prior",
  xlab=expression(phi)
))

# (C) prec(ID)
keep_precID <- c("BASE","PREC_more","PREC_less")
print(plot_post(
  hyper_dens_df, "Precision for ID", keep_precID,
  "Sensibilidad posterior de Precisión espacial (ID) al prior",
  xlab="Precisión ID"
))

# (D) prec(TIME)
keep_time <- c("BASE","TIME_more","TIME_less")
print(plot_post(
  hyper_dens_df, "Precision for TIME", keep_time,
  "Sensibilidad posterior de Precisión temporal (RW1) al prior",
  xlab="Precisión TIME"
))

# (E) prec(IDTIME)
keep_it <- c("BASE","IDTIME_more","IDTIME_less")
print(plot_post(
  hyper_dens_df, "Precision for IDTIME", keep_it,
  "Sensibilidad posterior de Precisión de interacción (IDTIME) al prior",
  xlab="Precisión IDTIME"
))

# =========================================================
# 9) Probabilidad posterior P(RR > c) por región y tiempo

library(paletteer)

RM_ID <- 13L

# ---- 1) Modelo (mIV_prob) con pendiente de tasa indexada por edad
mIV_prob <- inla(
  Dx ~ 1 +
    GrupoEdad_bin +          # diferencia de intercepto (25+ vs <25)
    tasa_pos_1pp +           # pendiente para <25 (GrupoEdad_bin=0)
    tasa_x_edad +            # delta de pendiente para 25+ (vs <25)
    f(ID, model="bym2", graph=H, scale.model=TRUE,
      hyper=list(
        phi  = list(prior="pc", param=c(0.5, 2/3)),
        prec = list(prior="pc.prec", param=c(0.75, 0.05))
      )) +
    f(TIME, model="rw1") +
    f(IDTIME, model="generic0", Cmatrix=C4, constr=TRUE,
      extraconstr=list(A=A4, e=rep(1e-5, nrow(A4)))
    ),
  family="poisson",
  data=df0,
  E=Esperadas,
  control.predictor=list(compute=TRUE),
  control.compute=list(return.marginals.predictor=TRUE),
  control.inla=list(strategy="laplace")
)
summary(mIV_prob)

eta_marg <- mIV_prob$marginals.linear.predictor
stopifnot(length(eta_marg) == nrow(df0))

# ---- 2) Función: P(RR > c) = P(exp(eta) > c) = 1 - F_eta(log c)
prob_RR_gt <- function(eta_marg, c){
  vapply(seq_along(eta_marg),
         \(i) 1 - INLA::inla.pmarginal(log(c), eta_marg[[i]]),
         numeric(1))
}

# ---- 3) Umbrales por grupo (ajusta si quieres más contraste)
c_lt25 <- 1
c_25p  <- 1

idx_lt25 <- df0$GrupoEdad_bin == 0L
idx_25p  <- df0$GrupoEdad_bin == 1L

df0 <- df0 %>%
  mutate(P_RR_gt_grp = NA_real_)

df0$P_RR_gt_grp[idx_lt25] <- prob_RR_gt(eta_marg[idx_lt25], c_lt25)
df0$P_RR_gt_grp[idx_25p]  <- prob_RR_gt(eta_marg[idx_25p],  c_25p)

summary(df0$P_RR_gt_grp[idx_25p])
# ---- 4) Resumen semestral por región (promedio de probabilidades dentro del semestre)
df_sem <- df0 %>%
  mutate(
    IdRegion = ID,
    AÑO = factor(AÑO),
    Semestre = factor(Semestre, levels=c("S1","S2")),
    GrupoEdad_lab = factor(GrupoEdad_lab, levels=c("<25","25+")),
    P_plot = if_else(IdRegion == RM_ID, NA_real_, P_RR_gt_grp)
  )

summary(df_sem$P_plot[df_sem$GrupoEdad_lab == "25+"])

map_sem <- regiones_shp_15 %>%
  left_join(df_sem, by="IdRegion")

# ---- 5) Escala de color continua 0-1 con paleta Tropic
pal_tropic <- as.character(paletteer::paletteer_c("grDevices::Tropic", 30))

fill_scale <- scale_fill_gradientn(
  colours = pal_tropic,
  limits = c(0, 1),
  oob = scales::squish,
  breaks = seq(0, 1, by = 0.1),
  labels = scales::number_format(accuracy = 0.1),
  name = "P(RR>c)"
)

# ---- 6) Mapas separados
p_lt25 <- ggplot(subset(map_sem, GrupoEdad_lab == "<25")) +
  geom_sf(aes(fill = P_plot), color="grey30", linewidth=0.2) +
  fill_scale +
  facet_grid(Semestre ~ AÑO) +
  labs(title = paste0("<25: Probabilidad posterior de RR>", c_lt25),
       subtitle = "RM (referencia) en gris (NA)") +
  theme_minimal() +
  theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank())

p_25p <- ggplot(subset(map_sem, GrupoEdad_lab == "25+")) +
  geom_sf(aes(fill = P_plot), color="grey30", linewidth=0.2) +
  fill_scale +
  facet_grid(Semestre ~ AÑO) +
  labs(title = paste0("25+: Probabilidad posterior de RR>", c_25p),
       subtitle = "RM (referencia) en gris (NA)") +
  theme_minimal() +
  theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank())


####mIV_prob$summary.linear.predictor %>% plot.ts()
########################
comp_age <- df %>%
  group_by(ID, AÑO, Semestre, GrupoEdad_bin) %>%
  summarise(
    Dx_sum  = sum(Dx, na.rm = TRUE),
    Esp_sum = sum(Esperadas, na.rm = TRUE),
    diff    = Dx_sum - Esp_sum,
    ratio   = if_else(Esp_sum > 0, Dx_sum / Esp_sum, NA_real_),
    sobrepasa = Dx_sum > Esp_sum,
    .groups = "drop"
  ) %>%
  arrange(ID, AÑO, Semestre, GrupoEdad_bin)

comp_age
View(comp_age)
write_xlsx(
  list(
    Comparacion_Por_Edad = comp_age
  ),
  "comparacion_Dx_vs_Esperadas.xlsx"
)


# =========================
# 1) Fórmula base con "doble intercepto + pendiente distinta"
# =========================
base_fixed2 <- Dx ~ 1 +
  GrupoEdad_bin +      # diferencia de intercepto (25+ vs <25)
  tasa_pos_1pp +       # pendiente para <25
  tasa_x_edad +        # delta de pendiente para 25+
  f(ID, model="bym2", graph=H, scale.model=TRUE,
    hyper=list(
      phi  = list(prior="pc",      param=c(0.5, 2/3)),
      prec = list(prior="pc.prec", param=c(0.75, 0.05))
    )
  ) +
  f(TIME, model="rw1")

ctrl_fixed <- list(
  mean = 0, prec = 0.0001,
  mean.intercept = 0, prec.intercept = 0.0001
)

# =========================
# 2) Modelos para comparar (mST2, mI2..mIV2)
# =========================

# (a) sin interacción espacio-tiempo (equivalente a tu mST pero con fijos nuevos)
mST2 <- inla(
  base_fixed2,
  family="poisson",
  data=df0, E=Esperadas,
  control.fixed=ctrl_fixed,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE),
  control.inla=list(strategy="laplace")
)

# KH-I (C1)
mI2 <- inla(
  update(base_fixed2, . ~ . + f(IDTIME, model="generic0", Cmatrix=C1)),
  family="poisson",
  data=df0, E=Esperadas,
  control.fixed=ctrl_fixed,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE),
  control.inla=list(strategy="laplace")
)

# KH-II (C2) con constraints
mII2 <- inla(
  update(base_fixed2, . ~ . +
           f(IDTIME, model="generic0", Cmatrix=C2,
             constr=TRUE,
             extraconstr=list(A=A2, e=rep(1e-5, nrow(A2)))
           )),
  family="poisson",
  data=df0, E=Esperadas,
  control.fixed=ctrl_fixed,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE),
  control.inla=list(strategy="laplace")
)

# KH-III (C3) con constraints
mIII2 <- inla(
  update(base_fixed2, . ~ . +
           f(IDTIME, model="generic0", Cmatrix=C3,
             constr=TRUE,
             extraconstr=list(A=A3, e=rep(1e-5, nrow(A3)))
           )),
  family="poisson",
  data=df0, E=Esperadas,
  control.fixed=ctrl_fixed,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE),
  control.inla=list(strategy="laplace")
)

# KH-IV (C4) con constraints
mIV2 <- inla(
  update(base_fixed2, . ~ . +
           f(IDTIME, model="generic0", Cmatrix=C4,
             constr=TRUE,
             extraconstr=list(A=A4, e=rep(1e-5, nrow(A4)))
           )),
  family="poisson",
  data=df0, E=Esperadas,
  control.fixed=ctrl_fixed,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE),
  control.inla=list(strategy="laplace")
)

# =========================
# 3) Tabla comparación DIC / WAIC
# =========================
cmp2_new <- tibble(
  modelo = c("mST2 (sin interaccion; fijos nuevos)",
             "Knorr-Held I (fijos nuevos)",
             "Knorr-Held II (fijos nuevos)",
             "Knorr-Held III (fijos nuevos)",
             "Knorr-Held IV (fijos nuevos)"),
  DIC  = c(mST2$dic$dic,  mI2$dic$dic,  mII2$dic$dic,  mIII2$dic$dic,  mIV2$dic$dic),
  WAIC = c(mST2$waic$waic, mI2$waic$waic, mII2$waic$waic, mIII2$waic$waic, mIV2$waic$waic)
) %>% arrange(WAIC)

cmp2
cmp2_new
summary(mIV2)
summary(mIV)


######################

# =========================================================
# ==========================================
# MAPAS 4-en-1: SOLO GUARDAR IMAGEN
# ==========================================

library(patchwork)

RM_ID <- 13L
thr_p <- 0.85

# Paleta continua
pal_tropic <- as.character(paletteer::paletteer_c("grDevices::Tropic", 30))

fill_scale_cont <- scale_fill_gradientn(
  colours = pal_tropic,
  limits = c(0, 1),
  oob = scales::squish,
  breaks = seq(0, 1, by = 0.1),
  labels = scales::number_format(accuracy = 0.1),
  name = "P(RR>c)"
)

# Escala binaria
fill_scale_bin <- scale_fill_manual(
  values = c("NO" = "#009B9F", "SI" = "#C75DAA"),
  limits = c("NO", "SI"),
  drop = FALSE,
  na.value = "grey85",
  name = paste0("Sobremortalidad\n(P > ", thr_p, ")")
)

make_4maps_yearblock <- function(years_vec, suf){
  
  dat_block <- map_sem %>%
    filter(AÑO %in% as.character(years_vec)) %>%
    mutate(
      Sobremort = case_when(
        is.na(P_plot) ~ NA_character_,
        P_plot > thr_p ~ "SI",
        TRUE ~ "NO"
      ),
      Sobremort = factor(Sobremort, levels = c("NO","SI"))
    )
  
  base_theme <- theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  p_lt25_cont <- ggplot(subset(dat_block, GrupoEdad_lab == "<25")) +
    geom_sf(aes(fill = P_plot), color="grey30", linewidth=0.2) +
    fill_scale_cont +
    facet_grid(Semestre ~ AÑO) +
    labs(title = "<25 | Probabilidad") +
    base_theme
  
  p_lt25_bin <- ggplot(subset(dat_block, GrupoEdad_lab == "<25")) +
    geom_sf(aes(fill = Sobremort), color="grey30", linewidth=0.2) +
    fill_scale_bin +
    facet_grid(Semestre ~ AÑO) +
    labs(title = "<25 | SI / NO") +
    base_theme
  
  p_25p_cont <- ggplot(subset(dat_block, GrupoEdad_lab == "25+")) +
    geom_sf(aes(fill = P_plot), color="grey30", linewidth=0.2) +
    fill_scale_cont +
    facet_grid(Semestre ~ AÑO) +
    labs(title = "25+ | Probabilidad") +
    base_theme
  
  p_25p_bin <- ggplot(subset(dat_block, GrupoEdad_lab == "25+")) +
    geom_sf(aes(fill = Sobremort), color="grey30", linewidth=0.2) +
    fill_scale_bin +
    facet_grid(Semestre ~ AÑO) +
    labs(title = "25+ | SI / NO") +
    base_theme
  
  p_all <- (p_lt25_cont | p_lt25_bin) / (p_25p_cont | p_25p_bin)
  
  ggsave(
    filename = paste0("prob_sobre_4maps_", suf, ".png"),
    plot = p_all,
    width = 16,
    height = 10,
    dpi = 200
  )
  
  invisible(NULL)  # <- no imprime nada
}

# =========================
# GUARDAR BLOQUES
# =========================

make_4maps_yearblock(c(2011, 2012), "2011_2012")
make_4maps_yearblock(c(2013, 2014), "2013_2014")
make_4maps_yearblock(c(2015, 2016), "2015_2016")
make_4maps_yearblock(c(2017, 2018), "2017_2018")



#########  ANALISIS DESCRIPTIVO ENTRE EDADES #####
df_plot <- df0 %>%
  group_by(AÑO, Semestre, GrupoEdad_lab) %>%
  summarise(Dx = sum(Dx, na.rm=TRUE), .groups="drop") %>%
  mutate(periodo = interaction(AÑO, Semestre, lex.order=TRUE))


ggplot(df_plot,
       aes(x=periodo, y=Dx, fill=GrupoEdad_lab)) +
  geom_col(position="dodge") +
  theme_minimal() +
  labs(title="Fallecidas por grupo etario y periodo",
       x="Periodo",
       y="Número de fallecidas",
       fill="Grupo etario") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(df_plot,
       aes(x = periodo, y = Dx, fill = GrupoEdad_lab)) +
  
  geom_col(position = position_dodge(width = 0.9)) +
  
  geom_text(
    aes(label = Dx),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "25+" = "#27CED7",   # Turquesa
      "<25" = "#98D9E4"    # Verde mar
    )
  ) +
  
  theme_minimal() +
  labs(
    title = "Fallecidas por grupo etario y periodo",
    x = "Periodo",
    y = "Número de mujeres fallecidas",
    fill = "Grupo etario"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
