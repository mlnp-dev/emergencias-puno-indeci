# =============================================================================
#  Tablero de analítica y BI: emergencias por peligros en la región Puno
#  Fuente: SINPAD, Instituto Nacional de Defensa Civil (INDECI), 2003 a 2026
#  Curso: Business Intelligence and Data Analytics, Actividad semana 4
#  Ejecutar desde esta carpeta:  shiny::runApp()
# =============================================================================
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(DT)
library(scales)
library(sf)
library(leaflet)


# ---- Datos ------------------------------------------------------------------
D <- function(x) file.path("data", x)
emer <- read_csv(D("emergencias_puno.csv"), show_col_types = FALSE,
                 col_types = cols(UBIGEO = "c", FECHA = col_date()))
emer <- emer |> mutate(MES_NUM = as.integer(format(FECHA, "%m")),
                       VIV_DESTRUIDAS = coalesce(VIV_DESTRUIDAS, 0))
mapa_puno <- st_read(D("puno_distritos.geojson"), quiet = TRUE)

calidad   <- read_csv(D("calidad_por_anio.csv"), show_col_types = FALSE)
sel_mod   <- read_csv(D("seleccion_modelos_cv.csv"), show_col_types = FALSE)
opt       <- read_csv(D("comparacion_optimizadores.csv"), show_col_types = FALSE)
prueba    <- read_csv(D("evaluacion_prueba_2022_2026.csv"), show_col_types = FALSE)
imp       <- read_csv(D("importancia_permutacion.csv"), show_col_types = FALSE)
names(imp)[1] <- "variable"
diag_chi  <- read_csv(D("pruebas_diagnosticas.csv"), show_col_types = FALSE)
riesgo    <- read_csv(D("matriz_riesgo_provincia_mes.csv"), show_col_types = FALSE)
plan      <- read_csv(D("plan_kits_grilla.csv"), show_col_types = FALSE) |>
  mutate(temporada = recode(temporada, "Lluvias (dic-mar)" = "Lluvias, diciembre a marzo", "Heladas (may-set)" = "Heladas, mayo a septiembre"))
ganancia  <- read_csv(D("curva_ganancia.csv"), show_col_types = FALSE)
roc       <- read_csv(D("roc_prueba.csv"), show_col_types = FALSE)
confusion <- as.matrix(read_csv(D("matriz_confusion.csv"), show_col_types = FALSE))
extra     <- jsonlite::fromJSON(D("extra.json"))
hiper     <- jsonlite::fromJSON(D("mejores_hiperparametros.json"))

titulo <- function(x) {
  y <- tools::toTitleCase(tolower(x))
  y <- gsub(" De ", " de ", y, fixed = TRUE); y <- gsub(" Del ", " del ", y, fixed = TRUE)
  y <- sub("^Azangaro$", "Azángaro", y); y <- sub("^San Roman$", "San Román", y); y <- sub("^Huancane$", "Huancané", y)
  y
}
mayus <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
MESES <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
MESES_L <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre",
             "octubre", "noviembre", "diciembre")

# ---- Paleta -----------------------------------------------------------------
AZUL <- "#2a78d6"; NARANJA <- "#eb6834"; AQUA <- "#1baf7a"; AMARILLO <- "#eda100"
MAGENTA <- "#e87ba4"; VERDE <- "#008300"; VIOLETA <- "#6250d6"; ROJO <- "#e34948"
GRIS <- "#9a9893"; TINTA <- "#1f2340"; TINTA2 <- "#5b5f77"; REJILLA <- "#e9ecf3"; MARINO <- "#1f2470"
GRUPOS <- names(sort(table(emer$GRUPO), decreasing = TRUE))
COL_GRUPO <- setNames(c(AZUL, NARANJA, AQUA, AMARILLO, VIOLETA, MAGENTA, VERDE, ROJO, "#7b6a58", GRIS)[seq_along(GRUPOS)], GRUPOS)
COL_GRUPO["Otros"] <- GRIS
ESC_AZUL <- list(c(0, "#eef5fe"), c(0.15, "#cde2fb"), c(0.35, "#86b6ef"), c(0.6, "#3987e5"),
                 c(0.8, "#1c5cab"), c(1, "#0d366b"))
ESC_ROJO <- list(c(0, "#fff4ee"), c(0.2, "#fcd5c0"), c(0.45, "#f5a47c"), c(0.7, "#eb6834"),
                 c(0.88, "#bf4a1b"), c(1, "#83300f"))

fmt_n <- function(x, acc = 1) number(x, big.mark = " ", accuracy = acc, decimal.mark = ",")
pct <- function(x, acc = 0.1) percent(x, accuracy = acc, decimal.mark = ",")
dec <- function(x, acc = 0.001) number(x, accuracy = acc, decimal.mark = ",")
grande <- function(x) ifelse(x >= 1e6, paste(number(x / 1e6, accuracy = 0.01, decimal.mark = ","), "M"),
                      ifelse(x >= 1e4, paste(number(x / 1e3, accuracy = 0.1, decimal.mark = ","), "mil"), fmt_n(x)))
b <- function(x) paste0("<b>", x, "</b>")

estilo <- function(p, ytitulo = NULL, xtitulo = NULL, leyenda = TRUE) {
  p |>
    layout(
      font = list(family = "Inter, Segoe UI, sans-serif", size = 12, color = TINTA2),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = list(text = if (is.null(xtitulo)) "" else xtitulo, font = list(size = 11)),
                   gridcolor = REJILLA, zeroline = FALSE, linecolor = "#d5d9e4"),
      yaxis = list(title = list(text = if (is.null(ytitulo)) "" else ytitulo, font = list(size = 11)),
                   gridcolor = REJILLA, zeroline = FALSE),
      margin = list(l = 10, r = 10, t = 10, b = 10),
      hoverlabel = list(bgcolor = "white", bordercolor = "#d5d9e4",
                        font = list(color = TINTA, family = "Inter, Segoe UI, sans-serif")),
      legend = list(orientation = "h", x = 0, y = 1.14, font = list(size = 11, color = TINTA2)),
      showlegend = leyenda, separators = ", "
    ) |>
    config(displayModeBar = FALSE, locale = "es")
}

# ---- Tema y CSS -------------------------------------------------------------
OLAS <- paste0(
  "data:image/svg+xml;utf8,",
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 1440 320' preserveAspectRatio='none'>",
  "<path fill='%232a78d6' fill-opacity='.20' d='M0,192L60,176C120,160,240,128,360,138.7C480,149,600,203,720,208C840,213,960,171,1080,149.3C1200,128,1320,128,1380,128L1440,128L1440,320L0,320Z'/>",
  "<path fill='%232fb7a7' fill-opacity='.20' d='M0,256L80,240C160,224,320,192,480,197.3C640,203,800,245,960,250.7C1120,256,1280,224,1360,208L1440,192L1440,320L0,320Z'/>",
  "<path fill='%231f2470' fill-opacity='.14' d='M0,288L90,277.3C180,267,360,245,540,250.7C720,256,900,288,1080,293.3C1260,299,1350,277,1395,266.7L1440,256L1440,320L0,320Z'/>",
  "</svg>")
css <- "
body { background-color: #e9f2fb;
       background-image: url(\"OLAS_URL\"),
         radial-gradient(ellipse at 12% 0%, rgba(42,120,214,.16) 0, transparent 45%),
         radial-gradient(ellipse at 95% 25%, rgba(47,183,167,.14) 0, transparent 40%),
         linear-gradient(180deg, #f1f6fc 0%, #e3eef9 100%);
       background-repeat: no-repeat; background-attachment: fixed;
       background-position: bottom center, 0 0, 0 0, 0 0;
       background-size: 100% 380px, 100% 100%, 100% 100%, 100% 100%; }
.bslib-page-main, .bslib-sidebar-layout > .main { background: transparent !important; }
.navbar { background: linear-gradient(100deg, #10173a 0%, #1f2470 45%, #2b2a8c 70%, #2a6fd0 100%) !important;
          box-shadow: 0 4px 18px rgba(20,27,61,.28); padding-top: 10px; padding-bottom: 10px; }
.navbar > .container-fluid { align-items: center; flex-wrap: nowrap; }
.navbar-brand { margin-right: 16px; padding: 0; display: flex; align-items: center; }
.marca { display: flex; align-items: center; gap: 12px; }
.marca-logo { width: 50px; height: 50px; border-radius: 15px; display: inline-flex; align-items: center;
              justify-content: center; font-size: 1.3rem; color: #fff;
              background: linear-gradient(135deg, #3d8bff 0%, #8a6cff 100%);
              box-shadow: 0 4px 14px rgba(61,139,255,.45), inset 0 1px 0 rgba(255,255,255,.35); }
.marca-tit { font-size: 1.15rem; font-weight: 700; color: #fff; letter-spacing: .6px; white-space: nowrap; line-height: 1.2; }
.marca-sub { font-size: .74rem; white-space: nowrap; line-height: 1.3; color: rgba(255,255,255,.62); margin-top: 2px; }
.navbar .navbar-nav { background: rgba(255,255,255,.07); border: 1px solid rgba(255,255,255,.1);
                      border-radius: 14px; padding: 4px; gap: 2px; flex-wrap: nowrap; }
.navbar .nav-link { font-weight: 500; font-size: .8rem; border-radius: 10px; padding: 6px 10px !important;
                    color: rgba(255,255,255,.72) !important; white-space: nowrap; }
.navbar .nav-link:hover { background: rgba(255,255,255,.1); color: #fff !important; }
.navbar .nav-link.active { background: #fff !important; color: #1f2470 !important; font-weight: 600;
                           box-shadow: 0 2px 8px rgba(0,0,0,.18); }
.navbar .nav-link.active i, .navbar .nav-link.active svg { color: #2a78d6; }
@media (min-width: 992px) {
  .navbar-collapse { flex-wrap: nowrap; flex: 1; }
  .navbar .navbar-nav { margin-left: auto; flex: 0 0 auto !important; width: auto !important; }
  .navbar .navbar-nav .nav-link { display: flex; align-items: center; justify-content: center; gap: 6px; }
}
.bslib-sidebar-layout > .sidebar { background: rgba(255,255,255,.72); backdrop-filter: blur(10px);
                                   border-right: 1px solid rgba(42,120,214,.12); }
.bslib-sidebar-layout > .sidebar > .sidebar-content { padding-top: 14px; gap: 12px; }
.sidebar .control-label { font-weight: 600; color: #3a3f5c; font-size: .8rem; margin-bottom: 4px; }
.filtros-cab h6 { margin: 0; font-weight: 700; color: #1f2340; font-size: .95rem; display: flex; align-items: center; gap: 8px; }
.filtros-cab h6 i, .filtros-cab h6 svg { color: #2a78d6; }
.grupo-filtro { background: #fff; border: 1px solid #dbe3f1; border-radius: 12px; overflow: hidden;
                box-shadow: 0 1px 3px rgba(16,24,40,.06); }
.grupo-tit { display: flex; align-items: center; gap: 9px; font-size: .74rem; font-weight: 700; color: #fff;
             text-transform: uppercase; letter-spacing: .8px; padding: 9px 14px;
             background: linear-gradient(100deg, #1f2470 0%, #2a6fd0 100%); }
.grupo-cuerpo { padding: 12px 14px 4px; }
.sidebar .selectize-input { border-radius: 10px; border-color: #dfe4ee; font-size: .84rem; min-height: 38px; box-shadow: none; }
.selectize-control.multi .selectize-input > div { background: #eef3fd; color: #1c5cab; border-radius: 6px; border: 0; padding: 1px 7px; }
body > .selectize-dropdown { z-index: 2000; border: 1px solid #dfe4ee; border-radius: 10px; font-size: .84rem;
          box-shadow: 0 10px 28px rgba(16,24,40,.16); margin-top: 4px; }
.nota-filtro { font-size: .72rem; color: #7a7f96; line-height: 1.35; margin: 4px 0 6px; }
.fuente { display: flex; gap: 9px; font-size: .7rem; color: #7a7f96; line-height: 1.4;
          border-top: 1px dashed #dfe4ee; padding-top: 12px; }
.btn-aplicar { border-radius: 10px; font-weight: 700; font-size: .86rem; padding: 9px 0; border: 0;
               background: linear-gradient(100deg, #1f2470 0%, #2a6fd0 100%); color: #fff;
               box-shadow: 0 4px 12px rgba(42,111,208,.28); }
.btn-aplicar:hover { color: #fff; }
.btn-limpiar { border-radius: 10px; font-weight: 600; font-size: .82rem; padding: 7px 0; }
.botones-filtro { display: flex; flex-direction: column; gap: 8px; }
.card { border: 0; border-radius: 16px; box-shadow: 0 1px 2px rgba(16,24,40,.05), 0 6px 22px rgba(16,24,40,.06); }
.card-header { background: transparent; border-bottom: 0; font-weight: 600; color: #1f2340; font-size: .95rem; padding-top: 14px; }
.card-header::before { content: ''; display: inline-block; width: 4px; height: 15px; border-radius: 2px;
               background: linear-gradient(#2a78d6, #6250d6); margin-right: 9px; vertical-align: -2px; }
.card-header.cab-flex::before { display: none; }
.subtitulo { font-size: .78rem; color: #5b5f77; font-weight: 400; margin-left: 13px; display: block; }
/* Caja de interpretación bajo cada gráfico */
.lectura { margin: 4px 14px 14px; padding: 11px 14px 11px 14px; border-radius: 12px; background: #f3f7fe;
           border-left: 4px solid #2a78d6; font-size: .82rem; line-height: 1.5; color: #2f3450; }
.lectura .lt { display: flex; align-items: center; gap: 6px; font-size: .68rem; font-weight: 700; letter-spacing: .8px;
               text-transform: uppercase; color: #1c5cab; margin-bottom: 3px; }
.lectura b { color: #1f2340; }
.lectura.alerta { background: #fff6f0; border-left-color: #eb6834; }
.lectura.alerta .lt { color: #bf4a1b; }
.intro { background: linear-gradient(100deg, #1f2470 0%, #2a6fd0 100%); color: #fff; border-radius: 16px;
         padding: 16px 20px; box-shadow: 0 8px 22px rgba(31,36,112,.22); margin-bottom: 4px; }
.intro h5 { font-weight: 800; margin: 0 0 4px; font-size: 1.02rem; display: flex; gap: 9px; align-items: center; }
.intro p { margin: 0; font-size: .85rem; line-height: 1.5; color: rgba(255,255,255,.9); }
.intro .pasos { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 10px; }
.intro .paso { background: rgba(255,255,255,.14); border-radius: 999px; padding: 4px 12px; font-size: .74rem; font-weight: 600; }
/* KPI */
.kpi { border: 0 !important; border-radius: 16px !important; color: #fff !important;
       box-shadow: 0 8px 22px rgba(42,120,214,.18) !important; transition: transform .2s ease; }
.kpi:hover { transform: translateY(-3px); z-index: 30; }
.kpi .value-box-title { color: rgba(255,255,255,.92) !important; font-size: .88rem; font-weight: 600; }
.kpi .value-box-value { color: #fff !important; font-weight: 800 !important; letter-spacing: -1px;
                        font-size: clamp(1.4rem, 2vw, 2.2rem) !important; white-space: nowrap; line-height: 1.05; }
.kpi .value-box-area { justify-content: flex-start !important; padding-bottom: 16px !important; }
.kpi-1 { background: linear-gradient(135deg, #2a78d6 0%, #6250d6 100%) !important; }
.kpi-2 { background: linear-gradient(135deg, #eb6834 0%, #d23d5b 100%) !important; }
.kpi-3 { background: linear-gradient(135deg, #0d8a63 0%, #1baf7a 60%, #2fb7a7 100%) !important; }
.kpi-4 { background: linear-gradient(135deg, #6250d6 0%, #b8509a 100%) !important; }
.kpi-tit { display: flex; align-items: center; gap: 9px; width: 100%; white-space: nowrap; }
.kpi-ico { display: inline-flex; align-items: center; justify-content: center; width: 30px; height: 30px;
           border-radius: 9px; background: rgba(255,255,255,.2); font-size: .85rem; flex-shrink: 0; }
.kpi-info { margin-left: auto; font-size: .75rem; opacity: .6; }
.kpi-pie { font-size: .76rem; color: rgba(255,255,255,.88); margin-top: 2px; white-space: normal; }
.kpi, .kpi .card-body, .kpi .value-box-grid, .kpi .value-box-area { overflow: visible !important; }
.kpi-det { position: absolute; top: calc(100% + 12px); left: -4px; right: -4px; z-index: 1000;
           border-radius: 14px; padding: 15px 17px 13px; background: rgba(16,23,58,.96); color: #fff;
           box-shadow: 0 18px 40px rgba(16,23,58,.35); opacity: 0; transform: translateY(-6px);
           pointer-events: none; transition: opacity .2s ease, transform .2s ease; }
.kpi-det::before { content: ''; position: absolute; top: -7px; left: 34px; width: 14px; height: 14px;
                   background: rgba(16,23,58,.96); transform: rotate(45deg); border-radius: 3px; }
.kpi:hover .kpi-det { opacity: 1; transform: none; }
.kpi-det-sec + .kpi-det-sec { margin-top: 9px; padding-top: 9px; border-top: 1px solid rgba(255,255,255,.12); }
.kpi-det-sec h6 { font-size: .68rem; font-weight: 700; text-transform: uppercase; letter-spacing: .8px;
                  color: #9cc4ff; margin: 0 0 4px; display: flex; align-items: center; gap: 6px; }
.kpi-det-sec p { font-size: .8rem; line-height: 1.45; margin: 0; color: rgba(255,255,255,.88); white-space: normal; }
.kpi-det-sec p b { color: #fff; }
.mini-kpi { background: #f3f6fd; border-radius: 12px; padding: 10px 14px; }
.mini-kpi > span { display: block; font-size: .72rem; color: #5b5f77; text-transform: uppercase; letter-spacing: .4px; }
.mini-kpi strong { font-size: 1.25rem; color: #1f2340; font-weight: 700; display: block; margin-top: 2px; }
.seg .shiny-options-group { display: flex; background: #eef1f8; border-radius: 10px; padding: 3px; flex-wrap: wrap; }
.seg .form-check { padding: 0; margin: 0; }
.seg input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
.seg label { margin: 0; padding: 0; }
.seg label span { display: inline-block; padding: 4px 12px; border-radius: 8px; font-size: .8rem; cursor: pointer; color: #5b5f77; }
.seg input:checked + span { background: #fff; color: #1c5cab; font-weight: 600; box-shadow: 0 1px 3px rgba(16,24,40,.12); }
.seg .form-group { margin: 0; }
.leaflet-container { background: #f7f9fc !important; font-family: Inter, 'Segoe UI', sans-serif; }
.leaflet-tooltip { border-radius: 10px; border: 1px solid #d5d9e4; box-shadow: 0 6px 18px rgba(16,24,40,.12); }
.info.legend { border-radius: 10px; box-shadow: 0 4px 14px rgba(16,24,40,.12); font-size: .75rem; }
.dataTables_wrapper { font-size: .82rem; }
table.dataTable thead th { font-weight: 600; color: #5b5f77; text-transform: uppercase; font-size: .72rem;
                           letter-spacing: .4px; white-space: nowrap; }
.tabla-simple { width: 100%; font-size: .84rem; }
.tabla-simple th { font-size: .72rem; text-transform: uppercase; color: #5b5f77; letter-spacing: .4px; font-weight: 600;
                   border-bottom: 1px solid #e6e9f2; padding: 6px 8px; }
.tabla-simple td { padding: 6px 8px; border-bottom: 1px solid #f0f2f7; }
.tabla-simple tr.gana td { background: #eef5fe; font-weight: 700; color: #1f2340; }
.irs--shiny .irs-bar { background: #2a78d6; border-color: #2a78d6; }
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #1f2470; }
.membrete { position: relative; display: flex; align-items: center; justify-content: space-between; gap: 24px;
            padding: 10px 28px 12px; background: linear-gradient(180deg, #ffffff 0%, #f7f9fd 100%); z-index: 1031; }
.membrete::after { content: ''; position: absolute; left: 0; right: 0; bottom: 0; height: 3px;
            background: linear-gradient(90deg, #2fb7a7 0%, #2a78d6 40%, #1f2470 75%, #10173a 100%); }
.membrete-lado { display: flex; align-items: center; gap: 16px; min-width: 0; }
.membrete-epg { height: 66px; width: auto; }
.membrete-fac { height: 58px; width: auto; }
.membrete-sep { width: 1px; height: 46px; background: #d5dbe8; flex: 0 0 auto; }
.membrete-inst { font-size: .68rem; font-weight: 600; letter-spacing: 1.6px; color: #6b7590; text-transform: uppercase; white-space: nowrap; }
.membrete-curso { font-size: 1.28rem; font-weight: 800; color: #1f2470; line-height: 1.2; white-space: nowrap; margin-top: 1px; }
.membrete-act { font-size: .76rem; color: #5b6478; white-space: nowrap; margin-top: 1px; }
.membrete-autor { text-align: right; line-height: 1.25; }
.membrete-autor .etq { font-size: .66rem; font-weight: 600; letter-spacing: 1.6px; text-transform: uppercase; color: #6b7590; }
.membrete-autor .nom { font-size: 1rem; font-weight: 700; color: #1f2340; white-space: nowrap; margin-top: 2px; }
@media (max-width: 991px) {
  .membrete { padding: 8px 16px 10px; gap: 6px 12px; flex-wrap: wrap; }
  .membrete-inst, .membrete-act, .membrete-lado .membrete-sep { display: none; }
  .membrete-epg { height: 46px; } .membrete-fac { height: 34px; }
  .membrete-curso { font-size: .95rem; white-space: normal; }
}
"
tema <- bs_theme(version = 5, primary = AZUL, secondary = VIOLETA, bg = "#ffffff", fg = TINTA,
                 base_font = font_google("Inter", wght = "400..800"), "border-radius" = "12px") |>
  bs_add_rules(sub("OLAS_URL", OLAS, css, fixed = TRUE))

# ---- Componentes ------------------------------------------------------------
titulo_card <- function(titulo, subtitulo = NULL) card_header(titulo, if (!is.null(subtitulo)) span(class = "subtitulo", subtitulo))
lectura <- function(id) uiOutput(id)
caja <- function(txt, alerta = FALSE, tit = "Interpretación") {
  div(class = paste("lectura", if (alerta) "alerta"), div(class = "lt", icon(if (alerta) "triangle-exclamation" else "lightbulb"), tit), HTML(txt))
}
kpi <- function(titulo, icono, salida, n) {
  value_box(div(class = "kpi-tit", span(class = "kpi-ico", icon(icono)), titulo, span(class = "kpi-info", icon("circle-info"))),
            textOutput(salida), uiOutput(paste0(salida, "_pie")), uiOutput(paste0(salida, "_det")),
            class = paste0("kpi kpi-", n))
}
panel <- function(que_es, como_leer) {
  sec <- function(ico, tit, txt) div(class = "kpi-det-sec", h6(icon(ico), tit), p(HTML(txt)))
  div(class = "kpi-det", sec("circle-question", "Qué es", que_es), sec("lightbulb", "Cómo leerlo", como_leer))
}
grupo <- function(titulo, icono, ...) {
  div(class = "grupo-filtro", div(class = "grupo-tit", icon(icono), titulo), div(class = "grupo-cuerpo", ...))
}
intro <- function(icono, tit, txt, pasos = NULL) {
  div(class = "intro", h5(icon(icono), tit), p(HTML(txt)),
      if (!is.null(pasos)) div(class = "pasos", lapply(pasos, function(x) span(class = "paso", x))))
}

# ---- Interfaz ---------------------------------------------------------------
PROVS <- sort(unique(emer$PROV))
filtros <- sidebar(
  width = 300,
  div(class = "filtros-cab", h6(icon("sliders"), "Filtros")),
  grupo("Periodo", "calendar-days",
        sliderInput("anios", NULL, min = 2003, max = 2026, value = c(2003, 2026), step = 1, sep = "", ticks = FALSE),
        div(class = "nota-filtro", "El año 2026 solo incluye el primer semestre.")),
  grupo("Dónde y qué", "location-dot",
        selectizeInput("prov", "Provincia", choices = setNames(PROVS, titulo(PROVS)), multiple = TRUE,
                       options = list(placeholder = "Las 13 provincias", dropdownParent = "body")),
        selectizeInput("grupo", "Tipo de peligro", choices = GRUPOS, multiple = TRUE,
                       options = list(placeholder = "Todos los peligros", dropdownParent = "body"))),
  div(class = "botones-filtro",
      actionButton("aplicar", "Aplicar filtros", icon = icon("check"), class = "w-100 btn-aplicar"),
      actionButton("limpiar", "Restablecer filtros", icon = icon("rotate-left"), class = "btn-outline-primary w-100 btn-limpiar")),
  div(class = "nota-filtro", "Los filtros actúan sobre las pestañas Resumen, Mapa, Diagnóstico y Datos. Las pestañas del modelo muestran resultados fijos del análisis."),
  div(class = "fuente", icon("database"),
      div("Fuente: SINPAD, Instituto Nacional de Defensa Civil. Conjunto Emergencias y daños a nivel nacional por departamento, Plataforma Nacional de Datos Abiertos, licencia ODC BY."))
)

pag_resumen <- nav_panel(
  "Resumen", icon = icon("chart-line"),
  intro("circle-info", "Qué muestra este tablero",
        "Las emergencias que el INDECI registró en la región Puno desde 2003 se analizan en cuatro niveles. Cada pestaña responde una pregunta y cada gráfico trae debajo su interpretación. Una emergencia se considera <b>de alto impacto</b> cuando afecta a 100 personas o más.",
        c("1. Qué pasó: Resumen y Mapa", "2. Por qué pasó: Diagnóstico", "3. Qué puede pasar: Modelo y Predicción", "4. Qué conviene hacer: Prescripción")),
  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    kpi("Emergencias", "triangle-exclamation", "k_emer", 1),
    kpi("Personas impactadas", "people-group", "k_pers", 2),
    kpi("De alto impacto", "fire", "k_alto", 3),
    kpi("Camélidos perdidos", "paw", "k_cam", 4)
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE, titulo_card("Cuántas emergencias hubo cada año", "Barras: emergencias registradas. Línea: porcentaje de alto impacto"),
         plotlyOutput("g_anio", height = 330), lectura("l_anio")),
    card(full_screen = TRUE, titulo_card("Lo más frecuente no es lo más dañino", "Participación de cada peligro en las emergencias y en las personas impactadas"),
         plotlyOutput("g_freq", height = 330), lectura("l_freq"))
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE, titulo_card("En qué meses ocurre cada peligro", "Número de emergencias por mes y tipo de peligro"),
         plotlyOutput("g_mes", height = 380), lectura("l_mes")),
    card(full_screen = TRUE, titulo_card("Distritos más golpeados", "Los 12 distritos con más personas impactadas"),
         plotlyOutput("g_dist", height = 380), lectura("l_dist"))
  )
)

pag_mapa <- nav_panel(
  "Mapa", icon = icon("map-location-dot"),
  layout_columns(
    col_widths = c(8, 4),
    card(full_screen = TRUE,
         card_header(class = "cab-flex",
                     div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2 w-100",
                         div(span("Dónde ocurren las emergencias"),
                             span(class = "subtitulo ms-0", "Distritos de la región Puno. Haga clic en un distrito para ver su detalle")),
                         div(class = "seg", radioButtons("met_mapa", NULL, inline = TRUE,
                                                         choices = c("Emergencias" = "n", "Personas" = "p", "% alto impacto" = "a", "Riesgo del modelo" = "r"))))),
         card_body(padding = 0, leafletOutput("mapa", height = 540)),
         lectura("l_mapa")),
    card(full_screen = TRUE,
         card_header(uiOutput("tit_dist", inline = TRUE)),
         layout_columns(fill = FALSE, col_widths = c(6, 6),
                        div(class = "mini-kpi", span("Emergencias"), strong(textOutput("d_n", inline = TRUE))),
                        div(class = "mini-kpi", span("Alto impacto"), strong(textOutput("d_a", inline = TRUE)))),
         plotlyOutput("g_dist_grupo", height = 250),
         plotlyOutput("g_dist_mes", height = 190),
         lectura("l_dist_det"))
  )
)

pag_diag <- nav_panel(
  "Diagnóstico", icon = icon("stethoscope"),
  intro("stethoscope", "Por qué algunas emergencias son más graves",
        "La analítica diagnóstica busca las causas. Aquí se compara qué tan seguido una emergencia llega a 100 personas afectadas según el peligro, el lugar y el mes, y se revisa la calidad del registro, porque un dato mal anotado puede llevar a conclusiones falsas."),
  layout_columns(
    col_widths = c(5, 7),
    card(full_screen = TRUE, titulo_card("Probabilidad de alto impacto según el peligro", "De cada 100 emergencias de ese tipo, cuántas afectaron a 100 personas o más"),
         plotlyOutput("g_tasa", height = 360), lectura("l_tasa")),
    card(full_screen = TRUE, titulo_card("El mismo peligro no golpea igual en todas las provincias", "Porcentaje de alto impacto por provincia y peligro, solo celdas con 15 casos o más"),
         plotlyOutput("g_calor", height = 360), lectura("l_calor"))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(full_screen = TRUE, titulo_card("Qué factor pesa más", "Fuerza de la asociación de cada variable con el alto impacto, medida con la V de Cramér"),
         plotlyOutput("g_cramer", height = 260), caja(paste0(
           "La V de Cramér va de 0, sin relación, a 1, relación total. El <b>tipo de peligro</b> obtiene ", b(dec(diag_chi$`V de Cramér`[1], 0.01)),
           ", una asociación fuerte, y el <b>mes</b> ", b(dec(diag_chi$`V de Cramér`[3], 0.01)), ". La provincia y la región natural, por sí solas, pesan poco. ",
           "Todas las pruebas chi cuadrado resultaron significativas, con valores p menores que 0,001."))),
    card(full_screen = TRUE, titulo_card("Calidad del registro por año", "Porcentaje de emergencias con la ayuda humanitaria anotada y sin ninguna persona registrada"),
         plotlyOutput("g_calidad", height = 260), caja(
           "Desde 2015 casi no se anota la ayuda entregada y en 2019 y entre 2024 y 2026 el registro es nulo. Por eso la ayuda no se usó como variable del modelo. Los años 2014 y 2019 tienen muy pocas emergencias porque el sistema registró poco, no porque fueran años tranquilos.", alerta = TRUE, tit = "Advertencia sobre los datos"))
  )
)

pag_modelo <- nav_panel(
  "Modelo", icon = icon("brain"),
  intro("brain", "Se puede anticipar si una emergencia será grave",
        "Cuando un distrito reporta una emergencia, todavía no se sabe cuántas personas resultarán afectadas. Se entrenaron ocho modelos de aprendizaje automático con las emergencias de 2003 a 2021, usando solo datos que se conocen en el momento del reporte, y se probaron con las de 2022 a 2026, que el modelo nunca vio.",
        c("Paso 3: comparar modelos", "Paso 4: optimizar el mejor", "Prueba final con 2022 a 2026")),
  layout_columns(
    col_widths = c(6, 6),
    card(full_screen = TRUE, titulo_card("Comparación de ocho modelos", "ROC AUC promedio en cinco bloques de validación temporal. Más cerca de 1 es mejor"),
         plotlyOutput("g_modelos", height = 340), caja(paste0(
           "El ROC AUC mide qué tan bien un modelo ordena las emergencias de la más riesgosa a la menos riesgosa. Un valor de 0,5 equivale a lanzar una moneda y 1 sería un acierto perfecto. ",
           "<b>Random Forest</b> fue el mejor con ", b(dec(sel_mod$`ROC-AUC`[1])), ", aunque varios modelos quedaron muy cerca. Todos superan con claridad al azar, lo que confirma que el lugar, el mes y el historial del distrito sí anticipan la gravedad."))),
    card(full_screen = TRUE, titulo_card("Optimización de hiperparámetros", "Cuatro estrategias con el mismo presupuesto de 40 pruebas"),
         plotlyOutput("g_opt", height = 250), uiOutput("t_opt"), caja(paste0(
           "Los hiperparámetros son los ajustes del modelo, como el número de árboles o su profundidad. Las cuatro estrategias mejoraron el modelo de ",
           b("0,882"), " a cerca de ", b("0,891"), ". <b>Optuna</b> logró el mejor valor en el menor tiempo porque abandona a mitad de camino las pruebas que no prometen.")))
  ),
  layout_columns(
    col_widths = c(5, 7),
    card(full_screen = TRUE, titulo_card("Prueba final: curva ROC", "Emergencias de 2022 a 2026, que el modelo no vio durante el entrenamiento"),
         plotlyOutput("g_roc", height = 330), caja(paste0(
           "Cuanto más se acerca la curva azul a la esquina superior izquierda, mejor distingue el modelo los casos graves de los leves. La línea punteada es el azar. ",
           "En la prueba el modelo optimizado obtuvo ", b(dec(prueba$`ROC-AUC`[2])), ", frente a ", dec(prueba$`ROC-AUC`[1]), " sin optimizar."))),
    card(full_screen = TRUE, titulo_card("Aciertos y errores del modelo en la prueba", "Matriz de confusión con el umbral elegido en la validación"),
         layout_columns(col_widths = c(6, 6), plotlyOutput("g_conf", height = 300), plotlyOutput("g_imp", height = 300)),
         uiOutput("l_conf"))
  )
)

pag_pred <- nav_panel(
  "Predicción", icon = icon("binoculars"),
  intro("binoculars", "Cuántas emergencias graves se esperan, dónde y cuándo",
        "Con el modelo se estimó, para cada provincia y mes, cuántas emergencias de alto impacto se esperan en un año típico. Se multiplica el número habitual de emergencias de los últimos cinco años por la probabilidad de que cada una sea grave."),
  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    kpi("Graves esperadas al año", "calendar-check", "p_total", 1),
    kpi("Provincia con más riesgo", "location-crosshairs", "p_prov", 2),
    kpi("Mes más crítico", "calendar-day", "p_mes", 3),
    kpi("Celda más crítica", "bullseye", "p_celda", 4)
  ),
  card(full_screen = TRUE,
       card_header(class = "cab-flex",
                   div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2 w-100",
                       div(span("Matriz de riesgo por provincia y mes"),
                           span(class = "subtitulo ms-0", "Emergencias de alto impacto esperadas en un año típico. Más oscuro significa más riesgo")),
                       div(class = "seg", radioButtons("temp_pred", NULL, inline = TRUE,
                                                       choices = c("Todo el año" = "t", "Lluvias, dic a mar" = "l", "Heladas, may a sep" = "h"))))),
       plotlyOutput("g_riesgo", height = 430), lectura("l_riesgo")),
  card(full_screen = TRUE, titulo_card("Riesgo esperado por provincia", "Suma de emergencias graves esperadas en la temporada elegida"),
       plotlyOutput("g_riesgo_prov", height = 330))
)

pag_presc <- nav_panel(
  "Prescripción", icon = icon("truck-medical"),
  intro("truck-medical", "Qué conviene hacer con esta información",
        "La analítica prescriptiva recomienda acciones. Se proponen dos decisiones: evaluar primero los reportes con más riesgo y repartir los kits de ayuda entre provincias antes de cada temporada con un modelo de optimización lineal resuelto con PuLP. Mueva los controles para ver cómo cambia la recomendación."),
  layout_columns(
    col_widths = c(5, 7),
    card(full_screen = TRUE, titulo_card("Decisión A. Evaluar primero lo más riesgoso", "Emergencias graves cubiertas según la capacidad de evaluación, prueba 2022 a 2026"),
         sliderInput("capac", "Capacidad: porcentaje de reportes que se pueden evaluar primero", min = 5, max = 100, value = 20, step = 5, post = " %", width = "100%"),
         plotlyOutput("g_ganancia", height = 300), lectura("l_ganancia")),
    card(full_screen = TRUE,
         card_header(class = "cab-flex",
                     div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2 w-100",
                         div(span("Decisión B. Kits de ayuda por provincia"),
                             span(class = "subtitulo ms-0", "Demanda esperada en gris y kits asignados por el modelo en azul")),
                         div(class = "seg", radioButtons("temp_kit", NULL, inline = TRUE,
                                                         choices = c("Lluvias" = "Lluvias, diciembre a marzo", "Heladas" = "Heladas, mayo a septiembre"))))),
         sliderInput("stock", "Kits disponibles como porcentaje de la demanda esperada", min = 30, max = 120, value = 75, step = 5, post = " %", width = "100%"),
         layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
                        div(class = "mini-kpi", span("Kits disponibles"), strong(textOutput("s_kits", inline = TRUE))),
                        div(class = "mini-kpi", span("Necesidad sin cubrir"), strong(textOutput("s_nc", inline = TRUE))),
                        div(class = "mini-kpi", span("Valor de un kit extra"), strong(textOutput("s_sombra", inline = TRUE)))),
         plotlyOutput("g_kits", height = 380), lectura("l_kits"))
  ),
  card(full_screen = TRUE, titulo_card("Cuánto stock hace falta", "Necesidad ponderada que queda sin cubrir según los kits disponibles"),
       plotlyOutput("g_sens", height = 280), caja(
         "Las dos temporadas siguen prácticamente la misma curva, por eso las líneas se superponen. Pasar de tener la mitad de los kits a tener tres cuartas partes reduce la necesidad sin cubrir de 33 % a 13 %. Completar el último cuarto exige la misma cantidad de kits pero rinde menos, porque esos kits cubren necesidades menos urgentes. Por eso la recomendación es contar con al menos el 75 % de la demanda esperada antes de cada temporada."))
)

pag_datos <- nav_panel(
  "Datos", icon = icon("table"),
  card(full_screen = TRUE, fill = FALSE,
       card_header(class = "cab-flex", div(class = "d-flex justify-content-between align-items-center w-100",
                       div(span("Emergencias filtradas"), span(class = "subtitulo ms-0", textOutput("txt_n", inline = TRUE))),
                       downloadButton("descargar", "Descargar CSV", class = "btn-sm btn-primary"))),
       card_body(fillable = FALSE, DTOutput("t_datos")))
)

membrete <- tagList(
  div(id = "membrete", class = "membrete",
      div(class = "membrete-lado",
          img(src = "logo_epg.png", class = "membrete-epg", alt = "Escuela de Posgrado UNA Puno"),
          span(class = "membrete-sep"),
          div(div(class = "membrete-inst", "Universidad Nacional del Altiplano · Escuela de Posgrado"),
              div(class = "membrete-curso", "Business Intelligence and Data Analytics"),
              div(class = "membrete-act", "Maestría · Actividad Semana 4: Analítica, modelos y optimización"))),
      div(class = "membrete-lado",
          div(class = "membrete-autor", div(class = "etq", "Elaborado por"), div(class = "nom", "Mary Luz Nina Palacios")),
          span(class = "membrete-sep"),
          img(src = "logo_facultad.png", class = "membrete-fac", alt = "Facultad de Ingeniería Estadística e Informática"))),
  tags$script(HTML("document.addEventListener('DOMContentLoaded', function () {
    var m = document.getElementById('membrete'); if (m) document.body.insertBefore(m, document.body.firstChild);
  });"))
)

ui <- page_navbar(
  header = membrete,
  title = div(class = "marca", span(class = "marca-logo", icon("house-flood-water")),
              div(div(class = "marca-tit", "EMERGENCIAS · REGIÓN PUNO"),
                  div(class = "marca-sub", "Registro SINPAD del INDECI"),
                  div(class = "marca-sub", "Enero de 2003 a junio de 2026"))),
  window_title = "Emergencias en la región Puno · Analítica y BI",
  theme = tema, fillable = FALSE, sidebar = filtros, inverse = TRUE,
  pag_resumen, pag_mapa, pag_diag, pag_modelo, pag_pred, pag_presc, pag_datos
)

# ---- Servidor ---------------------------------------------------------------
server <- function(input, output, session) {

  DEF <- list(anios = c(2003, 2026), prov = character(0), grupo = character(0))
  aplicados <- reactiveVal(DEF)
  observeEvent(input$aplicar, aplicados(list(anios = input$anios, prov = as.character(input$prov), grupo = as.character(input$grupo))))
  observeEvent(input$limpiar, {
    updateSliderInput(session, "anios", value = c(2003, 2026))
    updateSelectizeInput(session, "prov", selected = character(0))
    updateSelectizeInput(session, "grupo", selected = character(0))
    aplicados(DEF)
  })
  datos <- reactive({
    f <- aplicados()
    d <- emer |> filter(ANIO >= f$anios[1], ANIO <= f$anios[2])
    if (length(f$prov)) d <- d |> filter(PROV %in% f$prov)
    if (length(f$grupo)) d <- d |> filter(GRUPO %in% f$grupo)
    validate(need(nrow(d) > 0, "No hay emergencias con esos filtros."))
    d
  })
  rango_txt <- reactive({ f <- aplicados(); if (f$anios[1] == f$anios[2]) f$anios[1] else paste(f$anios[1], "y", f$anios[2]) })

  # ---- KPI ----
  output$k_emer <- renderText(fmt_n(nrow(datos())))
  output$k_pers <- renderText(grande(sum(datos()$PERSONAS)))
  output$k_alto <- renderText(pct(mean(datos()$ALTO)))
  output$k_cam  <- renderText(grande(sum(datos()$CAMELIDO_PERD)))
  output$k_emer_pie <- renderUI(div(class = "kpi-pie", paste("Promedio de", fmt_n(nrow(datos()) / n_distinct(datos()$ANIO)), "por año")))
  output$k_pers_pie <- renderUI(div(class = "kpi-pie", paste(fmt_n(sum(datos()$DAMNIFICADOS)), "damnificados")))
  output$k_alto_pie <- renderUI(div(class = "kpi-pie", paste(fmt_n(sum(datos()$ALTO)), "emergencias con 100 personas o más")))
  output$k_cam_pie  <- renderUI(div(class = "kpi-pie", "Alpacas y llamas muertas"))
  output$k_emer_det <- renderUI(panel(
    "Cada emergencia es un evento registrado por un distrito en el SINPAD: una helada, una inundación, un incendio, entre otros.",
    paste0("Entre ", rango_txt(), " se registraron ", b(fmt_n(nrow(datos()))), " emergencias. El peligro más frecuente fue ",
           b(tolower(names(which.max(table(datos()$GRUPO))))), ".")))
  output$k_pers_det <- renderUI(panel(
    "Suma de fallecidos, desaparecidos, lesionados, damnificados y afectados de todas las emergencias.",
    "Una familia afectada por heladas en varios años se cuenta una vez por cada emergencia. Por eso la cifra puede superar a la población de la región y debe leerse como personas por evento."))
  output$k_alto_det <- renderUI(panel(
    "Porcentaje de emergencias que afectaron a 100 personas o más. Es la condición que el modelo aprende a anticipar.",
    paste0("Aproximadamente ", b(round(mean(datos()$ALTO) * 10)), " de cada 10 emergencias registradas fueron de alto impacto.")))
  output$k_cam_det <- renderUI(panel(
    "Animales camélidos, sobre todo alpacas y llamas, que murieron por la emergencia. Es la pérdida económica más sensible para las familias altoandinas.",
    "Las heladas, las nevadas y la sequía explican casi todas estas pérdidas. Los años 2010, 2015 y 2023 concentran las mayores cifras."))

  # ---- Resumen ----
  output$g_anio <- renderPlotly({
    a <- datos() |> group_by(ANIO) |> summarise(n = n(), alto = mean(ALTO), .groups = "drop")
    plot_ly(a, x = ~ANIO) |>
      add_bars(y = ~n, name = "Emergencias", marker = list(color = AZUL),
               text = ~paste0(ANIO, ": ", fmt_n(n), " emergencias"), hoverinfo = "text", textposition = "none") |>
      add_lines(y = ~alto, name = "% de alto impacto", yaxis = "y2", line = list(color = NARANJA, width = 2.5),
                text = ~paste0(ANIO, ": ", pct(alto), " de alto impacto"), hoverinfo = "text") |>
      estilo() |>
      layout(yaxis2 = list(overlaying = "y", side = "right", tickformat = ".0%", showgrid = FALSE, rangemode = "tozero"), bargap = 0.25, margin = list(r = 45))
  })
  output$l_anio <- renderUI({
    a <- datos() |> count(ANIO)
    top <- a[which.max(a$n), ]; low <- a |> filter(ANIO < 2026) |> slice_min(n, n = 1, with_ties = FALSE)
    caja(paste0("El año con más emergencias registradas fue ", b(top$ANIO), " con ", b(fmt_n(top$n)), " casos y el de menos fue ",
                b(low$ANIO), " con ", fmt_n(low$n), ". Un año con muy pocos registros no significa que haya sido tranquilo: en 2014 y 2019 el sistema registró poco. ",
                "Desde 2022 se anotan muchos eventos pequeños, por eso sube el número de emergencias y baja el porcentaje de alto impacto."))
  })
  output$g_freq <- renderPlotly({
    g <- datos() |> group_by(GRUPO) |> summarise(n = n(), p = sum(PERSONAS), .groups = "drop") |>
      mutate(pn = n / sum(n), pp = p / sum(p)) |> arrange(pp)
    g$GRUPO <- factor(g$GRUPO, levels = g$GRUPO)
    plot_ly(g, y = ~GRUPO) |>
      add_bars(x = ~pn, name = "% de emergencias", orientation = "h", marker = list(color = "#b9cbe6"),
               text = ~paste0(GRUPO, ": ", pct(pn), " de las emergencias"), hoverinfo = "text", textposition = "none") |>
      add_bars(x = ~pp, name = "% de personas impactadas", orientation = "h", marker = list(color = NARANJA),
               text = ~paste0(GRUPO, ": ", pct(pp), " de las personas impactadas"), hoverinfo = "text", textposition = "none") |>
      estilo() |> layout(barmode = "group", xaxis = list(tickformat = ".0%"), bargap = 0.25, legend = list(y = -0.12, orientation = "h"))
  })
  output$l_freq <- renderUI({
    g <- datos() |> group_by(GRUPO) |> summarise(n = n(), p = sum(PERSONAS), .groups = "drop") |>
      mutate(pn = n / sum(n), pp = p / sum(p)) |> arrange(desc(pp))
    caja(paste0(b(g$GRUPO[1]), " representa el ", b(pct(g$pn[1], 1)), " de las emergencias, pero reúne el ", b(pct(g$pp[1], 1)),
                " de las personas impactadas. Cuando una barra naranja es mucho más larga que la gris, ese peligro ocurre poco pero causa mucho daño cada vez que ocurre."))
  })
  output$g_mes <- renderPlotly({
    m <- datos() |> count(GRUPO, MES_NUM) |> complete(GRUPO, MES_NUM = 1:12, fill = list(n = 0))
    p <- plot_ly()
    for (g in rev(intersect(GRUPOS, unique(m$GRUPO)))) {
      s <- m |> filter(GRUPO == g)
      p <- p |> add_bars(data = s, x = ~MESES[MES_NUM], y = ~n, name = g, marker = list(color = COL_GRUPO[[g]]),
                         text = ~paste0(g, ", ", MESES_L[MES_NUM], ": ", fmt_n(n)), hoverinfo = "text", textposition = "none")
    }
    p |> estilo() |> layout(barmode = "stack", xaxis = list(categoryorder = "array", categoryarray = MESES), bargap = 0.2,
                            legend = list(y = -0.12, orientation = "h"))
  })
  output$l_mes <- renderUI({
    m <- datos() |> count(MES_NUM)
    caja(paste0("El mes con más emergencias es ", b(MESES_L[m$MES_NUM[which.max(m$n)]]), ". La región tiene dos temporadas: ",
                "las <b>lluvias</b> de diciembre a marzo traen inundaciones y huaicos, y las <b>heladas</b> de mayo a agosto, con su pico en junio y julio, enferman a la población y matan al ganado. ",
                "Abril y los meses previos a cada temporada son la ventana para preparar la respuesta."))
  })
  output$g_dist <- renderPlotly({
    t <- datos() |> group_by(DIST, PROV) |> summarise(p = sum(PERSONAS), n = n(), .groups = "drop") |> slice_max(p, n = 12, with_ties = FALSE)
    t$et <- factor(titulo(t$DIST), levels = rev(titulo(t$DIST)))
    plot_ly(t, x = ~p, y = ~et, type = "bar", orientation = "h",
            marker = list(color = ~p, colorscale = ESC_AZUL, cmin = 0, cmax = max(t$p)),
            text = ~paste0(titulo(DIST), ", provincia de ", titulo(PROV), "<br>", fmt_n(p), " personas en ", n, " emergencias"),
            hoverinfo = "text", textposition = "none") |> estilo(leyenda = FALSE) |> layout(bargap = 0.28)
  })
  output$l_dist <- renderUI({
    t <- datos() |> group_by(DIST) |> summarise(p = sum(PERSONAS), .groups = "drop") |> arrange(desc(p))
    caja(paste0(b(titulo(t$DIST[1])), " es el distrito con más personas impactadas en el periodo. Los 12 distritos de la lista reúnen el ",
                b(pct(sum(head(t$p, 12)) / sum(t$p), 1)), " del total, de modo que el daño se concentra en pocos lugares."))
  })

  # ---- Mapa ----
  por_dist <- reactive({
    datos() |> group_by(UBIGEO) |> summarise(n = n(), p = sum(PERSONAS), a = mean(ALTO), r = mean(PROB), .groups = "drop")
  })
  output$mapa <- renderLeaflet({
    m <- mapa_puno |> left_join(por_dist(), by = c("ubigeo" = "UBIGEO"))
    met <- input$met_mapa
    v <- switch(met, n = m$n, p = log10(m$p + 1), a = m$a, r = m$r)
    etq <- switch(met, n = "Emergencias", p = "Personas, escala log", a = "% alto impacto", r = "Riesgo medio")
    cols <- if (met %in% c("a", "r")) c("#fff4ee", "#f5a47c", "#eb6834", "#bf4a1b", "#83300f") else c("#eef5fe", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")
    pal <- colorNumeric(cols, domain = v, na.color = "#f0f0f0")
    txt <- sprintf("<b>%s</b><br>Provincia de %s<br>%s emergencias<br>%s personas impactadas<br>%s de alto impacto<br>Riesgo medio del modelo: %s",
                   titulo(m$distrito), titulo(m$provincia), fmt_n(coalesce(m$n, 0)), fmt_n(coalesce(m$p, 0)),
                   ifelse(is.na(m$a), "sin datos", pct(m$a)), ifelse(is.na(m$r), "sin datos", pct(m$r)))
    leaflet(m, options = leafletOptions(attributionControl = FALSE, zoomSnap = 0.25)) |>
      addPolygons(layerId = ~ubigeo, fillColor = pal(v), fillOpacity = 0.88, color = "#ffffff", weight = 0.8,
                  label = lapply(txt, HTML), highlightOptions = highlightOptions(weight = 2.5, color = TINTA, bringToFront = TRUE)) |>
      addPolygons(data = st_union(mapa_puno), fill = FALSE, color = TINTA, weight = 2, opacity = 0.5) |>
      addLegend("bottomright", pal = pal, values = v, title = etq, opacity = 0.9,
                labFormat = if (met == "p") labelFormat(transform = function(x) round(10^x)) else if (met %in% c("a", "r")) labelFormat(suffix = " %", transform = function(x) round(100 * x)) else labelFormat())
  })
  output$l_mapa <- renderUI({
    pd <- por_dist() |> left_join(st_drop_geometry(mapa_puno), by = c("UBIGEO" = "ubigeo"))
    k <- switch(input$met_mapa, n = "n", p = "p", a = "a", r = "r")
    top <- pd[which.max(pd[[k]]), ]
    expl <- switch(input$met_mapa,
                   n = "Más oscuro significa más emergencias registradas.",
                   p = "Más oscuro significa más personas impactadas. Se usa escala logarítmica para que se distingan también los distritos con cifras pequeñas.",
                   a = "Más oscuro significa que una mayor parte de sus emergencias afectó a 100 personas o más.",
                   r = "Es la probabilidad media de alto impacto que el modelo asigna a las emergencias del distrito. Más oscuro significa más riesgo.")
    caja(paste0(expl, " El valor más alto corresponde a ", b(titulo(top$distrito)), ", en la provincia de ", titulo(top$provincia), "."))
  })
  sel <- reactiveVal(NULL)
  observeEvent(input$mapa_shape_click, sel(input$mapa_shape_click$id))
  dsel <- reactive({
    u <- sel(); d <- datos()
    if (is.null(u)) u <- names(which.max(tapply(d$PERSONAS, d$UBIGEO, sum)))
    list(u = u, d = d |> filter(UBIGEO == u), nom = mapa_puno$distrito[mapa_puno$ubigeo == u], prov = mapa_puno$provincia[mapa_puno$ubigeo == u])
  })
  output$tit_dist <- renderUI(span(paste0("Distrito de ", titulo(dsel()$nom)), span(class = "subtitulo", paste("Provincia de", titulo(dsel()$prov)))))
  output$d_n <- renderText(fmt_n(nrow(dsel()$d)))
  output$d_a <- renderText(if (nrow(dsel()$d)) pct(mean(dsel()$d$ALTO)) else "sin datos")
  output$g_dist_grupo <- renderPlotly({
    d <- dsel()$d; validate(need(nrow(d) > 0, "Sin emergencias en este distrito con los filtros actuales."))
    g <- d |> count(GRUPO) |> arrange(n)
    g$GRUPO <- factor(g$GRUPO, levels = g$GRUPO)
    plot_ly(g, x = ~n, y = ~GRUPO, type = "bar", orientation = "h", marker = list(color = unname(COL_GRUPO[as.character(g$GRUPO)])),
            text = ~paste0(GRUPO, ": ", n), hoverinfo = "text", textposition = "none") |> estilo(leyenda = FALSE)
  })
  output$g_dist_mes <- renderPlotly({
    d <- dsel()$d; req(nrow(d) > 0)
    m <- d |> count(MES_NUM) |> complete(MES_NUM = 1:12, fill = list(n = 0))
    plot_ly(m, x = ~MESES[MES_NUM], y = ~n, type = "bar", marker = list(color = AZUL), text = ~paste0(MESES_L[MES_NUM], ": ", n),
            hoverinfo = "text", textposition = "none") |> estilo(leyenda = FALSE) |>
      layout(xaxis = list(categoryorder = "array", categoryarray = MESES))
  })
  output$l_dist_det <- renderUI({
    d <- dsel()$d; req(nrow(d) > 0)
    g <- names(which.max(table(d$GRUPO))); m <- MESES_L[as.integer(names(which.max(table(d$MES_NUM))))]
    caja(paste0("En ", titulo(dsel()$nom), " el peligro más frecuente es ", b(tolower(g)), " y el mes con más emergencias es ", b(m),
                ". Se registraron ", fmt_n(sum(d$PERSONAS)), " personas impactadas en total."))
  })

  # ---- Diagnóstico ----
  output$g_tasa <- renderPlotly({
    g <- datos() |> group_by(GRUPO) |> summarise(t = mean(ALTO), n = n(), .groups = "drop") |> filter(n >= 10) |> arrange(t)
    g$GRUPO <- factor(g$GRUPO, levels = g$GRUPO)
    plot_ly(g, x = ~t, y = ~GRUPO, type = "bar", orientation = "h",
            marker = list(color = ~t, colorscale = ESC_ROJO, cmin = 0, cmax = 1),
            text = ~paste0(GRUPO, ": ", pct(t), " de ", fmt_n(n), " emergencias"), hoverinfo = "text", textposition = "none") |>
      estilo(leyenda = FALSE) |> layout(xaxis = list(tickformat = ".0%", range = c(0, 1)))
  })
  output$l_tasa <- renderUI({
    g <- datos() |> group_by(GRUPO) |> summarise(t = mean(ALTO), n = n(), .groups = "drop") |> filter(n >= 10) |> arrange(desc(t))
    caja(paste0("De cada 10 emergencias por ", b(tolower(g$GRUPO[1])), ", unas ", b(round(g$t[1] * 10)), " afectan a 100 personas o más. ",
                "En cambio, en ", tolower(g$GRUPO[nrow(g)]), " la proporción es de ", pct(g$t[nrow(g)], 1), ". El tipo de peligro es la primera pista de la gravedad."))
  })
  output$g_calor <- renderPlotly({
    t <- datos() |> group_by(PROV, GRUPO) |> summarise(t = mean(ALTO), n = n(), .groups = "drop") |> filter(n >= 15)
    validate(need(nrow(t) > 0, "No hay celdas con 15 casos o más."))
    w <- t |> select(PROV, GRUPO, t) |> pivot_wider(names_from = GRUPO, values_from = t)
    nn <- t |> select(PROV, GRUPO, n) |> pivot_wider(names_from = GRUPO, values_from = n)
    z <- as.matrix(w[, -1]); nm <- as.matrix(nn[, names(w)[-1]])
    hov <- matrix(paste0(titulo(rep(w$PROV, ncol(z))), ", ", rep(colnames(z), each = nrow(z)), "<br>", ifelse(is.na(z), "menos de 15 casos", paste0(pct(z), " de ", nm, " emergencias"))), nrow(z))
    plot_ly(x = colnames(z), y = titulo(w$PROV), z = z, type = "heatmap", colorscale = ESC_ROJO, zmin = 0, zmax = 1,
            text = hov, hoverinfo = "text", xgap = 2, ygap = 2, colorbar = list(tickformat = ".0%", len = 0.8, thickness = 12)) |>
      estilo(leyenda = FALSE) |> layout(xaxis = list(tickangle = -25), yaxis = list(autorange = "reversed"))
  })
  output$l_calor <- renderUI({
    t <- datos() |> filter(GRUPO == "Heladas y bajas temperaturas") |> group_by(PROV) |> summarise(t = mean(ALTO), n = n(), .groups = "drop") |> filter(n >= 15) |> arrange(desc(t))
    if (nrow(t) < 2) return(caja("Cada celda indica qué porcentaje de las emergencias de ese peligro en esa provincia fue de alto impacto. Más oscuro significa más grave."))
    caja(paste0("Cada celda indica qué porcentaje de las emergencias de ese peligro en esa provincia fue de alto impacto. Por ejemplo, en ", b(titulo(t$PROV[1])),
                " el ", b(pct(t$t[1], 1)), " de las heladas superó las 100 personas, mientras que en ", titulo(t$PROV[nrow(t)]), " fue el ", pct(t$t[nrow(t)], 1),
                ". La vulnerabilidad de cada lugar frente a cada peligro es lo que mejor explica la gravedad."))
  })
  output$g_cramer <- renderPlotly({
    d <- diag_chi |> arrange(`V de Cramér`); d$Variable <- factor(d$Variable, levels = d$Variable)
    plot_ly(d, x = ~`V de Cramér`, y = ~Variable, type = "bar", orientation = "h", marker = list(color = c(GRIS, "#86b6ef", AZUL, MARINO)),
            text = ~paste0(Variable, ": V = ", dec(`V de Cramér`, 0.01)), hoverinfo = "text", textposition = "none") |>
      estilo(leyenda = FALSE) |> layout(xaxis = list(range = c(0, 0.7)))
  })
  output$g_calidad <- renderPlotly({
    plot_ly(calidad, x = ~ANIO) |>
      add_lines(y = ~pct_con_ayuda, name = "Con ayuda anotada", line = list(color = AZUL, width = 2.5),
                text = ~paste0(ANIO, ": ", pct(pct_con_ayuda), " con ayuda anotada"), hoverinfo = "text") |>
      add_lines(y = ~pct_sin_personas, name = "Sin personas registradas", line = list(color = NARANJA, width = 2.5, dash = "dot"),
                text = ~paste0(ANIO, ": ", pct(pct_sin_personas), " sin personas"), hoverinfo = "text") |>
      estilo() |> layout(yaxis = list(tickformat = ".0%"))
  })

  # ---- Modelo ----
  output$g_modelos <- renderPlotly({
    m <- sel_mod |> filter(!grepl("Línea base", Modelo)) |> arrange(`ROC-AUC`)
    m$Modelo <- factor(m$Modelo, levels = m$Modelo)
    plot_ly(m, x = ~`ROC-AUC`, y = ~Modelo, type = "bar", orientation = "h",
            marker = list(color = ifelse(m$Modelo == "Random Forest", MARINO, "#86b6ef")),
            error_x = list(array = ~`ROC-AUC (de)`, color = "#9aa1b8", thickness = 1.2),
            text = ~paste0(Modelo, ": ", dec(`ROC-AUC`), " ± ", dec(`ROC-AUC (de)`)), hoverinfo = "text", textposition = "none") |>
      estilo(leyenda = FALSE) |> layout(xaxis = list(range = c(0.5, 0.95)))
  })
  output$g_opt <- renderPlotly({
    o <- opt; o$Estrategia <- factor(o$Estrategia, levels = rev(o$Estrategia))
    plot_ly(o, x = ~`Tiempo total (s)`, y = ~Estrategia, type = "bar", orientation = "h",
            marker = list(color = ifelse(grepl("Optuna", o$Estrategia), MARINO, "#86b6ef")),
            text = ~paste0(Estrategia, ": ", round(`Tiempo total (s)`), " segundos, ROC AUC ", dec(`Mejor ROC-AUC (CV)`, 0.0001)),
            hoverinfo = "text", textposition = "none") |>
      estilo(leyenda = FALSE, xtitulo = "Tiempo total en segundos")
  })
  output$t_opt <- renderUI({
    filas <- lapply(seq_len(nrow(opt)), function(i) tags$tr(class = if (i == 1) "gana",
      tags$td(opt$Estrategia[i]), tags$td(dec(opt$`Mejor ROC-AUC (CV)`[i], 0.0001)),
      tags$td(opt$`Evaluación en que lo alcanzó`[i]), tags$td(paste(round(opt$`Tiempo total (s)`[i]), "s"))))
    div(class = "px-3", tags$table(class = "tabla-simple",
        tags$thead(tags$tr(tags$th("Estrategia"), tags$th("ROC AUC"), tags$th("Prueba n.º"), tags$th("Tiempo"))), tags$tbody(filas)))
  })
  output$g_roc <- renderPlotly({
    plot_ly(roc, x = ~fpr, y = ~tpr, type = "scatter", mode = "lines", line = list(color = AZUL, width = 3), name = "Modelo optimizado",
            hoverinfo = "none", fill = "tozeroy", fillcolor = "rgba(42,120,214,.10)") |>
      add_lines(x = c(0, 1), y = c(0, 1), line = list(color = GRIS, dash = "dot"), name = "Azar") |>
      estilo(xtitulo = "Falsas alarmas: proporción de casos leves marcados como graves", ytitulo = "Casos graves detectados") |>
      layout(xaxis = list(tickformat = ".0%"), yaxis = list(tickformat = ".0%"))
  })
  output$g_conf <- renderPlotly({
    z <- confusion; et <- c("Leve", "Grave")
    txt <- matrix(c(paste0("Leves bien clasificadas: ", z[1, 1]), paste0("Graves no detectadas: ", z[2, 1]),
                    paste0("Falsas alarmas: ", z[1, 2]), paste0("Graves detectadas: ", z[2, 2])), 2)
    plot_ly(x = paste("Predicho", et), y = paste("Real", et), z = z, type = "heatmap", colorscale = ESC_AZUL, showscale = FALSE,
            text = txt, hoverinfo = "text", xgap = 3, ygap = 3) |>
      layout(annotations = lapply(1:4, function(i) list(x = rep(paste("Predicho", et), each = 2)[i], y = rep(paste("Real", et), 2)[i],
                                                         text = paste0("<b>", as.vector(z)[i], "</b>"), showarrow = FALSE,
                                                         font = list(size = 20, color = if (i == 1) "#ffffff" else TINTA)))) |>
      estilo(leyenda = FALSE) |> layout(yaxis = list(autorange = "reversed"))
  })
  output$g_imp <- renderPlotly({
    i <- imp |> arrange(media) |> tail(6); i$variable <- factor(i$variable, levels = i$variable)
    plot_ly(i, x = ~media, y = ~variable, type = "bar", orientation = "h", marker = list(color = MARINO),
            text = ~paste0(variable, ": el ROC AUC cae ", dec(media), " si se desordena"), hoverinfo = "text", textposition = "none") |>
      estilo(leyenda = FALSE, xtitulo = "Importancia de cada variable") |> layout(yaxis = list(tickfont = list(size = 10)))
  })
  output$l_conf <- renderUI({
    z <- confusion
    caja(paste0("A la izquierda, de las ", b(fmt_n(z[2, 1] + z[2, 2])), " emergencias graves de 2022 a 2026 el modelo marcó ", b(z[2, 2]),
                " como graves, y de las que marcó como graves acertó en el ", b(pct(z[2, 2] / (z[2, 2] + z[1, 2]), 1)), ". Detecta pocas porque en estos años bajó la proporción de casos graves y el umbral, fijado con datos antiguos, quedó alto. ",
                "Por eso las decisiones se basan en el orden de riesgo y no en un corte fijo. A la derecha, la variable que más ayuda es la historia de alto impacto de ese peligro en la provincia."))
  })

  # ---- Predicción ----
  R <- as.data.frame(riesgo); names(R) <- c("PROV", 1:12)
  meses_temp <- reactive(switch(input$temp_pred, t = 1:12, l = c(12, 1, 2, 3), h = 5:9))
  output$p_total <- renderText(dec(sum(R[, -1]), 1))
  output$p_prov  <- renderText(titulo(R$PROV[which.max(rowSums(R[, -1]))]))
  output$p_mes   <- renderText(mayus(MESES_L[which.max(colSums(R[, -1]))]))
  pos <- which(as.matrix(R[, -1]) == max(as.matrix(R[, -1])), arr.ind = TRUE)[1, ]
  output$p_celda <- renderText(paste(titulo(R$PROV[pos[1]]), MESES[pos[2]]))
  output$p_total_pie <- renderUI(div(class = "kpi-pie", "En toda la región en un año típico"))
  output$p_prov_pie  <- renderUI(div(class = "kpi-pie", paste(dec(max(rowSums(R[, -1])), 0.1), "emergencias graves esperadas al año")))
  output$p_mes_pie   <- renderUI(div(class = "kpi-pie", paste(dec(max(colSums(R[, -1])), 0.1), "graves esperadas en la región")))
  output$p_celda_pie <- renderUI(div(class = "kpi-pie", paste(dec(max(as.matrix(R[, -1])), 0.1), "graves esperadas en ese mes")))
  output$p_total_det <- renderUI(panel("Número de emergencias de alto impacto que se esperan en toda la región en un año típico, según el modelo.",
                                       "Es un promedio: algunos años serán peores y otros mejores, pero sirve para planificar recursos."))
  output$p_prov_det  <- renderUI(panel("Provincia con la mayor suma anual de emergencias graves esperadas.", "Combina cuántas emergencias suele tener y qué tan graves suelen ser."))
  output$p_mes_det   <- renderUI(panel("Mes con más emergencias graves esperadas en toda la región.", "Los recursos deben estar listos antes de ese mes."))
  output$p_celda_det <- renderUI(panel("Combinación de provincia y mes con el mayor riesgo del año.", "Es el punto donde conviene concentrar la preparación."))
  output$g_riesgo <- renderPlotly({
    z <- as.matrix(R[, -1]); ms <- meses_temp()
    z2 <- z; z2[, -ms] <- NA
    hov <- matrix(paste0(titulo(rep(R$PROV, 12)), ", ", rep(MESES_L, each = nrow(z)), ": ", dec(as.vector(z), 0.1), " emergencias graves esperadas"), nrow(z))
    plot_ly(x = MESES, y = titulo(R$PROV), z = z2, type = "heatmap", colorscale = ESC_ROJO, zmin = 0, zmax = max(z),
            text = hov, hoverinfo = "text", xgap = 2, ygap = 2, colorbar = list(len = 0.8, thickness = 12)) |>
      add_annotations(x = rep(MESES, each = nrow(z)), y = rep(titulo(R$PROV), 12), text = ifelse(is.na(as.vector(z2)), "", dec(as.vector(z), 0.1)),
                      showarrow = FALSE, font = list(size = 10, color = ifelse(as.vector(z) > max(z) * 0.55, "#ffffff", TINTA))) |>
      estilo(leyenda = FALSE) |> layout(yaxis = list(autorange = "reversed"), xaxis = list(categoryorder = "array", categoryarray = MESES))
  })
  output$l_riesgo <- renderUI({
    ms <- meses_temp(); s <- sort(setNames(rowSums(R[, as.character(ms), drop = FALSE]), R$PROV), decreasing = TRUE)
    nom <- switch(input$temp_pred, t = "en todo el año", l = "en la temporada de lluvias", h = "en la temporada de heladas")
    caja(paste0("Cada número es la cantidad de emergencias graves que se esperan en esa provincia y mes. ", mayus(nom), " encabezan ",
                b(titulo(names(s)[1])), ", ", b(titulo(names(s)[2])), " y ", b(titulo(names(s)[3])), ", con ", dec(s[1], 0.1), ", ", dec(s[2], 0.1), " y ", dec(s[3], 0.1),
                " casos esperados. Estas provincias deben recibir primero la ayuda y los equipos de evaluación."))
  })
  output$g_riesgo_prov <- renderPlotly({
    ms <- meses_temp(); s <- data.frame(PROV = R$PROV, v = rowSums(R[, as.character(ms), drop = FALSE])) |> arrange(v)
    s$et <- factor(titulo(s$PROV), levels = titulo(s$PROV))
    plot_ly(s, x = ~v, y = ~et, type = "bar", orientation = "h", marker = list(color = ~v, colorscale = ESC_ROJO, cmin = 0, cmax = max(s$v) * 1.1),
            text = ~paste0(et, ": ", dec(v, 0.1), " graves esperadas"), hoverinfo = "text", textposition = "none") |> estilo(leyenda = FALSE)
  })

  # ---- Prescripción ----
  output$g_ganancia <- renderPlotly({
    c0 <- input$capac / 100
    y0 <- approx(ganancia$capacidad, ganancia$captura, xout = c0)$y
    plot_ly(ganancia, x = ~capacidad, y = ~captura, type = "scatter", mode = "lines", name = "Priorizar con el modelo",
            line = list(color = AZUL, width = 3), hoverinfo = "none") |>
      add_lines(x = c(0, 1), y = c(0, 1), name = "Por orden de llegada", line = list(color = GRIS, dash = "dash")) |>
      add_markers(x = c0, y = y0, marker = list(color = ROJO, size = 12), name = "Capacidad elegida", hoverinfo = "none") |>
      estilo(xtitulo = "Reportes evaluados primero", ytitulo = "Emergencias graves cubiertas") |>
      layout(xaxis = list(tickformat = ".0%"), yaxis = list(tickformat = ".0%"))
  })
  output$l_ganancia <- renderUI({
    c0 <- input$capac / 100; y0 <- approx(ganancia$capacidad, ganancia$captura, xout = c0)$y
    caja(paste0("Si solo se puede evaluar primero el ", b(pct(c0, 1)), " de los reportes y se eligen los que el modelo considera más riesgosos, se atiende el ",
                b(pct(y0, 1)), " de las emergencias graves. Atendiendo por orden de llegada se cubriría solo el ", pct(c0, 1),
                ". La diferencia es el valor práctico del modelo: con el mismo personal se llega antes a quienes más lo necesitan."))
  })
  kits <- reactive(plan |> filter(temporada == input$temp_kit, abs(nivel - input$stock / 100) < 1e-6))
  output$s_kits <- renderText(fmt_n(sum(kits()$asignado)))
  output$s_nc <- renderText(pct(kits()$no_cubierto_total[1], 1))
  output$s_sombra <- renderText(dec(kits()$precio_sombra[1], 0.01))
  output$g_kits <- renderPlotly({
    k <- kits() |> arrange(demanda); k$et <- factor(titulo(k$provincia), levels = titulo(k$provincia))
    k$cob <- pmin(k$asignado / k$demanda, 1)
    plot_ly(k, y = ~et) |>
      add_bars(x = ~demanda, name = "Demanda esperada", orientation = "h", marker = list(color = "#dfe4ee"),
               text = ~paste0(et, ": demanda de ", fmt_n(demanda), " kits"), hoverinfo = "text", textposition = "none") |>
      add_bars(x = ~asignado, name = "Kits asignados", orientation = "h", marker = list(color = AZUL),
               text = ~paste0(et, ": ", fmt_n(asignado), " kits, cubre el ", pct(cob, 1)), hoverinfo = "text", textposition = "none") |>
      estilo(xtitulo = "Kits") |> layout(barmode = "overlay", bargap = 0.3)
  })
  output$l_kits <- renderUI({
    k <- kits() |> mutate(cob = asignado / demanda) |> arrange(desc(demanda))
    llenas <- titulo(k$provincia[k$cob >= 0.99])
    t <- if (input$temp_kit == "Lluvias, diciembre a marzo") "noviembre, antes de las lluvias" else "abril, antes de las heladas"
    caja(paste0("Con ", b(paste0(input$stock, " %")), " de la demanda disponible, el modelo reparte ", b(fmt_n(sum(k$asignado))), " kits. ",
                "Primero asegura a cada provincia una parte de su necesidad urgente y luego completa a las más cercanas al almacén de Juliaca",
                if (length(llenas)) paste0(", que en este caso quedan cubiertas al 100 %: ", paste(llenas, collapse = ", ")) else "", ". ",
                "El valor de un kit extra, llamado precio sombra, indica cuánta necesidad urgente evita cada kit adicional: cuanto más alto, más falta hace aumentar el stock. ",
                "Los kits deben estar en las provincias antes de ", t, "."))
  })
  output$g_sens <- renderPlotly({
    s <- plan |> distinct(temporada, nivel, no_cubierto_total)
    plot_ly(s, x = ~nivel, y = ~no_cubierto_total, color = ~temporada, colors = c(NARANJA, AZUL), type = "scatter", mode = "lines+markers",
            text = ~paste0(temporada, ": con ", pct(nivel, 1), " de stock queda sin cubrir el ", pct(no_cubierto_total, 1)), hoverinfo = "text") |>
      estilo(xtitulo = "Kits disponibles como porcentaje de la demanda", ytitulo = "Necesidad sin cubrir") |>
      layout(xaxis = list(tickformat = ".0%"), yaxis = list(tickformat = ".0%"))
  })

  # ---- Datos ----
  output$txt_n <- renderText(paste(fmt_n(nrow(datos())), "emergencias con los filtros actuales"))
  tabla <- reactive(datos() |> transmute(Fecha = FECHA, Provincia = titulo(PROV), Distrito = titulo(DIST), Peligro = titulo(PELIGRO_N),
                                         `Tipo de peligro` = GRUPO, `Personas impactadas` = PERSONAS, Damnificados = DAMNIFICADOS,
                                         Fallecidos = FALLECIDOS, `Camélidos perdidos` = CAMELIDO_PERD,
                                         `Alto impacto` = ifelse(ALTO == 1, "Sí", "No"), `Riesgo del modelo` = round(PROB, 3)))
  output$t_datos <- renderDT(datatable(tabla(), rownames = FALSE, style = "bootstrap5", class = "table-sm",
                                       options = list(pageLength = 15, scrollX = TRUE, order = list(list(0, "desc")),
                                                      language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"))))
  output$descargar <- downloadHandler(filename = function() "emergencias_puno_filtradas.csv",
                                      content = function(f) write_csv(tabla(), f))
}

shinyApp(ui, server)
