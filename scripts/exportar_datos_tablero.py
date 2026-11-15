"""Ejecuta el código del cuaderno Semana4_INDECI_Puno.ipynb y exporta los insumos del tablero Shiny.

Uso, desde la carpeta raíz de la actividad:
    python scripts/exportar_datos_tablero.py

Genera en shiny/data las probabilidades del modelo por emergencia, la curva de ganancia, la curva ROC de prueba,
la matriz de confusión, el plan de kits resuelto con PuLP para stocks entre el 30 y el 120 % de la demanda y los
parámetros por provincia. También copia las tablas de resultados/ que usa el tablero.
"""
import json
import shutil
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import nbformat

RAIZ = Path(__file__).resolve().parents[1]
SALIDA = RAIZ / "shiny" / "data"
SALIDA.mkdir(parents=True, exist_ok=True)

nb = nbformat.read(RAIZ / "Semana4_INDECI_Puno.ipynb", as_version=4)
codigo = "\n\n".join(c.source for c in nb.cells if c.cell_type == "code")
# Las figuras ya están en figuras/: aquí solo se cierran sin mostrarlas
codigo = codigo.replace("    fig.savefig(FIG / f\"{nombre}.png\")\n    plt.show()", "    import matplotlib.pyplot as _p; _p.close(fig)")

import os
os.chdir(RAIZ)
entorno = {"__name__": "__main__"}
exec(compile(codigo, "Semana4_INDECI_Puno.ipynb", "exec"), entorno)
g = entorno
np, pd = g["np"], g["pd"]

# ---- Probabilidades del modelo final para cada emergencia ----
df, es_train = g["df"], g["es_train"]
e = df[["COD", "PROB", "ALTO"]].copy()
e["CONJUNTO"] = np.where(es_train.values, "entrenamiento", "prueba")
limpio = pd.read_csv(RAIZ / "datos" / "puno_emergencias_limpio.csv")
cols = ["COD", "FECHA", "ANIO", "MES", "UBIGEO", "PROV", "DIST", "PELIGRO_N", "GRUPO", "FALLECIDOS", "DAMNIFICADOS",
        "AFECTADOS", "PERSONAS", "ALTO", "VIV_DESTRUIDAS", "VIV_AFECTADAS", "CAMELIDO_PERD", "HA_CULT_AFECT",
        "HA_CULT_DESTR", "AYUDA_PESO"]
limpio[cols].merge(e[["COD", "PROB", "CONJUNTO"]], on="COD", how="left").to_csv(
    SALIDA / "emergencias_puno.csv", index=False, encoding="utf-8")

# ---- Evaluación en prueba ----
pd.DataFrame({"capacidad": g["capacidad"], "captura": g["captura"]}).iloc[::5].to_csv(SALIDA / "curva_ganancia.csv", index=False)
fpr, tpr, _ = g["roc_curve"](g["yte"], g["p_final"])
pd.DataFrame({"fpr": fpr, "tpr": tpr}).to_csv(SALIDA / "roc_prueba.csv", index=False)
pd.DataFrame(g["confusion_matrix"](g["yte"], g["PRED_FINAL"])).to_csv(SALIDA / "matriz_confusion.csv", index=False)

# ---- Plan de kits para una grilla de niveles de stock ----
filas = []
for temp in g["TEMPORADAS"]:
    dem = g["demandas"][temp]
    for n in np.round(np.arange(0.30, 1.201, 0.05), 2):
        a, estado, pi, z = g["asignar"](dem, round(n * dem.sum()))
        nc = g["no_cubierto"](dem, a)
        for p in dem.index:
            filas.append({"temporada": temp, "nivel": n, "provincia": p, "demanda": dem[p], "asignado": round(a[p]),
                          "no_cubierto_total": nc, "precio_sombra": abs(pi)})
pd.DataFrame(filas).to_csv(SALIDA / "plan_kits_grilla.csv", index=False)
pd.DataFrame({"peso": g["peso"], "km": pd.Series(g["DIST_JULIACA"]), "mediana_graves": g["mediana_graves"]}).to_csv(
    SALIDA / "parametros_provincia.csv")
json.dump({"umbral": float(g["UMBRAL_FINAL"]), "puntos": {str(k): float(v) for k, v in g["puntos"].items()}},
          open(SALIDA / "extra.json", "w"))

# ---- Tablas de resultados que usa el tablero ----
for f in ["calidad_por_anio.csv", "seleccion_modelos_cv.csv", "comparacion_optimizadores.csv",
          "evaluacion_prueba_2022_2026.csv", "importancia_permutacion.csv", "pruebas_diagnosticas.csv",
          "matriz_riesgo_provincia_mes.csv", "mejores_hiperparametros.json", "kpi_por_periodo.csv",
          "resumen_por_grupo.csv", "plan_prepos_kits.csv"]:
    shutil.copy(RAIZ / "resultados" / f, SALIDA / f)
print("Insumos del tablero exportados en", SALIDA)
