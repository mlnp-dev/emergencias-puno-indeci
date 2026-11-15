# Emergencias por peligros naturales en la región Puno (2003-2026)

Actividad de la Semana 4 del curso **Business Intelligence and Data Analytics** (Maestría, EPG, UNA Puno; docente: Dr. Fred Torres Cruz).
Recorre el proceso completo de analítica con las 6 593 emergencias que el SINPAD del INDECI registró en Puno entre enero de 2003 y junio de 2026: revisión de calidad, análisis exploratorio, selección de modelos, optimización de hiperparámetros y analítica descriptiva, diagnóstica, predictiva y prescriptiva.

- **Tablero interactivo:** https://mlnp.shinyapps.io/emergencias-puno/
- **Informe:** `Semana 4 - Informe Mary Luz Nina Palacios.pdf`

## Resultados principales

- Las heladas son el 29 % de las emergencias, pero concentran el 77 % de las personas impactadas y el 76 % de los camélidos perdidos.
- Objetivo predictivo: si una emergencia afectará a 100 o más personas, con la información disponible al momento del reporte.
- De ocho modelos (validación cruzada temporal), Random Forest fue el mejor (ROC AUC 0,882). Optuna, scikit-optimize, Hyperopt y búsqueda aleatoria, con el mismo presupuesto, llegaron a 0,890-0,891; Optuna fue el más rápido.
- Prueba 2022-2026: ROC AUC 0,821 (0,800 con valores por defecto).
- Prescriptiva: priorizar evaluaciones de daños por riesgo (54 % de casos graves revisando el 20 % de reportes) y reparto de kits entre provincias con programación lineal (PuLP).

## Contenido

| Ruta | Qué contiene |
|---|---|
| `Semana4_INDECI_Puno.ipynb` / `.html` | Cuaderno ejecutado con los cinco pasos e interpretación de cada resultado |
| `datos/` | Archivos originales del INDECI (base 2003-2025, 2026-I, diccionario y metadatos) y la base limpia de Puno |
| `figuras/` | Las 20 figuras del análisis |
| `resultados/` | Tablas CSV, mejores hiperparámetros (JSON) y caché de los optimizadores (evita repetir la optimización) |
| `shiny/` | Código (`app.R`) y datos del tablero |
| `capturas/` | Imágenes de cada pestaña del tablero |
| `scripts/` | `exportar_datos_tablero.py` (genera `shiny/data`) y `capturas_tablero.py` |
| `informe/informe.tex` | Fuente LaTeX del informe |

## Cómo reproducir

```bash
pip install pandas numpy scikit-learn xgboost lightgbm optuna scikit-optimize hyperopt pulp matplotlib seaborn nbformat jupyter
jupyter nbconvert --to notebook --execute Semana4_INDECI_Puno.ipynb
python scripts/exportar_datos_tablero.py
```

El tablero se ejecuta en R con `shiny::runApp("shiny")`.

## Fuente de datos

Instituto Nacional de Defensa Civil (INDECI), *Emergencias y daños a nivel nacional por departamento*, Plataforma Nacional de Datos Abiertos del Perú. Licencia Open Data Commons Attribution (ODC-BY). Los archivos de `datos/` se redistribuyen sin cambios, salvo `puno_emergencias_limpio.csv`, que es derivado.

## Autora

Mary Luz Nina Palacios
