"""Toma capturas de cada pestaña del tablero Shiny (debe estar corriendo, por defecto en el puerto 8766)."""
import os, sys
from playwright.sync_api import sync_playwright

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(BASE, "capturas")
URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8766"
PESTANAS = [("Resumen", "resumen"), ("Mapa", "mapa"), ("Diagnóstico", "diagnostico"), ("Modelo", "modelo"),
            ("Predicción", "prediccion"), ("Prescripción", "prescripcion"), ("Datos", "datos")]

def alto_completo(pag):
    h = pag.evaluate("""() => Math.max(document.body.scrollHeight,
        ...Array.from(document.querySelectorAll('.bslib-page-main, .main, .tab-content, .bslib-sidebar-layout > .main'))
            .map(e => e.scrollHeight + e.getBoundingClientRect().top))""")
    h = min(int(h) + 40, 4200)
    pag.set_viewport_size({"width": 1500, "height": h})
    pag.wait_for_timeout(2500)

with sync_playwright() as p:
    nav = p.chromium.launch(channel="chrome", args=["--no-sandbox", "--disable-gpu"])
    pag = nav.new_page(viewport={"width": 1500, "height": 900}, device_scale_factor=1.25, locale="es-PE")
    errores = []
    pag.on("console", lambda m: errores.append(m.text) if m.type == "error" else None)
    pag.goto(URL, wait_until="networkidle")
    pag.wait_for_timeout(5000)
    for i, (nombre, corto) in enumerate(PESTANAS, 1):
        pag.set_viewport_size({"width": 1500, "height": 900})
        pag.get_by_role("tab", name=nombre).click()
        pag.wait_for_timeout(5000)
        alto_completo(pag)
        archivo = os.path.join(OUT, f"{i:02d}_{corto}.png")
        pag.screenshot(path=archivo, full_page=True, timeout=120000)
        print("captura:", archivo)
    print("errores de consola:", errores[:8])
    nav.close()
