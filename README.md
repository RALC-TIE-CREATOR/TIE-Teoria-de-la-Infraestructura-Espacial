# TIE: Un banco de pruebas computacional para la Teoría de la Infraestructura Espacial

**Autor:** Rubén A. Lecona Curto (R@LC)
**Contacto:** ralc007@hotmail.com
**ORCID:** 0009-0008-4935-9010

## ⚠️ Aviso Importante

Este repositorio contiene el código fuente de una **simulación computacional** de los postulados de la Teoría de la Infraestructura Espacial (TIE). **No se presenta como una teoría científica validada**, sino como un **prototipo funcional** y una **herramienta de exploración conceptual**.

Invitamos a físicos, matemáticos y científicos de la computación a examinar el código, modificar los parámetros y explorar las consecuencias emergentes de este modelo de universo.

## 🚀 ¿Qué es esto?

Este proyecto implementa los axiomas fundamentales de TIE (red cúbica, Motor Absoluto, campo de fase `2π`) en el lenguaje de pruebas formales **Lean 4**. La simulación reproduce, con precisión sorprendente, un conjunto de constantes y fenómenos físicos, incluyendo:

- **Escala de Planck (`a`)**.
- **Materia Oscura (`84.1%`)** como el volumen no excitado de la red.
- **Energía Oscura (`Λ`)** como un residuo de eficiencia de la red (`5%`).
- **Ley de Gravedad (`1/r²`)** emergente de la red.
- **Masas del Protón y Neutrón** (con error < 0.1% en unidades de masa del electrón).
- **Teoremas formales verificados**: La ley de gravedad `1/r²` y otras propiedades estructurales han sido demostradas matemáticamente dentro de Lean 4.

### 🏆 Logros Clave de esta Simulación

| Categoría | Resultado |
| :--- | :--- |
| **Teoremas Demostrados** | 6 (incluyendo Ley de Gravedad `1/r²`) |
| **Predicciones Numéricas** | 5 (respaldadas por `#eval`) |
| **Axiomas Formales** | 1 (Periodicidad del Motor Absoluto) |

#### 🛠️ Cómo Ejecutar el Código
Opción 1:
Esta simulación está escrita en **Lean 4**. Para ejecutarla en tu propio ordenador, sigue estos pasos:

1.  **Instala Lean 4**: Sigue la guía oficial en [lean-lang.org](https://lean-lang.org/). La forma más sencilla es usando el script `elan`.
2.  **Clona este repositorio**:
    ```bash
    git clone https://github.com/RALC-TIE-CREATOR/TIE-Simulation.git
    cd TIE-Simulation
Opción 2:
1.  Descarga el archivo de este respositorio llamado "TIE.lean".
2.  Abre Lean 4 desde tu explorador de internet (https://lean-lang.org/).
3.  En el menú del lado derecho, busca la opcion que dice: "Load"
4.  Luego usa la opción "Load file from disk"
5.  Localiza en un equipo la ubicación donde guardaste "TIE.lean" y cargalo.
6.  Visualiza la información generada en el panel (a la derecha del editor). Allí verás los resultados de las simulaciones (`#eval`) como "📏 Tamaño del píxel fundamental, etc..."
