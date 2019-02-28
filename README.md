# Análisis de la consulta sobre la termoeléctrica de Huexca

Este repo contiene los insumos y resultados del análisis que hice de la base de datos del "ejercicio participativo" sobre la termoeléctrica de Huexca y el Plan Integral Morelos. 

Obtuve la base de datos de los resultados de [esta liga](http://resultados.participacionsocial.gob.mx/resultado_mesas.zip). En [esta otra](http://participacionsocial.gob.mx/) puedes ver el portal de inicio del ejercicio participativo.

Los datos de la Lista Nominal a nivel municipal los obtuve del Instituto Nacional Electoral (INE) y tienen corte del 30 de septiembre de 2018. 

![My image](https://github.com/segasi/analisis_consulta_termo_huexca/blob/master/03_graficas/numero_mpos_por_rango_participacion.png)


**Importante**: la base de datos con los resultados del ejercicio participativo incluye dos renglones para cada una de los **147 módulos de participación** (o casillas) instalados en los **60** municipios seleccionados por la Secretaría de Gobernación. Un renglón registra los resultados del sábado y el otro los del domingo.

**Tres** de los **60** municipios -Coatetelco, Xoxocotla y Hueyapan- se crearon en 2017 (ver los detelles y fuentes hemerográficas en el código), por lo que para el INE su Lista Nominal sigue siendo parte de la del municipio que integraban antes (Miactlán, Puente de Ixtla y Tetela del Volcán, respectivamente).

Dada la discrepancia recién descrita, con el fin de calcular la participación ciudadana por municipio decidí **sumar** los votos registrados en los módulos de participación de Coatetelco, Xoxocotla y Hueyapan a los de Miactlán, Puente de Ixtla y Tetela del Volcán, respectivamente.

Por ello, la base de datos con resultados agregados a nivel municipal (la puedes descargar [aquí](https://github.com/segasi/analisis_consulta_termo_huexca/blob/master/04_datos_generados/bd_resultados_por_mpo.csv)), así como las gráficas y mapas que construyo a partir de ella, solo reportan datos de 57 municipios. 
