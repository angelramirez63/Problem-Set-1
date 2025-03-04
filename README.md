# Problem-Set-1

#### Juan Esteban Díaz Torres - 202020319
#### Natalia Plata Ángel - 201730699
#### Ángel David Ramírez Torres - 202112704

## Tabla de Contenidos
- [Documentación](#Documentación)
- [Scripts](#Scripts)
- [Stores](#Stores)
- [Views](#Views)

## Documentación
Texto de la introducción.

## Scripts
La carpeta de scripts contiene la solución del taller diferenciado, organizada en un script por sección, junto con una carpeta adicional que incluye los scripts de prueba. A continuación, se detalla la estructura y contenido:

**Script de limpieza:** El archivo "1_Data_cleaning.R" se encarga del procesamiento de los datos desde su descarga inicial.

**Script de estadísticas descriptivas:** En el archivo "2_descriptive_statistics.R" se realiza la revisión de las observaciones y las variables de interés.

**Desarrollo de los puntos 3, 4 y 5:** Estos apartados están organizados en scripts independientes, numerados y nombrados según el tema que abordan.

**Carpeta de scripts iniciales:** Finalmente, se incluye una carpeta con los scripts que cada miembro del equipo utilizó para comenzar a desarrollar el taller.

## Stores
Esta carpeta contiene las bases de datos utilizadas para la solución de los modelos. Incluye la base de datos original, llamada "datos_GEIH.rds", y la base de datos después del proceso de limpieza, denominada "datos_modelos.rds". Adicionalmente, se encuentran las bases de datos generadas mediante el método de Bootstrap con 1000 y 10000 iteraciones. Todos los archivos están almacenados en formato .rds para garantizar su compatibilidad y eficiencia en el manejo de la información.

## Views
Esta carpeta contiene las gráficas y tablas generadas en la solución del taller. Las gráficas están en formato PNG y las tablas en formato txt.

### Gráficas (formato PNG):
**desempeño_validation_set.png:** Gráfica que muestra el desempeño del modelo en el conjunto de validación.

**dinamica_promedio_salario_por_hora_con_la_edad:** Gráfica que ilustra la dinámica del salario promedio en función de la edad diferenciado por género.

**distribucion_errores.png:** Gráfica que representa la distribución de los errores del modelo.

**salario_por_edad.png:** Gráfica que muestra la relación entre el salario y la edad.

**salario_por_edad_genero.png:** Gráfica que compara los salarios por edad y género.

**salario_por_edad_scatter.png:** Gráfica de dispersión que representa la relación entre el salario y la edad.

**y_ingLab_m_ha.png:** Gráfica que posiblemente muestra ingresos laborales en función de alguna variable (el nombre completo no está especificado).

### Tablas (formato TXT):
**desc_est.txt:** Tabla con estadísticas descriptivas de los datos.

**mod1.txt:** Tabla que contiene los resultados o parámetros del primer modelo.

**tabla_comparacion_errores_prediccion.txt:** Tabla que compara los errores de predicción de las dififerentes aproximaciones.

**tabla_desempeño_validation_set.txt:** Tabla que detalla el desempeño de los diferentes modelos en el conjunto de validación.

**tabla_intervalos_edad_pico.txt:** Tabla que presenta intervalos de edad donde se observan picos de ingreso.
