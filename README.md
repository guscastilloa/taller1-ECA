# Taller 1 - ECA

Este repositorio contiene el código y los datos para el Taller 1 del curso de Evaluación de Impacto.

## Estructura del Directorio

```
taller1-ECA/
│
├── config/              # Archivos de configuración
│   └── stata_config.do  # Configuración de Stata
├── code/               # Códigos de análisis
├── data/              # Datos del proyecto
├── docs/              # Documentación del proyecto
└── output/           # Resultados generados
    ├── figures/      # Gráficos
    └── tablas/       # Tablas
```

## Configuración Local

### Requisitos Previos
- Stata 17 o superior
- Git
- Cuenta de GitHub

### Pasos para Trabajar Localmente

1. Clonar el repositorio:
```bash
git clone https://github.com/usuario/taller1-ECA.git
cd taller1-ECA
```

2. Configurar Stata:
   - Abrir Stata
   - Ejecutar el archivo de configuración:
   ```stata
   do "config/stata_config.do"
   ```
   - El script detectará automáticamente su sistema operativo y configurará las rutas correspondientes

3. Estructura de trabajo:
   - Los datos originales deben colocarse en `data/raw/`
   - Los códigos de análisis se encuentran en `code/`
   - Los resultados se generarán automáticamente en `output/`

## Integración con Overleaf

Este proyecto está vinculado a un documento de LaTeX en Overleaf para facilitar la colaboración en la redacción del informe. Para acceder:

1. Solicitar acceso al proyecto en Overleaf al administrador
2. Los cambios en el documento se sincronizan automáticamente
3. Las figuras y tablas generadas localmente se pueden importar directamente al documento de Overleaf

## Contribuciones

- Gustavo Castillo (Mac OS)
- Ana María Patrón (Windows)

## Notas Importantes

- Mantener actualizado el archivo de configuración para ambos sistemas operativos
- Documentar todos los cambios importantes en los commits
