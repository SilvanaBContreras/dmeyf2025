convertir_ipynb_a_rmd <- function(archivo_ipynb, archivo_rmd, 
                                  filtrar_python = TRUE,
                                  titulo = "Análisis",
                                  ruta_local = NULL) {
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  notebook <- read_json(archivo_ipynb, simplifyVector = FALSE)
  con <- file(archivo_rmd, "w", encoding = "UTF-8")
  
  # Encabezado
  cat("---\n", file = con)
  cat("title: '", titulo, "'\n", sep = "", file = con)
  cat("output: html_document\n", file = con)
  cat("---\n\n", file = con)
  
  chunk_num <- 1
  
  for(celda in notebook$cells) {
    
    if(celda$cell_type == "markdown") {
      texto <- unlist(celda$source)
      cat(texto, sep = "", file = con)
      cat("\n", file = con)
      
    } else if(celda$cell_type == "code") {
      codigo <- unlist(celda$source)
      codigo_junto <- paste(codigo, collapse = "")
      
      # Filtrar Python
      es_python <- any(grepl("from google.colab|%%shell|^!|import |GPUtil|pip install", 
                             codigo_junto, ignore.case = TRUE))
      
      if(filtrar_python && es_python) next
      if(nchar(trimws(codigo_junto)) == 0) next
      
      # Reemplazar rutas si se especificó ruta_local
      if(!is.null(ruta_local)) {
        codigo_modificado <- character(length(codigo))
        
        for(i in seq_along(codigo)) {
          linea <- codigo[i]
          
          # Detectar líneas con fread o fwrite
          if(grepl("fread\\(|fwrite\\(", linea)) {
            
            # Comentar línea original
            codigo_modificado[i] <- paste0("# ", linea)
            
            # Crear línea modificada
            linea_nueva <- linea
            linea_nueva <- gsub('"/content/datasets/', 
                                paste0('"', ruta_local, 'datos/'), 
                                linea_nueva)
            linea_nueva <- gsub('"/content/buckets/b1/', 
                                paste0('"', ruta_local, 'resultados/'), 
                                linea_nueva)
            linea_nueva <- gsub('"/content/buckets/b1/datasets/', 
                                paste0('"', ruta_local, 'datos/'), 
                                linea_nueva)
            linea_nueva <- gsub('"/content/buckets/b1/exp/', 
                                paste0('"', ruta_local, 'experimentos/'), 
                                linea_nueva)
            
            # Agregar línea nueva (si tiene \n al final, lo mantiene)
            if(grepl("\n$", linea)) {
              codigo_modificado[i] <- paste0(codigo_modificado[i], linea_nueva)
            } else {
              codigo_modificado[i] <- paste0(codigo_modificado[i], "\n", linea_nueva)
            }
            
          } else {
            # Líneas sin fread/fwrite se mantienen igual
            codigo_modificado[i] <- linea
          }
        }
        
        codigo <- codigo_modificado
      }
      
      # Escribir chunk
      cat("```{r chunk", chunk_num, "}\n", sep = "", file = con)
      cat(codigo, sep = "", file = con)
      
      # Si el último carácter NO es \n, agregarlo antes del cierre
      ultimo_caracter <- substr(codigo[length(codigo)], 
                                nchar(codigo[length(codigo)]), 
                                nchar(codigo[length(codigo)]))
      
      if(ultimo_caracter != "\n") {
        cat("\n", file = con)
      }
      
      cat("```\n\n", file = con)
      
      chunk_num <- chunk_num + 1
    }
  }
  
  close(con)
  cat("Archivo creado:", archivo_rmd, "\n")
  cat("Chunks convertidos:", chunk_num - 1, "\n")
  
  # Crear carpetas necesarias
  if(!is.null(ruta_local)) {
    dir.create(paste0(ruta_local, "datos"), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0(ruta_local, "resultados"), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0(ruta_local, "experimentos"), showWarnings = FALSE, recursive = TRUE)
    cat("\nCarpetas creadas en:", ruta_local, "\n")
    cat("  - datos/\n")
    cat("  - resultados/\n")
    cat("  - experimentos/\n")
  }
}

# USAR ASÍ:
convertir_ipynb_a_rmd(
  "C:/Users/Silvana/Downloads/competencia_01_9004_pruebo_test_y_sub",
  "C:/Users/Silvana/Documents/Maestria Exactas/DMEyF/competencia 1/competencia_01_9004_pruebo_test_y_sub.Rmd",
  ruta_local = "C:/Users/Silvana/Documents/Maestria Exactas/DMEyF/competencia 1/"
)

# ABRIR

# Opción 1: Abrirlo directamente en RStudio
file.edit("competencia_01_9004_pruebo_test_y_sub.Rmd")

# Opción 2: Si no sabés dónde quedó, primero ver la ubicación
getwd()  # Te muestra dónde está

# Opción 3: Abrirlo con ruta completa
# file.edit("C:/ruta/completa/competencia_01_9004_pruebo_test_y_sub.Rmd")
