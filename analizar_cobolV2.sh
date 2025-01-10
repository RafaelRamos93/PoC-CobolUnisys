#!/bin/bash

# Directorio de búsqueda
SEARCH_DIR="${1:-$(pwd)}"

# Archivo de salida JSON
OUTPUT_FILE="cobol_variables.json"

# Inicializa el archivo de salida JSON
echo "[" > "$OUTPUT_FILE"

# Recorre todos los archivos COBOL en el directorio
find "$SEARCH_DIR" -type f -name "*.cbl" | while read -r FILE; do
  # Inicializa JSON para el archivo actual
  echo "  {" >> "$OUTPUT_FILE"
  echo "    \"file_name\": \"$(basename "$FILE")\"," >> "$OUTPUT_FILE"
  echo "    \"variables\": [" >> "$OUTPUT_FILE"

  # Procesa el archivo para extraer variables
  LINE_NUM=0
  VARIABLE_FOUND=false
  while IFS= read -r LINE; do
    LINE_NUM=$((LINE_NUM + 1))
    
    # Detecta variables sencillas y compuestas en COBOL
    if [[ $LINE =~ ^[[:space:]]*[0-9]{2}[[:space:]]+([A-Za-z0-9_-]+)[[:space:]]+PIC[[:space:]]+([A-Z]+)(\([0-9]+\))? ]]; then
      VAR_NAME="${BASH_REMATCH[1]}"
      VAR_TYPE="${BASH_REMATCH[2]}"
      VAR_SIZE="${BASH_REMATCH[3]}"
      VARIABLE_FOUND=true
      echo "      {" >> "$OUTPUT_FILE"
      echo "        \"name\": \"$VAR_NAME\"," >> "$OUTPUT_FILE"
      echo "        \"type\": \"$VAR_TYPE\"," >> "$OUTPUT_FILE"
      echo "        \"size\": \"${VAR_SIZE:-unknown}\"," >> "$OUTPUT_FILE"
      echo "        \"line\": $LINE_NUM" >> "$OUTPUT_FILE"
      echo "      }," >> "$OUTPUT_FILE"
    elif [[ $LINE =~ ^[[:space:]]*01[[:space:]]+([A-Za-z0-9_-]+) ]]; then
      COMPOUND_NAME="${BASH_REMATCH[1]}"
      VARIABLE_FOUND=true
      echo "      {" >> "$OUTPUT_FILE"
      echo "        \"name\": \"$COMPOUND_NAME\"," >> "$OUTPUT_FILE"
      echo "        \"type\": \"compound\"," >> "$OUTPUT_FILE"
      echo "        \"line\": $LINE_NUM," >> "$OUTPUT_FILE"
      echo "        \"subvariables\": [" >> "$OUTPUT_FILE"
      # Captura las subvariables dentro de un bloque compuesto
      while IFS= read -r SUBLINE && [[ $SUBLINE =~ ^[[:space:]]*[0-9]{2}[[:space:]]+([A-Za-z0-9_-]+)[[:space:]]+PIC[[:space:]]+([A-Z]+)(\([0-9]+\))? ]]; do
        SUB_VAR_NAME="${BASH_REMATCH[1]}"
        SUB_VAR_TYPE="${BASH_REMATCH[2]}"
        SUB_VAR_SIZE="${BASH_REMATCH[3]}"
        echo "          {" >> "$OUTPUT_FILE"
        echo "            \"name\": \"$SUB_VAR_NAME\"," >> "$OUTPUT_FILE"
        echo "            \"type\": \"$SUB_VAR_TYPE\"," >> "$OUTPUT_FILE"
        echo "            \"size\": \"${SUB_VAR_SIZE:-unknown}\"" >> "$OUTPUT_FILE"
        echo "          }," >> "$OUTPUT_FILE"
      done
      # Elimina la última coma de subvariables
      sed -i '$ s/,$//' "$OUTPUT_FILE"
      echo "        ]" >> "$OUTPUT_FILE"
      echo "      }," >> "$OUTPUT_FILE"
    fi
  done < "$FILE"
  
  # Elimina la última coma de variables si existen
  if $VARIABLE_FOUND; then
    sed -i '$ s/,$//' "$OUTPUT_FILE"
  fi

  echo "    ]" >> "$OUTPUT_FILE"
  echo "  }," >> "$OUTPUT_FILE"
done

# Elimina la última coma del JSON global y cierra el arreglo
sed -i '$ s/,$//' "$OUTPUT_FILE"
echo "]" >> "$OUTPUT_FILE"

echo "Análisis completado. Resultados guardados en $OUTPUT_FILE"
