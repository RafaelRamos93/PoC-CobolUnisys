#!/bin/bash

echo "Analizando archivos COBOL en el directorio: $(pwd)"

temp_file="variables_detectadas.txt"
> "$temp_file"
errores=0

for archivo in $(find "$(pwd)" -type f \( -name "*.cobol" -o -name "*.cbl" \)); do
  echo "Analizando archivo: $archivo"
  en_data_division=false

  while IFS= read -r linea; do
    num_linea=$((num_linea + 1))

    # Detectar DATA DIVISION
    if [[ "$linea" =~ DATA\ DIVISION ]]; then
      en_data_division=true
    fi

    if [[ "$linea" =~ PROCEDURE\ DIVISION ]]; then
      en_data_division=false
    fi

    # Detectar variables en DATA DIVISION
    if $en_data_division; then
      if [[ "$linea" =~ ([0-9]+)\ +([A-Z0-9-]+)\ +PIC\ +([A-Z0-9()V]+) ]]; then
        nombre="${BASH_REMATCH[2]}"
        pic="${BASH_REMATCH[3]}"
        echo "$nombre ${#pic}" >> "$temp_file"
      fi
    fi

    # Detectar cambios en PROCEDURE DIVISION
    if [[ "$linea" =~ MOVE\ +([A-Z0-9-]+)\ +TO\ +([A-Z0-9-]+) ]]; then
      origen="${BASH_REMATCH[1]}"
      destino="${BASH_REMATCH[2]}"
      
      origen_tam=$(grep "^$origen " "$temp_file" | awk '{print $2}')
      destino_tam=$(grep "^$destino " "$temp_file" | awk '{print $2}')

      if [[ -n "$origen_tam" && -n "$destino_tam" && "$origen_tam" -gt "$destino_tam" ]]; then
        echo "Error en archivo $archivo, línea $num_linea: $origen (tamaño $origen_tam) no cabe en $destino (tamaño $destino_tam)."
        ((errores++))
      fi
    fi
  done < "$archivo"
done

if (( errores > 0 )); then
  echo "Se encontraron $errores errores."
  exit 1
else
  echo "No se encontraron errores."
  exit 0
fi
