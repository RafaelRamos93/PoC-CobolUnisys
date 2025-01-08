#!/bin/bash

archivo="$1"

if [[ ! -f "$archivo" ]]; then
  echo "Error: El archivo $archivo no existe."
  exit 1
fi

echo "Leyendo el archivo COBOL: $archivo"

# Archivo temporal para almacenar las variables detectadas
temp_file="variables_detectadas.txt"
> "$temp_file"

# Leer variables desde la DATA DIVISION
en_data_division=false
errores=0

while IFS= read -r linea; do
  # Detectar el inicio de la DATA DIVISION
  if [[ "$linea" =~ DATA\ DIVISION ]]; then
    en_data_division=true
  fi

  # Detectar el final de la DATA DIVISION
  if [[ "$linea" =~ PROCEDURE\ DIVISION ]]; then
    en_data_division=false
  fi

  # Analizar variables en la DATA DIVISION
  if $en_data_division; then
    if [[ "$linea" =~ ([0-9]+)\ +([A-Z0-9-]+)\ +PIC\ +([A-Z0-9()V]+) ]]; then
      nivel="${BASH_REMATCH[1]}"
      nombre="${BASH_REMATCH[2]}"
      pic="${BASH_REMATCH[3]}"
      echo "Variable detectada: $nombre, Nivel: $nivel, PIC: $pic" >> "$temp_file"
    fi
  fi

  # Detectar cambios en las variables en la PROCEDURE DIVISION
  if [[ "$linea" =~ MOVE\ +([A-Z0-9-]+)\ +TO\ +([A-Z0-9-]+) ]]; then
    origen="${BASH_REMATCH[1]}"
    destino="${BASH_REMATCH[2]}"

    # Comprobar si el origen y el destino están en la lista de variables detectadas
    origen_info=$(grep "$origen" "$temp_file")
    destino_info=$(grep "$destino" "$temp_file")

    if [[ -n "$origen_info" && -n "$destino_info" ]]; then
      origen_pic=$(echo "$origen_info" | awk -F'PIC:' '{print $2}' | xargs)
      destino_pic=$(echo "$destino_info" | awk -F'PIC:' '{print $2}' | xargs)

      origen_tam=${#origen_pic}
      destino_tam=${#destino_pic}

      if (( origen_tam > destino_tam )); then
        echo "Error: Asignación de $origen (tamaño $origen_tam) a $destino (tamaño $destino_tam)."
        ((errores++))
      fi
    fi
  fi
done < "$archivo"

if (( errores > 0 )); then
  echo "Se encontraron $errores errores en las variables."
  exit 1
else
  echo "No se encontraron errores en las variables."
  exit 0
fi
