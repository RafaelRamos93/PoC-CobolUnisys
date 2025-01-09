#!/bin/bash

echo "Analizando archivos COBOL en el directorio: $(pwd)"

json_output="variables_detectadas.json"
echo "{" >"$json_output"
errores=0
primero=true

obtener_tipo() {
  local pic=$1
  if [[ $pic =~ 9 ]]; then
    if [[ $pic =~ V ]]; then
      echo "decimal"
    else
      echo "entero"
    fi
  elif [[ $pic =~ X ]] || [[ $pic =~ A ]]; then
    echo "string"
  else
    echo "desconocido"
  fi
}

for archivo in $(find "$(pwd)" -type f \( -name "*.cobol" -o -name "*.cbl" \)); do
  echo "Analizando archivo: $archivo"
  en_data_division=false
  declare -A variables
  num_linea=0

  if ! $primero; then
    echo "," >>"$json_output"
  else
    primero=false
  fi

  echo "\"$archivo\": {" >>"$json_output"
  echo "  \"variables_simples\": [" >>"$json_output"
  primero_var=true

  while IFS= read -r linea; do
    num_linea=$((num_linea + 1))

    # Detectar DATA DIVISION
    if [[ "$linea" =~ DATA\ DIVISION ]]; then
      en_data_division=true
    fi

    if [[ "$linea" =~ PROCEDURE\ DIVISION ]]; then
      en_data_division=false
    fi

    # Detectar variables simples en DATA DIVISION
    if $en_data_division; then
      if [[ "$linea" =~ ([0-9]+)\ +([A-Z0-9-]+)\ +PIC\ +([A-Z0-9()V]+) ]]; then
        nombre="${BASH_REMATCH[2]}"
        pic="${BASH_REMATCH[3]}"
        tipo=$(obtener_tipo "$pic")

        variables["$nombre"]="$tipo"
        if ! $primero_var; then
          echo "," >>"$json_output"
        else
          primero_var=false
        fi
        echo "    {\"nombre\": \"$nombre\", \"linea\": $num_linea, \"tipo\": \"$tipo\"}" >>"$json_output"
      fi
    fi

    # Validar movimientos en PROCEDURE DIVISION
    if ! $en_data_division; then
      if [[ "$linea" =~ MOVE\ +([A-Z0-9-]+)\ +TO\ +([A-Z0-9-]+) ]]; then
        origen="${BASH_REMATCH[1]}"
        destino="${BASH_REMATCH[2]}"

        if [[ -n ${variables["$origen"]} ]] && [[ -n ${variables["$destino"]} ]]; then
          tipo_origen=${variables["$origen"]}
          tipo_destino=${variables["$destino"]}

          if [[ "$tipo_origen" != "$tipo_destino" ]]; then
            echo "Tipo incompatible en línea $num_linea: $origen ($tipo_origen) no puede asignarse a $destino ($tipo_destino)"
            errores=$((errores + 1))
          fi
        fi
      fi
    fi
  done <"$archivo"

  echo "  ]" >>"$json_output"
  echo "}" >>"$json_output"
done

echo "}" >>"$json_output"

if ((errores > 0)); then
  echo "Se encontraron $errores errores."
  exit 1
else
  echo "No se encontraron errores."
  exit 0
fi
