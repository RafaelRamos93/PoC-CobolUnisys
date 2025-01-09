#!/bin/bash

output_file="variables_detectadas.json"
declare -A variables
declare -A grupos

# Inicializar el archivo JSON
echo "{" > $output_file

# Función para determinar el tipo de una variable según su cláusula PIC
get_variable_type() {
    local pic=$1
    if [[ $pic == *"9"* ]]; then
        if [[ $pic == *"V"* ]]; then
            echo "decimal"
        else
            echo "entero"
        fi
    elif [[ $pic == *"X"* || $pic == *"A"* ]]; then
        echo "string"
    else
        echo "desconocido"
    fi
}

# Procesar un archivo COBOL
process_file() {
    local file="$1"
    local in_data_division=0
    local in_procedure_division=0
    local line_number=0

    echo "  \"$file\": {" >> $output_file
    echo "    \"variables_simples\": [" >> $output_file
    echo "    \"grupos\": [" >> $output_file
    echo "    \"errores\": [" >> $output_file

    while IFS= read -r line; do
        line_number=$((line_number + 1))
        line=$(echo "$line" | sed 's/^ *//;s/ *$//') # Eliminar espacios en blanco

        # Detectar las divisiones
        if [[ $line =~ DATA\ DIVISION ]]; then
            in_data_division=1
            in_procedure_division=0
            continue
        fi
        if [[ $line =~ PROCEDURE\ DIVISION ]]; then
            in_data_division=0
            in_procedure_division=1
            continue
        fi

        # Procesar variables en DATA DIVISION
        if ((in_data_division)); then
            if [[ $line =~ ^([0-9]+)\ ([A-Z0-9-]+)\ +PIC\ +([A-Z0-9()V]+) ]]; then
                local nivel="${BASH_REMATCH[1]}"
                local nombre="${BASH_REMATCH[2]}"
                local pic="${BASH_REMATCH[3]}"
                local tipo
                tipo=$(get_variable_type "$pic")
                local size="${#pic}"

                variables["$nombre"]="$tipo:$size:$nivel"

                # Si es un nivel 1, es un grupo
                if [[ $nivel -eq 1 ]]; then
                    grupos["$nombre"]="$line_number"
                fi

                echo "      {\"nombre\": \"$nombre\", \"linea\": $line_number, \"tamaño\": \"$size\", \"tipo\": \"$tipo\"}," >> $output_file
            fi
        fi

        # Validar movimientos en PROCEDURE DIVISION
        if ((in_procedure_division)); then
            if [[ $line =~ MOVE\ ([A-Z0-9-]+)\ TO\ ([A-Z0-9-]+) ]]; then
                local origen="${BASH_REMATCH[1]}"
                local destino="${BASH_REMATCH[2]}"

                if [[ -n ${variables[$origen]} && -n ${variables[$destino]} ]]; then
                    IFS=':' read -r tipo_origen size_origen nivel_origen <<<"${variables[$origen]}"
                    IFS=':' read -r tipo_destino size_destino nivel_destino <<<"${variables[$destino]}"

                    # Validar tamaños
                    if ((size_origen > size_destino)); then
                        echo "      {\"linea\": $line_number, \"mensaje\": \"Error: $origen (tamaño $size_origen) no cabe en $destino (tamaño $size_destino).\"}," >> $output_file
                    fi

                    # Validar tipos
                    if [[ $tipo_origen != "$tipo_destino" ]]; then
                        echo "      {\"linea\": $line_number, \"mensaje\": \"Error: Tipo incompatible entre $origen ($tipo_origen) y $destino ($tipo_destino).\"}," >> $output_file
                    fi
                fi
            fi
        fi
    done <"$file"

    # Cerrar los arreglos de JSON
    sed -i '$ s/,$//' $output_file
    echo "    ]," >> $output_file
    echo "    \"grupos\": [" >> $output_file
    for grupo in "${!grupos[@]}"; do
        echo "      {\"nombre\": \"$grupo\", \"linea\": ${grupos[$grupo]}}," >> $output_file
    done
    sed -i '$ s/,$//' $output_file
    echo "    ]" >> $output_file
    echo "  }," >> $output_file
}

# Procesar todos los archivos COBOL en el directorio actual
for file in *.cobol *.cbl; do
    if [[ -f "$file" ]]; then
        process_file "$file"
    fi
done

# Cerrar el archivo JSON
sed -i '$ s/,$//' $output_file
echo "}" >> $output_file

echo "Análisis completado. Resultados guardados en $output_file."
