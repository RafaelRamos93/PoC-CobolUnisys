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

# Variable para verificar si se encontraron errores
found_error=0

# Procesar un archivo COBOL
process_file() {
    local file="$1"
    local in_data_division=0
    local in_procedure_division=0
    local line_number=0

    echo "  \"$file\": {" >> $output_file
    echo "    \"variables_simples\": [" >> $output_file
    local first_simple=1
    echo "    ]," >> $output_file
    echo "    \"grupos\": [" >> $output_file
    local first_group=1
    echo "    ]," >> $output_file
    echo "    \"errores\": [" >> $output_file
    local first_error=1

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
            if [[ $line =~ ^([0-9]+)\ ([A-Z0-9-]+)\ +(PIC\ ([A-Z0-9()V]+))? ]]; then
                local nivel="${BASH_REMATCH[1]}"
                local nombre="${BASH_REMATCH[2]}"
                local pic="${BASH_REMATCH[4]}"
                local tipo size

                # Determinar tipo y tamaño solo si tiene PIC
                if [[ -n $pic ]]; then
                    tipo=$(get_variable_type "$pic")
                    size="${#pic}"
                else
                    tipo="grupo"
                    size=0
                fi

                if [[ $tipo == "grupo" ]]; then
                    # Añadir como grupo
                    if ((first_group)); then
                        first_group=0
                    else
                        echo "      ," >> $output_file
                    fi
                    echo "      {\"nombre\": \"$nombre\", \"linea\": $line_number}" >> $output_file
                    grupos["$nombre"]="$nivel"
                else
                    # Añadir como variable simple
                    if ((first_simple)); then
                        first_simple=0
                    else
                        echo "      ," >> $output_file
                    fi
                    echo "      {\"nombre\": \"$nombre\", \"tipo\": \"$tipo\", \"tamaño\": $size, \"linea\": $line_number}" >> $output_file
                    variables["$nombre"]="$tipo:$size"
                fi
            fi
        fi

        # Validar asignaciones en PROCEDURE DIVISION
        if ((in_procedure_division)); then
            if [[ $line =~ MOVE\ ([A-Z0-9-]+)\ TO\ ([A-Z0-9-]+) ]]; then
                local var_from="${BASH_REMATCH[1]}"
                local var_to="${BASH_REMATCH[2]}"
                local from_info="${variables[$var_from]}"
                local to_info="${variables[$var_to]}"

                if [[ -z $from_info || -z $to_info ]]; then
                    # Error: Variable no definida
                    if ((first_error)); then
                        first_error=0
                    else
                        echo "      ," >> $output_file
                    fi
                    echo "      {\"error\": \"Asignación con variable no definida\", \"linea\": $line_number, \"detalle\": \"MOVE $var_from TO $var_to\"}" >> $output_file
                    found_error=1
                else
                    # Comparar tipo y tamaño
                    IFS=':' read -r from_type from_size <<< "$from_info"
                    IFS=':' read -r to_type to_size <<< "$to_info"
                    if [[ $from_type != $to_type || $from_size -gt $to_size ]]; then
                        if ((first_error)); then
                            first_error=0
                        else
                            echo "      ," >> $output_file
                        fi
                        echo "      {\"error\": \"Asignación incompatible\", \"linea\": $line_number, \"detalle\": \"MOVE $var_from TO $var_to\", \"from_tipo\": \"$from_type\", \"to_tipo\": \"$to_type\"}" >> $output_file
                        found_error=1
                    fi
                fi
            fi
        fi
    done < "$file"

    echo "    ]" >> $output_file
    echo "  }," >> $output_file
}

# Procesar todos los archivos COBOL en el directorio actual
for file in $(find . -type f -name "*.cbl"); do
    process_file "$file"
done

# Cerrar el archivo JSON
sed -i '$ s/,$//' $output_file
echo "}" >> $output_file

# Verificar si hubo errores y detener el pipeline
if ((found_error)); then
    echo "Errores detectados durante el análisis. Deteniendo pipeline."
    exit 1
else
    echo "Análisis completado sin errores."
    exit 0
fi
