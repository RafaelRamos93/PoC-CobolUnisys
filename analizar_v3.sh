#!/bin/bash

# Función para analizar un archivo COBOL
analyze_cobol_file() {
    local cobol_file="$1"
    local current_line=0
    local current_composite=""
    local simple_vars=()
    local composite_vars=()

    while IFS= read -r line || [ -n "$line" ]; do
        ((current_line++))
        # Eliminar espacios al inicio y al final
        line=$(echo "$line" | sed 's/^[ \t]*//;s/[ \t]*$//')

        # Detectar variables de nivel (01, 05, 10, etc.)
        if [[ $line =~ ^(01|05|10|66|77|88)\ ([A-Z0-9-]+)(\.*)(.*)$ ]]; then
            level="${BASH_REMATCH[1]}"
            name="${BASH_REMATCH[2]}"
            rest="${BASH_REMATCH[4]}"

            # Si contiene PIC, es una variable simple
            if [[ $rest =~ PIC\ ([A9X9]+)\(([0-9]+)\) ]]; then
                type="${BASH_REMATCH[1]}"
                size="${BASH_REMATCH[2]}"
                simple_vars+=("{\"name\": \"$name\", \"line\": $current_line, \"type\": \"$type\", \"size\": $size}")
                current_composite=""
            # Si no tiene PIC, es una variable compuesta
            elif [[ $level -lt 77 ]]; then
                composite_vars+=("{\"name\": \"$name\", \"line\": $current_line, \"level\": $level, \"subvars\": []}")
                current_composite="${#composite_vars[@]}" # Índice del compuesto actual
            fi
        elif [[ -n $current_composite && $line =~ ^[0-9]+\ ([A-Z0-9-]+)(.*)$ ]]; then
            sub_name="${BASH_REMATCH[1]}"
            sub_line="$current_line"
            composite_vars[$((current_composite - 1))]=$(echo "${composite_vars[$((current_composite - 1))]}" | jq ".subvars += [{\"name\": \"$sub_name\", \"line\": $sub_line}]")
        fi
    done < "$cobol_file"

    # Convertir resultados a JSON
    echo "{\"file\": \"$cobol_file\", \"simple_vars\": [${simple_vars[*]}], \"composite_vars\": [${composite_vars[*]}]}"
}

# Directorio de búsqueda (predeterminado a pwd si no se proporciona)
SEARCH_DIR="${1:-$(pwd)}"
OUTPUT_FILE="cobol_analysis.json"

# Verificar si el directorio existe
if [[ ! -d $SEARCH_DIR ]]; then
    echo "El directorio $SEARCH_DIR no existe."
    exit 1
fi

# Inicializar archivo JSON
echo "[" > "$OUTPUT_FILE"
first_file=true

# Buscar y analizar archivos COBOL en el directorio
for cobol_file in "$SEARCH_DIR"/*.cbl; do
    if [[ -f $cobol_file ]]; then
        if [[ $first_file == false ]]; then
            echo "," >> "$OUTPUT_FILE"
        fi
        analyze_cobol_file "$cobol_file" >> "$OUTPUT_FILE"
        first_file=false
    fi
done

# Cerrar el archivo JSON
echo "]" >> "$OUTPUT_FILE"

# Mensaje final
echo "Análisis completado. Resultados guardados en $OUTPUT_FILE"
