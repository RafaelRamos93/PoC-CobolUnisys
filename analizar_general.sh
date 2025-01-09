#!/bin/bash

output_file="variables_detectadas.json"
echo "{" > $output_file

process_file() {
    local file="$1"
    echo "Analizando archivo: $file"

    echo "  \"$file\": {" >> $output_file
    echo "    \"variables_simples\": [" >> $output_file

    awk '/DATA DIVISION/,/PROCEDURE DIVISION/ {
        if ($0 ~ /^[0-9]+ [A-Z0-9-]+ PIC [A-Z0-9()V]+/) {
            split($0, parts, " ")
            nivel=parts[1]
            nombre=parts[2]
            pic=parts[4]
            tipo=""

            if (pic ~ /9/) tipo="entero"
            else if (pic ~ /X|A/) tipo="string"
            else tipo="desconocido"

            printf "      {\"nombre\": \"%s\", \"nivel\": %d, \"tipo\": \"%s\"},\n", nombre, nivel, tipo >> "'"$output_file"'"
        }
    }' "$file"

    echo "    ]," >> $output_file
    echo "    \"errores\": [" >> $output_file

    awk '/MOVE/ {
        if ($0 ~ /MOVE [A-Z0-9-]+ TO [A-Z0-9-]+/) {
            print "      {\"linea\": NR, \"mensaje\": \"Validar tipos o tamaños\"}," >> "'"$output_file"'"
        }
    }' "$file"

    echo "    ]" >> $output_file
    echo "  }," >> $output_file
}

for file in *.cbl *.cobol; do
    [ -f "$file" ] && process_file "$file"
done

# Cerrar el JSON
sed -i '$ s/,$//' $output_file
echo "}" >> $output_file
