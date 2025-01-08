import re
import sys

def analizar_archivo_cobol(archivo):
    try:
        with open(archivo, 'r') as f:
            contenido = f.readlines()
    except FileNotFoundError:
        print(f"Error: El archivo {archivo} no se encontró.")
        sys.exit(1)

    # Patrón para detectar variables en la DATA DIVISION
    patron_variables = re.compile(r"^\s*\d{2,}\s+([\w-]+)\s+PIC\s+(\w+).*")
    variables = {}

    # Patrón para detectar cambios en las variables
    patron_movimiento = re.compile(r"\bMOVE\b\s+([\w-]+)\s+TO\s+([\w-]+)", re.IGNORECASE)

    en_data_division = False
    for linea in contenido:
        # Identificar inicio de DATA DIVISION
        if "DATA DIVISION" in linea:
            en_data_division = True

        # Identificar fin de DATA DIVISION
        if "PROCEDURE DIVISION" in linea:
            en_data_division = False

        # Extraer variables si estamos en la DATA DIVISION
        if en_data_division:
            match = patron_variables.match(linea)
            if match:
                nombre, pic = match.groups()
                variables[nombre] = {
                    'pic': pic,
                    'valor_inicial': None,
                    'modificado': False,
                }

        # Detectar cambios en las variables en la PROCEDURE DIVISION
        match = patron_movimiento.search(linea)
        if match:
            origen, destino = match.groups()
            if origen in variables and destino in variables:
                origen_tam = len(variables[origen]['pic'])
                destino_tam = len(variables[destino]['pic'])
                if origen_tam > destino_tam:
                    print(f"Advertencia: Asignación de {origen} (tamaño {origen_tam}) a {destino} (tamaño {destino_tam}).")
                    variables[destino]['modificado'] = True

    # Verificar si hubo modificaciones
    errores = False
    for nombre, info in variables.items():
        if info['modificado']:
            print(f"Error: La variable {nombre} fue modificada o tiene inconsistencias.")
            errores = True

    if errores:
        sys.exit(1)  # Salir con error para detener el pipeline
    else:
        print("No se encontraron errores en las variables.")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Uso: python3 analizar_cobol.py <archivo_cobol>")
        sys.exit(1)

    archivo_cobol = sys.argv[1]
    analizar_archivo_cobol(archivo_cobol)
