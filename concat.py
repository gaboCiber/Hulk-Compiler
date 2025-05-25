import os

def concatenar_archivos(directorio_entrada, archivo_salida):
    """
    Concatena todos los archivos de texto en un directorio y sus subdirectorios.
    
    Args:
        directorio_entrada (str): Directorio donde buscar archivos
        archivo_salida (str): Nombre del archivo de salida
    """
    try:
        # Crear el archivo de salida
        with open(archivo_salida, 'w', encoding='utf-8') as salida:
            
            # Recorrer todos los archivos en el directorio y subdirectorios
            for raiz, dirs, archivos in os.walk(directorio_entrada):

                for archivo in archivos:
                    ruta_completa = os.path.join(raiz, archivo)
                    
                    # Escribir la ruta del archivo
                    salida.write(f"--- {ruta_completa} ---\n")
                    
                    # Leer y escribir el contenido del archivo
                    try:
                        with open(ruta_completa, 'r', encoding='utf-8') as entrada:
                            contenido = entrada.read()
                            salida.write(contenido)
                            salida.write("\n\n")  # Separador entre archivos
                    except Exception as e:
                        print(f"No se pudo leer {ruta_completa}: {str(e)}")
        
        print(f"¡Proceso completado! Los archivos han sido concatenados en {archivo_salida}")
    
    except Exception as e:
        print(f"Error durante el proceso: {str(e)}")

# Ejemplo de uso
directorio = r"./src"  # Cambia esta ruta por la que necesites
archivo_resultado = "concatenacion.txt"

concatenar_archivos(directorio, archivo_resultado)