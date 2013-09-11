La función Comin tiene 6 argumentos de entrada los cuales son:

1)	it (Input type): es el tipo de dato. Según su naturaleza se identifican por: 
•	1 para vectores en R 
•	2 para matrices extraídas de otros programas  
•	3 para matrices en R (Matrices en el disco duro tipo csv)
•	4 para vectores extraídos  
2)	b (base): es la base a la cual se quiere trabajar, al invocarla realiza la transformación de los datos originales a la base deseada.
3)	ld (local data) : se utiliza sólo si son datos en R, y es la matriz o el vector al cual se desea calcular
Advertencia: “para datos en R, sólo se utilizan las 3 variables: it, b, ld. Ellas se guardan en la memoria del pc”
4)	Otro argumento es: ext (extensión): el programa corre varios tipos de archivos los cuales son los siguientes y se debe denotar con el numero a continuación: 1) para datos csv 2) para datos en txt 3) para datos en spps.
5)	r (route) : es la ruta en la cual está la matriz o el vector se denota con doble slash ejemplo: r<-"C:\\Users\\Cristian\\Downloads"
6)	f (file) : es el archivo y se debe anexar .la extensión ejemplo f<-"ArticoFQ_Normal.csv" 
Advertencia: para datos extraídos se deben tener todos las variables excepto ld y se llama igual que el anterior.
7)	Luego y se escribe la función Comin()  la cual calcula la emergencia, autoorganización, complejidad, homeostasis, autopoiesis. Genera sus gráficas y tablas correspondientes.