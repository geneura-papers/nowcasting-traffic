#Articles on traffic analysis and prediction for several venues

Art�culo para revista Future Generation Computer System, en n�mero especial de "Smart City and Internet of Things".

En �l se analizar�n:
- flujos de tr�fico en carrera
- flujos de tr�fico en ciudad
- movilidad dentro de un local

#Correo de Antonio Mora de 16/Nov explicando el plan de trabajo#

##Discoteca:##

(Datos anonimizados y normalizados)
	- Se tienen datos de 5 nodos (las salas y la terraza)
	- Vamos a generar un juego de datos en base a los pasos con las caracter�sticas (para cada dispositivo detectado):
		o   Tiempo_Relat_P1, ? ,Tiempo_Relat_P5 -> tiempos de estancia junto a cada nodo en relaci�n al tiempo de estancia total de la persona en la discoteca
		o   Tiempo_Relat_T1, ? ,Tiempo_Relat_T5 -> tiempos de estancia junto a cada nodo en relaci�n al tiempo de estancia respecto a la apertura/cierre de la discoteca
		o   Tiempo_Absol1, ? ,Tiempo_Absol5 -> tiempos de estancia real junto a cada nodo
		o   Tiempo_no_detect -> tiempo que se ha pasado sin detectar al dispositivo antes de su salida definitiva del local
		o   Hora_entrada, Hora_salida -> pos eso

De los datos se pueden extraer m�s caracter�sticas, como n�mero de nodos por los que ha pasado, marca/modelo del dispositivo, pero tenemos que pensar si son de inter�s.
	- Clustering de comportamientos -> aplicar sobre esos datos selecci�n de caracter�sticas, clustering, clasificaci�n, etc. Y analizar los diferentes comportamientos que se detecten.
Con esto se pretende obtener informaci�n �til para estrategias de empresa:

	- estudio de mercado
	- h�bitos de consumo
	- etc

##ETSIIT:##
	-  Se tienen datos de 4 nodos (hall, entrada alumnos, entrada parking exterior, entrada parking interior)
	-  Se har�n matrices de entrada/salida, organizadas por d�as y rangos horarios
Puede ser de inter�s para cuestiones de seguridad, publicidad, vending, no s�?


##Granada Ciudad (MOSOS):##
	- Se tienen dos nodos en Doctor Ol�riz
	- Se har�n series temporales para modelado y predicci�n de atascos
	- Se considerar�n datos de Bluetooth (filtrando por coches) y WiFi
	- Se calcular�n y mostrar�n gr�ficamente tiempos de desplazamiento y densidad de tr�fico

Se quiere modelar una funci�n que defina los intervalos de duraci�n del sem�foro para que la densidad de tr�fico sea constante y el tiempo de desplazamiento m�nimo.

##Carreteras (PETRA):##
	- Se tienen 6 nodos: 4 entre Granada (Albolote) y M�laga, uno en Guadix y otro m�s lejos en la carretera de Almer�a
	- Se tienen datos de algunos aforadores junto a los nodos (o muy cerca)
	- Se har�n series temporales
	- Se calcular�n velocidades promedio
	- Se obtendr�n porcentajes de detecci�n
	- Se intentar� obtener el ratio de ocupaci�n de los coches, en base a los pasos detectados y su relaci�n con los de los aforadores

Si se os ocurren m�s cosas, ser�n bienvenidas.

Respecto a las tareas, yo hab�a pensado, grosso modo, algo as�:
	- Antares -> Extracci�n de datos. Ayuda en matrices E/S. Supervisi�n del art�culo.
	- Yo mismo -> Estado del arte (smart cities y smart traffic). M�todos de clustering y clasificaci�n. Organizaci�n del art�culo. Ayuda en todo.
	- Fergu -> Descripci�n de problema y datos de PETRA. Matrices de E/S. Ayuda en Extracci�n de datos.
	- Maribel -> Descripci�n de problema y datos de MOSOS. Estado del arte (series temporales). Ayuda en extracci�n de datos.
	- Gustavo -> Descripci�n de problema y datos de ETSIIT. Estado del arte (otros sistemas de detecci�n). Ayuda en estado del arte (smart cities y smart traffic).
	- V�ctor -> Descripci�n de m�todos de series temporales a aplicar. Aplicaci�n de �stos sobre los datos.
	- Pedro -> Ayuda en descripciones de problemas y datos. Ayuda en an�lisis de datos (estad�stica). Ayuda en series temporales.
	- JJ -> Descripci�n de problema y datos de discoteca. Ayuda en m�todos de clustering y clasificaci�n. Ayuda en an�lisis.

Todo esto lo iremos ajustando sobre la marcha, es una primera aproximaci�n.

Si vamos terminando tareas ayudamos a otros, claro.

La cuesti�n es que todos trabajemos en el art�culo y que cada uno aporte donde sea mejor, como debe ser. Aunque algunas cosas las he ?estimado?, sorry si he metido la gamba. ;D
