***SQL

		cMascara='@Z 999,999,999'


***Impresion
SET DEVICE TO FILE c:\sistemas\Etiqueta.txt
fpos=4
nCol=4
nEspacio=36
SCAN
cPos=0
FOR i=1 TO nCol
	Imprimir(PROW(),cPos,'Cod.'+ALLTRIM(IdProducto))
	cPos=cPos+nEspacio
ENDFOR

cPos=0
Imprimir(PROW()+1,0,"")
FOR i=1 TO nCol
	Imprimir(PROW(),cPos,left(Producto,29),REPLICATE('X',29))	
	cPos=cPos+nEspacio
ENDFOR

cPos=0
Imprimir(PROW()+1,0,"")
FOR i=1 TO nCol
	Imprimir(PROW(),cPos,Precio,cMascara)	
	cPos=cPos+nEspacio
ENDFOR
	Imprimir(PROW()+2,0,"")
ENDSCAN

SET DEVICE TO SCREEN 

*COPY FILE c:\sistemas\Etiqueta.txt TO lpt1
