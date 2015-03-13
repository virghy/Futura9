PARAMETERS lcIDEmpresa
*SET PROCEDURE TO progs\main, progs\utility ADDITIVE 
SET PROCEDURE TO main, utility ADDITIVE 
=setPath()
OPEN DATABASE datos SHARED
SET DATABASE TO DATOS 
SET STEP ON

IF LEN(ALLTRIM(lcIdEmpresa))=3

sql("SElect MAX(fechaRegistro) as Fecha from cn_importarIVA where IdEmpresa=?lcIDEmpresa",'cFecha')

	InputFecha = NVL(cFecha.Fecha,DATETIME(2009,1,1,0,0,0))

lfecha=right('0'+ALLTRIM(STR(DAY(InputFecha ))),2)
lfecha= lfecha+ right('0'+ALLTRIM(STR(month(InputFecha ))),2)
lfecha= lfecha+ right('0'+ALLTRIM(STR(year(InputFecha ))),4)
lfecha= lfecha+ right('0'+ALLTRIM(STR(hour(InputFecha ))),2)
lfecha= lfecha+ right('0'+ALLTRIM(STR(minute(InputFecha ))),2)
lfecha= lfecha+ right('0'+ALLTRIM(STR(sec(InputFecha ))),2)
ELSE

	lFecha = SUBSTR(ALLTRIM(lcIdEmpresa),5)
	lcIdEmpresa = LEFT(ALLTRIM(lcIdEmpresa),3)
ENDIF

*MESSAGEBOX(lFecha)

*RETURN


lcmd="! aGeneraTXTFutura.exe " + lFecha
&lcmd

archivo=lfecha + 'V.TXT'
=procesar(Archivo,'V')

archivo=lfecha + 'C.TXT'
=procesar(Archivo,'C')

*MESSAGEBOX("Proceso Finalizado")


QUIT 


FUNCTION Procesar()
PARAMETERS Archivo, lOrigen

cOrigen=lOrigen

 lnHndconn = SQLCONNECT('datos_Sql')
 
 IF lnHndconn= -1
 	RETURN
 ENDIF
 	 

 = SQLSETPROP(lnHndconn, "Transactions", 1)
 
     
*SET STEP ON
		hnd= FOPEN(archivo)
		IF !EMPTY(hnd)
			resultado=FGETS(hnd,350)	&&Leemos la primera linea de cabecera
			resultado=STRCONV(FGETS(hnd,350),11)	&&Leemos la segunda linea de datos
			TEXT TO cmdSQL NOSHOW
				INSERT INTO [cn_ImportarIVA]
	           ([Nro]
	           ,[Fecha]
	           ,[IdCliente]
	           ,[Cliente]
	           ,[IdSucursal]
	           ,[Sucursal]
	           ,[IdCondicion]
	           ,[IdProducto]
	           ,[Producto]
	           ,[Iva]
	           ,[Importe]
	           ,[FechaRegistro]
	           ,[Tipo]
	           ,[AuditFecha],
	           Origen,
	           RUC,
	           Estado,
	           IdEmpresa)
	     		VALUES
				(?m.Nro,
				?m.Fecha,
				?m.IdCliente,
				?m.Cliente,
				?m.IdSucursal,
				?m.Sucursal,
				?m.IdCondicion,
				?m.IdProducto,
				?m.Producto,
				?m.Iva,
				?m.Importe,
				?m.FechaRegistro,
				?m.Tipo,
				getdate(),?cOrigen,
				?m.Ruc,
				?m.Estado,
				?m.lcIDEmpresa)

			ENDTEXT 
			
			
			DO while !EMPTY(resultado) AND ASC(resultado)<>12
				*Analizar y Procesar
				m.Nro=ALLTRIM(SUBSTR(resultado,1,17))
				m.Fecha=SUBSTR(resultado,18,10)
				m.IdCliente=allTRIM(SUBSTR(resultado,29,20))
				m.Cliente=allTRIM(SUBSTR(resultado,50,50))
				m.RUC=allTRIM(SUBSTR(resultado,103,20))
				m.IdSucursal=allTRIM(SUBSTR(resultado,123,3))
				m.Sucursal=allTRIM(SUBSTR(resultado,127,50))
				m.IdCondicion=SUBSTR(resultado,178,1)
				m.Estado=SUBSTR(resultado,180,1)				
				m.IdProducto=allTRIM(SUBSTR(resultado,182,20))
				m.Producto=allTRIM(SUBSTR(resultado,203,48))
				m.Iva=allTRIM(SUBSTR(resultado,252,3))
				m.Importe=STRTRAN(SUBSTR(resultado,256,20),".","")
				m.FechaRegistro=SUBSTR(resultado,276,20)
				m.Tipo=SUBSTR(resultado,297,1)	
						
				sql(cmdsql,"",lnHndconn)
				*WAIT windows LEFT(resultado,255) 
				resultado=STRCONV(FGETS(hnd,350),11)
			ENDDO

			FCLOSE(hnd)
			
			
			
		 ENDIF  
		 
		 IF lOrigen='V'
			 TEXT TO cmdSQL NOSHOW 
			 		SELECT Id,Nro FROM cn_ImportarIVA
			 		where Procesado is null and Tipo in('V','N')
			 		and IdEmpresa=?lcIDEmpresa
			 		order by Id 
			 ENDTEXT 
		 ELSE
			 TEXT TO cmdSQL NOSHOW 
			 		SELECT Id,Nro FROM cn_ImportarIVA
			 		where Procesado is null and Tipo='C'
			 		and IdEmpresa=?lcIDEmpresa
			 		order by Id 
			 ENDTEXT 

		ENDIF
			 
		 IF sql(cmdSQL,'cIVA',lnHndconn)>0
			 SELECT cIVA
			 m.lNro = "" 
			 m.IdIva=0
			 
			 SCAN
			 	IF m.lNro<> cIVA.Nro		&&Corte de Nro
			 			IF lOrigen='V'
			 				SQLEXP('exec cn_ImportarVenta ?cIva.Id,?lcIDEmpresa,?@m.IdIVA','',lnHndconn)
			 			ELSE
			 				SQLEXP('exec cn_ImportarCompra ?cIva.Id,?lcIDEmpresa,?@m.IdIVA','',lnHndconn)			 			
			 			ENDIF
			 			
			 	ELSE
			 			***Si devuelve un valor positivo, significa que ejecuto correctamente
			 			***e insertamos el detalle
			 			IF m.IdIVa>0
			 				SQLEXP('exec cn_ImportarVentaDet ?cIva.Id,?lcIDEmpresa,?m.IdIVA','',lnHndconn)
			 			ENDIF
			 	ENDIF
			 	
			 	m.lNro=cIVA.Nro
			 ENDSCAN 
		 ENDIF
	

	SQLCANCEL(lnHndconn)
    SQLDISCONNECT(lnHndconn)		 
		 
ENDFUNC 

FUNCTION SQLEXP
LPARAMETERS lcmdsql, lccursor,  ;
            lnconn
LOCAL _hndconn, _lnresultado,_lcalias
      
_lcalias = ALIAS()
IF TYPE('lnConn') = 'N'
     _hndconn = lnconn
ELSE
     lnconn = 0
     IF TYPE('oApp') <> 'O'
          sistema = 'ND'
          _hndconn = SQLCONNECT('datos_Sql')
     ELSE
          sistema = oapp.sistema
          _hndconn = SQLCONNECT(oapp.sistema)
     ENDIF
     = SQLSETPROP(_hndconn, "Transactions", 1)
     
ENDIF
IF _hndconn <= 0
*     MESSAGEBOX( 'No se pudo conectar al Servidor' +  CHR(13) +  'Sistema: ' +  sistema + CHR(13) +  'BD: ' + DBC())
     RETURN -1
ENDIF
*!*	DO WHILE SQLGETPROP(_hndconn,'ConnectBusy')
*!*	     WAIT WINDOW NOCLEAR NOWAIT  'Conexion Ocupada...'
*!*	ENDDO
= SQLSETPROP(_hndconn, 'BatchMode', .T.)
*= SQLSETPROP(_hndconn, "Transactions", 1)
IF TYPE('lcCursor') = 'C'
     _lnresultado = SQLEXEC(_hndconn,  ;
                    lcmdsql,  ;
                    lccursor)
ELSE
     _lnresultado = SQLEXEC(_hndconn,  ;
                    lcmdsql)
ENDIF
IF _lnresultado < 0
     lnelemento = AERROR(laerror)
     lcerror = ''
     FOR i = 1 TO lnelemento
          lcerror = lcerror +  ;
                    TRANSFORM(i) +  ;
                    ') ' +  ;
                    laerror(i,3) +  ;
                    CHR(13)
     ENDFOR
*     MESSAGEBOX(lcerror)
ENDIF

IF lnconn = 0
     SQLCANCEL(_hndconn)
     SQLDISCONNECT(_hndconn)
ENDIF
IF  .NOT. EMPTY(_lcalias)
     SELECT (_lcalias)
ENDIF
RETURN _lnresultado
ENDFUNC