  ,�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      	inmediato      "cuenta"+alltrim(str(m.nivel))      0      s1      0iif( cabeza(1) = 0 , 0, saldos.mes1 / cabeza(1))      0      s2      0iif( cabeza(2) = 0 , 0, saldos.mes2 / cabeza(2))      0      s3      0iif( cabeza(3) = 0 , 0, saldos.mes3 / cabeza(3))      0      s4      0iif( cabeza(4) = 0 , 0, saldos.mes4 / cabeza(4))      0      s5      0iif( cabeza(5) = 0 , 0, saldos.mes5 / cabeza(5))      0      s6      0iif( cabeza(6) = 0 , 0, saldos.mes6 / cabeza(6))      0      s7      0iif( cabeza(7) = 0 , 0, saldos.mes7 / cabeza(7))      0      s8      0iif( cabeza(8) = 0 , 0, saldos.mes8 / cabeza(8))      0      s9      /iif( cabeza(9) = 0 , 0, saldos.mes9/ cabeza(9))      0      s10      2iif( cabeza(10) = 0 , 0, saldos.mes10/ cabeza(10))      0      s11      2iif( cabeza(11) = 0 , 0, saldos.mes11/ cabeza(11))      0      s12      2iif( cabeza(12) = 0 , 0, saldos.mes12/ cabeza(12))      0      suma3      �saldos.mes1+ saldos.mes2+ saldos.mes3+ saldos.mes4+ saldos.mes5+ saldos.mes6+ saldos.mes7+ saldos.mes8+ saldos.mes9+ saldos.mes10+ saldos.mes11+ saldos.mes12      0      suma2      �saldos.mes1+ saldos.mes2+ saldos.mes3+ saldos.mes4+ saldos.mes5+ saldos.mes6+ saldos.mes7+ saldos.mes8+ saldos.mes9+ saldos.mes10+ saldos.mes11+ saldos.mes12      0      suma1      �saldos.mes1+ saldos.mes2+ saldos.mes3+ saldos.mes4+ saldos.mes5+ saldos.mes6+ saldos.mes7+ saldos.mes8+ saldos.mes9+ saldos.mes10+ saldos.mes11+ saldos.mes12      0      Arial      Arial      Arial      Arial      Arial      Arial      Arial      Arial      +"Cuadro Demostrativo de Incidencia Cab/Mes"             Arial      	m.empresa             Arial      )"Rango: " + m.dcuenta + ' al ' +m.hcuenta             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      Aiif(m.centro='%',"Todos",m.centro + " - " +  centros.descripci�n)      Arial      6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')      Arial      "Centro Costo:"      Arial      ,"Nivel de Cuentas: " + alltrim(str(m.nivel))             Arial      "Cuenta"      Arial      "Enero"             Arial      	"Febrero"             Arial      "Marzo"             Arial      "Abril"             Arial      "Mayo"             Arial      "Junio"             Arial      "Julio"             Arial      "Agosto"             Arial      "Setiembre"             Arial      	"Octubre"             Arial      "Noviembre"             Arial      "Diciembre"             Arial      "Total"             Arial      "%"             Arial      !"Existencia de Ganado(Cabezas)
"             Arial      	1 <= m.ni      m.cabeza(1)      "999,999,999"             Arial      	1 <= m.ni      m.cabeza(2)      "999,999,999"             Arial      	1 <= m.ni       m.cabeza(3)      "999,999,999"             Arial      	1 <= m.ni       m.cabeza(4)      "999,999,999"             Arial      	1 <= m.ni      m.cabeza(5)      "999,999,999"             Arial      	1 <= m.ni      m.cabeza(6)      "999,999,999"             Arial      	1 <= m.ni      m.cabeza(7)      "999,999,999"             Arial      	1 <= m.ni      m.cabeza(8)      "999,999,999"             Arial      	1 <= m.ni       m.cabeza(9)      "999,999,999"             Arial      	1 <= m.ni       m.cabeza(10)      "999,999,999"             Arial      	1 <= m.ni      m.cabeza(11)      "999,999,999"             Arial      	1 <= m.ni      m.cabeza(12)      "999,999,999"             Arial      	1 <= m.ni      �round((m.cabeza(1) +m.cabeza(2) + m.cabeza(3) + m.cabeza(4) + m.cabeza(5) + m.cabeza(6) + m.cabeza(7) + m.cabeza(8) + m.cabeza(9) + m.cabeza(10) + m.cabeza(11) + m.cabeza(12)) /(m.hmes-m.dmes+1),0)      "999,999,999"      Arial      1<= m.ni      nivel<=3      replicate(' ',nivel*3)+ cuenta      Arial      between(nivel,1, m.nivel )      saldos.mes1      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes2      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes3      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes4      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes5      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes6      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes7      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes8      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes9      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes10      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes11      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      saldos.mes12      "9,999,999,999"      Arial      between(nivel,1, m.nivel )      Rmes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12      "999,999,999,999"      Arial      between(nivel,1, m.nivel )      rround(((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12)/m.totalGeneral)*100,4)      	"999.999"      Arial      between(nivel,1, m.nivel )      s1      "9,999,999.99"      Arial      nivel <= m.ni      s2      "9,999,999.99"      Arial      nivel <= m.ni      s3      "9,999,999.99"      Arial      nivel<= m.ni      s4      "9,999,999.99"      Arial      nivel<= m.ni      s5      "9,999,999.99"      Arial      nivel<= m.ni      s6      "9,999,999.99"      Arial      nivel<= m.ni      s7      "9,999,999.99"      Arial      nivel<= m.ni      s8      "9,999,999.99"      Arial      nivel<= m.ni      s9      "9,999,999.99"      Arial      nivel<= m.ni      s10      "9,999,999.99"      Arial      nivel<= m.ni      s11      "9,999,999.99"      Arial      nivel<= m.ni      s12      "9,999,999.99"      Arial      nivel<= m.ni      (suma3/totalcabeza)      "99,999,999.99"      Arial      nivel<= m.ni      "Incidencia por Cabeza
"      Arial      nivel <= m.ni      
"Cab./A�o"      Arial      nivel <= m.ni      'P�g. '+alltrim(Str(_pageno))             Arial      m.TotalGeneral      "999,999,999,999,999"      Arial      
datetime()             Arial      dataenvironment      `Top = 77
Left = -17
Width = 792
Height = 419
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
release cabeza,totalcabeza,m.decimales, m.sucursal,m.TotalGeneral
ENDPROC
PROCEDURE Init
Public array cabeza[12]
public totalcabeza,m.decimales, m.sucursal,m.TotalGeneral
xproceso = SYS(2015)
set data to datos
IF m.tipoMoneda = 'L'
      m.decimales = 0
ELSE
      m.decimales = 0
ENDIF



*!*   set filter to oapp.empresa=idempresa

select hc_Ganados
sum C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12 to array cabeza for hc_Ganados.centro = m.centro
totalcabeza=0
** Ahora limpiamos los meses que no necesitamos
for i = 1 to 12
*!*	      if m.hmes < i
*!*	           cabeza(i)  = 0
*!*	      ENDIF
*!*	      if m.dmes < i
*!*	           cabeza(i)  = 0
*!*	      ENDIF

      IF !BETWEEN(i,m.dmes,m.hmes)
       cabeza(i)  = 0

      ENDIF 
      
      totalcabeza=totalcabeza+cabeza(i)
      
endfor          
*set step on
totalcabeza=totalcabeza/(m.hmes -m.dmes +1)
**** Usamos las mismas variables del primer semestre
lnIndiceMes = 6 
*!*   cabeza(1) = cabeza(7)
*!*   cabeza(2) = cabeza(8)
*!*   cabeza(3) = cabeza(9)
*!*   cabeza(4) = cabeza(10)
*!*   cabeza(5) = cabeza(11)
*!*   cabeza(6) = cabeza(12)


IF m.tipomoneda = 'L'
     m.decimales = 0
     m.dec = 0
ELSE
     m.decimales = 2
     m.dec = 2
ENDIF
*!*	IF EMPTY(m.sucursal)
*!*	     m.sucursal = '%'
*!*	ELSE
*!*	     = sql('Select descripci�n sucursal from sucursal where idempresa = ?oApp.Empresa and sucursal = ?m.sucursal','Suc')
*!*	ENDIF
m.sucursal = '%'

IF EMPTY(m.centro)
     m.centro = '%'
ELSE
     = sql('Select descripci�n from centros where idempresa = ?oApp.Empresa and centro = ?m.centro','Centros')
ENDIF

= sql('exec cn_balance_comp ?oApp.empresa, ?m.dfecha,?m.hfecha, ?oApp.Ejercicio, ?m.sucursal, ?m.centro, ?m.TipoMoneda,?m.dCuenta,?m.hCuenta','Saldos')
SELECT saldos
SET FILTER TO nivel <= m.nivel &&AND BETWEEN(LEFT(cuenta,10),m.dcuenta,m.hcuenta)
GOTO TOP
*SUM mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12 TO m.totalGeneral
m.totalGeneral= mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12


GOTO TOP


ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
      cursor      �Top = 139
Left = 430
Height = 90
Width = 96
Alias = "hc_ganados"
Database = ..\data\datos.dbc
CursorSource = "hc_ganados"
Name = "Cursor9"
     U���    <  <                        F�   %   @      �  -   y          �  U     <�  � �� �� �� � U  CABEZA TOTALCABEZA	 DECIMALES SUCURSAL TOTALGENERAL� 7�  ���� 7� �� �� �� � T� �C��]�� G(� datos� %��� � L��g � T�� �� �� �~ � T�� �� �� � F� �_ K�� � �� �(�  ��	 ���
 ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� �� T� �� �� �� ���(����S� %�C� �� �� �
��9� T�  �� ��� �� � T� �� C � �  �� �� T� �� �� �� ��� T� ���� %��� � L���� T�� �� �� T�� �� �� ��� T�� ���� T�� ���� � T�� �� %�� %�C�� ���� T�� �� %�� ���m ��C�V Select descripci�n from centros where idempresa = ?oApp.Empresa and centro = ?m.centro� Centros� �� �� ��C�� exec cn_balance_comp ?oApp.empresa, ?m.dfecha,?m.hfecha, ?oApp.Ejercicio, ?m.sucursal, ?m.centro, ?m.TipoMoneda,?m.dCuenta,?m.hCuenta� Saldos� �� F� � G(�� �� �� #)�; T�� �� � �  �! �" �# �$ �% �& �' �( �) �� #)� U*  CABEZA TOTALCABEZA	 DECIMALES SUCURSAL TOTALGENERAL XPROCESO DATOS
 TIPOMONEDA
 HC_GANADOS C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 CENTRO I DMES HMES LNINDICEMES DEC SQL SALDOS NIVEL MES1 MES2 MES3 MES4 MES5 MES6 MES7 MES8 MES9 MES10 MES11 MES12
  �  � U  SETEO Destroy,     �� Init�     �� BeforeOpenTables+    ��1 �2 � �� A� � � A v �� r�1B bB �� I� � � � � A � �A �	q Q �S 4 q 2                       T         o   m     +   �  �  S    )   <                  