  ]B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      saldos.cuenta1      saldos.cuenta2      saldos.cuenta3      saldos.cuenta4      saldos.cuenta5      saldos.cuenta6      	inmediato      "cuenta"+alltrim(str(m.nivel))      0      s1      $iif( mm1 = 0 , 0, saldos.mes1 / mm1)      0      s2      $iif( mm2 = 0 , 0, saldos.mes2 / mm2)      0      s3      $iif( mm3 = 0 , 0, saldos.mes3 / mm3)      0      s4      $iif( mm4 = 0 , 0, saldos.mes4 / mm4)      0      s5      $iif( mm5 = 0 , 0, saldos.mes5 / mm5)      0      s6      $iif( mm6 = 0 , 0, saldos.mes6 / mm6)      0      Arial      Arial      Arial      Arial      Arial      ?"Cuadro Demostrativo de Incidencia Gs./Cab/Mes (1er. Semestre)"             Arial      	m.empresa             Arial      )"Rango: " + m.dcuenta + ' al ' +m.hcuenta             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      Iiif(empty(m.centro),"Todos",m.centro + " - " +  centros_base.descripci�n)             Arial      ,"Nivel de Cuentas: " + alltrim(str(m.nivel))             Arial      "Centro Costo:"      Arial      6'Moneda: '+iif(m.tipoMoneda='L','Guaranies','Dolares')             Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "%"      "@J"             Arial      "Enero"      "@J"             Arial      	"Febrero"      "@J"             Arial      "Marzo"      "@J"             Arial      "Abril"      "@J"             Arial      "Mayo"      "@J"             Arial      "Junio"      "@J"             Arial      "Total"      "@J"             Arial      mm1      "999,999,999,999"             Arial      	1 <= m.ni      mm2      "999,999,999,999"             Arial      	1 <= m.ni      mm3      "999,999,999,999"             Arial      	1 <= m.ni      mm4      "999,999,999,999"             Arial      	1 <= m.ni      mm5      "999,999,999,999"             Arial      	1 <= m.ni      mm6      "999,999,999,999"             Arial      	1 <= m.ni      0round((mm1 +mm2 +mm3+mm4 + mm5 + mm6) /m.hmes,0)      "999,999,999,999"             Arial      1<= m.ni      !"Existencia de Ganado(Cabezas)
"             Arial      	1 <= m.ni      saldos.cuenta1             Arial      cuentas_base1.descripci�n             Arial      saldos.cuenta2             Arial      cuentas_base2.nivel <= m.nivel      cuentas_base2.descripci�n             Arial      cuentas_base2.nivel <= m.nivel      saldos.cuenta3             Arial      "cuentas_base3.nivel <= m.nivel - 1      cuentas_base3.descripci�n             Arial      "cuentas_base3.nivel <= m.nivel - 1      saldos.cuenta4             Arial      Ebetween(cuentas_base4.nivel,1, m.nivel-1 ) and cuentas_base5.nivel >0      cuentas_base4.descripci�n             Arial      Ebetween(cuentas_base4.nivel,1, m.nivel-1 ) and cuentas_base5.nivel >0      saldos.cuenta5             Arial      Ebetween(cuentas_base5.nivel,1, m.nivel-1 ) and cuentas_base6.nivel >0      cuentas_base5.descripci�n             Arial      Ebetween(cuentas_base5.nivel,1, m.nivel-1 ) and cuentas_base6.nivel >0      saldos.cuenta6             Arial      (between(cuentas_base6.nivel,1, m.nivel )      cuentas_base6.descripci�n             Arial      (between(cuentas_base6.nivel,1, m.nivel )      saldos.mes1      "999,999,999,999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      saldos.mes2      "999,999,999,999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      saldos.mes3      "999,999,999,999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      saldos.mes4      "999,999,999,999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      saldos.mes5      "999,999,999,999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      saldos.mes6      "999,999,999,999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      'mes1+ mes2 + mes3 + mes4 +  mes5 + mes6      "999,999,999,999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      qround(((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12)/xtsaldo.total)*100,4)      "99,999.999"             Arial      (between(cuentas_base6.nivel,1, m.nivel )      saldos.cuenta5             Arial      (between(cuentas_base5.nivel,1, m.nivel )      cuentas_base5.descripci�n             Arial      (between(cuentas_base5.nivel,1, m.nivel )      saldos.mes1      "999,999,999,999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      saldos.mes2      "999,999,999,999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      saldos.mes3      "999,999,999,999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      saldos.mes4      "999,999,999,999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      saldos.mes5      "999,999,999,999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      saldos.mes6      "999,999,999,999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      'mes1+ mes2 + mes3 + mes4 +  mes5 + mes6      "999,999,999,999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      wmton(round(((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12)/xtsaldo.total)*100,4))      "99,999.999"             Arial      (between(cuentas_base5.nivel,1, m.nivel )      saldos.cuenta4             Arial      cuentas_base4.nivel <= m.nivel      cuentas_base4.descripci�n             Arial      cuentas_base4.nivel <= m.nivel      saldos.mes1      "999,999,999,999"             Arial      (between(cuentas_base4.nivel,1, m.nivel )      saldos.mes2      "999,999,999,999"             Arial      cuentas_base4.nivel <= m.nivel      saldos.mes3      "999,999,999,999"             Arial      cuentas_base4.nivel <= m.nivel      saldos.mes4      "999,999,999,999"             Arial      cuentas_base4.nivel <= m.nivel      saldos.mes5      "999,999,999,999"             Arial      cuentas_base4.nivel <= m.nivel      saldos.mes6      "999,999,999,999"             Arial      cuentas_base4.nivel <= m.nivel      'mes1+ mes2 + mes3 + mes4 +  mes5 + mes6      "999,999,999,999"             Arial      cuentas_base4.nivel <= m.nivel      wmton(round(((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12)/xtsaldo.total)*100,4))      "99,999.999"             Arial      cuentas_base4.nivel <= m.nivel      saldos.cuenta3             Arial      cuentas_base3.nivel <= m.nivel      cuentas_base3.descripci�n             Arial      cuentas_base3.nivel <= m.nivel      saldos.mes1      "999,999,999,999"             Arial      saldos.mes2      "999,999,999,999"             Arial      saldos.mes3      "999,999,999,999"             Arial      saldos.mes4      "999,999,999,999"             Arial      saldos.mes5      "999,999,999,999"             Arial      saldos.mes6      "999,999,999,999"             Arial      'mes1+ mes2 + mes3 + mes4 +  mes5 + mes6      "999,999,999,999"             Arial      wmton(round(((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12)/xtsaldo.total)*100,4))      	"999.999"             Arial      s1      "999,999,999.99"             Arial      	3 <= m.ni      s2      "999,999,999.99"             Arial      	3 <= m.ni      s3      "999,999,999.99"             Arial      	3 <= m.ni      s4      "999,999,999.99"             Arial      	3 <= m.ni      s5      "999,999,999.99"             Arial      	3 <= m.ni      s6      "999,999,999.99"             Arial      	3 <= m.ni      Wround((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 )/(mm1+mm2+mm3+mm4+mm5+mm6),m.decimales)      "999,999,999.99"             Arial      	3 <= m.ni      "Incidencia por Cabeza
"             Arial      	3 <= m.ni      "Cab./Prom.Mes"             Arial      	3 <= m.ni      saldos.cuenta2             Arial      cuentas_base2.nivel <= m.nivel      cuentas_base2.descripci�n             Arial      cuentas_base2.nivel <= m.nivel      saldos.mes1      "999,999,999,999"             Arial      saldos.mes2      "999,999,999,999"             Arial      saldos.mes3      "999,999,999,999"             Arial      saldos.mes4      "999,999,999,999"             Arial      saldos.mes5      "999,999,999,999"             Arial      saldos.mes6      "999,999,999,999"             Arial      'mes1+ mes2 + mes3 + mes4 +  mes5 + mes6      "999,999,999,999"             Arial      wmton(round(((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12)*100/xtsaldo.total),4))      	"999.999"             Arial      s1      "999,999,999.99"             Arial      	2 <= m.ni      s2      "999,999,999.99"             Arial      	2 <= m.ni      s3      "999,999,999.99"             Arial      	2 <= m.ni      s4      "999,999,999.99"             Arial      	2 <= m.ni      s5      "999,999,999.99"             Arial      	2 <= m.ni      s6      "999,999,999.99"             Arial      	2 <= m.ni      Wround((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 )/(mm1+mm2+mm3+mm4+mm5+mm6),m.decimales)      "999,999,999.99"             Arial      	2 <= m.ni      "Incidencia por Cabeza
"             Arial      	2 <= m.ni      "Cab./Prom.Mes"             Arial      	2 <= m.ni             saldos.cuenta1             Arial      cuentas_base1.descripci�n             Arial      saldos.mes1      "999,999,999,999"             Arial      saldos.mes2      "999,999,999,999"             Arial      saldos.mes3      "999,999,999,999"             Arial      saldos.mes4      "999,999,999,999"             Arial      saldos.mes5      "999,999,999,999"             Arial      saldos.mes6      "999,999,999,999"             Arial      'mes1+ mes2 + mes3 + mes4 +  mes5 + mes6      "999,999,999,999"             Arial      wmton(round(((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6+mes7+ mes8 + mes9 + mes10 +  mes11 + mes12)*100/xtsaldo.total),6))      	"999.999"             Arial      s1      "999,999,999.99"             Arial      	1 <= m.ni      s2      "999,999,999.99"             Arial      	1 <= m.ni      s3      "999,999,999.99"             Arial      	1 <= m.ni      s4      "999,999,999.99"             Arial      	1 <= m.ni      s5      "999,999,999.99"             Arial      	1 <= m.ni      s6      "999,999,999.99"             Arial      	1 <= m.ni      Wround((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6 )/(mm1+mm2+mm3+mm4+mm5+mm6),m.decimales)      "999,999,999.99"             Arial      	1 <= m.ni      "Incidencia por Cabeza
"             Arial      	1 <= m.ni      "Cab./Prom.Mes"             Arial      	1 <= m.ni      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 43
Left = 13
Width = 792
Height = 419
InitialSelectedAlias = "cuentas_y_presupuestos"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Destroy
RELEASE cabeza, mm1, mm2, mm3, mm4, mm5, mm6, m.decimales
ENDPROC
PROCEDURE Init
PUBLIC cabeza[12]
PUBLIC mm1, mm2, mm3, mm4, mm5, mm6, m.decimales
xproceso = SYS(2015)
SET DATABASE TO datos
IF m.tipomoneda = 'L'
     m.decimales = 0
ELSE
     m.decimales = 2
ENDIF
SELECT hc_ganados
mm1 = 0
mm2 = 0
mm3 = 0
mm4 = 0
mm5 = 0
mm6 = 0
SUM TO ARRAY cabeza FOR centro = m.centro
FOR i = 1 TO 12
     IF m.hmes < i
          cabeza(i) = 0
     ENDIF
ENDFOR
SELECT hc_ganados
REQUERY()
IF m.hmes >= 1
     mm1 = hc_ganados.c1
     IF m.hmes >= 2
          mm2 = hc_ganados.c2
          IF m.hmes >= 3
               mm3 = hc_ganados.c3
               IF m.hmes >= 4
                    mm4 = hc_ganados.c4
                    IF m.hmes >= 5
                         mm5 = hc_ganados.c5
                         IF m.hmes >= 6
                              mm6 = hc_ganados.c6
                         ELSE
                              mm6 = 0
                         ENDIF
                    ELSE
                         mm5 = 0
                    ENDIF
               ELSE
                    mm4 = 0
               ENDIF
          ELSE
               mm3 = 0
          ENDIF
     ELSE
          mm2 = hc_ganados.c2
     ENDIF
ELSE
     mm1 = 0
ENDIF
SELECT detalle.cuenta, .T. AS detalle, SUM(ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales)) AS debe, SUM(ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales)) AS haber, cuentas.integradora, nivel, cuentas.descripci�n, SUM(IIF(MONTH(asientos.fecha) = 1 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes1, SUM(IIF(MONTH(asientos.fecha) = 2 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes2, SUM(IIF(MONTH(asientos.fecha) = 3 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes3, SUM(IIF(MONTH(asientos.fecha) = 4 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes4, SUM(IIF(MONTH(asientos.fecha) = 5 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes5, SUM(IIF(MONTH(asientos.fecha) = 6 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes6, SUM(IIF(MONTH(asientos.fecha) = 7 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes7, SUM(IIF(MONTH(asientos.fecha) = 8 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes8, SUM(IIF(MONTH(asientos.fecha) = 9 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes9, SUM(IIF(MONTH(asientos.fecha) = 10 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes10, SUM(IIF(MONTH(asientos.fecha) = 11 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes11, SUM(IIF(MONTH(asientos.fecha) = 12 AND MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS mes12 FROM datos!asientos_base asientos, datos!detalle_base detalle, datos!cuentas WHERE BETWEEN(detalle.cuenta, m.dcuenta, m.hcuenta) AND BETWEEN(asientos.fecha, oapp.inicioejercicio, oapp.finalejercicio) AND asientos.tipo <> "C" AND asientos.idasiento = detalle.idasiento AND asientos.idempresa = oapp.empresa AND detalle.cuenta = cuentas.cuenta AND detalle.centro = m.centro GROUP BY 1 INTO CURSOR xsaldo
SELECT detalle.cuenta, .T. AS detalle, SUM(ROUND(MTON(debe) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales)) AS debe, SUM(ROUND(MTON(haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales)) AS haber, cuentas.integradora, nivel, cuentas.descripci�n, SUM(IIF(MONTH(asientos.fecha) <= m.hmes, ROUND(MTON(detalle.debe - detalle.haber) / IIF(m.tipomoneda = 'L', 1, cotizacion), m.decimales), $0.0000)) AS total FROM datos!asientos_base asientos, datos!detalle_base detalle, datos!cuentas WHERE BETWEEN(detalle.cuenta, m.dcuenta, m.hcuenta) AND BETWEEN(asientos.fecha, m.dfecha, m.hfecha) AND asientos.tipo <> "C" AND asientos.idasiento = detalle.idasiento AND asientos.idempresa = oapp.empresa AND detalle.cuenta = cuentas.cuenta AND detalle.centro = m.centro INTO CURSOR xtsaldo
SELECT xsaldo
GOTO TOP
DO WHILE  .NOT. EOF()
     SELECT saldos
     APPEND BLANK
     cuentanivel = "cuenta" + ALLTRIM(STR(xsaldo.nivel))
     replace proceso with xproceso, cuenta with xsaldo.cuenta, &cuentaNivel with xSaldo.Cuenta, integradora with xsaldo.integradora, detalle with xsaldo.detalle, debe with xsaldo.debe, haber with xsaldo.haber, mes1 with xsaldo.mes1, mes2 with xsaldo.mes2, mes3 with xsaldo.mes3, mes4 with xsaldo.mes4, mes5 with xsaldo.mes5, mes6 with xsaldo.mes6, mes7 with xsaldo.mes7, mes8 with xsaldo.mes8, mes9 with xsaldo.mes9, mes10 with xsaldo.mes10, mes11 with xsaldo.mes11, mes12 with xsaldo.mes12
     IF ATC(LEFT(xsaldo.cuenta, 1), "15") > 0
          REPLACE saldo WITH debe - haber
     ELSE
          REPLACE saldo WITH haber - debe
     ENDIF
     REPLACE nivel WITH xsaldo.nivel
     REPLACE dec WITH 0
     REPLACE descripci�n WITH xsaldo.descripci�n
     xintegradora = xsaldo.integradora
     DO WHILE  .NOT. EMPTY(xintegradora)
          SELECT cuentas
          LOCATE FOR xintegradora = cuentas.cuenta
          IF FOUND()
               SELECT saldos
               cuentanivel = "cuenta" + ALLTRIM(STR(cuentas.nivel))
               replace &CuentaNivel with cuentas.cuenta
               xintegradora = cuentas.integradora
          ELSE
               EXIT
          ENDIF
          IF EMPTY(xintegradora)
               EXIT
          ENDIF
     ENDDO
     SELECT xsaldo
     SKIP
ENDDO
SELECT saldos
ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
      cursor      �Top = 140
Left = 430
Height = 90
Width = 96
Alias = "hc_ganados"
Database = ..\data\datos.dbc
CursorSource = "hc_ganados"
NoDataOnLoad = .T.
Name = "Cursor9"
     i���    P  P                        �$   %   �      �  a   %          �  U  ( % <�  � � � � � � �� � U  CABEZA MM1 MM2 MM3 MM4 MM5 MM6	 DECIMALESb 7�  ����! 7� � � � � � �� � T� �C��]�� G(� datos� %���
 � L��o � T�� �� �� �� � T�� ���� � F� � T� �� �� T� �� �� T� �� �� T� �� �� T� �� �� T� �� �� K�� �� �(�  � �� ���(����5� %��� � ��1� T�  �� ��� �� � �� F� �	 ��C��� %��� ����� T� �� � �� %��� ���� T� �� � �� %��� ���f� T� �� � �� %��� ���M� T� �� � �� %��� ���4� T� �� � �� %��� ���� T� �� � �� �0� T� �� �� � �I� T� �� �� � �b� T� �� �� � �{� T� �� �� � ��� T� �� � �� � ��� T� �� �� �fo� datos!asientos_baseQ� � datos!detalle_baseQ� � datos!cuentas�� � ��a�Q� �CCC� �}C��
 � L� �� � 6�� T���Q� �CCC� �}C��
 � L� �� � 6�� T���Q� �� � ��� ��� � ��CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�  �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�! �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�" �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�# �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�$ �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�% �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�& �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�' �CCC� � H�	� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�( �CCC� � H�
� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�) �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�* �CCC� � H�� C� � H�� 	�3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�+ ��C� � ��, ��- �� C� � �. �/ �. �0 �	� � �1 � C	� � �2 � �2 	� � �3 �. �4 	� � � � � 	� � � �� 	�������� xsaldo��o� datos!asientos_baseQ� � datos!detalle_baseQ� � datos!cuentas�� � ��a�Q� �CCC� �}C��
 � L� �� � 6�� T���Q� �CCC� �}C��
 � L� �� � 6�� T���Q� �� � ��� ��� � ��CCC� � H�� �3 CC� � � � �}C��
 � L� �� � 6�� T� �        6���Q�6 ��C� � ��, ��- �� C� � ��7 ��8 �	� � �1 � C	� � �2 � �2 	� � �3 �. �4 	� � � � � 	� � � �� 	���� xtsaldo� F�5 � #)� +�C+
��T� F�: � � T�; �� cuentaCC�5 � Z����replace proceso with xproceso, cuenta with xsaldo.cuenta, &cuentaNivel with xSaldo.Cuenta, integradora with xsaldo.integradora, detalle with xsaldo.detalle, debe with xsaldo.debe, haber with xsaldo.haber, mes1 with xsaldo.mes1, mes2 with xsaldo.mes2, mes3 with xsaldo.mes3, mes4 with xsaldo.mes4, mes5 with xsaldo.mes5, mes6 with xsaldo.mes6, mes7 with xsaldo.mes7, mes8 with xsaldo.mes8, mes9 with xsaldo.mes9, mes10 with xsaldo.mes10, mes11 with xsaldo.mes11, mes12 with xsaldo.mes12
  %�CC�5 � �=� 15�� ��.� >�< ��� � �� �G� >�< ��� � �� � >� ���5 � �� >�= ��� �� >� ���5 � �� T�> ��5 � �� +�C�> �
��E� F� � -��> � � �� %�C4��� F�: � T�; �� cuentaCC� � Z���, replace &CuentaNivel with cuentas.cuenta
 T�> �� � �� �*� !� � %�C�> ���A� !� � � F�5 � H� � F�: � U?  CABEZA MM1 MM2 MM3 MM4 MM5 MM6	 DECIMALES XPROCESO DATOS
 TIPOMONEDA
 HC_GANADOS CENTRO I HMES C1 C2 C3 C4 C5 C6 DETALLE CUENTA DEBE
 COTIZACION HABER CUENTAS INTEGRADORA NIVEL DESCRIPCI�N ASIENTOS FECHA MES1 MES2 MES3 MES4 MES5 MES6 MES7 MES8 MES9 MES10 MES11 MES12 DCUENTA HCUENTA OAPP INICIOEJERCICIO FINALEJERCICIO TIPO	 IDASIENTO	 IDEMPRESA EMPRESA XSALDO TOTAL DFECHA HFECHA XTSALDO SALDOS CUENTANIVEL SALDO DEC XINTEGRADORA
  �  � U  SETEO Destroy,     �� Init�     �� BeforeOpenTables�    ��1 Q2 � � A� � � A q � � � � � � Qq11A A q � 111111� � A � � A � � A � � A � A � � A afaq Q � q Q ��� A � q � q ��� A A � A A A q A A q 2 q 2                       L         g   q     _   �  �  a    )   P                  