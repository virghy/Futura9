                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Courier New      rh_rcompl_concepto.idconcepto      Arial      Arial      Arial      Courier New      Arial      ("Deducciones y Beneficios por Conceptos"             Arial      empresa             Arial      m.dfecha, ' al ' , m.hfecha             Arial      
"Periodo:"             Arial      
"Concepto"      Arial      "Fecha"      Arial      "Legajo"      Arial      "Nombre"      Arial      "Nro."      Arial      	"Importe"      Arial      "Cuota"      Arial      
"Aplicado"      Arial      "Saldo"      Arial      rh_rcompl_concepto.idconcepto             Arial      rh_rcompl_concepto.concepto             Arial      rh_rcompl_concepto.fecha             Arial      rh_rcompl_concepto.idempleado             Arial      Dalltrim(rh_rcompl_concepto.apellido) +" "+ rh_rcompl_concepto.nombre             Arial      rh_rcompl_concepto.nro             Arial      rh_rcompl_concepto.total      "@Z 999,999,999"             Arial      rh_rcompl_concepto.cuota      "999,999,999"             Arial      rh_rcompl_concepto.aplicado      "999,999,999"             Arial      rh_rcompl_concepto.saldo      "999,999,999"             Arial      rh_rcompl_concepto.total>0      rh_rcompl_concepto.total      "@Z 999,999,999"             Arial      rh_rcompl_concepto.aplicado      "999,999,999"             Arial      rh_rcompl_concepto.saldo      "999,999,999"             Arial      rh_rcompl_concepto.total>0      "Total Concepto"      Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      dataenvironment      �Top = 53
Left = 222
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
DataSource = .NULL.
Name = "Dataenvironment"
     BPROCEDURE BeforeOpenTables
DO seteo

TEXT TO cmdSQL noshow
SELECT Rh_complemento.idempresa,
  Rh_complemento.idcomplemento, Rh_complemento.fecha,
  Rh_complemento.nro, Rh_complemento.idempleado,
  Rh_complemento.idconcepto, Rh_complemento.total,
  Rh_complemento.saldo, Rh_complemento.cuota,
  Rh_complemento.cancelado, Rh_complemento.tipo,
  Rh_complemento.aplicado, Rh_empleado.nombre,
  Rh_empleado.apellido, Rh_conceptos.concepto
 FROM 
     rh_complemento,
    rh_empleado,
    rh_conceptos
 WHERE   Rh_complemento.idempleado = Rh_empleado.idempleado
 and Rh_complemento.IdEmpresa= Rh_empleado.IdEmpresa
   AND  Rh_complemento.idconcepto = Rh_conceptos.idconcepto 
   AND  Rh_complemento.IdEmpresa= Rh_conceptos.IdEmpresa
   AND  Rh_complemento.idempresa =  ?oApp.Empresa 
   AND  Rh_complemento.idconcepto = ?m.conceptorh
   AND  Rh_complemento.fecha BETWEEN ?m.dfecha AND ?m.hfecha
 ORDER BY Rh_complemento.idconcepto, Rh_complemento.fecha,
  Rh_complemento.idempleado
 endtext 
 
 sql(cmdSQL,'rh_rcompl_concepto')
 SELECT rh_rcompl_concepto
 
ENDPROC
     K���    2  2                        ;�   %   �      �     �          �  U  ? �  �	 M(� ��& �  SELECT Rh_complemento.idempresa,�; �5   Rh_complemento.idcomplemento, Rh_complemento.fecha,�6 �0   Rh_complemento.nro, Rh_complemento.idempleado,�8 �2   Rh_complemento.idconcepto, Rh_complemento.total,�3 �-   Rh_complemento.saldo, Rh_complemento.cuota,�6 �0   Rh_complemento.cancelado, Rh_complemento.tipo,�4 �.   Rh_complemento.aplicado, Rh_empleado.nombre,�3 �-   Rh_empleado.apellido, Rh_conceptos.concepto� �  FROM � �      rh_complemento,� �     rh_empleado,� �     rh_conceptos�A �;  WHERE   Rh_complemento.idempleado = Rh_empleado.idempleado�: �4  and Rh_complemento.IdEmpresa= Rh_empleado.IdEmpresa�B �<    AND  Rh_complemento.idconcepto = Rh_conceptos.idconcepto �> �8    AND  Rh_complemento.IdEmpresa= Rh_conceptos.IdEmpresa�8 �2    AND  Rh_complemento.idempresa =  ?oApp.Empresa �7 �1    AND  Rh_complemento.idconcepto = ?m.conceptorh�B �<    AND  Rh_complemento.fecha BETWEEN ?m.dfecha AND ?m.hfecha�@ �:  ORDER BY Rh_complemento.idconcepto, Rh_complemento.fecha,�! �   Rh_complemento.idempleado� �# ��C � � rh_rcompl_concepto� �� F� � U  SETEO CMDSQL SQL RH_RCOMPL_CONCEPTO BeforeOpenTables,     ��1 q � a�a�1aA1� �aa�!��q!A 2q 2                       7      )   2                  