LPARAMETERS lcmenu
IF PCOUNT() > 0
     DO (lcmenu)
ELSE
     DO main.prg
ENDIF
ENDPROC
*
PROCEDURE loadForm
LPARAMETERS lcform, lcparam
IF PCOUNT() = 2
     DO FORM (lcform) WITH lcparam
ELSE
     DO FORM (lcform)
ENDIF
ENDPROC
*
PROCEDURE SetInforme
LPARAMETERS informe
LOCAL lcorientacion, lcmargen,  ;
      obj
lcmargen = leerini('Margen', ;
           'System')
IF EMPTY(lcmargen)
     obj = CREATEOBJECT('OpcionesImpresion')
     obj.show()
     lcmargen = leerini('Margen', ;
                'System')
ENDIF
SELECT * FROM (informe) INTO  ;
         TABLE lcRepofile.frx
lcorientacion = MLINE(expr,  ;
                ATCLINE('ORIENTATION',  ;
                expr))
IF RIGHT(lcorientacion, 1) = '0'
     lcpapel = MLINE(expr,  ;
               ATCLINE('papersize',  ;
               expr))
     lcnuevopapel = 'PAPERSIZE=' +  ;
                    ALLTRIM(STR(PRTINFO(2)))
     REPLACE expr WITH  ;
             STRTRAN(expr,  ;
             lcpapel,  ;
             lcnuevopapel) IN  ;
             lcrepofile
ENDIF
IF  .NOT. EMPTY(lcmargen) .AND.  ;
    RIGHT(lcorientacion, 1) =  ;
    '1'
     REPLACE hpos WITH  ;
             ROUND(VAL(lcmargen) *  ;
             393.7 , 0) IN  ;
             lcrepofile
ENDIF
IF USED(informe)
     USE IN (informe)
ENDIF
USE IN lcrepofile
informe = 'lcRepofile.frx'
ENDPROC
*
