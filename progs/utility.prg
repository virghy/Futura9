
EXTERNAL ARRAY lmmatriz
EXTERNAL ARRAY tafiles
ENDPROC
*
FUNCTION IsTag
LPARAMETERS tctagname, tcalias
LOCAL llistag, lctagfound
IF PARAMETERS() < 2
     tcalias = ALIAS()
ENDIF
IF EMPTY(tcalias)
     RETURN .F.
ENDIF
llistag = .F.
tctagname = UPPER(ALLTRIM(tctagname))
lntagnum = 1
lctagfound = TAG(lntagnum,  ;
             tcalias)
DO WHILE  .NOT. EMPTY(lctagfound)
     IF UPPER(ALLTRIM(lctagfound)) ==  ;
        tctagname
          llistag = .T.
          EXIT
     ENDIF
     lntagnum = lntagnum + 1
     lctagfound = TAG(lntagnum,  ;
                  tcalias)
ENDDO
RETURN llistag
ENDFUNC
*
PROCEDURE NotYet
= MESSAGEBOX("En construcción",  ;
  64, "Futura Software")
RETURN
ENDPROC
*
FUNCTION FileSize
LPARAMETERS tcfilename
LOCAL lcsetcompatible, lnfilesize
lcsetcompatible = SET('COMPATIBLE')
SET COMPATIBLE ON
lnfilesize = FSIZE(tcfilename)
SET COMPATIBLE &lcSetCompatible
RETURN lnfilesize
ENDFUNC
*
FUNCTION FormIsObject
*SET COVERAGE TO futura.log ADDITIVE 

*RETURN (TYPE("_screen.activeform") ==  "O" .AND.  UPPER(_SCREEN.activeform.baseclass) =  "FORM")
RETURN (TYPE("_screen.activeform") ==  "O" .AND.  INLIST(UPPER(_SCREEN.activeform.class),'TSBASEFORM','TSMOVFORM','TSMAINTFORM'))
*SET COVERAGE TO 
ENDFUNC

*
FUNCTION ToolBarEnabled
PARAMETER oobject
LOCAL otoolobj
otoolobj = "oApp.oToolBar." +  oobject + ".enabled"
IF TYPE(otoolobj) <> "L"
     RETURN .F.
ELSE
     RETURN EVALUATE(otoolobj)
ENDIF
ENDFUNC
*
FUNCTION OnShutdown
LOCAL lcrespuesta
lcrespuesta = MESSAGEBOX( ;
              "Realmente desea salir del sistema ?",  ;
              0292,  ;
              "Futura Software")
IF lcrespuesta = 6
     CLEAR EVENTS
ELSE
     RETURN .F.
ENDIF
ENDFUNC
*
PROCEDURE MAINHWND
*
** ReFox - no body found for this  ;
   procedure **
*
ENDPROC
*
PROCEDURE _WHTOHWND
*
** ReFox - no body found for this  ;
   procedure **
*
ENDPROC
*
PROCEDURE _WONTOP
*
** ReFox - no body found for this  ;
   procedure **
*
ENDPROC
*
FUNCTION ValHora
LPARAMETERS lchora
RETURN (BETWEEN(VAL(SUBSTR(lchora,  ;
       1, 2)), 0, 23) .AND.  ;
       BETWEEN(VAL(SUBSTR(lchora,  ;
       4, 2)), 0, 59))
ENDFUNC
*
FUNCTION Descuento
PARAMETER gvalor, gdescuento
LOCAL lnvalor
IF gdescuento = 0
     lnvalor = gvalor
ELSE
     lnvalor = 0
     lnvalor = ROUND(gvalor -  ;
               (gvalor *  ;
               (gdescuento /  ;
               100)), 2)
ENDIF
RETURN lnvalor
ENDFUNC
*
FUNCTION Numeral
LPARAMETERS valornumero
LOCAL lcValor
IF valorNumero >999999999
	lcvalor = word(VAL(LEFT(STR(valornumero,12),3))*1000)+" "+numeral1(valornumero-1000000000)
ELSE
	lcValor = word(valornumero)
ENDIF
&&VG 25/02/09
&&Corregimos en valor VEINTE Y UN MILLONES, ETC.
IF RIGHT(RTRIM(lcValor),2)='UN'
	lcValor=RTRIM(lcValor) + 'O'
ENDIF
RETURN lcValor 


ENDFUNC 

FUNCTION NUMERAL1
LPARAMETERS valornumero

DIMENSION unidad[10]
unidad[1] = " "
unidad[2] = "un"
unidad[3] = "dos"
unidad[4] = "tres"
unidad[5] = "cuatro"
unidad[6] = "cinco"
unidad[7] = "seis"
unidad[8] = "siete"
unidad[9] = "ocho"
unidad[10] = "nueve"
DIMENSION decena[10]
decena[1] = " "
decena[2] = "diez"
decena[3] = "veinte"
decena[4] = "treinta"
decena[5] = "cuarenta"
decena[6] = "cincuenta"
decena[7] = "sesenta"
decena[8] = "setenta"
decena[9] = "ochenta"
decena[10] = "noventa"
DIMENSION centena[10]
centena[1] = " "
centena[2] = "ciento"
centena[3] = "doscientos"
centena[4] = "trescientos"
centena[5] = "cuatrocientos"
centena[6] = "quinientos"
centena[7] = "seiscientos"
centena[8] = "setecientos"
centena[9] = "ochocientos"
centena[10] = "novecientos"
ndig = 0
aux = valornumero
DO WHILE aux >= 1
     aux = aux / 10
     ndig = ndig + 1
ENDDO
DIMENSION tab1[10]
tab1 = " "
var = valornumero
a = 1
DO WHILE var >= 1
     var = var / 10
     tab1[a] = STR((var -  ;
         INT(var)) * 10, 1, 0)
     var = INT(var)
     a = a + 1
ENDDO

DIMENSION letra[20]
letra = " "
pp = 1
DO WHILE pp <= ndig
     letra[pp] =  ;
          ALLTRIM(unidad(VAL(tab1(pp)) +  ;
          1)) + " "
     pp = pp + 1
     IF pp <= ndig
          letra[pp] =  ;
               ALLTRIM(decena(VAL(tab1(pp)) +  ;
               1)) + " "
     ENDIF
     pp = pp + 1
     IF pp <= ndig
          letra[pp] =  ;
               ALLTRIM(centena(VAL(tab1(pp)) +  ;
               1)) + " "
     ENDIF
     pp = pp + 1
ENDDO
DIMENSION otro[20]
otro = " "
FOR xx = 1 TO ndig
     otro[xx] = letra(xx)
ENDFOR
letra[1] = otro(1)
letra[2] = "Y "
letra[3] = otro(2)
letra[4] = otro(3)
letra[5] = " "
letra[6] = "MIL "
letra[7] = otro(4)
letra[8] = "Y "
letra[9] = otro(5)
letra[10] = otro(6)
letra[11] = " "
letra[12] = "MILLONES "
letra[13] = otro(7)
letra[14] = "Y "
letra[15] = otro(8)
letra[16] = otro(9)



IF tab1(2) = "1"
     IF tab1(1) = "1"
          otro[1] = "ONCE "
          otro[2] = " "
     ELSE
          IF tab1(1) = "2"
               otro[1] = "DOCE "
               otro[2] = " "
          ELSE
               IF tab1(1) = "3"
                    otro[1] =  "TRECE "
                    otro[2] = " "
               ELSE
                    IF tab1(1) =  "4"
                         otro[1] =  "CATORCE "
                         otro[2] =  " "
                    ELSE
                         IF tab1(1) =  "5"
                              otro[1] =  "QUINCE "
                              otro[2] =  " "
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF

IF tab1(5) = "1"
     IF tab1(4) = "1"
          otro[4] = "ONCE "
          otro[5] = " "
     ELSE
          IF tab1(4) = "2"
               otro[4] = "DOCE "
               otro[5] = " "
          ELSE
               IF tab1(4) = "3"
                    otro[4] = "TRECE "
                    otro[5] = " "
               ELSE
                    IF tab1(4) = "4"
                         otro[4] =  "CATORCE "
                         otro[5] = " "
                    ELSE
                         IF tab1(4) =  "5"
                              otro[4] =  "QUINCE "
                              otro[5] =  " "
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
IF tab1(8) = "1"
     IF tab1(7) = "1"
          otro[7] = "ONCE "
          otro[8] = " "
     ELSE
          IF tab1(7) = "2"
               otro[7] = "DOCE "
               otro[8] = " "
          ELSE
               IF tab1(7) = "3"
                    otro[7] =  ;
                        "TRECE "
                    otro[8] = " "
               ELSE
                    IF tab1(7) =  ;
                       "4"
                         otro[7] =  ;
                             "CATORCE "
                         otro[8] =  ;
                             " "
                    ELSE
                         IF tab1(7) =  ;
                            "5"
                              otro[7] =  ;
                               "QUINCE "
                              otro[8] =  ;
                               " "
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
SET STEP ON
IF otro(1) = " " .OR. otro(2) =  " "
     letra[2] = " "
ENDIF
IF otro(4) = " " .OR. otro(5) =  " "
     letra[8] = " "
ENDIF
IF otro(7) = " " .OR. otro(8) =  " "
     letra[14] = " "
ENDIF
IF otro(4) = " " .AND. otro(5) =  " " .AND. otro(6) = " "
     letra[6] = " "
ENDIF
IF otro(1) + otro(2) + otro(3) =  " "
     letra[5] = " "
ENDIF
IF otro(4) + otro(5) + otro(6) =  " "
     letra[5] = " "
ENDIF
IF otro(4) + otro(5) + otro(6) =  " "
     letra[11] = " "
ENDIF
IF otro(7) + otro(8) + otro(9) =  " "
     letra[11] = " "
ENDIF
IF otro(7) = " " .AND. otro(8) =  " " .AND. otro(9) = " "
     letra[12] = " "
ENDIF
IF tab1(3) = "1" .AND. otro(1) +  otro(2) = " "
     otro[3] = "CIEN "
ENDIF
IF tab1(6) = "1" .AND. otro(5) +  otro(4) = " "
     otro[6] = "CIEN "
ENDIF
IF tab1(9) = "1" .AND. otro(8) +  otro(7) = " "
     otro[9] = "CIEN "
ENDIF
IF tab1(4) = "1" .AND. otro(5) +  otro(6) + otro(7) = " "
     otro[4] = " "
ENDIF
IF tab1(7) = "1" .AND.  EMPTY(tab1(8) + tab1(9) +  tab1(10))
     otro[7] = "UN "
     letra[12] = "MILLON "
ENDIF

SET STEP ON


letra[1] = otro(1)
letra[3] = otro(2)
letra[4] = otro(3)
letra[7] = otro(4)
letra[9] = otro(5)
letra[10] = otro(6)
letra[13] = otro(7)
letra[15] = otro(8)
letra[16] = otro(9)
letra[18] = otro(10)

aaa = SPACE(80)
zzz = LTRIM(letra(16)) +  ;
      LTRIM(letra(15)) +  ;
      LTRIM(letra(14)) +  ;
      LTRIM(letra(13)) +  ;
      LTRIM(letra(12)) +  ;
      LTRIM(letra(11)) +  ;
      LTRIM(letra(10)) +  ;
      LTRIM(letra(9)) +  ;
      LTRIM(letra(8)) +  ;
      LTRIM(letra(7)) +  ;
      LTRIM(letra(6)) +  ;
      LTRIM(letra(5)) +  ;
      LTRIM(letra(4)) +  ;
      LTRIM(letra(3)) +  ;
      LTRIM(letra(2)) +  ;
      LTRIM(letra(1))
aaa = zzz
RETURN UPPER(aaa)
ENDFUNC
*

FUNCTION WORD()
 LPARAMETERS _NUM
 IF TYPE('_NUM')#'N'
*  RETURN 'Solo traduce Cadenas Numericas'
 ENDIF
 STORE ' ' TO _LMILLON,_LMILES,_LET
 _L1='UN DOS TRES CUATRO CINCO SEIS SIETE OCHO NUEVE DIEZ ONCE DOCE TRECE CATORCE QUINCE'
 _G1=' 1  2  3 4  5  6  7  8  9  10 11 12 13 14 15'
 _X1=' 1  4  8 13 20 26 31 37 42 48 53 58 63 69 77'
 _Y1=' 2  3  4 6  5  4  5  4  5  4  4  4  5  7  6'
 _L2='VEINTE TREINTA CUARENTA CINCUENTA SESENTA SETENTA OCHENTA NOVENTA'
 _G2='20 30 40 50 60 70 80 90'
 _X2=' 1  8 16 25 35 43 51 59'
 _Y2=' 6  7 8  9  7  7  7  7'
 _L3='CIENTO DOSC TRESC CUATROC QUIN SEISC SETEC OCHOC NOVEC'
 _G3=' 1  2 3  4  5  6  7  8  9'
 _X3=' 1  8 13 19 27 32 38 44 50'
 _Y3=' 6  4 5  7  4  5  5  5  5'
 IF _NUM=0
  RETURN 'CERO'
 ENDIF
 IF _NUM>999999999
  RETURN 'Solo traduce Numeros inferiores a Mil Millones......'
 ENDIF
 _NOM=_NUM
 _NAM=INT(_NUM)
 _MILLON=VAL(SUBSTR(STR(_NAM,9),1,3))
 _MILES=VAL(SUBSTR(STR(_NAM,9),4,3))
 _CIENTOS=VAL(SUBSTR(STR(_NAM,9),7,3))
 IF _CIENTOS>0
  _X=1
 ENDIF
 IF _MILES>0
  _X=2
 ENDIF
 IF _MILLON>0
  _X=3
 ENDIF
 DO WHILE _X>0
  IF _X=3
   _NUM=_MILLON
  ELSE
   IF _X=2
    _NUM=_MILES
   ELSE
    IF _X=1
     _NUM=_CIENTOS
    ENDIF
   ENDIF
  ENDIF
  _NUM1=_NUM
  _CAD=STR(_NUM,3)
  _C1=SUBSTR(_CAD,2,2)
  IF _NUM>0
   IF _NUM>100
    _NUM2=VAL(_C1)
    _NUM=_NUM2
    _C3=SUBSTR(_CAD,1,1)
    _N3=AT(_C3,_G3)
    _INI3=VAL(SUBSTR(_X3,_N3,2))
    _LON3=VAL(SUBSTR(_Y3,_N3,2))
    _LET3=SUBSTR(_L3,_INI3,_LON3)+IIF(_C3='1',' ','IENTOS ')
   ENDIF
   IF _NUM<10
    _CAD=STR(_NUM,1)
    _C1=SUBSTR(_CAD,1,1)
   ELSE
    IF _NUM<100
     _CAD=STR(_NUM,2)
    ENDIF
   ENDIF
   IF _NUM<16
    _N1=AT(_C1,_G1)
    _INI=VAL(SUBSTR(_X1,_N1,2))
    _LON=VAL(SUBSTR(_Y1,_N1,2))
    _LET=SUBSTR(_L1,_INI,_LON)
   ELSE
    IF _NUM<20
     _C1=SUBSTR(_CAD,2,1)
     _N1=AT(_C1,_G1)
     _INI=VAL(SUBSTR(_X1,_N1,2))
     _LON=VAL(SUBSTR(_Y1,_N1,2))
     _LET='DIECI'+SUBSTR(_L1,_INI,_LON)
    ELSE
     _C2=VAL(SUBSTR(_CAD,2,1))
     _NUM=_NUM-_C2
     _C1=STR(_NUM,2)
     _N1=AT(_C1,_G2)
     _INI=VAL(SUBSTR(_X2,_N1,2))
     _LON=VAL(SUBSTR(_Y2,_N1,2))
     _LET=SUBSTR(_L2,_INI,_LON)
     IF SUBSTR(_CAD,2,1)#'0'
      _C2=SUBSTR(_CAD,2,1)
      _N2=AT(_C2,_G1)
      _INI2=VAL(SUBSTR(_X1,_N2,2))
      _LON2=VAL(SUBSTR(_Y1,_N2,2))
      _LET=_LET+' Y '+SUBSTR(_L1,_INI2,_LON2)
     ENDIF
    ENDIF
   ENDIF
   IF _NUM1>99
    IF _NUM1=100
     _LET='CIEN '
    ELSE
     _LET=_LET3+_LET
    ENDIF
   ENDIF
   IF _X=3
    _LMILLON=_LET+' MILLONES'
    IF SUBSTR(_LET,1,2)='UN'
     _LMILLON=_LET+' MILLON'
    ENDIF
   ENDIF
   IF _X=2
    _LMILES=_LET+' MIL'
    IF SUBSTR(_LET,1,2)='UN'
     _LMILES='MIL'
    ENDIF
   ENDIF
  ENDIF
  _X=_X-1
 ENDDO
 IF _MILLON>0
  IF _MILES=0
   IF _cientos=0
    _BB=_LMILLON
   ELSE
    _BB=_LMILLON+' '+_LET
   ENDIF
  ELSE
   IF _cientos=0
    _BB=_lmillon+' '+_lmiles
   ELSE
    _BB=_LMILLON+' '+_LMILES+' '+_LET
   ENDIF
  ENDIF
 ELSE
  IF _MILES>0
   IF _cientos=0
    _BB=_lmiles
   ELSE
    _BB=_LMILES+' '+_LET
   ENDIF
  ELSE
   _BB=_LET
  ENDIF
 ENDIF
 *_BB=_BB+' CON '+SUBSTR(STR(_NOM,13,2),12,2)+'/100'
 RETURN _BB
ENDFUNC

PROCEDURE LogCambio
lctabla = ALIAS()
lnrecno = RECNO()
IF TYPE('OAPP') = 'O'
     lcusuario = oapp.getemployeeid()
ELSE
     lcusuario = "Desconocido"
ENDIF
ctabla = ALIAS()
nrocampos = FCOUNT(ctabla)
DIMENSION campos[nrocampos, 3]
campos = ''
FOR nc = 1 TO nrocampos
     campos[nc, 1] = FIELD(nc)
     IF GETFLDSTATE(campos(nc,1),  ;
        ctabla) = 2
          campos[nc,2]=&campos[nc,1]
          campos[nc, 3] = 'OK'
     ENDIF
ENDFOR
FOR nc = 1 TO nrocampos
     IF campos(nc,3) = 'OK'
          DO CASE
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'N'
                    campos[nc, 2] =  ;
                          ALLTRIM(STR(campos(nc, ;
                          2)))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'D'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'T'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'L'
                    campos[nc, 2] =  ;
                          IIF(campos(nc, ;
                          2),  ;
                          'Verdadero',  ;
                          'Falso')
          ENDCASE
          STRTOFILE(campos(nc,1) +  ;
                   ' = ' +  ;
                   ALLTRIM(campos(nc, ;
                   2)) + CHR(13),  ;
                   'auditoria.log',  ;
                   .T.)
     ENDIF
ENDFOR
INSERT INTO Datos!AuditoriaLog  ;
       (operacion, usuario, tabla,  ;
       registro) VALUES  ;
       ("Edición", lcusuario,  ;
       lctabla, lnrecno)
SELECT auditorialog
APPEND MEMO detalle FROM  ;
       Auditoria.LOG
DELETE FILE Auditoria.LOG
USE IN auditorialog
ENDPROC
*
PROCEDURE LogBorrar
lctabla = ALIAS()
lnrecno = RECNO()
IF TYPE('OAPP') = 'O'
     lcusuario = oapp.getemployeeid()
ELSE
     lcusuario = "Desconocido"
ENDIF
INSERT INTO Datos!AuditoriaLog  ;
       (operacion, usuario, tabla,  ;
       registro) VALUES  ;
       ("Eliminación", lcusuario,  ;
       lctabla, lnrecno)
USE IN auditorialog
ENDPROC
*
PROCEDURE LogInsertar
lnrecno = RECCOUNT() + 1
IF TYPE('OAPP') = 'O'
     lcusuario = oapp.getemployeeid()
ELSE
     lcusuario = "Desconocido"
ENDIF
ctabla = ALIAS()
nrocampos = FCOUNT(ctabla)
DIMENSION campos[nrocampos, 3]
campos = ''
FOR nc = 1 TO nrocampos
     campos[nc, 1] = FIELD(nc)
     IF GETFLDSTATE(campos(nc,1),  ;
        ctabla) = 3 .OR.  ;
        GETFLDSTATE(campos(nc,1),  ;
        ctabla) = 4
          campos[nc,2]=&campos[nc,1]
          IF GETFLDSTATE(campos(nc, ;
             1), ctabla) = 4
               campos[nc, 3] =  ;
                     'OK'
          ENDIF
     ENDIF
ENDFOR
FOR nc = 1 TO nrocampos
     IF campos(nc,3) = 'OK'
          DO CASE
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'N'
                    campos[nc, 2] =  ;
                          ALLTRIM(STR(campos(nc, ;
                          2)))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'D'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'T'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'L'
                    campos[nc, 2] =  ;
                          IIF(campos(nc, ;
                          2),  ;
                          'Verdadero',  ;
                          'Falso')
          ENDCASE
          STRTOFILE(campos(nc,1) +  ;
                   ' = ' +  ;
                   ALLTRIM(campos(nc, ;
                   2)) + CHR(13),  ;
                   'auditoria.log',  ;
                   .T.)
     ENDIF
ENDFOR
INSERT INTO Datos!AuditoriaLog  ;
       (operacion, usuario, tabla,  ;
       registro) VALUES  ;
       ("Agregar", lcusuario,  ;
       ctabla, lnrecno)
SELECT auditorialog
APPEND MEMO detalle FROM  ;
       Auditoria.LOG
DELETE FILE Auditoria.LOG
USE IN auditorialog
ENDPROC
*
FUNCTION VERIFICAR
RETURN .F.
ENDFUNC
*
FUNCTION LeerIni
LPARAMETERS tcentry, tcgrupo
LOCAL lcbuffer, lcolderror, lntop,  ;
      lnleft, llerror, lncommapos,  ;
      lcentry
IF PCOUNT() = 0
     lcentry = thisform.caption
ELSE
     lcentry = tcentry
ENDIF
lcbuffer = SPACE(200) + CHR(0)
lcolderror = ON('ERROR')
IF getprivstr(tcgrupo,lcentry,"", ;
   @lcbuffer,LEN(lcbuffer), ;
   CURDIR() + "FUTURA.INI") > 0
     ON ERROR LLERROR = .T.
     ON ERROR &lcOldError
     IF  .NOT. llerror
          lcbuffer = ALLTRIM(STRTRAN(lcbuffer,  ;
                     CHR(0),  ;
                     ' '))
          RETURN lcbuffer
     ENDIF
ENDIF
RETURN ''
ENDFUNC
*
PROCEDURE EscribirIni
LPARAMETERS tcvalor, tcentry,  ;
            tcgrupo
LOCAL lcvalue, lcentry
IF PCOUNT() = 0
     lcentry = thisform.caption
ELSE
     lcentry = tcentry
ENDIF
lcvalue = tcvalor
= writeprivstr(tcgrupo,lcentry, ;
  lcvalue,CURDIR() +  ;
  "FUTURA.INI")
ENDPROC
*
PROCEDURE Generar_turno
LPARAMETERS lmmatriz, lcestado,  ;
            lndiasestado,  ;
            lndiastrabajados,  ;
            lndiaslibres,  ;
            lcturnoanterior,  ;
            lnrotativo,  ;
            lcturno_a_rotar,  ;
            lndias_a_generar
LOCAL lncontinua, lnposicion,  ;
      lcturno, x
lnposicion = 1
IF lcestado = 'F'
     lncontinua = lndiaslibres -  ;
                  lndiasestado
ELSE
     lncontinua = lndiastrabajados -  ;
                  lndiasestado
ENDIF
IF lncontinua <> 0
     IF lcestado <> 'F'
          lcturno = lcturnoanterior
     ELSE
          lcturno = 'F'
     ENDIF
     FOR x = 1 TO lncontinua
          lmmatriz(lnposicion) =  ;
                  lcturno
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
ENDIF
IF lcestado <> 'F'
     FOR x = 1 TO lndiaslibres
          lmmatriz(lnposicion) =  ;
                  'F'
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
ENDIF
FOR x = 1 TO lndias_a_generar
     IF lnposicion >  ;
        lndias_a_generar
          EXIT
     ENDIF
     IF lnrotativo <> 0
          lcturno = ALLTRIM(STR(lnrotativo))
     ELSE
          lcturno = SUBSTR(lcturno_a_rotar,  ;
                    AT(lcturnoanterior,  ;
                    lcturno_a_rotar) +  ;
                    1, 1)
          IF EMPTY(lcturno)
               lcturno = SUBSTR(lcturno_a_rotar,  ;
                         1, 1)
          ENDIF
          lcturnoanterior = lcturno
     ENDIF
     FOR x = 1 TO  ;
         lndiastrabajados
          IF lnposicion >  ;
             lndias_a_generar
               EXIT
          ENDIF
          lmmatriz(lnposicion) =  ;
                  lcturno
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
     FOR x = 1 TO lndiaslibres
          IF lnposicion >  ;
             lndias_a_generar
               EXIT
          ENDIF
          lmmatriz(lnposicion) =  ;
                  'F'
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
ENDFOR
RETURN
ENDPROC
*
PROCEDURE seteo
SET TALK OFF
SET CONSOLE OFF
SET SYSFORMATS ON
SET CENTURY ON
SET DATE short
SET POINT TO ','
SET SEPARATOR TO '.'
SET DATABASE TO (oapp.cdatabase)
SET DELETED ON
SET NULLDISPLAY TO ' '
SET MULTILOCKS ON
SET SAFETY OFF 
_REPORTOUTPUT = "ReportOutput.app"
RETURN
ENDPROC
*
FUNCTION SetInforme
LPARAMETERS informe
LOCAL lcorientacion, lcmargen, lcmargentop, obj

lcmargen = leerini('Margen', 'System')
lcmargentop = leerini('MargenTOP', 'System')

IF EMPTY(lcmargen) .OR.  EMPTY(lcmargentop)
     obj = CREATEOBJECT('OpcionesImpresion')
     obj.show()
     lcmargen = leerini('Margen', 'System')
ENDIF

SELECT * FROM (informe) INTO  TABLE lcRepofile.frx
lcorientacion = MLINE(expr,  ATCLINE('ORIENTATION',  expr))

IF RIGHT(lcorientacion, 1) = '0'
     lcpapel = MLINE(expr,  ATCLINE('papersize',  expr))
     lcnuevopapel = 'PAPERSIZE=' +  ALLTRIM(STR(PRTINFO(2)))
     REPLACE expr WITH  STRTRAN(expr,  lcpapel,  lcnuevopapel) IN  lcrepofile
ENDIF

IF  .NOT. EMPTY(lcmargen) .AND.  RIGHT(lcorientacion, 1) =  '1'
     REPLACE hpos WITH  ROUND(VAL(lcmargen) *  393.7 , 0) IN  lcrepofile
ENDIF

IF  .NOT. EMPTY(lcmargentop)
     REPLACE vpos WITH vpos +  ROUND(VAL(lcmargentop) *  393.7 , 0) ALL FOR  objtype = 8 IN  lcrepofile
ENDIF

*	USE IN lcrepofile
	USE IN (informe)
*	RETURN 'lcRepofile.frx'
	
	use in lcRepofile
	informe= 'lcRepofile.frx'
	return informe
		
ENDFUNC

*
FUNCTION nulltostr
LPARAMETERS lcvalor
RETURN IIF(ISNULL(lcvalor), '',  ;
       lcvalor)
ENDFUNC
*
FUNCTION SQL2
LPARAMETERS cmdsql, lccursor,  ;
            lnconn
LOCAL _hndconn, _lnresultado,  ;
      _lcalias
_lcalias = ALIAS()
IF TYPE('lnConn') = 'N'
     _hndconn = lnconn
ELSE
     lnconn = 0

    
	     IF TYPE('oApp') <> 'O'
	          sistema = 'ND'
	          _hndconn = SQLCONNECT('datos_Sql')
	     ELSE
		     IF TYPE('oApp.hndConn') <> 'N'	     
		        sistema = oapp.sistema
				oApp.hndConn = SQLCONNECT(oApp.Sistema,.T.)
		     	= SQLSETPROP(oApp.hndConn, 'BatchMode', .T.)
				= SQLSETPROP(oApp.hndConn, "Transactions", 1)
		     ENDIF			
			_hndconn=oApp.hndConn
	     ENDIF
		

     
              	
     
     
ENDIF
IF _hndconn <= 0
     MESSAGEBOX( ;
               'No se pudo conectar al Servidor' +  ;
               CHR(13) +  ;
               'Sistema: ' +  ;
               sistema + CHR(13) +  ;
               'BD: ' + DBC())
     RETURN -1
ENDIF
DO WHILE SQLGETPROP(_hndconn,  ;
   'ConnectBusy')
     WAIT WINDOW NOCLEAR NOWAIT  ;
          'Conexion Ocupada...'
ENDDO

IF TYPE('lcCursor') = 'C'
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql,  ;
                    lccursor)
ELSE
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql)
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
     MESSAGEBOX(lcerror)
ENDIF
IF lnconn = 0
     SQLCANCEL(_hndconn)
*     SQLDISCONNECT(_hndconn)
ENDIF
IF  .NOT. EMPTY(_lcalias)
     SELECT (_lcalias)
ENDIF
RETURN _lnresultado
ENDFUNC

FUNCTION SQL
LPARAMETERS lcmdsql, lccursor,  ;
            lnconn
LOCAL _hndconn, _lnresultado,  ;
      _lcalias
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
     MESSAGEBOX( ;
               'No se pudo conectar al Servidor' +  ;
               CHR(13) +  ;
               'Sistema: ' +  ;
               sistema + CHR(13) +  ;
               'BD: ' + DBC())
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
     =RegistrarError(lcError)
     MESSAGEBOX(lcerror)
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

*
FUNCTION SQLDB2
LPARAMETERS cmdsql, lccursor,  ;
            lnconn
LOCAL _hndconn, _lnresultado,  ;
      _lcalias
_lcalias = ALIAS()
IF TYPE('lnConn') = 'N'
     _hndconn = lnconn
ELSE
     lnconn = 0
     WAIT WINDOW NOCLEAR NOWAIT  ;
          'Conectando...'
     IF TYPE('oApp') <> 'O'
          sistema = 'ND'
          _hndconn = SQLCONNECT('CED')
     ELSE
          sistema = oapp.sistema
          _hndconn = SQLCONNECT("CED")
     ENDIF
ENDIF
IF _hndconn <= 0
     MESSAGEBOX( ;
               'No se pudo conectar al Servidor' +  ;
               CHR(13) +  ;
               'Sistema: ' +  ;
               sistema + CHR(13) +  ;
               'BD: ' + DBC())
     RETURN -1
ENDIF
DO WHILE SQLGETPROP(_hndconn,  ;
   'ConnectBusy')
     WAIT WINDOW NOCLEAR NOWAIT  ;
          'Conexion Ocupada...'
ENDDO
WAIT WINDOW NOCLEAR NOWAIT  ;
     'Procesando...'
= SQLSETPROP(_hndconn,  ;
  'BatchMode', .T.)
= SQLSETPROP(_hndconn,  ;
  "Transactions", 1)
IF TYPE('lcCursor') = 'C'
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql,  ;
                    lccursor)
ELSE
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql)
ENDIF
WAIT CLEAR
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
     MESSAGEBOX(lcerror)
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
*
FUNCTION Encriptar
LPARAMETERS lccadena
LOCAL lnlongitud, i, lcencriptado,  ;
      lccaracter
lnlongitud = LEN(ALLTRIM(lccadena))
lcencriptado = ''
FOR i = lnlongitud TO 1 STEP -1
     lccaracter = ASC(SUBSTR(lccadena,  ;
                  i, 1)) - (i *  ;
                  2)
     lcencriptado = lcencriptado +  ;
                    CHR(lccaracter)
ENDFOR
RETURN lcencriptado
ENDFUNC
*
FUNCTION CambiarHoja
LPARAMETERS lcselerepo
LOCAL lcpapel, lcnuevopapel
IF  .NOT. FILE(lcselerepo)
     RETURN lcselerepo
ENDIF
SELECT * FROM (lcselerepo) INTO  ;
         TABLE lcRepofile.frx
lcpapel = MLINE(expr,  ;
          ATCLINE('papersize',  ;
          expr))
lcnuevopapel = 'PAPERSIZE=' +  ;
               ALLTRIM(STR(PRTINFO(2)))
REPLACE expr WITH STRTRAN(expr,  ;
        lcpapel, lcnuevopapel)
USE IN lcrepofile
RETURN 'lcRepofile.frx'
ENDFUNC
*

FUNCTION LetraMes
LPARAMETERS lnMes
LOCAL lcMes as String
DO CASE 
	CASE lnMes = 1
		lcMes = "Enero"
	CASE lnMes = 2
		lcMes = "Febrero"
	CASE lnMes = 3
		lcMes = "Marzo"
	CASE lnMes = 4
		lcMes = "Abril"
	CASE lnMes = 5
		lcMes = "Mayo"
	CASE lnMes = 6
		lcMes = "Junio"
	CASE lnMes = 7
		lcMes = "Julio"
	CASE lnMes = 8
		lcMes = "Agosto"
	CASE lnMes = 9
		lcMes = "Setiembre"
	CASE lnMes = 10
		lcMes = "Octubre"
	CASE lnMes = 11
		lcMes = "Noviembre"
	CASE lnMes = 12
		lcMes = "Diciembre"
ENDCASE 
RETURN lcMEs
ENDFUNC 
		



FUNCTION LeerParam
LPARAMETERS lcdevuelve, lctabla,  ;
            lccondicion, lcodbc
LOCAL cadenasql, lcalias,  ;
      lxresultado
lcalias = ALIAS()
cadenasql = 'Select ' +  ;
            lcdevuelve +  ;
            ' as Devuelve From ' +  ;
            lctabla + ' where ' +  ;
            lccondicion
IF PCOUNT() < 4
     lcodbc = 'datos_sql'
ENDIF
IF sql(cadenasql,'xxParam') > 0
     SELECT (lcalias)
     lcresultado = xxparam.devuelve
     USE IN xxparam
     RETURN lcresultado
ENDIF
ENDFUNC
*
FUNCTION Cotizacion
PARAMETER lcmoneda, lctipo,  ;
          ldfecha
LOCAL lncotizacion, lcalias AS  ;
      CHARACTER
lcalias = ALIAS()
IF PCOUNT() < 3
     ldfecha = DATE()
ENDIF
TEXT TO cmdsql NOSHOW

	SELECT TOP 1 compra, venta,  fecha 
	FROM  bs_cotizacion 
	WHERE  bs_cotizacion.idmoneda =  ?lcmoneda AND fecha <=  ?ldfecha 
	ORDER BY fecha  DESC 

ENDTEXT
sql(cmdsql,'xxCotiza')
IF RECCOUNT('xxCotiza') > 0
     lncotizacion = IIF(lctipo =  ;
                    'V',  ;
                    xxcotiza.venta,  ;
                    xxcotiza.compra)
ELSE
     lncotizacion = 1
ENDIF
USE IN xxcotiza
IF  .NOT. EMPTY(lcalias)
     SELECT (lcalias)
ENDIF
RETURN lncotizacion
ENDFUNC
*
FUNCTION ret_tip_recibo
LPARAMETERS lccondicion
LOCAL vtip_reci, lcalias
strsql = "select tip_reci from vt_tipo_doc " +  ;
         "where idcondicion = '" +  ;
         ALLTRIM(lccondicion) +  ;
         "'"
lcalias = ALIAS()
IF sql(strsql,'cu_condi') > 0
     IF  .NOT. EOF('cu_condi')
          vtip_reci = cu_condi.tip_reci
     ELSE
          vtip_reci = ''
     ENDIF
     IF USED('cu_condi')
          USE IN 'cu_condi'
     ENDIF
ELSE
     vtip_reci = ''
ENDIF
SELECT (lcalias)
RETURN vtip_reci
ENDFUNC
*
PROCEDURE EXPORTAR
gcdelimname = ALIAS() + '.xls'
lcdefa = SYS(5) + SYS(2003)
gcdelimfile = PUTFILE('Tabla:',  ;
              gcdelimname,  ;
              'xls')
IF  .NOT. EMPTY(gcdelimfile)
     COPY TO (gcdelimfile) TYPE  ;
          XL5
     MESSAGEBOX( ;
               'El archivo ha sido copiado satisfactoriamente a ' +  ;
               gcdelimfile, 064,  ;
               tastrade_loc)
ENDIF
ENDPROC
*
FUNCTION SendViaMAPI
LPARAMETERS tcfrom, tcto,  ;
            tcsubject, tcbody,  ;
            tafiles
ON ERROR RETURN(.F.)
LOCAL losession, lomessages
losession = CREATEOBJECT("MSMAPI.MAPISession")
losession.signon()
IF (losession.sessionid > 0)
     lomessages = CREATEOBJECT("MSMAPI.MAPIMessages")
     lomessages.sessionid = losession.sessionid
ENDIF
WITH lomessages
     .compose()
     .recipdisplayname = tcto
     .reciptype = 1
     .resolvename()
     .msgsubject = tcsubject
     .msgnotetext = tcbody
     IF PCOUNT() > 4
          FOR lncountattachments =  ;
              1 TO ALEN(tafiles)
               .attachmentindex =  ;
                .attachmentcount
               .attachmentname = JUSTFNAME(tafiles(lncountattachments))
               .attachmentposition =  ;
                .attachmentindex
               .attachmentpathname =  ;
                tafiles(lncountattachments)
          ENDFOR
     ENDIF
     .send(.F.)
ENDWITH
losession.signoff()
STORE .NULL. TO losession,  ;
      lomessages
RELEASE losession, lomessages
RETURN .T.
ENDFUNC
*
DEFINE CLASS txtnumeric AS  ;
       textbox
alignment = 3
value = 0
width = 100
commas = .T.
decimals = 3
name = "txtnumeric"
currency = .F.
PROTECTED coldinputmask
PROTECTED coriginputmask
PROTECTED cvalue
PROTECTED oldcontrolsource
PROTECTED oldtype
*
PROTECTED FUNCTION setinputmask
     LOCAL lcmask, liintcount,  ;
           lnvalue, lix
     WITH this
          IF  .NOT.  ;
              EMPTY(.coriginputmask)
               .inputmask = .coldinputmask
               RETURN .T.
          ENDIF
          IF  .NOT. (.commas .OR.  ;
              .currency .OR.  ;
              (.decimals > 0))
               RETURN .T.
          ENDIF
          DO CASE
               CASE VARTYPE(.value) =  ;
                    'N'
                    lnvalue = .value
               CASE VARTYPE(.value) =  ;
                    'C'
                    lnvalue = VAL(.value)
               OTHERWISE
                    = MESSAGEBOX( ;
                      "Unhandled Value Type",  ;
                      0,  ;
                      "Error")
                    .enabled = .F.
                    RETURN .F.
          ENDCASE
          lcmask = ""
          liintcount = LEN(ALLTRIM(STR(lnvalue,  ;
                       20)))
          IF INT(lnvalue) = 0
               liintcount = 0
          ENDIF
          FOR lix = 1 TO  ;
              liintcount
               lcmask = '9' +  ;
                        lcmask
               IF MOD(lix, 3) = 0  ;
                  .AND. lix <  ;
                  liintcount  ;
                  .AND. .commas
                    lcmask = ',' +  ;
                             lcmask
               ENDIF
          ENDFOR
          IF .decimals > 0
               lcmask = lcmask +  ;
                        "."
               FOR lix = 1 TO  ;
                   .decimals
                    lcmask = lcmask +  ;
                             "9"
               ENDFOR
               IF LEFT(lcmask, 1) =  ;
                  '.'
                    lcmask = "9" +  ;
                             lcmask
               ENDIF
          ENDIF
          IF .currency
               lcmask = '$' +  ;
                        lcmask
          ENDIF
          this.inputmask = lcmask
     ENDWITH
ENDFUNC
*
PROCEDURE Refresh
     this.setinputmask()
ENDPROC
*
FUNCTION Init
     ASSERT VARTYPE(this.value) ==  ;
            'N' MESSAGE  ;
            "Value must be numeric"
     ASSERT VARTYPE(this.decimals) ==  ;
            'N' MESSAGE  ;
            "Decimals must be numeric"
     ASSERT VARTYPE(this.commas) ==  ;
            'L' MESSAGE  ;
            "Commas must be Logical"
     ASSERT VARTYPE(this.currency) ==  ;
            'L' MESSAGE  ;
            "Currencey must be Logical"
     IF VARTYPE(this.value) +  ;
        VARTYPE(this.decimals) +  ;
        VARTYPE(this.commas) +  ;
        VARTYPE(this.currency) <>  ;
        "NNLL"
          RETURN .F.
     ENDIF
     this.coriginputmask = this.inputmask
     this.coldinputmask = this.inputmask
     this.setinputmask()
ENDFUNC
*
PROCEDURE LostFocus
     LOCAL lcmask, lcolddecimals
     lcolddecimals = SET("DECIMALS")
     SET DECIMALS TO this.decimals
     WITH this
          IF VARTYPE(.oldtype) <>  ;
             'C'
               .oldtype = VARTYPE(.value)
          ENDIF
          .setinputmask()
          DO CASE
               CASE .oldtype =  ;
                    'N' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'C'
                    .value = VAL(.value)
               CASE .oldtype =  ;
                    'N' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'N'
               CASE .oldtype =  ;
                    'C' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'C'
               CASE .oldtype =  ;
                    'C' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'N'
                    .value = STR(.value)
               OTHERWISE
          ENDCASE
          lvalue = .value
          .controlsource = .oldcontrolsource
          .value = lvalue
     ENDWITH
     SET DECIMALS TO lcolddecimals
ENDPROC
*
FUNCTION GotFocus
     WITH this
          .oldcontrolsource = .controlsource
          .controlsource = ""
          .oldtype = VARTYPE(.value)
          DO CASE
               CASE .oldtype =  ;
                    'N'
                    IF INT(.value) <>  ;
                       .value
                         .value =  ;
                          ALLTRIM(STR(.value,  ;
                          20,  ;
                          .decimals))
                    ELSE
                         .value =  ;
                          ALLTRIM(STR(.value,  ;
                          20))
                    ENDIF
               CASE .oldtype =  ;
                    'C'
               OTHERWISE
                    MESSAGEBOX( ;
                              "Unhandled Type",  ;
                              0,  ;
                              "Unhandled Value Type" ;
                              )
                    .enabled = .F.
                    RETURN .F.
          ENDCASE
          .coldinputmask = .inputmask
          .inputmask = ""
          textbox::gotfocus()
          .selstart = 0
          .sellength = LEN(.value)
          NODEFAULT
     ENDWITH
ENDFUNC
*
PROCEDURE Key
     LPARAMETERS nkeycode,  ;
                 nshiftaltctrl
     LOCAL liloc
     WITH this
          DO CASE
               CASE nkeycode =  ;
                    ASC('.')
                    liloc = AT('.',  ;
                            .value)
                    IF (liloc <>  ;
                       0) .AND.   ;
                       .NOT.  ;
                       (BETWEEN(liloc -  ;
                       1,  ;
                       .selstart,  ;
                       .selstart +  ;
                       .sellength -  ;
                       1))
                         NODEFAULT
                    ENDIF
               CASE nkeycode =  ;
                    ASC('-')
                    liloc = AT( ;
                            '-',  ;
                            .value)
                    IF .selstart <>  ;
                       0 .OR.  ;
                       (liloc <>  ;
                       0 .AND.  ;
                       .sellength >  ;
                       0)
                         NODEFAULT
                    ENDIF
               CASE BETWEEN(nkeycode,  ;
                    ASC('0'),  ;
                    ASC('9'))
               CASE BETWEEN(nkeycode,  ;
                    32, 122)
                    NODEFAULT
               OTHERWISE
          ENDCASE
     ENDWITH
ENDPROC
*
ENDDEFINE
*

FUNCTION NombreMes
PARAMETERS nMes
LOCAL NombreMes

DO CASE 
	CASE nMes = 1
		NombreMes="Enero"
	CASE nMes = 2
		NombreMes="Febrero"
	CASE nMes = 3
		NombreMes="Marzo"
	CASE nMes = 4
		NombreMes="Abril"
	CASE nMes = 5
		NombreMes="Mayo"
	CASE nMes = 6
		NombreMes="Junio"
	CASE nMes = 7
		NombreMes="Julio"
	CASE nMes = 8
		NombreMes="Agosto"
	CASE nMes = 9
		NombreMes="Setiembre"
	CASE nMes = 10
		NombreMes="Octubre"
	CASE nMes = 11
		NombreMes="Noviembre"
	CASE nMes = 12
		NombreMes="Diciembre"
	ENDCASE 

RETURN 	NombreMes

ENDFUNC 

PROCEDURE Imprimir
LPARAMETERS Fila,Columna,Valor,Mascara
IF PCOUNT()=3
	Mascara="@X"
ENDIF

@fila,columna Say Valor picture Mascara
ENDPROC 
	
FUNCTION RPVersion
LPARAMETERS lcReportName
LOCAL ldFecha
ldFecha = FDATE(lcReportName)
ldFecha = STR(YEAR(ldFecha),4)+'.'+ STR(month(ldFecha),2)+'.'+ STR(day(ldFecha),2)
RETURN JUSTFNAME(lcReportName)+'.V:'+ldFecha 


FUNCTION Exportar
gcTable=GETFILE('XLS', 'Seleccione el archivo destino:','Browse', 0, 'Browse')
IF !EMPTY(gcTable)
	COPY TO (gcTable) TYPE xl5
ENDIF
ENDFUNC
	   
PROCEDURE  SETNull
LPARAMETERS Valor
	IF EMPTY(Valor)
		valor= null
	ENDIF
ENDPROC


PROCEDURE Recorrer(nColumna,Valor)
EXTERNAL ARRAY Matriz,Vector
LOCAL i as Integer
	FOR i=1 TO ALEN(matriz,1)
		*Usamos el indice de recursividad para guardar el valor
		vector(PROGRAM(-1)) = matriz(i,nColumna)
		IF ALEN(matriz,2)>nColumna
			Recorrer(nColumna+1,matriz(i,nColumna))
		ENDIF
		Valor=""
		*Solo en el ultimo nivel de recursividad recorremos el vector final
		IF PROGRAM(-1)=ALEN(vector)
		FOR x=1 TO ALEN(vector)
			Valor = Valor +  vector(x) + " "
		ENDFOR 	
		? Valor
		ENDIF
		
	ENDFOR

ENDPROC


PROCEDURE CapturarPantalla
DECLARE LONG keybd_event IN "user32" INTEGER bVk,   INTEGER bScan,   LONG dwFlags,  LONG dwExtraInfo
=keybd_event (44, 1, 0, 0)
ENDPROC

PROCEDURE AcercaDe
	
DECLARE LONG ShellAbout IN Shell32 ;
 LONG nHwnd,;
 STRING cTitulo,;
 STRING cCaption,;
 LONG nIcon
nHwnd = 0
cTitulo = "Titulo de la ventana"
cCaption = "Aqui se pone el autor del programa"
nIcon = 0
ShellAbout(@nHwnd,@cTitulo,@cCaption, @nIcon)
ENDPROC

PROCEDURE ImpresionFactura
	LPARAMETERS idformato
	m.codigo= idformato
	=Sql('select descripcionsql,comandoImpresion,Destino from sys_impresion where codigo=?m.codigo', 'csql')
	=Sql("select * from sys_impresiondet where codigo =?m.codigo order by ISNULL(ordenimpresion,0), fila, columna", 'detsql')
	*=sql("select * from sys_impresiondet where codigo =?idformato and tipo= 'P' order by fila, columna", 'psql')
	=Sql("select * from sys_impresiondet where codigo =?m.codigo and tipo= 'D' order by ISNULL(ordenimpresion,0), fila, columna", 'dsql')
	=Sql (csql.descripcionsql, 'cfactura')



IF RECCOUNT('cSQL')=0
	MESSAGEBOX("No se encuentra el formato indicado: " + STR(idformato),16,"Futura Software")
	RETURN
ENDIF
		
*SET STEP ON

	Create Cursor CampoMemo(Contenido m)
	Append Blank In CampoMemo



	Set Device To File (csql.Destino)
	*fpos=6
	*imprimir(0,0,CHR(27)+CHR(48))
	pos=0
	Select detsql
	Scan For tipo = 'C'
		Select cfactura
		If Empty(Nvl(detsql.condicion,'')) Or Evaluate(ALLTRIM(detsql.condicion))
			If (detsql.calculo)
				ejecutar=ALLTRIM(detsql.campo)
				&ejecutar
			Else
				imprimir(NVL(detsql.fila,0),NVL(detsql.columna,0),Evaluate(detsql.campo))
			Endif
		Endif

		Select detsql
	Endscan

	Select cfactura
	lpos=1
	Scan

		Select dsql

*SET STEP ON

		pos=0 &&prueba
		SCAN 	&& Para las lineas simples
			**pos=0 prueba
			
			Select cfactura
			If Empty(Nvl(dsql.condicion,'')) Or Evaluate(dsql.condicion)
				If (dsql.calculo)
					ejecutar=dsql.campo
					&ejecutar
				Else
					If Not Nvl(dsql.Estirar,.F.)
						If Not Empty(Nvl(dsql.LongitudMax,''))
							imprimir(dsql.fila+lpos,NVL(dsql.columna,0),Left(Evaluate(dsql.campo),dsql.LongitudMax))
						Else
							imprimir(dsql.fila+lpos,NVL(dsql.columna,0),Evaluate(dsql.campo))
						Endif

					Else
						m.Contenido = Evaluate(dsql.campo)
						Set Memowidth To Nvl(dsql.LongitudMax,40)
						Replace CampoMemo.Contenido With Alltrim(m.Contenido) In CampoMemo
						imprimir(dsql.fila+lpos,NVL(dsql.columna,0),Mline(CampoMemo.Contenido, 1) )
					ENDIF
					pos=1

				Endif


			Endif

			Select dsql
		Endscan

		SCAN FOR Nvl(dsql.Estirar,.F.) 	&& Para las multilineas

			pos=1
			
			Select cfactura
						m.Contenido = Evaluate(dsql.campo)
						Set Memowidth To Nvl(dsql.LongitudMax,40)
						Replace CampoMemo.Contenido With Alltrim(m.Contenido) In CampoMemo
	*	STORE 0 TO _MLINE             && Reset _MLINE to zero
						For i=2 To Memlines(CampoMemo.Contenido)
							imprimir(dsql.fila+lpos+pos,NVL(dsql.columna,0),Mline(CampoMemo.Contenido, i) )
							pos = pos +1
						ENDFOR
						
			Select dsql
		Endscan


		lpos=lpos + pos
		Select cfactura
	Endscan

	Goto Top In cfactura
	Select detsql
	Scan For tipo = 'P'
		Select cfactura
		If Empty(Nvl(detsql.condicion,'')) Or Evaluate(detsql.condicion)
			If (detsql.calculo)
				ejecutar=detsql.campo
				&ejecutar
			Else
				IF (detsql.fila=0) 
					lfila = PROW()
				ELSE  	
					IF (detsql.fila= -1)
						lfila = PROW()+1
					ELSE
						lfila = detsql.fila
					ENDIF
				endif	
					
				imprimir(lfila,detsql.columna,Evaluate(detsql.campo))
			Endif
		Endif


		Select detsql
	Endscan

	Set Device To Screen
	ejecutar=Nvl(csql.ComandoImpresion,'')
	**SET STEP ON
	
	If !Empty(ejecutar)
		EXECSCRIPT(ejecutar)
	ENDIF
ENDPROC

PROCEDURE PRINTRAW
	LPARAMETERS lcArchivo, lcPrinter
	
	IF PCOUNT()=2
		lcPrinter = "Rawprint\"+ALLTRIM(lcPrinter)
	ELSE
		lcPrinter = "Rawprint\DefaultPrinter"
	ENDIF
	
	LOCAL oRawPrinterCtl, lcDefaultPrinter

	IF TYPE("oApp.oRawPrinter")<> 'O'
		SET CLASSLIB TO ctl32 ADDITIVE 
		oApp.AddObject('oRawPrinter',"ctl32_rawprint")
	ENDIF

	oRawPrinterCtl = oApp.oRawPrinter	

	lcDefaultPrinter = oRawPrinterCtl.oREGISTRY.getvalue(lcPrinter ,"")

	IF EMPTY(lcDefaultPrinter)
		oRawPrinterCtl.ctlgetrawprinter()
		oRawPrinterCtl.oREGISTRY.setvalue(lcPrinter ,oRawPrinterCtl.ctlrawprinter)
	ENDIF

	oRawPrinterCtl.ctlprintfile(lcArchivo)
	 
	oRawPrinterCtl.ctlcloseprinter()

ENDPROC




PROCEDURE RegistrarError(lcmensaje as String)
LOCAL lcAlias
lcAlias=ALIAS()
	
	IF  !USED("_Errores")
		USE datos!Errores ALIAS _Errores IN 0 
	ENDIF
	IF TYPE("oApp")="O"
	 
		INSERT INTO  _Errores(IdEmpresa,Usuario,Detalle) vALUES(oApp.Empresa,oapp.getemployeeid(),lcmensaje)
	ELSE
		INSERT INTO  _Errores(IdEmpresa,Usuario,Detalle) vALUES("","",lcmensaje)
	ENDIF
	
	USE IN _Errores
	IF !EMPTY(lcAlias)
		SELECT (lcAlias)
	ENDIF
		
ENDPROC 
	

PROCEDURE SendMail(desde,A,Asunto,Mensaje,archivoAdjunto)
x=CREATEOBJECT("NET4COM.Network")
IF SQL("Select convert(varchar(200),dbo.LeerConstante(?oApp.Empresa,'SYS_SMTPSERVER')) as SMTP", 'cConfig')>0

p1=AT(',',cConfig.SMTP)
p2=AT(',',cConfig.SMTP,2)

lcServer = ALLTRIM(SUBSTR(cConfig.SMTP,1,p1-1))
lcUser = ALLTRIM(SUBSTR(cConfig.SMTP,p1+1,p2-p1-1))
lcPwd = ALLTRIM(SUBSTR(cConfig.SMTP,p2+1))

	
IF EMPTY(archivoAdjunto)
	x.SendEmail(desde,A,Asunto,Mensaje,lcServer,lcUser,lcPwd)
ELSE
	x.SendEmailWithAttachments(desde,A,Asunto,Mensaje,lcServer,lcUser,lcPwd,archivoAdjunto)	

	*x.SendEmailWithAttachments("vgonzalez@futura.com.py",A,Asunto,Mensaje,"mail.futura.com.py","Soporte","AppFutura2010",archivoAdjunto)
ENDIF

ENDIF

x=null

ENDPROC 

PROCEDURE CalculoInteres

Capital = 10000000
CantidadCuotas = 24
TasaAnual = 24  && Porcentaje
TasaMensual = TasaAnual/12/100


*-- Listado
? "    Nro.Cuota        Cuota             Cuota Capitalizada Interes"
? REPLICATE("-",80)
FOR NroCuota = 1 TO CantidadCuotas
Cuota = Capital * (TasaMensual/(1-(1+TasaMensual)^-CantidadCuotas))
CuotaCapitalizada = Cuota * (1-TasaMensual*(1-(1+TasaMensual)^-(CantidadCuotas-NroCuota+1))/TasaMensual)
Interes = TasaMensual * Cuota * (1-(1+TasaMensual)^-(CantidadCuotas-NroCuota+1))/TasaMensual
? NroCuota, ROUND(Cuota,2), ROUND(CuotaCapitalizada,2), ROUND(Interes,2)
ENDFOR

ENDPROC 



PROCEDURE ImprimirFactura
LPARAMETERS lnIdFactura

*!*	=THIS.RUNSQL("Select convert(int,dbo.LeerConstante(?oApp.Empresa,'VT_IDFORMATOTPV')) as IdFormato", 'cConfig')
*!*	IF RECCOUNT('cConfig')=0
*!*	 MESSAGEBOX("No se encuentra la constante VT_IDFORMATOTPV")
*!*		RETURN
*!*	ENDIF

*!*	M.IDFACTURA = lnIdFactura
*!*	m.IdFormato=cConfig.IdFormato
*!*	=ImpresionFactura(cConfig.IdFormato)


****

 M.IDFACTURA = lnIdFactura
*idFormato = 5
*IF CMONEDA.DEC=0
	M.DEC = 0
*ELSE
*	M.DEC = 2
*ENDIF

*this.impresion1.imprimir()

*Verificamos la forma de imprimir la factura
*Si tiene nombre formato, se usa el report
*Si no tiene, se usa ASCII
=SQL("Select convert(CHAR(50),dbo.LeerConstante(?oApp.Empresa,'VT_NOMBREFORMATO_FACT')) as NombreFormato", 'cTipo')
IF RECCOUNT('cTipo')=0
 MESSAGEBOX("No se encuentra la constante VT_NOMBREFORMATO_FACT")
	RETURN
ENDIF

IF !EMPTY(NVL(cTipo.NombreFormato,''))
	cTipoImpresion='R'		&&Report
ELSE
	cTipoImpresion='A'		&&Ascii
ENDIF


*VG 05/12/2010
*No se aplica nota de credito
*F xVenta.Tipo='D'

	IF cTipoImpresion='A'
		=SQL("Select convert(int,dbo.LeerConstante(?oApp.Empresa,'VT_IDFORMATOTPV')) as IdFormato", 'cConfig')
		IF RECCOUNT('cConfig')=0
		 MESSAGEBOX("No se encuentra la constante VT_IDFORMATOTPV")
			RETURN
		ENDIF
			=ImpresionFactura(cConfig.Idformato)
	ELSE
*		REPORT FORM (cTipo.NombreFormato)  TO PRINTER NOPAGEEJECT 
*		REPORT FORM (cTipo.NombreFormato)  TO PRINTER NOPAGEEJECT 
		REPORT FORM (cTipo.NombreFormato)  PREVIEW
	ENDIF
							
*ELSE
*	DO ('notacredito'+oApp.Empresa)
	*REPORT FORM vt_NotaCredito ASCII TO FILE c:\factura.txt
*ENDIF
ENDPROC 


PROCEDURE PrintShell(FileName, FilePath)

hndwin= 0 
cAction= "print" 
cFileName= FileName  
cParams= "" 
cDir= FilePath
nShowWin= 1

ShellExecute(hndWin,cAction,cFileName,cParams,cDir,nShowWin)

ENDPROC 	

FUNCTION TransNroFactura
LPARAMETERS _NroFact
	RETURN TRANSFORM(m._NroFact,"@L 999-999-9999999")
ENDFUNC 


********************VG
****** 08/05/2014
****** Para importar desde Excel


********************************
*!* Simple Sample Usage
********************************
*!*	DIMENSION aWrkSht(1), aCols(1)
*!*	m.lcXlsFile = GETFILE("Excel:XLS,XLSX,XLSB,XLSM")
*!*	IF FILE(m.lcXlsFile)
*!*		CLEAR
*!*		?AWorkSheets(@aWrkSht,m.lcXlsFile,.T.)
*!*		?AWorkSheetColumns(@aCols,m.lcXlsFile,"Sheet1")
*!*		AppendFromExcel(m.lcXlsFile, "Sheet1", "MyTable", "column1,column2,column3", "Recnum Is Not Null", "field1,field2,field3", "field1 > 14000")
*!*		SELECT MyTable
*!*		GO TOP IN "MyTable"
*!*		BROWSE LAST NOWAIT
*!*	ENDIF
*!*	CopyToExcel("C:\Test.xlsx", "Sheet1", "MyTable") && try xls, xlsb, and xlsm as well

**********************************
FUNCTION AppendFromExcel(tcXLSFile, tcSheet, tvWorkarea, tcExcelFieldList, tcExcelWhereExpr, tcTableFieldList, tcTableForExpr, tlNoHeaderRow)
	**********************************
	* PARAMETER Information
	* tcXLSFile := a string specifying an excel file (*.xls, *.xlsx, *.xlsm, *.xlsb) on disk
	* tcSheet := a string specifying the name of a worksheet within the excel workbook (can also be a range Sheet1$A1:C20 for instance)
	* tvWorkarea [optional] := the Alias, Work Area, or File Name of the table you want the worksheet result set appended to (default is currently selected Alias)
	* tcExcelFieldList [optional] := a comma delimited list of columns you want from the worksheet (default is '*' - all columns)
	* tcExcelWhereExpr [optional] := a valid SQL Where clause to be used when querying the worksheet (default is '1=1')
	* tcTableFieldList [optional] :=  a comma delimited list of fields you want the worksheet result set inserted into (default is '*' - all fields)
	* tcTableForExpr [optional] := a valid VFP Where clause to be used when querying the worksheet result set (cursor) (default is '.T.')
	* tlNoHeaderRow [optional] := pass .T. if the worksheet does not contain a header row, .F. is the default which specifies that a header row does exist
	*
	* RETURN Information
	* returns numeric, the number of records inserted into tvWorkArea
	*
	* Provider Information
	* the default provider being used in the SQLStringConnect function can be downloaded and installed from:
	* http://www.microsoft.com/downloads/details.aspx?FamilyID=7554F536-8C28-4598-9B72-EF94E038C891&displaylang=en
	**********************************
	LOCAL lnSelect, laErr[1], laTableFields[1], laExcelFields[1], lnFieldCounter, ;
		lcSQLAlias, lnResult, lcInsertValues, lcFieldList, lcNvlFieldList, ;
		lcFieldType, lcExcelFieldType, lcNvlFieldName, lcTempAlias, loExc, lnReturn, ;
		lcHeaderRow, llOpenedtvWorkArea
	m.lnSelect = SELECT(0)
	m.lnReturn = 0
	IF NOT FILE(m.tcXLSFile)
		ERROR 1, m.tcXLSFile
	ENDIF

	IF !USED(m.tvWorkarea) AND TYPE("m.tvWorkArea") = "C" AND FILE(DEFAULTEXT(m.tvWorkarea,"DBF"))
		SELECT 0
		USE (DEFAULTEXT(m.tvWorkarea,"DBF")) SHARED AGAIN
		m.tvWorkarea = ALIAS()
		m.llOpenedtvWorkArea = .T.
	ELSE
		IF !USED(m.tvWorkarea)
			m.tvWorkarea = ALIAS()
		ENDIF
	ENDIF
	IF TYPE("m.tvWorkArea") = "N"
		m.tvWorkArea = ALIAS(m.tvWorkArea)
	ENDIF
	
	m.tcSheet = ALLTRIM(EVL(m.tcSheet,"Sheet1$"))
	IF AT("$",m.tcSheet) = 0
		m.tcSheet = m.tcSheet + "$"
	ENDIF
	m.tcExcelFieldList = EVL(m.tcExcelFieldList,"*")
	m.tcExcelWhereExpr = EVL(m.tcExcelWhereExpr,"1=1")
	m.tcTableFieldList = EVL(m.tcTableFieldList,"*")
	m.tcTableForExpr = EVL(m.tcTableForExpr,".T.")
	m.lcSQLAlias = SYS(2015)
	m.lcTempAlias = SYS(2015)
	m.lnSQL = -1
	m.lcHeaderRow = IIF(EMPTY(m.tlNoHeaderRow), "Yes", "No")
	TRY
		SELECT (m.tvWorkarea)
		
		****m.lnSQL = SQLSTRINGCONNECT([Provider=Microsoft.ACE.OLEDB.12.0;Data Source="] + m.tcXLSFile + [";Extended Properties="Excel 12.0 Xml;HDR=] + m.lcHeaderRow + [;";])

		*!* Alternate using DSN that comes with Office install (MSDASQL = OLEDB wrapper for ODBC)
		*!*			m.lnSQL = SQLSTRINGCONNECT("Provider=MSDASQL.1;" ;
		*!*				+"Persist Security Info=False;" ;
		*!*				+"DSN=Excel Files;" ;
		*!*				+"DBQ="+FULLPATH(m.tcXLSFile)+";" ;
		*!*				+"DriverId=790;" ;
		*!*				+"MaxBufferSize=2048;" ;
		*!*				+"PageTimeout=5;")
		
		*!* Try a few other drivers that may be on the user's machine
		IF m.lnSQL < 0
			m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};" ;
				+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
			IF m.lnSQL < 0 AND UPPER(ALLTRIM(JUSTEXT(m.tcXLSFile))) == "XLS" && can we try using the older driver?
				IF m.lnSQL < 0
					m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls)};" ;
						+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
				ENDIF
			ENDIF
			IF m.lnSQL < 0
				AERROR(m.laErr)
				ERROR m.laErr[2]
			ENDIF
		ENDIF

		m.lnResult = SQLEXEC(m.lnSQL,[SELECT ] + m.tcExcelFieldList + [ FROM "] + m.tcSheet + [" Where ] + m.tcExcelWhereExpr, m.lcSQLAlias)
		IF m.lnResult < 0
			AERROR(m.laErr)
			ERROR m.laErr[2]
		ENDIF

		IF USED(m.lcSQLAlias)
			m.lcFieldList = ""
			m.lcNvlFieldList = ""
			m.lnTotalExcelFields = AFIELDS(m.laExcelFields, m.lcSQLAlias)
			SELECT &tcTableFieldList FROM (m.tvWorkarea) WHERE .F. INTO CURSOR (m.lcTempAlias)
			FOR m.lnFieldCounter = 1 TO MIN(AFIELDS(m.laTableFields, m.lcTempAlias), m.lnTotalExcelFields)
				m.lcFieldList = m.lcFieldList + IIF(!EMPTY(m.lcFieldList),",","")+m.laTableFields[m.lnFieldCounter,1]
				m.lcFieldType =  CHRTRAN(m.laTableFields[m.lnFieldCounter,2],"NIFYD","BBBBT")
				m.lcExcelFieldType = CHRTRAN(m.laExcelFields[m.lnFieldCounter,2],"CVNIFYD","MMBBBBT")
				m.lcNvlFieldName = m.laExcelFields[m.lnFieldCounter,1]
				IF !m.laTableFields[m.lnFieldCounter,5]
					m.lcNvlFieldName = [NVL(]+m.lcNvlFieldName+[,]+;
						ICASE(m.lcExcelFieldType="B", "0", ;
						m.lcExcelFieldType="M", "''", ;
						m.lcExcelFieldType="T", "{//}", ;
						m.lcExcelFieldType="L", ".F.", ;
						"''")+[)]
				ENDIF
				IF INLIST(m.lcFieldType, "C", "V")
					m.lcNvlFieldName = [CAST(]+m.lcNvlFieldName+[ AS ]+m.lcFieldType+[(] + TRANSFORM(m.laTableFields[m.lnFieldCounter,3]) + [))]
				ELSE
					m.lcNvlFieldName = [CAST(]+m.lcNvlFieldName+[ AS ]+m.lcFieldType+[)]
				ENDIF
				m.lcNvlFieldList = m.lcNvlFieldList + IIF(!EMPTY(m.lcNvlFieldList),",","") + m.lcNvlFieldName
			ENDFOR
			INSERT INTO (m.tvWorkarea) (&lcFieldList) SELECT &lcNvlFieldList FROM (m.lcSQLAlias) WHERE &tcTableForExpr
			m.lnReturn = _TALLY
		ENDIF

	CATCH TO m.loExc
		*!*			MESSAGEBOX(m.loExc.MESSAGE + " : " + TRANSFORM(m.loExc.LINENO))
	FINALLY
		IF m.llOpenedtvWorkArea
			USE IN SELECT(m.tvWorkArea)
		ENDIF
		IF m.lnSQL > 0
			SQLDISCONNECT(m.lnSQL)
		ENDIF
		USE IN SELECT(m.lcTempAlias)
		USE IN SELECT(m.lcSQLAlias)
		SELECT (m.lnSelect)
	ENDTRY
	RETURN m.lnReturn
ENDFUNC

**********************************
FUNCTION AWorkSheets(taArray, tcXLSFile, tlAllTables)
	**********************************
	* PARAMETER Information
	* taArray := an array sent in by reference to fill with Worksheet/Table information
	* tcXLSFile := a string specifying an excel file (*.xls, *.xlsx, *.xlsm, *.xlsb) on disk
	* tlAllTables := if .T., array will contain information regarding all tables in workbook; .F. returns only worksheets
	*
	* RETURN Information
	* returns numeric, the number of tables found in the workbook
	**********************************

	LOCAL lnSQL, laErr[1], lcSQLAlias, lnResult, lnReturn, loExc
	m.lnReturn = 0
	m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};" ;
		+"DBQ="+FULLPATH(m.tcXLSFile)+";")
		
	*!* Alternate using DSN that comes with Office install (MSDASQL = OLEDB wrapper for ODBC)
	*!*		m.lnSQL = SQLSTRINGCONNECT("Provider=MSDASQL.1;" ;
	*!*			+"Persist Security Info=False;" ;
	*!*			+"DSN=Excel Files;" ;
	*!*			+"DBQ="+FULLPATH(m.tcXLSFile)+";" ;
	*!*			+"DriverId=790;" ;
	*!*			+"MaxBufferSize=2048;" ;
	*!*			+"PageTimeout=5;")

		*!* Try a few other drivers that may be on the user's machine
	IF m.lnSQL < 0
		IF UPPER(ALLTRIM(JUSTEXT(m.tcXLSFile))) == "XLS" && can we try using the older driver?
			m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};" ;
				+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
			IF m.lnSQL < 0
				m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls)};" ;
					+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
			ENDIF
		ELSE
			m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};" ;
				+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
		ENDIF
		IF m.lnSQL < 0
			AERROR(m.laErr)
			ERROR m.laErr[2]
		ENDIF
	ENDIF

	m.lcSQLAlias = SYS(2015)
	m.lnResult = SQLTABLES(m.lnSQL,"VIEW,TABLE,SYSTEM TABLE",m.lcSQLAlias)

	IF m.lnSQL > 0
		SQLDISCONNECT(m.lnSQL)
	ENDIF

	IF m.lnResult < 0
		AERROR(m.laErr)
		ERROR m.laErr[2]
	ENDIF

	IF USED(m.lcSQLAlias)
		TRY
			IF tlAllTables
				SELECT CAST(ALLTRIM(table_name) AS V(100)), ;
					CAST(ALLTRIM(table_type) AS V(12)) ;
					FROM (m.lcSQLAlias) ;
					INTO ARRAY taArray
			ELSE
				SELECT CAST(ALLTRIM(table_name) AS V(100)), ;
					CAST(ALLTRIM(table_type) AS V(12)) ;
					FROM (m.lcSQLAlias) ;
					WHERE table_type = "SYSTEM TABLE" ;
					INTO ARRAY taArray
			ENDIF
			m.lnReturn = _TALLY
		CATCH TO m.loExc
			THROW
		FINALLY
			*!*				USE IN SELECT(m.lcSQLAlias)
		ENDTRY
	ENDIF

	RETURN m.lnReturn
ENDFUNC

**********************************
FUNCTION AWorkSheetColumns(taArray, tcXLSFile, tcSheet)
	**********************************
	* PARAMETER Information
	* taArray := an array sent in by reference to fill with the specified worksheet's column information
	* tcXLSFile := a string specifying an excel file (*.xls, *.xlsx, *.xlsm, *.xlsb) on disk
	* tcSheet := a string specifying the worksheet or table to use when retrieving column information
	*
	* RETURN Information
	* returns numeric, the number of columns found in the worksheet/table
	**********************************
	LOCAL lnSQL, laErr[1], lnResult, lnReturn, lcSQLAlias, loExc
	m.lnReturn = 0
	m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};" ;
		+"DBQ="+FULLPATH(m.tcXLSFile)+";")
	*!* Alternate using DSN that comes with Office install (MSDASQL = OLEDB wrapper for ODBC)
	*!*		m.lnSQL = SQLSTRINGCONNECT("Provider=MSDASQL.1;" ;
	*!*			+"Persist Security Info=False;" ;
	*!*			+"DSN=Excel Files;" ;
	*!*			+"DBQ="+FULLPATH(m.tcXLSFile)+";" ;
	*!*			+"DriverId=790;" ;
	*!*			+"MaxBufferSize=2048;" ;
	*!*			+"PageTimeout=5;")

	*!* Try a few other drivers that may be on the user's machine
	IF m.lnSQL < 0
		IF UPPER(ALLTRIM(JUSTEXT(m.tcXLSFile))) == "XLS" && can we try using the older driver?
			m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};" ;
				+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
			IF m.lnSQL < 0
				m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls)};" ;
					+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
			ENDIF
		ELSE
			m.lnSQL = SQLSTRINGCONNECT("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};" ;
				+ "DBQ="+FULLPATH(m.tcXLSFile)+";")
		ENDIF
		IF m.lnSQL < 0
			AERROR(m.laErr)
			ERROR m.laErr[2]
		ENDIF
	ENDIF

	m.lcSQLAlias = SYS(2015)
	m.lnResult = SQLEXEC(m.lnSQL,[SELECT * FROM "] + m.tcSheet + [$" Where 1=0], m.lcSQLAlias)

	IF m.lnSQL > 0
		SQLDISCONNECT(m.lnSQL)
	ENDIF

	IF m.lnResult < 0
		AERROR(m.laErr)
		ERROR m.laErr[2]
	ENDIF

	IF USED(m.lcSQLAlias)
		TRY
			m.lnReturn = AFIELDS(m.taArray, m.lcSQLAlias)
		CATCH TO m.loExc
			THROW
		FINALLY
			USE IN SELECT(m.lcSQLAlias)
		ENDTRY
	ENDIF

	RETURN m.lnReturn
ENDFUNC

***********************************
FUNCTION CopyToExcel(tcXLSFile, tcSheet, tvWorkArea, tcExcelFieldList, tcTableFieldList, tcTableForExpr)
	***********************************
	* PARAMETER Information
	* tcXLSFile := a string specifying an excel file (*.xls, *.xlsx, *.xlsm, *.xlsb) on disk
	* tcSheet := a string specifying the name of the worksheet to create within the excel workbook
	* tvWorkarea [optional] := the Alias, Work Area, or File Name of the table you want to be copied to the worksheet (default is currently selected Alias)
	* tcExcelFieldList [optional] := a comma delimited list of columns you want to create in the worksheet (default is '*' - columns will match table field list)
	* tcTableFieldList [optional] :=  a comma delimited list of fields you want this function to copy from tvWorkArea
	* tcTableForExpr [optional] := a valid VFP Where/For clause to be used when querying tvWorkArea for data to be copied to the worksheet
	*
	* RETURN Information
	* returns numeric, the number of records inserted into the worksheet
	*
	* Provider Information
	* the default provider being used in the SQLStringConnect function can be downloaded and installed from:
	* http://www.microsoft.com/downloads/details.aspx?FamilyID=7554F536-8C28-4598-9B72-EF94E038C891&displaylang=en
	**********************************
	#DEFINE adOpenStatic 3
	#DEFINE adOpenKeyset 1
	#DEFINE adLockOptimistic 3
	#DEFINE adUseClient 3
	#DEFINE adUseServer 2
	#DEFINE adCmdText 0x0001
	LOCAL loConnection as ADODB.Connection, lcCreateTableCommand, llOpenedtvWorkArea, loExc as Exception, ;
		lnReturn, lnResult, lnFieldCounter, lnSQL, loCursorAdapter as CursorAdapter, ;
		lcFieldName, lcFieldType, lcSelectFields, lcUpdateNameListFields, lcUpdatableFieldList, ;
		loRecordSet as ADODB.Recordset, lcConversionFunc, lcVFPFieldName, laTableFields[1], laErr[1]
	
	m.lnSelect = SELECT(0)
	m.lnReturn = 0
	
	m.llOpenedtvWorkArea = .F.
	IF !USED(m.tvWorkarea) AND TYPE("m.tvWorkArea") = "C" AND FILE(DEFAULTEXT(m.tvWorkarea,"DBF"))
		SELECT 0
		USE (DEFAULTEXT(m.tvWorkarea,"DBF")) SHARED AGAIN
		m.tvWorkarea = ALIAS()
		m.llOpenedtvWorkArea = .T.
	ELSE
		IF !USED(m.tvWorkarea)
			m.tvWorkarea = ALIAS()
		ENDIF
	ENDIF
	IF TYPE("m.tvWorkArea") = "N"
		m.tvWorkArea = ALIAS(m.tvWorkArea)
	ENDIF

	m.tcSheet = ALLTRIM(EVL(m.tcSheet,"Sheet1$"))
	IF AT("$",m.tcSheet) = 0
		m.tcSheet = m.tcSheet + "$"
	ENDIF
	m.tcExcelFieldList = EVL(m.tcExcelFieldList,"")
	m.tcTableFieldList = EVL(m.tcTableFieldList,"*")
	m.tcTableForExpr = EVL(m.tcTableForExpr,".T.")
	m.lnSQL = 0
	m.lcTempAlias = SYS(2015)
	
	TRY
		CreateExcelTemplate(m.tcXLSFile)
		IF !FILE(m.tcXLSFile)
			m.lnReturn
		ENDIF
		m.loConnection = CreateObject ( "ADODB.Connection")
		*!* This is the only provider/driver that appears to work without showing Select Data Source dialog
		*!* or throwing a weird error about the excel Database being readonly.
		m.loConnection.ConnectionString = [Provider=Microsoft.ACE.OLEDB.12.0;Data Source="] + m.tcXLSFile + [";Extended Properties="Excel 12.0 Xml;HDR=Yes;";]
		m.loConnection.Open()
		m.loConnection.Execute("DROP TABLE [Sheet1$]")
		SELECT &tcTableFieldList FROM (m.tvWorkarea) WHERE &tcTableForExpr INTO CURSOR (m.lcTempAlias) NOFILTER
		GO TOP IN (m.lcTempAlias)
		m.lnReturn = RECCOUNT(m.lcTempAlias)
		m.lcCreateTableCommand = ""
		m.lcSelectFields = ""
		m.lcUpdateNameListFields = ""
		m.lcUpdatableFieldList = ""
		m.lcConversionFunc = ""
		FOR m.lnFieldCounter = 1 TO AFIELDS(m.laTableFields, m.lcTempAlias)
			m.lcVFPFieldName = m.laTableFields(m.lnFieldCounter, 1)
			m.lcFieldName = ALLTRIM(GETWORDNUM(m.tcExcelFieldList, m.lnFieldCounter, ","))
			IF EMPTY(m.lcFieldName)
				m.lcFieldName = m.laTableFields(m.lnFieldCounter, 1)
			ENDIF
			m.lcSelectFields = m.lcSelectFields + "[" + m.lcFieldName + "] " + " AS " + m.lcVFPFieldName
			m.lcUpdateNameListFields = m.lcUpdateNameListFields + m.lcVFPFieldName + " [" + m.tcSheet + "].[" + m.lcFieldName + "]"
			m.lcUpdatableFieldList = m.lcUpdatableFieldList + m.lcVFPFieldName
			m.lcCreateTableCommand = m.lcCreateTableCommand + "[" + m.lcFieldName + "] "
			m.lcFieldType = m.laTableFields(m.lnFieldCounter, 2)
			m.lcCreateTableCommand = m.lcCreateTableCommand + ;
				ICASE(m.lcFieldType = 'C', 'Char(' + TRANSFORM(m.laTableFields(m.lnFieldCounter, 3)) + ')', ;
				 m.lcFieldType = 'Y', 'Currency', ;
				 m.lcFieldType = 'D', 'Date', ;
				 m.lcFieldType = 'T', 'DateTime', ;
				 m.lcFieldType = 'B', 'Double', ;
				 m.lcFieldType = 'F', 'Double', ;
				 m.lcFieldType = 'G', 'Binary', ;
				 m.lcFieldType = 'I', 'Integer', ;
				 m.lcFieldType = 'L', 'Logical', ;
				 m.lcFieldType = 'M', 'Text', ;
				 m.lcFieldType = 'N', 'Numeric(' + TRANSFORM(m.laTableFields(m.lnFieldCounter, 3)) + ',' + TRANSFORM(m.laTableFields(m.lnFieldCounter, 4)) + ')', ;
				 m.lcFieldType = 'Q', 'Binary', ;
				 m.lcFieldType = 'V', 'VarChar(' + TRANSFORM(m.laTableFields(m.lnFieldCounter, 3)) + ')', ;
				 m.lcFieldType = 'W', 'Blob', ;
				 'Char(' + TRANSFORM(m.laTableFields(m.lnFieldCounter, 3)) + ')')
			IF INLIST(m.lcFieldType,"T","D")
				m.lcConversionFunc = IIF(!EMPTY(m.lcConversionFunc), ", ", "") + m.lcConversionFunc + m.lcVFPFieldName + " EmptyFieldToNull"
			ENDIF
			IF m.lnFieldCounter != ALEN(m.laTableFields,1)
				m.lcCreateTableCommand = m.lcCreateTableCommand + ','
				m.lcSelectFields = m.lcSelectFields + ','
				m.lcUpdateNameListFields = m.lcUpdateNameListFields + ','
				m.lcUpdatableFieldList = m.lcUpdatableFieldList + ','
			ENDIF
		ENDFOR
		IF !EMPTY(m.lcCreateTableCommand)
			IF m.tcSheet != [Sheet1$]
				m.tcSheet = STRTRAN(m.tcSheet,"$","")
			ENDIF
			m.lcCreateTableCommand = "CREATE TABLE [" + m.tcSheet + "](" + m.lcCreateTableCommand + ")"
			m.loConnection.Errors.Clear()
			m.loConnection.Execute(m.lcCreateTableCommand)
			IF m.loConnection.Errors.Count>0
				ERROR m.loConnection.Errors(0).Description
			ENDIF
			m.loRecordSet = CreateObject("ADODB.Recordset")
			With m.loRecordSet
			    .ActiveConnection = m.loConnection
			    .CursorLocation = adUseClient
			    .CursorType = adOpenStatic
			    .LockType = adLockOptimistic
			ENDWITH
			m.loCursorAdapter = CREATEOBJECT("CursorAdapter")
			m.loCursorAdapter.Alias = SYS(2015)
			m.loCursorAdapter.DataSourceType = "ADO"
			m.loCursorAdapter.DataSource = m.loRecordSet
			m.loCursorAdapter.SelectCmd = "Select " + m.lcSelectFields + " From [" + m.tcSheet + "]"
			IF m.loCursorAdapter.CursorFill(.F.,.T.)
				m.loCursorAdapter.Tables = "[" + m.tcSheet + "]"
				m.loCursorAdapter.BufferModeOverride = 3 && faster than 5 when dealing with larger record sets
				m.loCursorAdapter.UpdateNameList = m.lcUpdateNameListFields
				m.loCursorAdapter.UpdatableFieldList = m.lcUpdatableFieldList
				IF !EMPTY(m.lcConversionFunc)
					m.loCursorAdapter.ConversionFunc = m.lcConversionFunc
				ENDIF
				INSERT INTO (m.loCursorAdapter.Alias) SELECT * FROM (m.lcTempAlias)
				m.lnReturn = TABLEUPDATE(.T.,.T.,m.loCursorAdapter.Alias)
			ELSE
				AERROR(m.laErr)
				ERROR m.laErr(2)
			ENDIF
		ENDIF
		m.loConnection.Close()
	CATCH TO m.loExc
		m.lnReturn = 0
	FINALLY
		m.loCursorAdapter = Null
		m.loConnection = Null
		RELEASE loCursorAdapter, loConnection
		IF m.llOpenedtvWorkArea
			USE IN SELECT(m.tvWorkarea)
		ENDIF
		USE IN SELECT(m.lcTempAlias)
		SELECT (m.lnSelect)
	ENDTRY
	RETURN m.lnReturn
ENDFUNC

*******************
FUNCTION EmptyFieldToNull(tdFieldValue)
*******************
	RETURN EVL(m.tdFieldValue,NULL)
ENDFUNC

*******************
FUNCTION CreateExcelTemplate(tcExcelFile)
*******************
	LOCAL lcExcelFileExtension, lcFileBinary
	m.llReturn = .F.
	IF FILE(m.tcExcelFile)
		m.llReturn = .T.
	ELSE
		m.lcExcelFileExtension = UPPER(JUSTEXT(m.tcExcelFile))
		DO case
		CASE m.lcExcelFileExtension = "XLSX"
			m.lcFileBinary = 0h504B030414000600080000002100CC7EE6A14E010000080400001300DF015B436F6E74656E745F54797065735D2E786D6C20A2DB0128A00002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000AC93CB4EC3301045F748FC83E52D8ADDB2400835ED82C712BA281F60EC4963D52F79DCD2FE3D938422814A455536B1226BEE9D39733D996DBD631BC86863A8F9588C3883A0A3B16159F3D7C55375CB1916158C723140CD77807C36BDBC982C7609905175C09AB7A5A43B2951B7E0158A9820D04D13B357857EF35226A5576A09F27A34BA913A8602A154A5D3E0D3C90B3590AD013657B93C2B4F3E72EB64213518BE63417A9CDD0F859D77CD554ACE6A55A873B909E6876B159BC66A3051AF3D79895EECAA5391BF1A62D939C0B3AD306550065B80E29D1844F7CE0FD0A8B52BEC714B0406E8191C9E36DA274C
			m.lcFileBinary = m.lcFileBinary + 0h4195FDF8D8DA84471C8EB33BCEE43DE6D55B8CABFFA6D2D1115ED9B0EFFB5008687BF31C134ADAF5D90D4087DC80A91249422E16BE981DF2A60076B3F76B44D91FE7A7F07B34BEF4FFC440C70CA743D867A5AB3E30B9ECDFF1F4030000FFFF0300504B030414000600080000002100B5553023F50000004C0200000B00CE015F72656C732F2E72656C7320A2CA0128A0000200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008C92CF4EC3300CC6EF48BC43E4FBEA6E4808A1A5BB4C48BB21541EC024EE1FB58DA32440F7F68403824A63DBD1F6E7CF3F5BDEEEE669541F1C622F4EC3BA2841B133627BD76A78AD9F560FA06222676914C71A8E1C6157DDDE6C5F78A4949B62D7FBA8B28B8B1ABA94FC2362341D4F140BF1EC72A5913051CA6168D19319A865DC94E53D86BF1E502D3CD5C16A08077B07AA3EFA3CF9B2B7344D6F782FE67D62974E8C409E
			m.lcFileBinary = m.lcFileBinary + 0h133BCB76E543660BA9CFDBA89A42CB498315F39CD311C9FB2263039E26DA5C4FF4FFB63871224B89D048E0F33CDF8A7340EBEB812E9F68A9F8BDCE3CE2A784E14D64F861C1C50F545F000000FFFF0300504B0304140006000800000021008D87DA70E00000002D0200001A000801786C2F5F72656C732F776F726B626F6F6B2E786D6C2E72656C7320A2040128A00001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000AC91CB6AC3301045F785FE83987D3D760AA594C8D99442B6C5FD00218F1FC496846692D67F5FE182DD404836D908AE06DD73246D773FE3A04E14B9F74E4391E5A0C8595FF7AED5F0557D3CBD826231AE368377A46122865DF9F8B0FDA4C1483AC45D1F58A516C71A3A91F086C8B6
			m.lcFileBinary = m.lcFileBinary + 0hA3D170E603B934697C1C8DA4185B0CC61E4C4BB8C9F3178CFF3BA03CEB54FB5A43DCD7CFA0AA2924F2ED6EDF34BDA5776F8F2339B98040966948175095892D8986BF9C2547C0CBF8CD3DF1929E8556FA1C715E8B6B0EC53D1DBE7D3C704724ABC7B2C5384F16193CFBE4F2170000FFFF0300504B030414000600080000002100A460FAFF52010000270200000F000000786C2F776F726B626F6F6B2E786D6C8C915D6FC2201486EF97EC3F10EE272DA9CE185B93655BE6CD6232A7D7AC9C5A228506A8D57FBF534CE776B72BCE073CE7BC2FCBD5B9D1E404CE2B6B729A4E124AC094562A73C8E9E7F6F5614E890FC248A1AD819C5EC0D355717FB7ECAD3B7E
			m.lcFileBinary = m.lcFileBinary + 0h597B2408303EA77508ED82315FD6D0083FB12D18EC54D6352260EA0ECCB70E84F435406834E34932638D50865E090BF71F86AD2A55C2B32DBB064CB8421C6811707D5FABD6D36259290DBBAB2222DAF65D34B8F75953A2850F2F52059039CD30B53DFC29B8AE7DEA941EBAD3644A59F12372E3085203B88D5327515ED0294A2454A2D3618B82C77958E719E7B3E1ED60CE4E41EF6F982125E7BD32D2F639E5199A7D19B31427923EB6F64A861A51D9FC567B0375A8434EE7499A0C74F60B1F2DC531F12426EAFD186CC625636D8D9230760B85815BCB3412C667A5D0250A1C8E78914F1F7954CFC63F2EBE010000FFFF0300504B0304140006000800000021
			m.lcFileBinary = m.lcFileBinary + 0h00A083C4AEA3010000640300000D000000786C2F7374796C65732E786D6CA453C16ADC3010BD17F20F42F746BB0B0D6DB19D436121909440B6D0AB6CC95EC16864A4F1B2EED767643BDEDD530EBD584F4F336F9E66E4E2F1EC419C6C4C2E6029B7F71B292C36C138EC4AF9E7B0FFFA5D8A441A8D8680B694A34DF2B1BAFB52241AC1BE1DAD25C112984A7924EA7F2A959AA3F53ADD87DE229FB4217A4DBC8D9D4A7DB4DAA49CE441ED369B07E5B54359156D404AA2090312BB5888AA48FFC44903335BA9AAA20910A2209667231383DADB39E29706574797C35AED1D8C33BDCBC4E46889F30E43CCA4CA259725719203580DECB20126AAA2D74436E29E3762
			m.lcFileBinary = m.lcFileBinary + 0hC187B1E7F2C8DD9865A6B84FA2BBA8C7EDEEDB55829A0A56451DA2E1EE5F5F7DA6AA026C4B6C34BAEE98570A3D7FEB40143C03E37417500343F591B100BE4E6301DEF284FEB637DAE756E0E0F79E9E4C2979D6B9091F902FB2C0596FDE64FD6BB559FBBF65C5B9BDD567C52BDB37A6D7F222CFBB94BFF39302B94A887A70400E6F05A7EBB3A6395F5AB0C913205DF3CBCDCD59AB70278C6DF50074580F4B79C12FD6B8C1FF58A35EDD29D02451CA0B7ECE93DA3E4C534EEBEF51BD030000FFFF0300504B030414000600080000002100E9A625B882060000531B000013000000786C2F7468656D652F7468656D65312E786D6CEC594F6FDB3614BF0FD87720
			m.lcFileBinary = m.lcFileBinary + 0h746F6D27B61B07758AD8B19BAD4D1BC46E871E6999965853A240D2497D1BDAE38001C3BA619701BBED306C2BD002BB749F265B87AD03FA15F6484AB218CB4BD2061BD6D58744227F7CFFDFE32375F5DA8388A1432224E571DBAB5DAE7A88C43E1FD338687B7786FD4B1B1E920AC763CC784CDADE9C48EFDAD6FBEF5DC59B2A241141B03E969BB8ED854A259B958AF46118CBCB3C2131CC4DB888B082571154C6021F01DD8855D6AAD56625C234F6508C23207B7B32A13E41434DD2DBCA88F718BCC64AEA019F8981264D9C15063B9ED63442CE659709748859DB033E637E34240F948718960A26DA5ED5FCBCCAD6D50ADE4C1731B5626D615DDFFCD275E982
			m.lcFileBinary = m.lcFileBinary + 0hF174CDF014C128675AEBD75B577672FA06C0D432AED7EB757BB59C9E0160DF074DAD2C459AF5FE46AD93D12C80ECE332ED6EB551ADBBF802FDF525995B9D4EA7D14A65B1440DC83ED697F01BD5667D7BCDC11B90C53796F0F5CE76B7DB74F00664F1CD257CFF4AAB5977F10614321A4F97D0DAA1FD7E4A3D874C38DB2D856F007CA39AC2172888863CBA348B098FD5AA588BF07D2EFA00D04086158D919A2764827D88E22E8E468262CD006F125C98B143BE5C1AD2BC90F4054D54DBFB30C190110B7AAF9E7FFFEAF953F4EAF993E387CF8E1FFE74FCE8D1F1C31F2D2D67E12E8E83E2C297DF7EF6E7D71FA33F9E7EF3F2F117E57859C4FFFAC327BFFCFC79
			m.lcFileBinary = m.lcFileBinary + 0h3910326821D18B2F9FFCF6ECC98BAF3EFDFDBBC725F06D814745F8904644A25BE4081DF0087433867125272371BE15C3105367050E817609E99E0A1DE0AD396665B80E718D775740F128035E9FDD77641D8462A66809E71B61E400F738671D2E4A0D7043F32A5878388B8372E66256C41D607C58C6BB8B63C7B5BD590255330B4AC7F6DD903862EE331C2B1C909828A4E7F8949012EDEE51EAD8758FFA824B3E51E81E451D4C4B4D32A4232790168B7669047E9997E90CAE766CB3771775382BD37A871CBA484808CC4A841F12E698F13A9E291C95911CE288150D7E13ABB04CC8C15CF8455C4F2AF074401847BD3191B26CCD6D01FA169C7E0343BD2A75FB
			m.lcFileBinary = m.lcFileBinary + 0h1E9B472E52283A2DA37913735E44EEF06937C45152861DD0382C623F905308518CF6B92A83EF713743F43BF801C72BDD7D9712C7DDA717823B3470445A04889E9909ED4B28D44EFD8D68FC77C59851A8C63606DE15E3B6B70D5B53594AEC9E28C1AB70FFC1C2BB8367F13E81585FDE78DED5DD7775D77BEBEBEEAA5C3E6BB55D1458A8BDBA79B07DB1E992A3954DF28432365073466E4AD3274BD82CC67D18D4EBCC0191E487A62484C7B4B83BB84060B30609AE3EA22A1C8438811EBBE66922814C490712255CC2D9CE0C97D2D678E8D3953D1936F499C1D60389D51E1FDBE1753D9C1D0D723266CB09CCF93363B4AE099C95D9FA959428A8FD3ACC6A5AA8
			m.lcFileBinary = m.lcFileBinary + 0h3373AB19D14CA973B8E52A830F975583C1DC9AD08520E85DC0CA4D38A26BD67036C18C8CB5DDED069CB9C578E1225D24433C26A98FB4DECB3EAA192765B1622E0320764A7CA4CF79A758ADC0ADA5C9BE01B7B338A9C8AEBE825DE6BD37F15216C10B2FE9BC3D918E2C2E26278BD151DB6B35D61A1EF271D2F62670AC85C72801AF4BDDF86116C0DD90AF840DFB5393D964F9C29BAD4C3137096A705361EDBEA4B053071221D50E96A10D0D339586008B35272BFF5A03CC7A510AD8487F0D29D6372018FE3529C08EAE6BC964427C5574766144DBCEBEA6A594CF141183707C84466C260E30B85F872AE833A6126E274C45D02F7095A6AD6DA6DCE29C265DF1
			m.lcFileBinary = m.lcFileBinary + 0h02CBE0EC38664988D372AB5334CB640B37799CCB60DE0AE2816EA5B21BE5CEAF8A49F90B52A518C6FF3355F47E02D705EB63ED011F6E7205463A5FDB1E172AE450859290FA7D018D83A91D102D701D0BD31054709F6CFE0B72A8FFDB9CB3344C5AC3A94F1DD000090AFB910A0521FB50964CF49D42AC96EE5D96244B0999882A882B132BF6881C1236D435B0A9F7760F8510EAA69AA465C0E04EC69FFB9E66D028D04D4E31DF9C1A92EFBD3607FEE9CEC7263328E5D661D3D064F6CF452CD955ED7AB33CDB7B8B8AE889459B55CFB2029815B682569AF6AF29C239B75A5BB196345E6B64C28117973586C1BC214AE0D207E93FB0FF51E133FB71426FA8437E
			m.lcFileBinary = m.lcFileBinary + 0h00B515C1B7064D0CC206A2FA926D3C902E907670048D931DB4C1A44959D3A6AD93B65AB6595F70A79BF33D616C2DD959FC7D4E63E7CD99CBCEC9C58B34766A61C7D6766CA5A9C1B32753148626D941C638C67CD52A7E78E2A3FBE0E81DB8E29F31254D30C1672581A1F51C983C80E4B71CCDD2ADBF000000FFFF0300504B030414000600080000002100075F38D31E010000C701000018000000786C2F776F726B7368656574732F7368656574312E786D6C8C514D6BC3300CBD0FF61F8CEE8BD38D6EA324298352B6C360ECEBEE2472626A5BC156D7EDDFCF496819F4B29B3EDE7BD2938AF5B7B3E20B4334E44B58643908F40DB5C677257CBC6FAFEE4144
			m.lcFileBinary = m.lcFileBinary + 0h56BE55963C96F08311D6D5E54571A0B08B3D228BA4E063093DF3B09232363D3A15331AD0A78EA6E014A73474320E01553B919C95D7797E2B9D321E668555F88F06696D1ADC50B377E87916096815A7FD636F860855D19AD41B0D8980BA848705C8AA98C67E1A3CC43FB16055BFA1C586B14DEE418CAE6AA2DD087C4AA57CA4CA33EE7672F512448B5AED2DBFD2E1114DD77312599EA66D14AB441F5487CF2A74C6476151274C9EDD8108337E8A9986A9BA04511333B963D6A783613A4C9EDD80D0447C4CC6B54E2FA87E010000FFFF0300504B030414000600080000002100A6A453EB3E0100005102000011000801646F6350726F70732F636F72652E786D
			m.lcFileBinary = m.lcFileBinary + 0h6C20A2040128A00001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000007C92516BC3201485DF07FB0FC1F7449342DB4992C236FAB4C260191B7B13BD6D65D188BAA5FDF733499BA65006BE78CFB9DF3D5ECC57075547BF609D6C7481D284A008346F84D4BB02BD57EB788922E79916AC6E3414E8080EADCAFBBB9C1BCA1B0BAFB63160BD0417059276949B02EDBD371463C7F7A0984B82430771DB58C57CB8DA1D368C7FB31DE08C903956E099609EE10E189B91884E48C147A4F9B1750F101C430D0AB477384D527CF17AB0CADD6CE8958953497F34E14DA7B853B6E08338BA0F4E8EC6B66D9376D6C708F953FCB97979EB9F1A4BDDED8A032A73C129B7C07C63CB1C4F2F617135737E1376BC95201E8F41
			m.lcFileBinary = m.lcFileBinary + 0hBF5113BC8F3B40404421001DE29E958FD9D373B54665B7C3983CC4E9BC2284F6E7AB1B79D5DF051A0AEA34F87FE2B223924595113A5BD02C9B10CF8021F7F52728FF000000FFFF0300504B03041400060008000000210027388BC4880100001103000010000801646F6350726F70732F6170702E786D6C20A2040128A000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009C92C16EDB300C86EF03F60E86EE8D9C601886405651A41D7AD8B00049BBB326D3B150591244D648F6F4A36DA471BA9DA613C99FF8F58994BA3D76BEE821A38BA112CB45290A0836D62E1C2AF1B4FF7AF345144826D4C6C7009538018A5BFDF183DAE6982093032CD82260255AA2B496126D0B9DC105CB819526E6CE10A7F9
			m.lcFileBinary = m.lcFileBinary + 0h2063D3380BF7D1BE761048AECAF2B3842341A8A1BE496F8662725CF7F4BFA675B4031F3EEF4F8981B5BA4BC93B6B885FA9BF3B9B23C6868A87A305AFE45C544CB703FB9A1D9D74A9E43C553B6B3C6CD85837C623287929A84730C3D0B6C665D4AAA7750F96622ED0FDE6B1AD44F1CB200C3895E84D762610630D6D5332C63E2165FD33E6176C010895E486A93886F3DE79EC3EE9E5D8C0C175E3603081B0708DB877E4017F345B93E91FC4CB39F1C830F14E38BB816FBA73CE373E996F7AE7BD895D32E1C4C25BF4CD85177C4AFB786F08CEE3BC2EAA5D6B32D4BC81B37E29A8479E64F683C9A635E100F5B9E76F6158FEF3F4C3F572B528F98C3B3FD794BC
			m.lcFileBinary = m.lcFileBinary + 0hFC65FD070000FFFF0300504B01022D0014000600080000002100CC7EE6A14E010000080400001300000000000000000000000000000000005B436F6E74656E745F54797065735D2E786D6C504B01022D0014000600080000002100B5553023F50000004C0200000B000000000000000000000000005E0300005F72656C732F2E72656C73504B01022D00140006000800000021008D87DA70E00000002D0200001A000000000000000000000000004A060000786C2F5F72656C732F776F726B626F6F6B2E786D6C2E72656C73504B01022D0014000600080000002100A460FAFF52010000270200000F000000000000000000000000006A080000786C2F776F
			m.lcFileBinary = m.lcFileBinary + 0h726B626F6F6B2E786D6C504B01022D0014000600080000002100A083C4AEA3010000640300000D00000000000000000000000000E9090000786C2F7374796C65732E786D6C504B01022D0014000600080000002100E9A625B882060000531B00001300000000000000000000000000B70B0000786C2F7468656D652F7468656D65312E786D6C504B01022D0014000600080000002100075F38D31E010000C701000018000000000000000000000000006A120000786C2F776F726B7368656574732F7368656574312E786D6C504B01022D0014000600080000002100A6A453EB3E010000510200001100000000000000000000000000BE130000646F635072
			m.lcFileBinary = m.lcFileBinary + 0h6F70732F636F72652E786D6C504B01022D001400060008000000210027388BC48801000011030000100000000000000000000000000033160000646F6350726F70732F6170702E786D6C504B050600000000090009003E020000F11800000000
		CASE m.lcExcelFileExtension = "XLSB"
			m.lcFileBinary = 0h504B030414000600080000002100558086C16D010000020400001300DD015B436F6E74656E745F54797065735D2E786D6C20A2D90128A00002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000000000000000000000A453CD4BC33014BF0BFE0F255769B2791091B53BE8042FBAC3C47396BEAE616912F2B2D9FEF7BEB6ACA0EC83E1A5A16D7E9F2F99CD9BDA247B08A89DCDD8944F580256B942DB4DC63E57AFE9234B304A5B48E32C64AC0564F3FCF666B66A3D6042688B19AB62F44F42A0AAA096C89D074B7F4A176A19E9356C84976A2B3720EE279307A19C8D60631A3B0E96CF5EA0943B139345439F07276B6D59F23CECEBA43226BD375AC94846C5DE16BCC6141A058663051039016468792D55700B2BD706688B241691CF3E2860D005244B19E2BBAC894D344644720BC373CAC9EF05C1DFA95257965A41E1D4AEA62CBC27BBEB
			m.lcFileBinary = m.lcFileBinary + 0h584E0B7EBBB0EDDDA2E89769E7FA82EA1873047702471A0B60F002D79F047E980927645F2B56DAE321C21185F3159D894E252D83F3286886FF2E1ABA435240917AA28410358C9E4FCC796C0EC5704ADE08DF5C553EE17AD0179E192FC6D6005E33D301718A72AC4DB900D7F776186F873E5296E86F70FE030000FFFF0300504B0304140006000800000021004382E3C5F70000004C0200000B00CE015F72656C732F2E72656C7320A2CA0128A000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008C92CF4EC3300CC6EF48BC43E4FBEA6E4808A1B5BB20A4DD102A0FE0256E1BB58DA32440F7F66427A834B61DFDEFF3CF9FBCDDCDD3A8BE38442BAE8275518262A7C558D755F0D1BCAE9E40C544CED0288E2B3872845D7D7FB77DE791521E8ABDF5516515172BE853F2CF8851F73C512CC4B3CB9556C2442987A1434F7AA08E7153968F18FE6A40BD
			m.lcFileBinary = m.lcFileBinary + 0hD0547B5341D89B0750CDD1E7CDD7B5A56DADE617D19F13BB746605F29CD819362B1F325B48365FA31A0A1DA70A8CE8B79C8E48DE17191BF03CD1E676A2FFAFC58913194A845A025FE639755C025ADF0E74DDA265C7AF3BF388DF128683C8501CAC3B99838B1FA87F000000FFFF0300504B03041400060008000000210006332055E50000002D0200001A000801786C2F5F72656C732F776F726B626F6F6B2E62696E2E72656C7320A2040128A000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000AC91C16AC3301044EF85FE83D87BBD760AA594C8B98442AEC1FD00555EDB22B624B4DB36FEFB0A97360DA4F4928B60B468DEEC68BD394EA37AA7C42E780D555182226F43EB7CAFE1A579BE7B04C562
			m.lcFileBinary = m.lcFileBinary + 0h7C6BC6E049C34C0C9BFAF666BDA7D1487EC4838BACB28B670D83487C42643BD064B808917C9E74214D46B24C3D46630FA6275C95E503A6DF1E509F79AA5DAB21EDDA7B50CD1C33F97FEFD075CED236D8B789BC5C4020CB3CE6055463524FA2E14B17AFCE035EC6AFAE89975C0B9DE88BC4E5AC8ADCD35F19AA6B66F808E9C003919C72FC5C312E93EABB103CFBE4FA130000FFFF0300504B030414000600080000002100A76734B792000000C00000000F000000786C2F776F726B626F6F6B2E62696E6A66646860346040034C407E05430E0323903681922C60B62983018329C34C461E0DA0B0D35390020686764686798CB21F80AC4C20BE6AC9C0E025CF
			m.lcFileBinary = m.lcFileBinary + 0hC0100132050A2A3A1819FA1919E630AA800440BA40C615317832A4301832B001D9C10C190CA9405802E44F606498CB2825FB9A11AC320528FB67E5C74BBE4901F6209D590CB31819196603710B2303000000FFFF0300504B030414000600080000002100F98F44D4E0000000CA0100000D000000786C2F7374796C65732E62696EA4904F4B424114C57FBEB9E8F6B9EB1BB410FF1104EE8C5CA608F60514438522100DFA16ADEAC3E52E15FCB3D255923CCFBC47E2D26860CE9C7BCF993B73EF7BC097590AC85D4E846F9E0690F127444146784B9B470674180AA7C6A7993CE46BDEE3AA1E89DC8D402B8EFE00F95A56EEFF569918B3A491C295CAA58E1F38
			m.lcFileBinary = m.lcFileBinary + 0h8FCD8D4D72BD189EB61086B03516BFDAB1AC88D796C62AD14A175ED36B515A6783670DEB291EDCDAF876E6D59DE3C7353DCB6ADF4BED68B00FB418F11AB3BAA2AE863CD6DD4AEC6A2A7A51B5D189EB4EB91E7DE5CA5CB3777C041C000000FFFF0300504B030414000600080000002100E9A625B882060000531B000013000000786C2F7468656D652F7468656D65312E786D6CEC594F6FDB3614BF0FD87720746F6D27B61B07758AD8B19BAD4D1BC46E871E6999965853A240D2497D1BDAE38001C3BA619701BBED306C2BD002BB749F265B87AD03FA15F6484AB218CB4BD2061BD6D58744227F7CFFDFE32375F5DA8388A1432224E571DBAB5DAE7A88C43E
			m.lcFileBinary = m.lcFileBinary + 0h1FD338687B7786FD4B1B1E920AC763CC784CDADE9C48EFDAD6FBEF5DC59B2A241141B03E969BB8ED854A259B958AF46118CBCB3C2131CC4DB888B082571154C6021F01DD8855D6AAD56625C234F6508C23207B7B32A13E41434DD2DBCA88F718BCC64AEA019F8981264D9C15063B9ED63442CE659709748859DB033E637E34240F948718960A26DA5ED5FCBCCAD6D50ADE4C1731B5626D615DDFFCD275E982F174CDF014C128675AEBD75B577672FA06C0D432AED7EB757BB59C9E0160DF074DAD2C459AF5FE46AD93D12C80ECE332ED6EB551ADBBF802FDF525995B9D4EA7D14A65B1440DC83ED697F01BD5667D7BCDC11B90C53796F0F5CE76B7DB74F006
			m.lcFileBinary = m.lcFileBinary + 0h64F1CD257CFF4AAB5977F10614321A4F97D0DAA1FD7E4A3D874C38DB2D856F007CA39AC2172888863CBA348B098FD5AA588BF07D2EFA00D04086158D919A2764827D88E22E8E468262CD006F125C98B143BE5C1AD2BC90F4054D54DBFB30C190110B7AAF9E7FFFEAF953F4EAF993E387CF8E1FFE74FCE8D1F1C31F2D2D67E12E8E83E2C297DF7EF6E7D71FA33F9E7EF3F2F117E57859C4FFFAC327BFFCFC793910326821D18B2F9FFCF6ECC98BAF3EFDFDBBC725F06D814745F8904644A25BE4081DF0087433867125272371BE15C3105367050E817609E99E0A1DE0AD396665B80E718D775740F128035E9FDD77641D8462A66809E71B61E400F738671D2E
			m.lcFileBinary = m.lcFileBinary + 0h4A0D7043F32A5878388B8372E66256C41D607C58C6BB8B63C7B5BD590255330B4AC7F6DD903862EE331C2B1C909828A4E7F8949012EDEE51EAD8758FFA824B3E51E81E451D4C4B4D32A4232790168B7669047E9997E90CAE766CB3771775382BD37A871CBA484808CC4A841F12E698F13A9E291C95911CE288150D7E13ABB04CC8C15CF8455C4F2AF074401847BD3191B26CCD6D01FA169C7E0343BD2A75FB1E9B472E52283A2DA37913735E44EEF06937C45152861DD0382C623F905308518CF6B92A83EF713743F43BF801C72BDD7D9712C7DDA717823B3470445A04889E9909ED4B28D44EFD8D68FC77C59851A8C63606DE15E3B6B70D5B53594AEC9E28
			m.lcFileBinary = m.lcFileBinary + 0hC1AB70FFC1C2BB8367F13E81585FDE78DED5DD7775D77BEBEBEEAA5C3E6BB55D1458A8BDBA79B07DB1E992A3954DF28432365073466E4AD3274BD82CC67D18D4EBCC0191E487A62484C7B4B83BB84060B30609AE3EA22A1C8438811EBBE66922814C490712255CC2D9CE0C97D2D678E8D3953D1936F499C1D60389D51E1FDBE1753D9C1D0D723266CB09CCF93363B4AE099C95D9FA959428A8FD3ACC6A5AA83373AB19D14CA973B8E52A830F975583C1DC9AD08520E85DC0CA4D38A26BD67036C18C8CB5DDED069CB9C578E1225D24433C26A98FB4DECB3EAA192765B1622E0320764A7CA4CF79A758ADC0ADA5C9BE01B7B338A9C8AEBE825DE6BD37F15216
			m.lcFileBinary = m.lcFileBinary + 0hC10B2FE9BC3D918E2C2E26278BD151DB6B35D61A1EF271D2F62670AC85C72801AF4BDDF86116C0DD90AF840DFB5393D964F9C29BAD4C3137096A705361EDBEA4B053071221D50E96A10D0D339586008B35272BFF5A03CC7A510AD8487F0D29D6372018FE3529C08EAE6BC964427C5574766144DBCEBEA6A594CF141183707C84466C260E30B85F872AE833A6126E274C45D02F7095A6AD6DA6DCE29C265DF102CBE0EC38664988D372AB5334CB640B37799CCB60DE0AE2816EA5B21BE5CEAF8A49F90B52A518C6FF3355F47E02D705EB63ED011F6E7205463A5FDB1E172AE450859290FA7D018D83A91D102D701D0BD31054709F6CFE0B72A8FFDB9CB3344C
			m.lcFileBinary = m.lcFileBinary + 0h5AC3A94F1DD000090AFB910A0521FB50964CF49D42AC96EE5D96244B0999882A882B132BF6881C1236D435B0A9F7760F8510EAA69AA465C0E04EC69FFB9E66D028D04D4E31DF9C1A92EFBD3607FEE9CEC7263328E5D661D3D064F6CF452CD955ED7AB33CDB7B8B8AE889459B55CFB2029815B682569AF6AF29C239B75A5BB196345E6B64C28117973586C1BC214AE0D207E93FB0FF51E133FB71426FA8437E00B515C1B7064D0CC206A2FA926D3C902E907670048D931DB4C1A44959D3A6AD93B65AB6595F70A79BF33D616C2DD959FC7D4E63E7CD99CBCEC9C58B34766A61C7D6766CA5A9C1B32753148626D941C638C67CD52A7E78E2A3FBE0E81DB8E29F
			m.lcFileBinary = m.lcFileBinary + 0h31254D30C1672581A1F51C983C80E4B71CCDD2ADBF000000FFFF0300504B030414000600080000002100A1512698C10000001C01000023000000786C2F776F726B7368656574732F5F72656C732F7368656574312E62696E2E72656C736CCFC16AC3300C06E0FBA0EF60745F9CF430C68853D861906BE91E40B395C434968D654AF2F6F56DEDD8F197F83FA1FEB48555DD288B8F6CA06B5A50C4363ACFB381EFCBD7EB3B2829C80ED7C864602781D37078E9CFB462A925597C12551516034B29E9436BB10B05942626E2BA99620E586ACCB34E68AF38933EB6ED9BCE8F060C4FA61A9D813CBA0ED4654FF5F21F3B789BA3C4A93436061DA7C9DBFF54BDAD9F
			m.lcFileBinary = m.lcFileBinary + 0h9E31EF233BDAAA8579A662E0E777D83535801E7AFDF4D370070000FFFF0300504B030414000600080000002100A273D38D810000001601000018000000786C2F776F726B7368656574732F7368656574312E62696E6A646498CC287E928589C181010CFE430188338551002C8644B432327432CADD61461202EB4C411298C1A882220F946244928631BB1819DA18199E32F3806CE460D001AB99C8C8308991613A8B134415480C9B5E9819301A5D0D887F97994980E10EB3411A183CB387D1103D2FEC61B431185CB687D14D8C0C00000000FFFF0300504B0304140006000800000021001E06781015000000200000001E000000786C2F776F726B73686565
			m.lcFileBinary = m.lcFileBinary + 0h74732F62696E617279496E646578312E62696ED29260000205108106A632310011000000FFFF0300504B0304140006000800000021004B7C9F083E0100005102000011000801646F6350726F70732F636F72652E786D6C20A2040128A0000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007C925F4BC33014C5DF05BF43C97B9B74835943DB81CA9E1C085614DF4272D7159B3F24D16EDFDEB4DD6A0743C84BEE39F7774F2EC9D707D9463F605DA35581D284A00814D7A2517581DEAA4D9CA1C879A6046BB582021DC1A175797B937343B9B6F062B501EB1B7051202947B929D0DE7B4331767C0F92B92438541077DA4AE6C3D5D6D830FEC56AC00B42565882678279867B606C26223A21059F90E6DBB6
			m.lcFileBinary = m.lcFileBinary + 0h0340700C2D4850DEE13449F19FD78395EE6AC3A0CC9CB2F14713DE748A3B670B3E8A93FBE09AC9D8755DD22D8718217F8A3FB6CFAFC353E346F5BBE280CA5C70CA2D30AF6D99E3F9252CAE65CE6FC38E770D888763D0AFD4041FE28E1010510840C7B867E57DF9F8546D50D9EF3026F771BAAA08A1C3F9EC475EF4F781C6823C0DFE9F98F54472572D085D66946433E21930E6BEFC04E52F000000FFFF0300504B0304140006000800000021006F7E771280010000FE02000010000801646F6350726F70732F6170702E786D6C20A2040128A00001000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009C92416BE3301085EF85FD0F46F7464E589625C82AA569E9614B0349B367551EC7228A64345393F4D7
			m.lcFileBinary = m.lcFileBinary + 0h776C93C6D9EDA93A8DE63D9E3E8DA46E0E7B9FB590D0C55088E9241719041B4B17B68578593F5CFF16199209A5F13140218E80E246FFB852CB141B48E400338E0858889AA8994B89B686BDC109CB81952AA6BD21DEA6AD8C55E52C2CA27DDB432039CBF35F120E04A184F2BAF90C1443E2BCA5EF8696D1767CB8591F1B06D6EAB669BCB386F896FAC9D914315694DD1F2C7825C7A262BA15D8B7E4E8A87325C75BB5B2C6C31D07EBCA780425CF0DF508A61BDAD2B8845AB5346FC1524C19BA771EDB4C64AF06A1C329446B92338118ABB30D9BBEF60D52D27F63DA610D40A8241B86665F8EBDE3DAFDD4D3DEC0C5A5B10B184058B8445C3BF280CFD5D224FA
			m.lcFileBinary = m.lcFileBinary + 0h82783A26EE1906DE0167D5F10D678EF9FA2BF349FF64FF7161872FCD3A2E0CC16976974DB5AA4D8292C77DD2CF0DF5C8634BBE0BB9AB4DD84279F2FC2F742FBD19BEB39ECE2639AFFE814F3D25CF1F577F000000FFFF0300504B01022D0014000600080000002100558086C16D010000020400001300000000000000000000000000000000005B436F6E74656E745F54797065735D2E786D6C504B01022D00140006000800000021004382E3C5F70000004C0200000B000000000000000000000000007B0300005F72656C732F2E72656C73504B01022D001400060008000000210006332055E50000002D0200001A00000000000000000000000000690600
			m.lcFileBinary = m.lcFileBinary + 0h00786C2F5F72656C732F776F726B626F6F6B2E62696E2E72656C73504B01022D0014000600080000002100A76734B792000000C00000000F000000000000000000000000008E080000786C2F776F726B626F6F6B2E62696E504B01022D0014000600080000002100F98F44D4E0000000CA0100000D000000000000000000000000004D090000786C2F7374796C65732E62696E504B01022D0014000600080000002100E9A625B882060000531B00001300000000000000000000000000580A0000786C2F7468656D652F7468656D65312E786D6C504B01022D0014000600080000002100A1512698C10000001C01000023000000000000000000000000000B
			m.lcFileBinary = m.lcFileBinary + 0h110000786C2F776F726B7368656574732F5F72656C732F7368656574312E62696E2E72656C73504B01022D0014000600080000002100A273D38D810000001601000018000000000000000000000000000D120000786C2F776F726B7368656574732F7368656574312E62696E504B01022D00140006000800000021001E06781015000000200000001E00000000000000000000000000C4120000786C2F776F726B7368656574732F62696E617279496E646578312E62696E504B01022D00140006000800000021004B7C9F083E01000051020000110000000000000000000000000015130000646F6350726F70732F636F72652E786D6C504B01022D001400
			m.lcFileBinary = m.lcFileBinary + 0h06000800000021006F7E771280010000FE02000010000000000000000000000000008A150000646F6350726F70732F6170702E786D6C504B0506000000000B000B00DB020000401800000000
		CASE m.lcExcelFileExtension = "XLSM"
			m.lcFileBinary = 0h504B03041400060008000000210038088E4F60010000F20300001300DB015B436F6E74656E745F54797065735D2E786D6C20A2D70128A00002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000000000000000000A4934D6EC2301085F7957A87C8DB2A31745155158105A5CB96053D80B127C4C27FF2181A6EDF495290401481D838B2A279DFBC37E3D1A4B126DB4244ED5DC986C58065E0A457DAAD4AF6BDF8C85F59864938258C7750B21D209B8C1F1F468B5D00CCA8DA61C9EA94C21BE7286BB0020B1FC0D19FCA472B125DE38A0721D76205FC793078E1D2BB042EE5A9D560E3D1173510B5826C2E62FA149638BC313C911AF4E7B0203D964DFBC2965D321182D15224EA9C6F9D3AA1E6BEAAB404E5E5C612ABE8C49E5A15FE2F10D3CE00DE8DC2104128AC019235452FBA27BF4325362665B38612E8438F60F0366B7F611654D9D9C7
			m.lcFileBinary = m.lcFileBinary + 0h5A07BC40B89CDDE54C7E7C5C2FBD5F5F918AC51C1A0964B9B55E5821A39F39B134A0E8A2DDBEC373E3A639CDA30FC869AA57A08E37EC74D6D086AB40E5812421260D8774CEB169D55A975DD7C8BBCFFDFB76BC0407FDAB32903EC2ED21ECB7A2AD3EE39C772F76FC0B0000FFFF0300504B030414000600080000002100B5553023F50000004C0200000B00CE015F72656C732F2E72656C7320A2CA0128A000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008C92CF4EC3300CC6EF48BC43E4FBEA6E4808A1A5BB4C48BB21541EC024EE1FB58DA32440F7F68403824A63DBD1F6E7CF3F5BDEEEE669541F1C622F4EC3BA2841B133627BD76A78AD9F560FA06222676914C71A8E1C6157DDDE6C5F78A4949B62D7FBA8B28B8B1ABA94FC2362341D4F140BF1EC72A5913051CA6168D19319A865DC94E53D86BF1E502D3CD5C16A08077B07AA3EFA3CF9B2
			m.lcFileBinary = m.lcFileBinary + 0hB7344D6F782FE67D62974E8C409E133BCB76E543660BA9CFDBA89A42CB498315F39CD311C9FB2263039E26DA5C4FF4FFB63871224B89D048E0F33CDF8A7340EBEB812E9F68A9F8BDCE3CE2A784E14D64F861C1C50F545F000000FFFF0300504B0304140006000800000021008D87DA70E00000002D0200001A000801786C2F5F72656C732F776F726B626F6F6B2E786D6C2E72656C7320A2040128A0000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000AC91CB6AC3301045F785FE83987D3D760AA594C8D99442B6C5FD00218F1FC496846692D67F5FE182DD404836D908AE06DD73246D773FE3A04E14B9F74E4391E5A0C8595FF7AED5F0557D3CBD826231AE368377A46122865DF9F8B0FDA4C1483A
			m.lcFileBinary = m.lcFileBinary + 0hC45D1F58A516C71A3A91F086C8B6A3D170E603B934697C1C8DA4185B0CC61E4C4BB8C9F3178CFF3BA03CEB54FB5A43DCD7CFA0AA2924F2ED6EDF34BDA5776F8F2339B98040966948175095892D8986BF9C2547C0CBF8CD3DF1929E8556FA1C715E8B6B0EC53D1DBE7D3C704724ABC7B2C5384F16193CFBE4F2170000FFFF0300504B030414000600080000002100A460FAFF52010000270200000F000000786C2F776F726B626F6F6B2E786D6C8C915D6FC2201486EF97EC3F10EE272DA9CE185B93655BE6CD6232A7D7AC9C5A228506A8D57FBF534CE776B72BCE073CE7BC2FCBD5B9D1E404CE2B6B729A4E124AC094562A73C8E9E7F6F5614E890FC248A1
			m.lcFileBinary = m.lcFileBinary + 0hAD819C5EC0D355717FB7ECAD3B7E597B2408303EA77508ED82315FD6D0083FB12D18EC54D6352260EA0ECCB70E84F435406834E34932638D50865E090BF71F86AD2A55C2B32DBB064CB8421C6811707D5FABD6D36259290DBBAB2222DAF65D34B8F75953A2850F2F52059039CD30B53DFC29B8AE7DEA941EBAD3644A59F12372E3085203B88D5327515ED0294A2454A2D3618B82C77958E719E7B3E1ED60CE4E41EF6F982125E7BD32D2F639E5199A7D19B31427923EB6F64A861A51D9FC567B0375A8434EE7499A0C74F60B1F2DC531F12426EAFD186CC625636D8D9230760B85815BCB3412C667A5D0250A1C8E78914F1F7954CFC63F2EBE010000FFFF03
			m.lcFileBinary = m.lcFileBinary + 0h00504B030414000600080000002100A083C4AEA3010000640300000D000000786C2F7374796C65732E786D6CA453C16ADC3010BD17F20F42F746BB0B0D6DB19D436121909440B6D0AB6CC95EC16864A4F1B2EED767643BDEDD530EBD584F4F336F9E66E4E2F1EC419C6C4C2E6029B7F71B292C36C138EC4AF9E7B0FFFA5D8A441A8D8680B694A34DF2B1BAFB52241AC1BE1DAD25C112984A7924EA7F2A959AA3F53ADD87DE229FB4217A4DBC8D9D4A7DB4DAA49CE441ED369B07E5B54359156D404AA2090312BB5888AA48FFC44903335BA9AAA20910A2209667231383DADB39E29706574797C35AED1D8C33BDCBC4E46889F30E43CCA4CA25972571920358
			m.lcFileBinary = m.lcFileBinary + 0h0DECB20126AAA2D74436E29E3762C187B1E7F2C8DD9865A6B84FA2BBA8C7EDEEDB55829A0A56451DA2E1EE5F5F7DA6AA026C4B6C34BAEE98570A3D7FEB40143C03E37417500343F591B100BE4E6301DEF284FEB637DAE756E0E0F79E9E4C2979D6B9091F902FB2C0596FDE64FD6BB559FBBF65C5B9BDD567C52BDB37A6D7F222CFBB94BFF39302B94A887A70400E6F05A7EBB3A6395F5AB0C913205DF3CBCDCD59AB70278C6DF50074580F4B79C12FD6B8C1FF58A35EDD29D02451CA0B7ECE93DA3E4C534EEBEF51BD030000FFFF0300504B030414000600080000002100E9A625B882060000531B000013000000786C2F7468656D652F7468656D65312E78
			m.lcFileBinary = m.lcFileBinary + 0h6D6CEC594F6FDB3614BF0FD87720746F6D27B61B07758AD8B19BAD4D1BC46E871E6999965853A240D2497D1BDAE38001C3BA619701BBED306C2BD002BB749F265B87AD03FA15F6484AB218CB4BD2061BD6D58744227F7CFFDFE32375F5DA8388A1432224E571DBAB5DAE7A88C43E1FD338687B7786FD4B1B1E920AC763CC784CDADE9C48EFDAD6FBEF5DC59B2A241141B03E969BB8ED854A259B958AF46118CBCB3C2131CC4DB888B082571154C6021F01DD8855D6AAD56625C234F6508C23207B7B32A13E41434DD2DBCA88F718BCC64AEA019F8981264D9C15063B9ED63442CE659709748859DB033E637E34240F948718960A26DA5ED5FCBCCAD6D50ADE
			m.lcFileBinary = m.lcFileBinary + 0h4C1731B5626D615DDFFCD275E982F174CDF014C128675AEBD75B577672FA06C0D432AED7EB757BB59C9E0160DF074DAD2C459AF5FE46AD93D12C80ECE332ED6EB551ADBBF802FDF525995B9D4EA7D14A65B1440DC83ED697F01BD5667D7BCDC11B90C53796F0F5CE76B7DB74F00664F1CD257CFF4AAB5977F10614321A4F97D0DAA1FD7E4A3D874C38DB2D856F007CA39AC2172888863CBA348B098FD5AA588BF07D2EFA00D04086158D919A2764827D88E22E8E468262CD006F125C98B143BE5C1AD2BC90F4054D54DBFB30C190110B7AAF9E7FFFEAF953F4EAF993E387CF8E1FFE74FCE8D1F1C31F2D2D67E12E8E83E2C297DF7EF6E7D71FA33F9E7EF3F2
			m.lcFileBinary = m.lcFileBinary + 0hF117E57859C4FFFAC327BFFCFC793910326821D18B2F9FFCF6ECC98BAF3EFDFDBBC725F06D814745F8904644A25BE4081DF0087433867125272371BE15C3105367050E817609E99E0A1DE0AD396665B80E718D775740F128035E9FDD77641D8462A66809E71B61E400F738671D2E4A0D7043F32A5878388B8372E66256C41D607C58C6BB8B63C7B5BD590255330B4AC7F6DD903862EE331C2B1C909828A4E7F8949012EDEE51EAD8758FFA824B3E51E81E451D4C4B4D32A4232790168B7669047E9997E90CAE766CB3771775382BD37A871CBA484808CC4A841F12E698F13A9E291C95911CE288150D7E13ABB04CC8C15CF8455C4F2AF074401847BD3191B2
			m.lcFileBinary = m.lcFileBinary + 0h6CCD6D01FA169C7E0343BD2A75FB1E9B472E52283A2DA37913735E44EEF06937C45152861DD0382C623F905308518CF6B92A83EF713743F43BF801C72BDD7D9712C7DDA717823B3470445A04889E9909ED4B28D44EFD8D68FC77C59851A8C63606DE15E3B6B70D5B53594AEC9E28C1AB70FFC1C2BB8367F13E81585FDE78DED5DD7775D77BEBEBEEAA5C3E6BB55D1458A8BDBA79B07DB1E992A3954DF28432365073466E4AD3274BD82CC67D18D4EBCC0191E487A62484C7B4B83BB84060B30609AE3EA22A1C8438811EBBE66922814C490712255CC2D9CE0C97D2D678E8D3953D1936F499C1D60389D51E1FDBE1753D9C1D0D723266CB09CCF93363B4AE09
			m.lcFileBinary = m.lcFileBinary + 0h9C95D9FA959428A8FD3ACC6A5AA83373AB19D14CA973B8E52A830F975583C1DC9AD08520E85DC0CA4D38A26BD67036C18C8CB5DDED069CB9C578E1225D24433C26A98FB4DECB3EAA192765B1622E0320764A7CA4CF79A758ADC0ADA5C9BE01B7B338A9C8AEBE825DE6BD37F15216C10B2FE9BC3D918E2C2E26278BD151DB6B35D61A1EF271D2F62670AC85C72801AF4BDDF86116C0DD90AF840DFB5393D964F9C29BAD4C3137096A705361EDBEA4B053071221D50E96A10D0D339586008B35272BFF5A03CC7A510AD8487F0D29D6372018FE3529C08EAE6BC964427C5574766144DBCEBEA6A594CF141183707C84466C260E30B85F872AE833A6126E274C45
			m.lcFileBinary = m.lcFileBinary + 0hD02F7095A6AD6DA6DCE29C265DF102CBE0EC38664988D372AB5334CB640B37799CCB60DE0AE2816EA5B21BE5CEAF8A49F90B52A518C6FF3355F47E02D705EB63ED011F6E7205463A5FDB1E172AE450859290FA7D018D83A91D102D701D0BD31054709F6CFE0B72A8FFDB9CB3344C5AC3A94F1DD000090AFB910A0521FB50964CF49D42AC96EE5D96244B0999882A882B132BF6881C1236D435B0A9F7760F8510EAA69AA465C0E04EC69FFB9E66D028D04D4E31DF9C1A92EFBD3607FEE9CEC7263328E5D661D3D064F6CF452CD955ED7AB33CDB7B8B8AE889459B55CFB2029815B682569AF6AF29C239B75A5BB196345E6B64C28117973586C1BC214AE0D207
			m.lcFileBinary = m.lcFileBinary + 0hE93FB0FF51E133FB71426FA8437E00B515C1B7064D0CC206A2FA926D3C902E907670048D931DB4C1A44959D3A6AD93B65AB6595F70A79BF33D616C2DD959FC7D4E63E7CD99CBCEC9C58B34766A61C7D6766CA5A9C1B32753148626D941C638C67CD52A7E78E2A3FBE0E81DB8E29F31254D30C1672581A1F51C983C80E4B71CCDD2ADBF000000FFFF0300504B030414000600080000002100075F38D31E010000C701000018000000786C2F776F726B7368656574732F7368656574312E786D6C8C514D6BC3300CBD0FF61F8CEE8BD38D6EA324298352B6C360ECEBEE2472626A5BC156D7EDDFCF496819F4B29B3EDE7BD2938AF5B7B3E20B4334E44B586439
			m.lcFileBinary = m.lcFileBinary + 0h08F40DB5C677257CBC6FAFEE414456BE55963C96F08311D6D5E54571A0B08B3D228BA4E063093DF3B09232363D3A15331AD0A78EA6E014A73474320E01553B919C95D7797E2B9D321E668555F88F06696D1ADC50B377E87916096815A7FD636F860855D19AD41B0D8980BA848705C8AA98C67E1A3CC43FB16055BFA1C586B14DEE418CAE6AA2DD087C4AA57CA4CA33EE7672F512448B5AED2DBFD2E1114DD77312599EA66D14AB441F5487CF2A74C6476151274C9EDD8108337E8A9986A9BA04511333B963D6A783613A4C9EDD80D0447C4CC6B54E2FA87E010000FFFF0300504B0304140006000800000021000ACFE8CF3F0100005102000011000801646F
			m.lcFileBinary = m.lcFileBinary + 0h6350726F70732F636F72652E786D6C20A2040128A0000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000000000000000000000000000000000000007C92CB6AC3301045F785FE83D1DE969C904785ED405BB26AA0509794EE8434494CAD07925A277F5FD94E5C0742411BCDBD73E66A50B63ACA3AFA01EB2AAD7294260445A0B81695DAE7E8BD5CC74B1439CF9460B55690A31338B42AEEEF326E28D7165EAD36607D052E0A24E52837393A786F28C68E1F40329704870AE24E5BC97CB8DA3D368C7FB13DE00921732CC133C13CC32D303603119D91820F48F36DEB0E2038861A2428EF709AA4F8CFEBC14A77B3A153464E59F993096F3AC71DB305EFC5C17D74D5606C9A2669A65D8C903FC51F9B97B7EEA971A5DA5D7140452638E51698D7B6C8F0
			m.lcFileBinary = m.lcFileBinary + 0hF812165733E73761C7BB0AC4E329E8376A8277717B08882804A07DDC8BB29D3E3D976B54B43B8CC9439CCE4B4268773EDB9157FD6DA0BE20CF83FF272E5B2259941342A70B3A9B8D8817409FFBFA1314BF000000FFFF0300504B0304140006000800000021006F7E771280010000FE02000010000801646F6350726F70732F6170702E786D6C20A2040128A000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009C92416BE3301085EF85FD0F46F7464E589625C82AA569E9614B0349B367551EC7228A64345393F4D7776C93C6D9EDA93A8DE63D9E3E8DA46E0E7B9FB590D0C55088E9241719041B4B17B68578593F5CFF16199209A5F13140218E80E246FFB852CB141B48E400338E0858889AA8994B
			m.lcFileBinary = m.lcFileBinary + 0h89B686BDC109CB81952AA6BD21DEA6AD8C55E52C2CA27DDB432039CBF35F120E04A184F2BAF90C1443E2BCA5EF8696D1767CB8591F1B06D6EAB669BCB386F896FAC9D914315694DD1F2C7825C7A262BA15D8B7E4E8A87325C75BB5B2C6C31D07EBCA780425CF0DF508A61BDAD2B8845AB5346FC1524C19BA771EDB4C64AF06A1C329446B92338118ABB30D9BBEF60D52D27F63DA610D40A8241B86665F8EBDE3DAFDD4D3DEC0C5A5B10B184058B8445C3BF280CFD5D224FA82783A26EE1906DE0167D5F10D678EF9FA2BF349FF64FF7161872FCD3A2E0CC16976974DB5AA4D8292C77DD2CF0DF5C8634BBE0BB9AB4DD84279F2FC2F742FBD19BEB39ECE2639
			m.lcFileBinary = m.lcFileBinary + 0hAFFE814F3D25CF1F577F000000FFFF0300504B01022D001400060008000000210038088E4F60010000F20300001300000000000000000000000000000000005B436F6E74656E745F54797065735D2E786D6C504B01022D0014000600080000002100B5553023F50000004C0200000B000000000000000000000000006C0300005F72656C732F2E72656C73504B01022D00140006000800000021008D87DA70E00000002D0200001A0000000000000000000000000058060000786C2F5F72656C732F776F726B626F6F6B2E786D6C2E72656C73504B01022D0014000600080000002100A460FAFF52010000270200000F000000000000000000000000007808
			m.lcFileBinary = m.lcFileBinary + 0h0000786C2F776F726B626F6F6B2E786D6C504B01022D0014000600080000002100A083C4AEA3010000640300000D00000000000000000000000000F7090000786C2F7374796C65732E786D6C504B01022D0014000600080000002100E9A625B882060000531B00001300000000000000000000000000C50B0000786C2F7468656D652F7468656D65312E786D6C504B01022D0014000600080000002100075F38D31E010000C7010000180000000000000000000000000078120000786C2F776F726B7368656574732F7368656574312E786D6C504B01022D00140006000800000021000ACFE8CF3F010000510200001100000000000000000000000000CC13
			m.lcFileBinary = m.lcFileBinary + 0h0000646F6350726F70732F636F72652E786D6C504B01022D00140006000800000021006F7E771280010000FE020000100000000000000000000000000042160000646F6350726F70732F6170702E786D6C504B050600000000090009003E020000F81800000000
		OTHERWISE && XLS
			m.lcFileBinary = 0hD0CF11E0A1B11AE1000000000000000000000000000000003E000300FEFF0900060000000000000000000000010000000100000000000000001000001A00000001000000FEFFFFFF0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFDFFFFFF1C000000030000000400000005000000060000000700000008000000090000000A0000000B0000000C0000000D0000000E0000000F00000010000000110000001200000013000000140000001500000016000000170000001800000019000000FEFFFFFFFEFFFFFF1D000000FEFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFFFF52006F006F007400200045006E00740072007900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000016000500FFFFFFFFFFFFFFFF020000002008020000000000C00000000000004600000000000000000000000030C843AA2911C9011B000000800200000000000057006F0072006B0062006F006F006B000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001200020104000000FFFFFFFFFFFFFFFF00000000000000000000000000000000000000000000000000000000000000000000000002000000892F00
			m.lcFileBinary = m.lcFileBinary + 0h00000000000500530075006D006D0061007200790049006E0066006F0072006D006100740069006F006E000000000000000000000000000000000000000000000000000000280002010100000003000000FFFFFFFF00000000000000000000000000000000000000000000000000000000000000000000000000000000C800000000000000050044006F00630075006D0065006E007400530075006D006D0061007200790049006E0066006F0072006D006100740069006F006E000000000000000000000038000201FFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000000000000000000000000000000000000000000004000000E000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000908100000060500A91FCD07C100010006040000E1000200B004C10002000000E20000005C0070000200002020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202042000200B004610102000000C00100003D01020001009C0002001000190002000000120002000000130002000000AF0102000000BC01020000003D001200F0006900D5394A1F380000000000010058024000020000008D00020000002200020000
			m.lcFileBinary = m.lcFileBinary + 0h000E0002000100B70102000000DA000200000031001E00DC000000080090010000000200AA0701430061006C00690062007200690031001E00DC000000080090010000000200AA0701430061006C00690062007200690031001E00DC000000080090010000000200AA0701430061006C00690062007200690031001E00DC000000080090010000000200AA0701430061006C00690062007200690031001E00DC000000080090010000000200AA0701430061006C00690062007200690031001E00680101003800BC020000000200AA0701430061006D00620072006900610031001E002C0101003800BC020000000200AA0701430061006C00690062007200
			m.lcFileBinary = m.lcFileBinary + 0h690031001E00040101003800BC020000000200AA0701430061006C00690062007200690031001E00DC0001003800BC020000000200AA0701430061006C00690062007200690031001E00DC000000110090010000000200AA0701430061006C00690062007200690031001E00DC000000140090010000000200AA0701430061006C00690062007200690031001E00DC0000003C0090010000000200AA0701430061006C00690062007200690031001E00DC0000003E0090010000000200AA0701430061006C00690062007200690031001E00DC0001003F00BC020000000200AA0701430061006C00690062007200690031001E00DC0001003400BC02000000
			m.lcFileBinary = m.lcFileBinary + 0h0200AA0701430061006C00690062007200690031001E00DC000000340090010000000200AA0701430061006C00690062007200690031001E00DC0001000900BC020000000200AA0701430061006C00690062007200690031001E00DC0000000A0090010000000200AA0701430061006C00690062007200690031001E00DC000200170090010000000200AA0701430061006C00690062007200690031001E00DC0001000800BC020000000200AA0701430061006C00690062007200690031001E00DC000000090090010000000200AA0701430061006C0069006200720069001E041C000500170000222422232C2323305F293B5C28222422232C2323305C29
			m.lcFileBinary = m.lcFileBinary + 0h1E04210006001C0000222422232C2323305F293B5B5265645D5C28222422232C2323305C291E04220007001D0000222422232C2323302E30305F293B5C28222422232C2323302E30305C291E0427000800220000222422232C2323302E30305F293B5B5265645D5C28222422232C2323302E30305C291E0437002A003200005F282224222A20232C2323305F293B5F282224222A205C28232C2323305C293B5F282224222A20222D225F293B5F28405F291E042E0029002900005F282A20232C2323305F293B5F282A205C28232C2323305C293B5F282A20222D225F293B5F28405F291E043F002C003A00005F282224222A20232C2323302E30305F293B5F
			m.lcFileBinary = m.lcFileBinary + 0h282224222A205C28232C2323302E30305C293B5F282224222A20222D223F3F5F293B5F28405F291E0436002B003100005F282A20232C2323302E30305F293B5F282A205C28232C2323302E30305C293B5F282A20222D223F3F5F293B5F28405F29E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF20000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E000140000000000F5FF200000000000000000000000C020E0001400000000000100200000000000000000000002C020E000140005000000F5FF200000B400000000000000049F20E000140005
			m.lcFileBinary = m.lcFileBinary + 0h000000F5FF200000B40000000000000004AD20E000140005000000F5FF200000B40000000000000004AA20E000140005000000F5FF200000B40000000000000004AE20E000140005000000F5FF200000B400000000000000049B20E000140005000000F5FF200000B40000000000000004AF20E000140005000000F5FF200000B40000000000000004AC20E000140005000000F5FF200000B400000000000000049D20E000140005000000F5FF200000B400000000000000048B20E000140005000000F5FF200000B40000000000000004AE20E000140005000000F5FF200000B40000000000000004AC20E000140005000000F5FF200000B4000000000000
			m.lcFileBinary = m.lcFileBinary + 0h0004B320E000140015000000F5FF200000B400000000000000049E20E000140015000000F5FF200000B400000000000000049D20E000140015000000F5FF200000B400000000000000048B20E000140015000000F5FF200000B40000000000000004A420E000140015000000F5FF200000B40000000000000004B120E000140015000000F5FF200000B40000000000000004B420E000140015000000F5FF200000B40000000000000004BE20E000140015000000F5FF200000B400000000000000048A20E000140015000000F5FF200000B40000000000000004B920E000140015000000F5FF200000B40000000000000004A420E000140015000000F5FF20
			m.lcFileBinary = m.lcFileBinary + 0h0000B40000000000000004B120E000140015000000F5FF200000B40000000000000004B520E00014000B000000F5FF200000B40000000000000004AD20E00014000F000000F5FF200000941111970B970B00049620E000140011000000F5FF200000946666BF1FBF1F0004B720E000140005002B00F5FF200000F80000000000000000C020E000140005002900F5FF200000F80000000000000000C020E000140005002C00F5FF200000F80000000000000000C020E000140005002A00F5FF200000F80000000000000000C020E000140013000000F5FF200000F40000000000000000C020E00014000A000000F5FF200000B40000000000000004AA20E000
			m.lcFileBinary = m.lcFileBinary + 0h140007000000F5FF200000D400500000001F0000C020E000140008000000F5FF200000D400500000000B0000C020E000140009000000F5FF200000D400200000000F0000C020E000140009000000F5FF200000F40000000000000000C020E00014000D000000F5FF200000941111970B970B0004AF20E000140010000000F5FF200000D400600000001A0000C020E00014000C000000F5FF200000B40000000000000004AB20E000140005000000F5FF2000009C1111160B160B00049A20E00014000E000000F5FF200000941111BF1FBF1F00049620E000140005000900F5FF200000F80000000000000000C020E000140006000000F5FF200000F4000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000C020E000140014000000F5FF200000D4006100003E1F0000C020E000140012000000F5FF200000F40000000000000000C0207C0814007C080000000000000000000000003E00DED9EE787D082D007D080000000000000000000000000000000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000100000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000200000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000300000002000D00140003
			m.lcFileBinary = m.lcFileBinary + 0h0000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000400000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000500000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000600000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000700000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000800000002000D001400030000000100000030305C
			m.lcFileBinary = m.lcFileBinary + 0h293B5F282A0E000500027D082D007D080000000000000000000000000900000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000A00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000B00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000C00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000D00000002000D001400030000000100000030305C293B5F282A0E00050002
			m.lcFileBinary = m.lcFileBinary + 0h7D082D007D080000000000000000000000000E00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000000F00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000002B00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000002C00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000002D00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D0800000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000002E00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000003A00000002000D001400030000000100000030305C293B5F282A0E000500027D082D007D080000000000000000000000003B00000002000D001400030000000300000030305C293B5F282A0E000500017D0841007D080000000000000000000000003100000003000D001400030000000300000030305C293B5F282A0E000500020800140003000000040000003B5F28405F2920207D0841007D080000000000000000000000003200000003000D001400030000000300000030305C293B5F282A0E00050002
			m.lcFileBinary = m.lcFileBinary + 0h080014000300FF3F040000003B5F28405F2920207D0841007D080000000000000000000000003300000003000D001400030000000300000030305C293B5F282A0E000500020800140003003233040000003B5F28405F2920207D082D007D080000000000000000000000003400000002000D001400030000000300000030305C293B5F282A0E000500027D0841007D080000000000000000000000003000000003000D00140002000000006100FF30305C293B5F282A0E000500020400140002000000C6EFCEFF3B5F28405F2920207D0841007D080000000000000000000000002800000003000D001400020000009C0006FF30305C293B5F282A0E000500
			m.lcFileBinary = m.lcFileBinary + 0h020400140002000000FFC7CEFF3B5F28405F2920207D0841007D080000000000000000000000003700000003000D001400020000009C6500FF30305C293B5F282A0E000500020400140002000000FFEB9CFF3B5F28405F2920207D0891007D080000000000000000000000003500000007000D001400020000003F3F76FF30305C293B5F282A0E000500020400140002000000FFCC99FF3B5F28405F29202007001400020000007F7F7FFF202020202020202008001400020000007F7F7FFF202020202020202009001400020000007F7F7FFF00000000000000000A001400020000007F7F7FFF00000000000000007D0891007D0800000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h00003900000007000D001400020000003F3F3FFF30305C293B5F282A0E000500020400140002000000F2F2F2FF3B5F28405F29202007001400020000003F3F3FFF202020202020202008001400020000003F3F3FFF202020202020202009001400020000003F3F3FFF00000000000000000A001400020000003F3F3FFF00000000000000007D0891007D080000000000000000000000002900000007000D00140002000000FA7D00FF30305C293B5F282A0E000500020400140002000000F2F2F2FF3B5F28405F29202007001400020000007F7F7FFF202020202020202008001400020000007F7F7FFF202020202020202009001400020000007F7F7FFF00
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000A001400020000007F7F7FFF00000000000000007D0841007D080000000000000000000000003600000003000D00140002000000FA7D00FF30305C293B5F282A0E000500020800140002000000FF8001FF3B5F28405F2920207D0891007D080000000000000000000000002A00000007000D001400030000000000000030305C293B5F282A0E000500020400140002000000A5A5A5FF3B5F28405F29202007001400020000003F3F3FFF202020202020202008001400020000003F3F3FFF202020202020202009001400020000003F3F3FFF00000000000000000A001400020000003F3F3FFF00000000000000007D082D007D0800000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000003D00000002000D00140002000000FF0000FF30305C293B5F282A0E000500027D0891007D080000000000000000000000003800000007000D001400030000000100000030305C293B5F282A0E000500020400140002000000FFFFCCFF3B5F28405F2920200700140002000000B2B2B2FF20202020202020200800140002000000B2B2B2FF20202020202020200900140002000000B2B2B2FF00000000000000000A00140002000000B2B2B2FF00000000000000007D082D007D080000000000000000000000002F00000002000D001400020000007F7F7FFF30305C293B5F282A0E000500027D0855007D08000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h3C00000004000D001400030000000100000030305C293B5F282A0E000500020700140003000000040000003B5F28405F29202008001400030000000400000020202020202020207D0841007D080000000000000000000000002200000003000D001400030000000000000030305C293B5F282A0E000500020400140003000000040000003B5F28405F2920207D0841007D080000000000000000000000001000000003000D001400030000000100000030305C293B5F282A0E000500020400140003006566040000003B5F28405F2920207D0841007D080000000000000000000000001600000003000D001400030000000100000030305C293B5F282A0E00
			m.lcFileBinary = m.lcFileBinary + 0h050002040014000300CC4C040000003B5F28405F2920207D0841007D080000000000000000000000001C00000003000D001400030000000000000030305C293B5F282A0E000500020400140003003233040000003B5F28405F2920207D0841007D080000000000000000000000002300000003000D001400030000000000000030305C293B5F282A0E000500020400140003000000050000003B5F28405F2920207D0841007D080000000000000000000000001100000003000D001400030000000100000030305C293B5F282A0E000500020400140003006566050000003B5F28405F2920207D0841007D080000000000000000000000001700000003000D
			m.lcFileBinary = m.lcFileBinary + 0h001400030000000100000030305C293B5F282A0E00050002040014000300CC4C050000003B5F28405F2920207D0841007D080000000000000000000000001D00000003000D001400030000000000000030305C293B5F282A0E000500020400140003003233050000003B5F28405F2920207D0841007D080000000000000000000000002400000003000D001400030000000000000030305C293B5F282A0E000500020400140003000000060000003B5F28405F2920207D0841007D080000000000000000000000001200000003000D001400030000000100000030305C293B5F282A0E000500020400140003006566060000003B5F28405F2920207D084100
			m.lcFileBinary = m.lcFileBinary + 0h7D080000000000000000000000001800000003000D001400030000000100000030305C293B5F282A0E00050002040014000300CC4C060000003B5F28405F2920207D0841007D080000000000000000000000001E00000003000D001400030000000000000030305C293B5F282A0E000500020400140003003233060000003B5F28405F2920207D0841007D080000000000000000000000002500000003000D001400030000000000000030305C293B5F282A0E000500020400140003000000070000003B5F28405F2920207D0841007D080000000000000000000000001300000003000D001400030000000100000030305C293B5F282A0E00050002040014
			m.lcFileBinary = m.lcFileBinary + 0h0003006566070000003B5F28405F2920207D0841007D080000000000000000000000001900000003000D001400030000000100000030305C293B5F282A0E00050002040014000300CC4C070000003B5F28405F2920207D0841007D080000000000000000000000001F00000003000D001400030000000000000030305C293B5F282A0E000500020400140003003233070000003B5F28405F2920207D0841007D080000000000000000000000002600000003000D001400030000000000000030305C293B5F282A0E000500020400140003000000080000003B5F28405F2920207D0841007D080000000000000000000000001400000003000D001400030000
			m.lcFileBinary = m.lcFileBinary + 0h000100000030305C293B5F282A0E000500020400140003006566080000003B5F28405F2920207D0841007D080000000000000000000000001A00000003000D001400030000000100000030305C293B5F282A0E00050002040014000300CC4C080000003B5F28405F2920207D0841007D080000000000000000000000002000000003000D001400030000000000000030305C293B5F282A0E000500020400140003003233080000003B5F28405F2920207D0841007D080000000000000000000000002700000003000D001400030000000000000030305C293B5F282A0E000500020400140003000000090000003B5F28405F2920207D0841007D0800000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000000001500000003000D001400030000000100000030305C293B5F282A0E000500020400140003006566090000003B5F28405F2920207D0841007D080000000000000000000000001B00000003000D001400030000000100000030305C293B5F282A0E00050002040014000300CC4C090000003B5F28405F2920207D0841007D080000000000000000000000002100000003000D001400030000000000000030305C293B5F282A0E000500020400140003003233090000003B5F28405F2920209302120010000D0000323025202D20416363656E743192084D0092080000000000000000000001041EFF0D0032003000250020002D0020004100
			m.lcFileBinary = m.lcFileBinary + 0h6300630065006E00740031000000030001000C0007046566DBE5F1FF05000C0007010000000000FF25000500029302120011000D0000323025202D20416363656E743292084D00920800000000000000000000010422FF0D0032003000250020002D00200041006300630065006E00740032000000030001000C0007056566F2DDDCFF05000C0007010000000000FF25000500029302120012000D0000323025202D20416363656E743392084D00920800000000000000000000010426FF0D0032003000250020002D00200041006300630065006E00740033000000030001000C0007066566EAF1DDFF05000C0007010000000000FF250005000293021200
			m.lcFileBinary = m.lcFileBinary + 0h13000D0000323025202D20416363656E743492084D0092080000000000000000000001042AFF0D0032003000250020002D00200041006300630065006E00740034000000030001000C0007076566E5E0ECFF05000C0007010000000000FF25000500029302120014000D0000323025202D20416363656E743592084D0092080000000000000000000001042EFF0D0032003000250020002D00200041006300630065006E00740035000000030001000C0007086566DBEEF3FF05000C0007010000000000FF25000500029302120015000D0000323025202D20416363656E743692084D00920800000000000000000000010432FF0D0032003000250020002D
			m.lcFileBinary = m.lcFileBinary + 0h00200041006300630065006E00740036000000030001000C0007096566FDE9D9FF05000C0007010000000000FF25000500029302120016000D0000343025202D20416363656E743192084D0092080000000000000000000001041FFF0D0034003000250020002D00200041006300630065006E00740031000000030001000C000704CC4CB8CCE4FF05000C0007010000000000FF25000500029302120017000D0000343025202D20416363656E743292084D00920800000000000000000000010423FF0D0034003000250020002D00200041006300630065006E00740032000000030001000C000705CC4CE6B9B8FF05000C0007010000000000FF25000500
			m.lcFileBinary = m.lcFileBinary + 0h029302120018000D0000343025202D20416363656E743392084D00920800000000000000000000010427FF0D0034003000250020002D00200041006300630065006E00740033000000030001000C000706CC4CD7E4BCFF05000C0007010000000000FF25000500029302120019000D0000343025202D20416363656E743492084D0092080000000000000000000001042BFF0D0034003000250020002D00200041006300630065006E00740034000000030001000C000707CC4CCCC0DAFF05000C0007010000000000FF2500050002930212001A000D0000343025202D20416363656E743592084D0092080000000000000000000001042FFF0D0034003000
			m.lcFileBinary = m.lcFileBinary + 0h250020002D00200041006300630065006E00740035000000030001000C000708CC4CB6DDE8FF05000C0007010000000000FF2500050002930212001B000D0000343025202D20416363656E743692084D00920800000000000000000000010433FF0D0034003000250020002D00200041006300630065006E00740036000000030001000C000709CC4CFCD5B4FF05000C0007010000000000FF2500050002930212001C000D0000363025202D20416363656E743192084D00920800000000000000000000010420FF0D0036003000250020002D00200041006300630065006E00740031000000030001000C000704323395B3D7FF05000C0007000000FFFFFF
			m.lcFileBinary = m.lcFileBinary + 0hFF2500050002930212001D000D0000363025202D20416363656E743292084D00920800000000000000000000010424FF0D0036003000250020002D00200041006300630065006E00740032000000030001000C0007053233D99795FF05000C0007000000FFFFFFFF2500050002930212001E000D0000363025202D20416363656E743392084D00920800000000000000000000010428FF0D0036003000250020002D00200041006300630065006E00740033000000030001000C0007063233C2D69AFF05000C0007000000FFFFFFFF2500050002930212001F000D0000363025202D20416363656E743492084D0092080000000000000000000001042CFF0D
			m.lcFileBinary = m.lcFileBinary + 0h0036003000250020002D00200041006300630065006E00740034000000030001000C0007073233B2A1C7FF05000C0007000000FFFFFFFF25000500029302120020000D0000363025202D20416363656E743592084D00920800000000000000000000010430FF0D0036003000250020002D00200041006300630065006E00740035000000030001000C000708323393CDDDFF05000C0007000000FFFFFFFF25000500029302120021000D0000363025202D20416363656E743692084D00920800000000000000000000010434FF0D0036003000250020002D00200041006300630065006E00740036000000030001000C0007093233FAC090FF05000C000700
			m.lcFileBinary = m.lcFileBinary + 0h0000FFFFFFFF250005000293020C002200070000416363656E74319208410092080000000000000000000001041DFF070041006300630065006E00740031000000030001000C00070400004F81BDFF05000C0007000000FFFFFFFF250005000293020C002300070000416363656E743292084100920800000000000000000000010421FF070041006300630065006E00740032000000030001000C0007050000C0504DFF05000C0007000000FFFFFFFF250005000293020C002400070000416363656E743392084100920800000000000000000000010425FF070041006300630065006E00740033000000030001000C00070600009BBB59FF05000C000700
			m.lcFileBinary = m.lcFileBinary + 0h0000FFFFFFFF250005000293020C002500070000416363656E743492084100920800000000000000000000010429FF070041006300630065006E00740034000000030001000C00070700008064A2FF05000C0007000000FFFFFFFF250005000293020C002600070000416363656E74359208410092080000000000000000000001042DFF070041006300630065006E00740035000000030001000C00070800004BACC6FF05000C0007000000FFFFFFFF250005000293020C002700070000416363656E743692084100920800000000000000000000010431FF070041006300630065006E00740036000000030001000C0007090000F79646FF05000C000700
			m.lcFileBinary = m.lcFileBinary + 0h0000FFFFFFFF25000500029302080028000300004261649208390092080000000000000000000001011BFF03004200610064000000030001000C0005FF0000FFC7CEFF05000C0005FF00009C0006FF25000500029302100029000B000043616C63756C6174696F6E92088100920800000000000000000000010216FF0B00430061006C00630075006C006100740069006F006E000000070001000C0005FF0000F2F2F2FF05000C0005FF0000FA7D00FF250005000206000E0005FF00007F7F7FFF010007000E0005FF00007F7F7FFF010008000E0005FF00007F7F7FFF010009000E0005FF00007F7F7FFF010093020F002A000A0000436865636B2043656C
			m.lcFileBinary = m.lcFileBinary + 0h6C92087F00920800000000000000000000010217FF0A0043006800650063006B002000430065006C006C000000070001000C0005FF0000A5A5A5FF05000C0007000000FFFFFFFF250005000206000E0005FF00003F3F3FFF060007000E0005FF00003F3F3FFF060008000E0005FF00003F3F3FFF060009000E0005FF00003F3F3FFF0600930204002B8003FF92082000920800000000000000000000010503FF050043006F006D006D00610000000000930204002C8006FF92082800920800000000000000000000010506FF090043006F006D006D00610020005B0030005D0000000000930204002D8004FF92082600920800000000000000000000010504
			m.lcFileBinary = m.lcFileBinary + 0hFF0800430075007200720065006E006300790000000000930204002E8007FF92082E00920800000000000000000000010507FF0C00430075007200720065006E006300790020005B0030005D0000000000930215002F001000004578706C616E61746F7279205465787492084700920800000000000000000000010235FF10004500780070006C0061006E00610074006F0072007900200054006500780074000000020005000C0005FF00007F7F7FFF2500050002930209003000040000476F6F6492083B0092080000000000000000000001011AFF040047006F006F0064000000030001000C0005FF0000C6EFCEFF05000C0005FF0000006100FF250005
			m.lcFileBinary = m.lcFileBinary + 0h000293020E00310009000048656164696E67203192084700920800000000000000000000010310FF0900480065006100640069006E006700200031000000030005000C00070300001F497DFF250005000207000E00070400004F81BDFF050093020E00320009000048656164696E67203292084700920800000000000000000000010311FF0900480065006100640069006E006700200032000000030005000C00070300001F497DFF250005000207000E000704FF3FA8C0DEFF050093020E00330009000048656164696E67203392084700920800000000000000000000010312FF0900480065006100640069006E006700200033000000030005000C0007
			m.lcFileBinary = m.lcFileBinary + 0h0300001F497DFF250005000207000E000704323395B3D7FF020093020E00340009000048656164696E67203492083900920800000000000000000000010313FF0900480065006100640069006E006700200034000000020005000C00070300001F497DFF250005000293020A003500050000496E70757492087500920800000000000000000000010214FF050049006E007000750074000000070001000C0005FF0000FFCC99FF05000C0005FF00003F3F76FF250005000206000E0005FF00007F7F7FFF010007000E0005FF00007F7F7FFF010008000E0005FF00007F7F7FFF010009000E0005FF00007F7F7FFF01009302100036000B00004C696E6B6564
			m.lcFileBinary = m.lcFileBinary + 0h2043656C6C92084B00920800000000000000000000010218FF0B004C0069006E006B00650064002000430065006C006C000000030005000C0005FF0000FA7D00FF250005000207000E0005FF0000FF8001FF060093020C0037000700004E65757472616C9208410092080000000000000000000001011CFF07004E00650075007400720061006C000000030001000C0005FF0000FFEB9CFF05000C0005FF00009C6500FF250005000293020400008000FF92083300920800000000000000000000010100FF06004E006F0072006D0061006C000000020005000C0007010000000000FF25000500029302090038000400004E6F746592086200920800000000
			m.lcFileBinary = m.lcFileBinary + 0h00000000000001020AFF04004E006F00740065000000050001000C0005FF0000FFFFCCFF06000E0005FF0000B2B2B2FF010007000E0005FF0000B2B2B2FF010008000E0005FF0000B2B2B2FF010009000E0005FF0000B2B2B2FF010093020B0039000600004F757470757492087700920800000000000000000000010215FF06004F00750074007000750074000000070001000C0005FF0000F2F2F2FF05000C0005FF00003F3F3FFF250005000206000E0005FF00003F3F3FFF010007000E0005FF00003F3F3FFF010008000E0005FF00003F3F3FFF010009000E0005FF00003F3F3FFF0100930204003A8005FF9208240092080000000000000000000001
			m.lcFileBinary = m.lcFileBinary + 0h0505FF0700500065007200630065006E0074000000000093020A003B000500005469746C659208310092080000000000000000000001030FFF05005400690074006C0065000000020005000C00070300001F497DFF250005000193020A003C00050000546F74616C92084D00920800000000000000000000010319FF050054006F00740061006C000000040005000C0007010000000000FF250005000206000E00070400004F81BDFF010007000E00070400004F81BDFF0600930211003D000C00005761726E696E67205465787492083F0092080000000000000000000001020BFF0C005700610072006E0069006E00670020005400650078007400000002
			m.lcFileBinary = m.lcFileBinary + 0h0005000C0005FF0000FF0000FF25000500028E0858008E080000000000000000000090000000110011005400610062006C0065005300740079006C0065004D0065006400690075006D0039005000690076006F0074005300740079006C0065004C0069006700680074003100360060010200000085000E00032E0000000006005368656574319A0818009A0800000000000000000000010000000000000001000000A3081000A30800000000000000000000000000008C00040001000100C1010800C10100001DEB0100FC0008000000000000000000FF00020008006308160063080000000000000000000016000000040000000200960810009608000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000042E501009B0810009B0800000000000000000000010000008C0810008C0800000000000000000000000000000A0000000908100000061000A91FCD07C1000100060400000B021000000000000000000000000000152F00000D00020001000C00020064000F000200010011000200000010000800FCA9F1D24D62503F5F00020001002A00020000002B00020000008200020001008000080000000000000000002502040000002C0181000200C104140000001500000083000200000084000200000026000800666666666666E63F27000800666666666666E63F28000800000000000000E83F29000800000000000000E83FA10022000000
			m.lcFileBinary = m.lcFileBinary + 0h2C01010001000100040042E50100333333333333D33F333333333333D33F74009C0826009C0800000000000000000000000000000000000000000000000000003C33000000000000000055000200080000020E0000000000000000000000000000003E021200B606000000004000000000000000000000008B0810008B0800000000000000000000000002001D000F0003000000000000010000000000000067081700670800000000000000000000020001FFFFFFFF034400000A0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000200000003000000FEFFFFFF050000000600000007000000FEFFFFFF09000000FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFF0000060002000000000000000000000000000000000001000000E0859FF2F94F6810AB9108002B27B3D930000000980000000700000001000000400000000400000048000000080000005400000012000000600000000C000000780000000D00000084000000130000009000000002000000E40400001E00000004000000000000001E00000004000000000000001E000000100000004D6963726F736F667420457863656C004000000000C07C0D23D9C601400000000086FCA92911C90103000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000FEFF000006000200000000000000000000000000000000000100000002D5CDD59C2E1B10939708002B2CF9AE30000000B000000008000000010000004800000017000000500000000B000000580000001000000060000000130000006800000016000000700000000D000000780000000C0000008B00000002000000E40400000300000000000C000B000000000000000B000000000000000B000000000000000B000000000000001E1000000100000007000000536865657431000C100000020000001E0000
			m.lcFileBinary = m.lcFileBinary + 0h000B000000576F726B73686565747300030000000100000000000000000000000000000000000000000000000000000000000000000000000000010043006F006D0070004F0062006A0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012000200FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000000000000000000000000000000000000800000072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000FF
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0hFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100FEFF030A0000FFFFFFFF2008020000000000C000000000000046260000004D6963726F736F6674204F666669636520457863656C203230303320576F726B736865657400060000004269666638000E000000457863656C2E53686565742E3800F439B271000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
			m.lcFileBinary = m.lcFileBinary + 0h0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
		ENDCASE
		IF !EMPTY(m.lcFileBinary)
			IF STRTOFILE(m.lcFileBinary, m.tcExcelFile, 0) > 0
				m.llReturn = .T.
			ENDIF
		ENDIF
	ENDIF
	RETURN m.llReturn
ENDFUNC

**************************************************
*!* Code used to create hex binary of worksheet templates
**************************************************
*!*	LOCAL lcBinary, lcCode
*!*	_cliptext = STRCONV(FILETOSTR(GETFILE()),15)
*!*	m.lcBinary = _ClipText
*!*	m.lcCode = ""
*!*	DO WHILE !EMPTY(m.lcBinary)
*!*		m.lcCode = m.lcCode + CHR(9) + iif(Empty(m.lcCode), "m.FileBinary = ", "m.FileBinary = m.FileBinary + ") + "0h" + LEFT(m.lcBinary, 510) + CHR(13) + CHR(10)
*!*		m.lcBinary = SUBSTR(m.lcBinary, 511)
*!*	ENDDO
*!*	_cliptext = m.lcCode


