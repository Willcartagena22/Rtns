*-----------------------------------------------------------------------------
* <Rating>-181</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.CUS.MAS.GEN
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
    $INSERT I_F.EB.SLV.VALIDACIONES.MASIVOS
*-----------------------------------------------------------------------------
    GOSUB INICIAR
    GOSUB VALIDAR.FECHAS
    GOSUB VALIDAR.DATOS
    GOSUB IMPRIMIR

INICIAR:
    FN.EB.SLV.ITEMS.MASIVOS='F.EB.SLV.ITEMS.MASIVOS'
    F.EB.SLV.ITEMS.MASIVOS=''
    FN.SLV.DUI.CUSTOMER.CNT = 'F.SLV.DUI.CUSTOMER.CNT'
    F.SLV.DUI.CUSTOMER.CNT  = ''
    R.SLV.DUI.CUSTOMER.CNT  = ''
    SLV.DUI.CUSTOMER.CNT.ER = ''
    FN.SLV.NIT.CUSTOMER.CNT = 'F.SLV.NIT.CUSTOMER.CNT'
    F.SLV.NIT.CUSTOMER.CNT  = ''
    R.SLV.NIT.CUSTOMER.CNT  = ''
    SLV.NIT.CUSTOMER.CNT.ER = ''
    FN.EB.SLV.VALIDACIONES.MASIVOS='F.EB.SLV.VALIDACIONES.MASIVOS'
	F.EB.SLV.VALIDACIONES.MASIVOS=''
    RETURN

ABRIR:
    CALL OPF(FN.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT)
    CALL OPF(FN.SLV.NIT.CUSTOMER.CNT, F.SLV.NIT.CUSTOMER.CNT)
    CALL OPF(FN.EB.SLV.VALIDACIONES.MASIVOS, F.EB.SLV.VALIDACIONES.MASIVOS)
    
    RETURN


VALIDAR.FECHAS:
R.NEW(EB.SLV3.RESERVADO.1)='Listo'
    STRERR=''
;*Validando Fechas
    V_DATE_CONTENT=R.NEW(EB.SLV3.LEGAL.ISS.DATE.1)
    V_NAME_FIELD="LEGAL.ISS.DATE.1"
    GOSUB VALIDA.FECHAS


    V_DATE_CONTENT=R.NEW(EB.SLV3.LEGAL.ISS.DATE.2)
    V_NAME_FIELD="LEGAL.ISS.DATE.2"
    GOSUB VALIDA.FECHAS


    V_DATE_CONTENT=R.NEW(EB.SLV3.LEGAL.EXP.DATE)
    V_NAME_FIELD="LEGAL.EXP.DATE"
    GOSUB VALIDA.FECHA.EXP


    V_DATE_CONTENT=R.NEW(EB.SLV3.DATE.OF.BIRTH)
    V_NAME_FIELD="DATE.OF.BIRTH"
    GOSUB VALIDA.FECHAS


    V_DATE_CONTENT=R.NEW(EB.SLV3.EMPLOYMENT.START)
    V_NAME_FIELD="EMPLOYMENT.START"
    GOSUB VALIDA.FECHAS


    RETURN


VALIDAR.DATOS:
*; Validar DUI

    V_CONTENT=R.NEW(EB.SLV3.LF.DUI)
    V_NAME_FIELD="LF.DUI"
	GOSUB VALIDAR.EXIST.DUI
	
    V_CONTENT=R.NEW(EB.SLV3.LF.DUI)
    GOSUB VALIDA.DUI.SECUENCIAL

    V_CONTENT=R.NEW(EB.SLV3.LF.DUI)

    GOSUB DIG.VAL.DUI


    V_CONTENT=R.NEW(EB.SLV3.LEGAL.ID.2)
    V_NAME_FIELD="LEGAL.ID.2"
    GOSUB VALIDA.DUI.SECUENCIAL

    V_CONTENT=R.NEW(EB.SLV3.LEGAL.ID.2)
    V_NAME_FIELD="LEGAL.ID.2"
    GOSUB DIG.VAL.DUI
    
    V_CONTENT=R.NEW(EB.SLV3.LEGAL.ID.2)
    V_NAME_FIELD="LEGAL.ID.2"
    GOSUB VALIDAR.EXIST.DUI


;*Validar NIT
    V_CONTENT=R.NEW(EB.SLV3.LF.NIT)
    V_NAME_FIELD="LF.NIT"
	GOSUB VALIDAR.EXIST.NIT
	
    V_CONTENT=R.NEW(EB.SLV3.LF.NIT)
    V_NAME_FIELD="LF.NIT"
    GOSUB DIG.VAL.NIT


    V_CONTENT=R.NEW(EB.SLV3.LEGAL.ID.1)
    V_NAME_FIELD="LEGAL.ID.1"
    GOSUB DIG.VAL.NIT
    
    V_CONTENT=R.NEW(EB.SLV3.LEGAL.ID.1)
    V_NAME_FIELD="LEGAL.ID.1"
    GOSUB VALIDAR.EXIST.NIT


;*Validar tl�fonos
    V_CONTENT=R.NEW(EB.SLV3.PHONE.1)
    V_NAME_FIELD="PHONE.1"
    GOSUB VALIDA.LONG.TEL


    V_CONTENT=R.NEW(EB.SLV3.SMS.1)
    V_NAME_FIELD="SMS.1"
    GOSUB VALIDA.LONG.TEL


    V_CONTENT=R.NEW(EB.SLV3.OFF.PHONE)
    V_NAME_FIELD="OFF.PHONE"
    GOSUB VALIDA.LONG.TEL

;*Validar Montos
    V_CONTENT=R.NEW(EB.SLV3.SALARY)
    V_NAME_FIELD="SALARY"
    GOSUB VALIDAR.DECIMAL


    V_CONTENT=R.NEW(EB.SLV3.ANNUAL.BONUS)
    V_NAME_FIELD="ANNUAL.BONUS"
    GOSUB VALIDAR.DECIMAL


    V_CONTENT=R.NEW(EB.SLV3.LF.MON.EST.DEP)
    V_NAME_FIELD="LF.MON.EST.DEP"
    GOSUB VALIDAR.DECIMAL


    V_CONTENT=R.NEW(EB.SLV3.LF.MON.EST.ING)
    V_NAME_FIELD="LF.MON.EST.ING"
    GOSUB VALIDAR.DECIMAL


;*Primer nombre
    V_CONTENT=R.NEW(EB.SLV3.NAME.1)
    V_NAME_FIELD="NAME.1"
    GOSUB VALIDA.ALFABETICOS


;*seg nombre
    V_CONTENT=R.NEW(EB.SLV3.NAME.2)
    V_NAME_FIELD="NAME.2"
    GOSUB VALIDA.ALFABETICOS

;*seg nombre
    V_CONTENT=R.NEW(EB.SLV3.GIVEN.NAMES)
    V_NAME_FIELD= "GIVEN.NAMES"
    GOSUB VALIDA.ALFABETICOS

;*PRI APE

    V_CONTENT=R.NEW(EB.SLV3.TEXT)
    V_NAME_FIELD="TEXT"
    GOSUB VALIDA.ALFABETICOS


;*SEG APE
    V_CONTENT=R.NEW(EB.SLV3.FAMILY.NAME)
    V_NAME_FIELD="FAMILY.NAME"
    GOSUB VALIDA.ALFABETICOS

;*TERC APE
    V_CONTENT=R.NEW(EB.SLV3.PREVIOUS.NAME)
    V_NAME_FIELD="PREVIOUS.NAME"
    GOSUB VALIDA.ALFABETICOS


;*CONOCIDO POR
    V_CONTENT=R.NEW(EB.SLV3.INTRODUCER)
    V_NAME_FIELD="INTRODUCER"
    GOSUB VALIDA.ALFABETICOS


    V_CONTENT=R.NEW(EB.SLV3.EMPLOYERS.NAME)
    V_NAME_FIELD="EMPLOYERS.NAME"
    GOSUB VALIDA.ALFABETICOS


    RETURN


DIG.VAL.DUI:

    CALL SLV.S.VALIDATE.DUI(V_CONTENT,Y.RESULT,Y.ERROR.CODE)
    IF Y.ERROR.CODE THEN
        STRERR='DUI Incorrecto: ':V_CONTENT
        GOSUB CRT_ERROR
    END
    RETURN

DIG.VAL.NIT:
    CALL SLV.S.VALIDATE.NIT(V_CONTENT,Y.RESULT,Y.ERROR.CODE)
    IF Y.ERROR.CODE THEN
        STRERR='NIT Incorrecto: ':V_CONTENT
        GOSUB CRT_ERROR
    END
    RETURN

VALIDA.FECHAS:
    IF  V_DATE_CONTENT NE '' AND (LEFT(V_DATE_CONTENT,4) < '1901' OR LEFT(V_DATE_CONTENT,4) > '2999') THEN
        STRERR ='Fecha no valida: ':V_DATE_CONTENT
        GOSUB CRT_ERROR

    END

    IF  V_CONTENT NE '' AND ISDIGIT( V_DATE_CONTENT) EQ 0 THEN
        STRERR ='Fecha no valida: ':V_DATE_CONTENT
        GOSUB CRT_ERROR
    END

    RETURN

VALIDA.FECHA.EXP:
FECHAHOY=TODAY
    IF V_DATE_CONTENT LE FECHAHOY THEN
        STRERR ='Fecha de expiracion no valida:  ':V_DATE_CONTENT
        GOSUB CRT_ERROR
    END
    RETURN


VALIDAR.DECIMAL:
    IF V_CONTENT NE '' AND NUM( V_CONTENT) EQ 0 THEN
        STRERR ='Tipo de dato incorrecto, Numericos con decimales: ':V_CONTENT
        GOSUB CRT_ERROR

    END
    RETURN

VALIDA.ENTEROS:
    IF  V_CONTENT NE '' AND ISDIGIT( V_CONTENT) EQ 0 THEN
        STRERR ='Tipo de dato incorrecto, Solo Enteros: ':V_CONTENT
        GOSUB CRT_ERROR
    END
    RETURN

VALIDA.ALFABETICOS:
    CHANGE '�' TO 'N' IN V_CONTENT
    CHANGE 'Ñ' TO 'N' IN V_CONTENT

    CHANGE 'ñ' TO 'N' IN V_CONTENT
    CHANGE CHAR(465) TO 'N' IN V_CONTENT
    CHANGE CHAR(497) TO 'N' IN V_CONTENT

    IF V_CONTENT MATCHES "0A'.'0A" AND V_CONTENT NE '' THEN
        STRERR ='Tipo de dato incorrecto, Solo Alfabeticos: ':V_CONTENT
        GOSUB CRT_ERROR

    END

    CHANGE ' ' TO 'A' IN V_CONTENT

    IF V_CONTENT NE '' AND ISALPHA(V_CONTENT) EQ 0  THEN
        STRERR ='Tipo de dato incorrecto, Solo Alfabeticos: ':V_CONTENT
        GOSUB CRT_ERROR
    END
    RETURN

VALIDA.DUI.SECUENCIAL:
;* N�mero de dui secuencial no valido
    LEN_NODOC = LEN(TRIM(V_CONTENT))
    FOR N.I=1 TO LEN_NODOC
        N_VALUE+=V_CONTENT[N.I,1]
    NEXT N.I
    N_INDEX=N_VALUE/LEN_NODOC

    IF  V_CONTENT NE '' AND V_CONTENT[1,1] EQ N_INDEX THEN
        STRERR ='Numero de documento no valido ':V_CONTENT
        GOSUB CRT_ERROR
    END
    
 VALIDAR.EXIST.DUI:
 	
    CALL F.READ(FN.SLV.DUI.CUSTOMER.CNT, V_CONTENT, R.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT, SLV.DUI.CUSTOMER.CNT.ER)
    IF R.SLV.DUI.CUSTOMER.CNT THEN
        STRERR ='Ya existe cliente registrado con DUI ': V_CONTENT
        GOSUB CRT_ERROR
   END
 RETURN   
 
  VALIDAR.EXIST.NIT: 	
    CALL F.READ(FN.SLV.NIT.CUSTOMER.CNT, V_CONTENT, R.SLV.NIT.CUSTOMER.CNT, F.SLV.NIT.CUSTOMER.CNT, SLV.NIT.CUSTOMER.CNT.ER)
    IF R.SLV.NIT.CUSTOMER.CNT THEN
        STRERR ='Ya existe cliente registrado con NIT ': V_CONTENT
        GOSUB CRT_ERROR
   END
 RETURN   
  
    
    RETURN

VALIDA.LONG.TEL:

    IF  V_CONTENT NE '' AND LEN(TRIM(V_CONTENT))<>8 THEN
        STRERR ='Numero de telefono no valido ':V_CONTENT
        GOSUB CRT_ERROR
    END

    IF  V_CONTENT NE '' AND ISDIGIT( V_CONTENT) EQ 0 THEN
        STRERR =' Invalido. Favor ingrese 8 numeros enteros.'
        GOSUB CRT_ERROR

    END
    RETURN

CRT_ERROR:

	ARRAY:=V_NAME_FIELD:"*":STRERR:"~"
	R.NEW(EB.SLV3.RESERVADO.1)='Rechazado'
	
RETURN

ESCRIBIR.ARCHIVO:
	    DIR.NAME= 'MASIVOS'
	    R.ID   = 'Validaciones.txt'
	    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
	    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
	    END
	    CLOSESEQ SEQ.PTR
    RETURN
    
    
IMPRIMIR:
AA=ARRAY

 TRAMA.MULTIVALOR.X2 = CHANGE(AA,'~',VM)

    CANTIDAD.REGISTROS.MULTIVALOR.X2 = DCOUNT(TRAMA.MULTIVALOR.X2,VM)-1 
    FOR J = 1 TO CANTIDAD.REGISTROS.MULTIVALOR.X2

        SEGMENTO.X2 = FIELD(TRAMA.MULTIVALOR.X2,VM,J)
        MARKER.FIELD = FIELD(SEGMENTO.X2,'*',1)
        MARKER.VALUE = FIELD(SEGMENTO.X2,'*',2)
		REC.VAL<EB.SLV30.NOMBRE.CAMPO,J> = MARKER.FIELD
		REC.VAL<EB.SLV30.VALIDACION,J> = MARKER.VALUE
		REC.VAL<EB.SLV30.ESTADO,J> = 'ACTIVO'
		REC.VAL<EB.SLV30.FECHA> = TODAY
		IDV=ID.NEW
		CALL F.WRITE(FN.EB.SLV.VALIDACIONES.MASIVOS, IDV, REC.VAL)

     
    NEXT J


RETURN
