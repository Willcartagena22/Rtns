*-----------------------------------------------------------------------------
* <Rating>-36</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.AML.VERIF.REMIT.REME(INFO.CUS)
*-----------------------------------------------------------------------------
* Obtener estado de cliente por verificacion de remitente en Cumplimiento AML.
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		Jonas		28.02.18	Inicial
* 2.0		Jonas		09.03.18	Revision de control de estados de registro.
* 3.0		Jonas		09.04.18	Modificar mensaje por rechazo de AML.
* 4.0		Jonas		13.04.18	Revisión de registro DISPO.ITEMS por rechazo de remitente.
* 5.0		Jonas		25.06.18	Agregar logs de control.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.CUSTOMER
$INSERT I_F.DISPO.ITEMS
$INSERT I_F.COMPANY
$INSERT I_F.EB.SLV.CONTROL.REMESAS
$INSERT I_F.EB.SLV.GLOBAL.PARAM

	;*INI Log de Control
	GOSUB LOGGER
	TEXTO.ARCHIVO = 'INICIO SLV.E.AML.VERIF.REMIT.REME-->'
	GOSUB ESCRIBIR.ARCHIVO

	GOSUB INIT
	GOSUB OPENFILE
	GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
    LOCATE "CUS.ID" IN D.FIELDS<1> SETTING POS THEN
    	Y.ID.CUS = D.RANGE.AND.VALUE<POS>
    END
	TEXTO.ARCHIVO = 'INIT Y.ID.CUS-->': Y.ID.CUS
	GOSUB ESCRIBIR.ARCHIVO

	;*Obtener informacion de cliente
	GOSUB GET.REF
    Y.REM.ID.CUS = REM.ID.CUS
    IF Y.REM.ID.CUS EQ '' OR Y.REM.ID.CUS EQ 0 THEN
    	Y.REM.ID.CUS = Y.ID.CUS
    	REM.ID.CUS = Y.ID.CUS
    END
    ERR 	 = ''
    R.CUS 	 = ''
    DELIM	 = '*'
    STAT	 = 'AUTH'
    STAT.DEL = 'DEL'
    STAT.REJ = 'REJECTED'
	;*Según correo lider de proyecto del 06/04/2018
    STAT.REJ.MSG = 'Remesa Familiar no puede ser pagada por politica interna'
    STAT.POS = 'POSSIBLE'
    Y.ESTADO.DISP = ''
    Y.ESTADO 	  = ''
    Y.EST.INAO    = 'INAO'
RETURN
*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    FN.CUS		 = 'F.CUSTOMER'
    F.CUS		 = ''
    FN.CUSNAU	 = 'F.CUSTOMER$NAU'
    F.CUSNAU	 = ''
    FN.DISPOH	 = 'F.DISPO.ITEMS$HIS'
    F.DISPOH	 = ''
    FN.COMPANY	 = 'F.COMPANY'
    F.COMPANY	 = ''

    CALL OPF(FN.CUS, F.CUS)
    CALL OPF(FN.CUSNAU, F.CUSNAU)
    CALL OPF(FN.DISPOH, F.DISPOH)
    CALL OPF(FN.COMPANY, F.COMPANY)
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	;*Validar estado de verificacion AML de remitente
	Y.INVALIDO = 0
	CALL F.READ(FN.CUSNAU, Y.REM.ID.CUS, R.CUS, F.CUSNAU, ERR.NAU)
	IF ERR.NAU THEN
		CALL F.READ(FN.CUS, Y.REM.ID.CUS, R.CUS, F.CUS, ERR)
	END	
	
	TEXTO.ARCHIVO = 'PROCESS Y.REM.ID.CUS-->': Y.REM.ID.CUS
	GOSUB ESCRIBIR.ARCHIVO
	TEXTO.ARCHIVO = 'PROCESS R.CUS-->': R.CUS
	GOSUB ESCRIBIR.ARCHIVO
	
	GOSUB VALIDA.REMIT
	TEXTO.ARCHIVO = 'PROCESS Y.INVALIDO-->': Y.INVALIDO
	GOSUB ESCRIBIR.ARCHIVO

	IF Y.INVALIDO EQ 1 THEN
		STR.ARR  = ''
		STR.ARR  = Y.REM.ID.CUS : DELIM
		STR.ARR := "-" 			: DELIM
		STR.ARR := STAT.REJ 	: DELIM
		STR.ARR := STAT.REJ 	: DELIM		
        STR.ARR := STAT.REJ.MSG
		INFO.CUS<-1> = STR.ARR
		TEXTO.ARCHIVO = 'PROCESS-RECHAZO INFO.CUS-->': INFO.CUS
		GOSUB ESCRIBIR.ARCHIVO

		RETURN
	END	

	IF R.CUS THEN
		Y.NOMBRE = TRIM(TRIM(R.CUS<EB.CUS.NAME.1>):" ":TRIM(R.CUS<EB.CUS.NAME.2>):" ":TRIM(R.CUS<EB.CUS.TEXT>):" ":TRIM(R.CUS<EB.CUS.PREVIOUS.NAME>))
		Y.ESTADO = R.CUS<EB.CUS.RECORD.STATUS>
		IF Y.ESTADO EQ '' THEN
			Y.ESTADO = STAT 
		END
		IF Y.ESTADO EQ STAT THEN
			Y.ESTADO.DISP = 'APPROVED'
		END
		STR.ARR  = ''
		STR.ARR  = Y.REM.ID.CUS : DELIM
		STR.ARR := Y.NOMBRE 	: DELIM
		STR.ARR := Y.ESTADO 	: DELIM
		STR.ARR := Y.ESTADO.DISP: DELIM		
		STR.ARR := ''
		INFO.CUS<-1> = STR.ARR
		TEXTO.ARCHIVO = 'PROCESS-APPROVED INFO.CUS-->': INFO.CUS
		GOSUB ESCRIBIR.ARCHIVO
		
	END ELSE 
		STR.ARR  = ''
		STR.ARR  = Y.REM.ID.CUS : DELIM
		STR.ARR := "-" 			: DELIM
		STR.ARR := STAT.DEL 	: DELIM
		STR.ARR := STAT.REJ 	: DELIM        
        STR.ARR := STAT.REJ.MSG
		INFO.CUS<-1> = STR.ARR
		TEXTO.ARCHIVO = 'PROCESS-RECHAZO2 INFO.CUS-->': INFO.CUS
		GOSUB ESCRIBIR.ARCHIVO

	END
RETURN
*-----------------------------------------------------------------------------
VALIDA.REMIT:
*-----------------------------------------------------------------------------
    ERR 	 = ''
    R.DISPO	 = ''
    Y.ESTADO.DISP = ''
    Y.ID.AGE = ID.COMPANY

    CALL F.READ(FN.COMPANY, Y.ID.AGE, R.CO, F.COMPANY, ERR.CO)
	Y.CODE.COMP = R.CO<EB.COM.MNEMONIC>
	SELECT.CMD = "SELECT ":FN.DISPOH:" WITH EVENT.REFERENCE EQ '":Y.ID.CUS:"'"
	CALL EB.READLIST(SELECT.CMD, ID.LIST, '', NO.OF.RECS, ERR.DISP)
	Y.LAST.ID.UPD = ''
	Y.FECHA 	  = ''
	Y.LAST.DATE   = ''
	ID.LIST = SORT(ID.LIST)
	TEXTO.ARCHIVO = 'VALIDA.REMIT SELECT.CMD-->': SELECT.CMD : ' NO.OF.RECS=':NO.OF.RECS
	GOSUB ESCRIBIR.ARCHIVO
	
	IF NO.OF.RECS GT 0 THEN
		LOOP
			REMOVE ID.DISP FROM ID.LIST SETTING POS.DES
	    WHILE ID.DISP:POS.DES
	    	;*Obtener registo DISPO.ITEMS
	        CALL F.READ(FN.DISPOH, ID.DISP, R.DISPOH, F.DISPOH, ERR)
	        Y.FECHA = R.DISPOH<DISP.ITM.TIME>
	        Y.CODE.AGE = FIELD(FIELD(ID.DISP,"*",2),";",1) 
	        IF Y.CODE.AGE EQ Y.CODE.COMP AND Y.FECHA GE Y.LAST.DATE THEN
	        	Y.LAST.DATE = Y.FECHA
	        	IF ID.DISP GT Y.LAST.ID THEN
	        		Y.LAST.ID = ID.DISP
	        	END
	        END
	    REPEAT
		Y.ID.DISPO = Y.LAST.ID
		TEXTO.ARCHIVO = 'VALID.REMIT Y.ID.DISPO-->': Y.ID.DISPO
		GOSUB ESCRIBIR.ARCHIVO
		
		CALL F.READ(FN.DISPOH, Y.ID.DISPO,R.DISPOH, F.DISPOH, ERR)
*		Y.ESTADO = R.DISPOH<DISP.ITM.DISPO.STATUS>
		Y.ESTADO.DISP = R.DISPOH<DISP.ITM.DISPO.STATUS>
		Y.ESTADO = R.CUS<EB.CUS.RECORD.STATUS>
		TEXTO.ARCHIVO = 'VALID.REMIT R.DISPOH-->': R.DISPOH : ' Y.ESTADO.DISP=':Y.ESTADO.DISP : ' Y.ESTADO=':Y.ESTADO 
		GOSUB ESCRIBIR.ARCHIVO
		
		IF Y.ESTADO EQ '' THEN
			Y.ESTADO = STAT
		END
		IF Y.ESTADO.DISP EQ STAT.REJ THEN
			Y.INVALIDO = 1
		END
		IF Y.ESTADO.DISP EQ STAT.POS AND R.CUS EQ "" THEN
			Y.INVALIDO = 1
		END
		IF Y.ESTADO.DISP EQ STAT.POS AND Y.ESTADO EQ STAT THEN
			Y.INVALIDO = 1
		END
	END
RETURN
*-----------------------------------------------------------------------------
GET.REF:
*-----------------------------------------------------------------------------
	Y.ID.REF   = ''
	REMESA.REC = ''
	;*Obtener referencias de EB.SLV.CONTROL.REMESA
	CALL SLV.UTIL.CONTROL.REMESA(Y.ID.REF, REMESA.REC)
	REM.ID.CUS = REMESA.REC<EB.SLV51.CLIENTE> 
	;*Definicion de estado P=Pendiente, A=Pagada, E=Error
RETURN
*-----------------------------------------------------------------------------
LOGGER:
*-----------------------------------------------------------------------------
	FN.GBL       = 'F.EB.SLV.GLOBAL.PARAM'
	F.GBL        = ''
	CALL OPF(FN.GBL, F.GBL)
	
	;*Extraer Parametrizacion de Log
	CALL F.READ(FN.GBL, 'LOG.REMESA', R.GBL, F.GBL, E.GBL)
	DIR.NAME   = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
	R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
	LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
RETURN
*-----------------------------------------------------------------------------
ESCRIBIR.ARCHIVO:
*-----------------------------------------------------------------------------
	;*Si el parametro de Log esta Activo Escribir Archivo
	IF LOG.ACTIVO EQ 'Y' THEN
		OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
		WRITESEQ '[' : LEFT(TIMEDATE(),8) : ']  ' : TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		END
		CLOSESEQ SEQ.PTR
	END
RETURN
END
