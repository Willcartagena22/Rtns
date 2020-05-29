*-----------------------------------------------------------------------------
* <Rating>-46</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.AML.VERIF.CUS.REME(INFO.CUS)
*-----------------------------------------------------------------------------
* Obtener cliente con verificacion de Cumplimiento AML
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		Jonas		01.01.18	Inicial
*			Jonas		22.02.18	Agregar estado rechazado para registros.
* 2.0		Jonas		09.03.18	Revision de control de estados de registro.
* 3.0		Jonas		09.04.18	Modificar mensaje por rechazo de AML.
* 4.0		Jonás 		22.04.18 	Guardar referencias en aplicacion EB.SLV.CONTROL.REMESAS.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.CONTROL.REMESAS

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
    Y.ESTADO.DISP = ''
RETURN
*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    FN.CUS		 = 'F.CUSTOMER'
    F.CUS		 = ''
    FN.CUSNAU	 = 'F.CUSTOMER$NAU'
    F.CUSNAU	 = ''

    CALL OPF(FN.CUS, F.CUS)
    CALL OPF(FN.CUSNAU, F.CUSNAU)
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	CALL F.READ(FN.CUSNAU, Y.REM.ID.CUS, R.CUS, F.CUSNAU, ERR.NAU)
	IF ERR.NAU THEN
		CALL F.READ(FN.CUS, Y.REM.ID.CUS, R.CUS, F.CUS, ERR)
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
	END ELSE 
		STR.ARR  = ''
		STR.ARR  = Y.REM.ID.CUS : DELIM
		STR.ARR := "-" 			: DELIM
		STR.ARR := STAT.DEL 	: DELIM
        STR.ARR := STAT.REJ 	: DELIM
        STR.ARR := STAT.REJ.MSG
		INFO.CUS<-1> = STR.ARR
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
END
