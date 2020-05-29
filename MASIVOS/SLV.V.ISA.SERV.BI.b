*------------------------------------------------------------------------------
* <Rating>-69</Rating>
*------------------------------------------------------------------------------
SUBROUTINE SLV.V.ISA.SERV.BI
*------------------------------------------------------------------------------
* Nombre:		SLV.V.ISA.SERV.BI
* Descripcion: 	Rutina para validar si el cliente ha aceptado el servicio de Banca en Linea
*				en la declaracion jurada (app Customer)
*------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*------------------------------------------------------------------------------
* 1.0		RCORTES		29.06.2016	Version inicial
* 1.1		JMCRUZ		13.02.2017	Se agregan validaciones específicas para empresas
*------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
*------------------------------------------------------------------------------        
	GOSUB INIT
	GOSUB GET_TIMEDATE
	GOSUB OPEN
	GOSUB PROCESS
	GOSUB GET_TIMEDATE
RETURN
*------------------------------------------------------------------------------
INIT:
	FN.CUS = 'F.CUSTOMER'
	F.CUS = ''
	
	EQU CUST TO 'CUSTOMER'
	Y.CUS = R.NEW(AA.ARR.ACT.CUSTOMER)
******************************************************
* DEBUG:
*
*	Y.CUS = '101519'
******************************************************
	EQU RTN TO 'SLV_V_ISA_SERV_BI'
	Y.TXT = ''
	AV = ''
RETURN
*------------------------------------------------------------------------------
;* Identificación de la Ejecución Actual
GET_TIMEDATE:
*	Y.TXT = RTN : ' = ' : TIMEDATE()
*	CRT RTN : ' = ' : TIMEDATE()
*	GOSUB WRITE_LOG_FILE
RETURN
*------------------------------------------------------------------------------
;* Archivo para Revisión de Errores
WRITE_LOG_FILE:
*	DIR.NAME = 'CHQ.OUT'
*	R.ID = RTN : '_' : TODAY : '.txt'
*
*	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
*		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
*		END
*	CLOSESEQ SEQ.PTR 
RETURN
*------------------------------------------------------------------------------
;* Impresión de Errores
CRT_ERROR:
	AS = 1
	ETEXT = Y.TXT
*	CRT 'CRT_ERROR = ' : Y.TXT
	CALL STORE.END.ERROR
RETURN
*------------------------------------------------------------------------------
OPEN:
	CALL OPF(FN.CUS, F.CUS)
RETURN
*------------------------------------------------------------------------------
PROCESS:
	CALL F.READ(FN.CUS, Y.CUS, R.CUS, F.CUS, CUS.ERR)

	IF R.CUS THEN
		CALL GET.LOC.REF(CUST, 'LF.AML.USA.TCIB', Y.POS)
		Y.SER.IB = R.CUS<EB.CUS.LOCAL.REF, Y.POS>

		IF Y.SER.IB THEN
			IF Y.SER.IB NE 'SI' THEN
				AF = AA.ARR.ACT.CUSTOMER
				Y.TXT = 'EB-SLV.ISA.SERV.BI'
				GOSUB CRT_ERROR
			END

		END ELSE
			AF = AA.ARR.ACT.CUSTOMER
		    Y.TXT = 'EB-SLV.ISA.SERV.BI'
			GOSUB CRT_ERROR
		END		
	END
RETURN    
*------------------------------------------------------------------------------
END
*------------------------------------------------------------------------------
