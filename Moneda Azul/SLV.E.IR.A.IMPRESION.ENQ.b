*-----------------------------------------------------------------------------
* <Rating>-22</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.IR.A.IMPRESION.ENQ
*-----------------------------------------------------------------------------
*
* Nombre: SLV.E.IR.A.IMPRESION.ENQ
* Descripción: Rutina para Lanzar Enquiry de Impresion de Comprobantes de Caja
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	24.01.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_GTS.COMMON
$INSERT I_F.SLV.GEN.PARAM
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.TELLER
$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

GOSUB INIT

BEGIN CASE
	CASE APPLICATION EQ 'TELLER.FINANCIAL.SERVICES'		 
	    IF OFS$OPERATION EQ 'PROCESS' THEN
	    	;*Lanzar Enquiry solo si el Autorizador es el Inputter
			IF FIELD(R.NEW(TFS.INPUTTER),'_',2) NE OPERATOR THEN
				RETURN
			END
			GOSUB PROCESS
		END
	
	CASE APPLICATION EQ 'FUNDS.TRANSFER'
		;*Lanzar Enquiry solo si el Autorizador es el Inputter
		IF FIELD(R.NEW(FT.INPUTTER),'_',2) NE OPERATOR THEN
			RETURN
		END		
		GOSUB PROCESS
END CASE
RETURN

	INIT:
		Y.ID      = ID.NEW
		Y.VERSION = APPLICATION : PGM.VERSION
		Y.ENQ     = 'SLV.E.NOF.TFS.IMPRESION'
		Y.PARAMS  = 'FECHA EQ ' : TODAY
	RETURN
	
	PROCESS:		
		;*Lanzar Enquiry
		Y.CMD = 'ENQ ' : Y.ENQ : ' ' : Y.PARAMS
		CALL EB.SET.NEW.TASK(Y.CMD)
	RETURN
END
