*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.REVIEW.AML.STATUS
*-----------------------------------------------------------------------------
* Nombre: SLV.E.REVIEW.AML.STATUS
* Descripción: Cargar ENQUIRY de revisión de estado de registro de AML Screen.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		Jonas		07.03.18	Version inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
	Y.ENQ.NAME 		 = 'ENQ SLV.E.AML.VERIF.CUS.REME'
	Y.ENQ.NAME.REMIT = 'ENQ SLV.E.AML.VERIF.CUS.REME.REMIT'

	Y.PARAM    = ' CUS.ID EQ ' : ID.NEW
	Y.TASK     = Y.ENQ.NAME : Y.PARAM
	
	Y.APP 		  = APPLICATION
    Y.PGM.VERSION = PGM.VERSION
    Y.VERSION     = Y.APP : Y.PGM.VERSION
    
    EQU REMESA.CUST  TO 'CUSTOMER,SLV.INPUT.REMESA'
    EQU REMESA.REMIT TO 'CUSTOMER,SLV.INPUT.REMESA.REMIT'
	
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	IF  OFS$OPERATION EQ 'PROCESS' THEN
	 	IF Y.VERSION EQ REMESA.CUST THEN
			Y.TASK     = Y.ENQ.NAME : Y.PARAM
		END
		IF Y.VERSION EQ REMESA.REMIT THEN
			Y.TASK     = Y.ENQ.NAME.REMIT : Y.PARAM
		END

		;*Enquiry para AML Screen
		CALL EB.SET.NEW.TASK(Y.TASK)
	END
RETURN
END
