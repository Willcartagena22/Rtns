*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.VAL.TCIB.FT.CTA
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Nombre: SLV.VAL.FT.TCIB.CTA
* Descripción:  Rutina para validar si la cuenta a acreditar se encuentra activa 
*				
*-----------------------------------------------------------------------------
* Version	Paquete								Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0											RCortes		30.10.14	Initial	  
*-----------------------------------------------------------------------------
	$INSERT I_COMMON
	$INSERT I_EQUATE
	$INSERT I_F.ACCOUNT
	$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

	GOSUB INIT
    GOSUB READ
    GOSUB PROCESS
    RETURN

*-----------------------------------------------------------------------------    
INIT:
*-----------------------------------------------------------------------------

	FN.ACC	= 'F.ACCOUNT'
	F.ACC	= ''
	
	FN.ACC.H	= 'F.ACCOUNT$HIS'
	F.ACC.H	= ''
	
RETURN 

*-----------------------------------------------------------------------------    
READ:
*-----------------------------------------------------------------------------

	CALL OPF(FN.ACC, F.ACC)
	CALL OPF(FN.ACC.H, F.ACC.H)
	
RETURN 

*-----------------------------------------------------------------------------    
PROCESS:
*-----------------------------------------------------------------------------
	Y.CTA.ACRE = COMI
	CALL F.READ(FN.ACC, Y.CTA.ACRE, R.ACC, F.ACC, ERR.ACC)
	IF R.ACC EQ '' THEN
		CALL F.READ.HISTORY(FN.ACC.H, Y.CTA.ACRE, R.ACC.H, F.ACC.H, ERR.ACC.H)
		ARR.ID = R.ACC.H<AC.RECORD.STATUS>
		IF ARR.ID EQ 'CLOSED' THEN
			ETEXT = 'EB-TCIB.FT.CTA.CAN'
	        CALL STORE.END.ERROR
	    END
	END
	
RETURN 

END
