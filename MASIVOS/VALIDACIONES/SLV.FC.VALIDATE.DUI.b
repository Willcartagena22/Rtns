*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.FC.VALIDATE.DUI
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.STM.FAVORITES
$INSERT I_F.EB.EXTERNAL.USER
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN
 INIT:
	FN.STM.FAVORITES	= 'F.EB.SLV.STM.FAVORITES'
	F.STM.FAVORITES		= ''
	CALL OPF(FN.STM.FAVORITES, F.STM.FAVORITES)
 RETURN
 PROCESS:
*DEBUG
*Y.DOC = '039207043'
*Y.TYPE = 'DUI'
	Y.DOC 	= R.NEW(EB.SLV83.DOCUMENT)
	Y.TYPE 	= R.NEW(EB.SLV83.DOCUMENT.TYPE)
	
	IF Y.DOC EQ '' OR Y.TYPE EQ '' THEN
		RETURN
	END
	
	IF Y.TYPE EQ 'DUI' THEN
		CALL SLV.S.VALIDATE.DUI(Y.DOC,Y.RESULT,Y.ERROR.CODE)
		
		IF Y.ERROR.CODE THEN
			ETEXT = Y.ERROR.CODE
			 CALL STORE.END.ERROR
			RETURN
		END
	END
 RETURN
END