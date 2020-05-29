*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.MA.VAL.DUI(A.INFO)
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.MA.VAL.DUI.b
* Descripción: Validacion de DUI en ATM
*-----------------------------------------------------------------------------
* Modification History : 
*-----------------------------------------------------------------------------
* Version	Autor	Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		vburgos	15.05.19	Version inicial
*-------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.SLV.ITEMS.MONEDA.AZUL
$INSERT I_F.EB.SLV.STM.FAVORITES
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------	
	FN.ITEMS.MAZUL  = 'F.EB.SLV.ITEMS.MONEDA.AZUL'
	F.ITEMS.MAZUL   = ''
	FN.FAVS  		= 'F.EB.SLV.STM.FAVORITES'
	F.FAVS   		= ''
	
	;*Obtener Parametros desde el Enquiry
	LOCATE "DOCUMENTO" IN D.FIELDS<1> SETTING LN.POS THEN
    	Y.DOC = D.RANGE.AND.VALUE<LN.POS>
    END
    
    LOCATE "TELEFONO" IN D.FIELDS<1> SETTING LN.POS THEN
    	Y.TELEFONO = D.RANGE.AND.VALUE<LN.POS>
    END
    
    ;*Y.DOC='019461263'
    Y.TELEFONO='79194550'
    
RETURN

OPENFILE:
	CALL OPF(FN.ITEMS.MAZUL, F.ITEMS.MAZUL)
	CALL OPF(FN.FAVS , F.FAVS)
RETURN


PROCESS:

	NOMBRE	=''
	TIPO.DOC=''
	DOC		=''
	TELEFONO=''
	FILA 	=''
	CMND	=''
	IF Y.TELEFONO='' THEN
		CMND = 'SELECT ' : FN.ITEMS.MAZUL : ' WITH DOCUMENTO EQ "' : Y.DOC : '" AND ESTADO EQ "AUTORIZADO"'					
	END
	ELSE 
		CMND = 'SELECT ' : FN.ITEMS.MAZUL : ' WITH MOVIL EQ "' : Y.TELEFONO : '" AND ESTADO EQ "AUTORIZADO"'			
	END
	CALL EB.READLIST(CMND, F.ITEMS.MAZUL.READ, '', NO.ITEMS, ERROR.READ)
	
	IF NO.ITEMS GT 0 THEN
		Y.ITEM=F.ITEMS.MAZUL.READ<1>
		CALL F.READ(FN.ITEMS.MAZUL, Y.ITEM, R.ITEM, F.ITEMS.MAZUL, ERROR.READ)
		
		NOMBRE		= R.ITEM<EB.IMA.NOMBRE>
		TIPO.DOC	= R.ITEM<EB.IMA.TIPO.DOCUMENTO>
		DOC			= R.ITEM<EB.IMA.DOCUMENTO>
		TELEFONO	= R.ITEM<EB.IMA.MOVIL>
		
		FILA = NOMBRE:'*':TIPO.DOC:'*':DOC:'*':TELEFONO
	END
	ELSE
		IF Y.TELEFONO='' THEN
			CMND = 'SELECT ' : FN.FAVS  : ' WITH DOCUMENT EQ "' : Y.DOC : '"'			
		END
		ELSE 
			CMND = 'SELECT ' : FN.ITEMS.MAZUL : ' WITH PHONE EQ "' : Y.TELEFONO : '"'			
		END
		CALL EB.READLIST(CMND, F.FAVS.READ, '', NO.FAVS, ERROR.READ)
		
		IF NO.FAVS GT 0 THEN
			Y.FAV=F.FAVS.READ<1>
			CALL F.READ(FN.FAVS, Y.FAV, R.FAVS, F.FAVS, ERROR.READ)
			
			NOMBRE		= R.FAVS<EB.SLV83.NAME>
			TIPO.DOC	= R.FAVS<EB.SLV83.DOCUMENT.TYPE>
			DOC			= R.FAVS<EB.SLV83.DOCUMENT>
			TELEFONO	= R.FAVS<EB.SLV83.PHONE>
			
			FILA = NOMBRE:'*':TIPO.DOC:'*':DOC:'*':TELEFONO
			CRT FILA
		END
		
	END
	
	A.INFO<-1> = FILA
RETURN 



END
