*-----------------------------------------------------------------------------
* <Rating>-76</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.SELECT.MAZUL(A.INFO)
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.SELECT.MAZUL.b
* Descripción: Validacion de token y telefono para selecionar si registro fue
*			   generado de Moneda Azul Masivo o individual
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Version	Autor	Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		Jonas	26.03.19	Version inicial
* 1.1		Jonas	10.04.19	Reinicar variable Y.ERROR.TOKEN en LOOP
* 1.2		Jonas	02.09.19	Validar registro con HIT y mostrar mensaje.
* 1.3		Jonas	02.09.19	Validar limite mensual y diario por beneficiario.
* 2.0		Jonas	12.09.19	Eliminar limite mensual y diario por beneficiario
*								(Por indicaciones de funcional se hará en autorizacion de carga de archivo)
* 2.1		Jonas	16.09.19    Cambiar estado RESTRINGIDO por NO.APLICADA
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.SLV.ITEMS.MONEDA.AZUL
$INSERT I_F.EB.SLV.MASTER.MONEDA.AZUL
$INSERT I_F.EB.ERROR
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
	FN.MAS.MAZUL  	= 'F.EB.SLV.MASTER.MONEDA.AZUL'
	F.MAS.MAZUL   	= ''
	FN.EB.ERR   	= 'F.EB.ERROR'
	F.EB.ERR   		= ''
	Y.PROCESAR 		= 'N'
	Y.MASIVO   		= 'MASIVO'
	Y.ID.TOKEN 		= 'TMMA'
	A.INFO 			= ''
	
	;*Obtener Parametros desde el Enquiry
	LOCATE "TELEFONO" IN D.FIELDS<1> SETTING LN.POS THEN
    	Y.TELEFONO = D.RANGE.AND.VALUE<LN.POS>
    END
    
    LOCATE "TOKEN" IN D.FIELDS<1> SETTING LN.POS THEN
    	Y.TOKEN = D.RANGE.AND.VALUE<LN.POS>
    END
    Y.TXT.NO.APLICA = "NO.APLICADA"
    
*    Y.TELEFONO = '72345577'
*    Y.TOKEN    = '6095825854'
RETURN
*-----------------------------------------------------------------------------	
OPENFILE:
*-----------------------------------------------------------------------------	
	CALL OPF(FN.ITEMS.MAZUL, F.ITEMS.MAZUL)
	CALL OPF(FN.MAS.MAZUL, F.MAS.MAZUL)
	CALL OPF(FN.EB.ERR,F.EB.ERR)
RETURN
*-----------------------------------------------------------------------------	
PROCESS:
*-----------------------------------------------------------------------------
	;*Obtener texto de mensajes a mostrar
	CALL F.READ(FN.EB.ERR, "EB-TXNEXT.NO.X.POL.INT", R.EB.ERR, F.EB.ERR, ERR)
	Y.TXT.POLITICA = FIELD(R.EB.ERR<EB.ERR.ERROR.MSG><1,1>,SM,2)
	
	R.EB.ERR = ''
	;*Obtener texto de mensajes a mostrar
	CALL F.READ(FN.EB.ERR, "EB-TXT.AML.SCREEN.BENEF", R.EB.ERR, F.EB.ERR, ERR)
	Y.TXT.AML.SCR = FIELD(R.EB.ERR<EB.ERR.ERROR.MSG><1,1>,SM,2)
	
	R.EB.ERR = ''
	;*Obtener texto de mensajes a mostrar limites
	CALL F.READ(FN.EB.ERR, "EB-LIMIT.AMONT.MAZUL", R.EB.ERR, F.EB.ERR, ERR)
	Y.TXT.LIMITE = R.EB.ERR<EB.ERR.ERROR.MSG><1,1>

	Y.MAZUL.MASIVO = 'N'	
	;*Buscar Favorito a partir del Numero de Telefono
	SELECT.CMD = "SELECT " : FN.ITEMS.MAZUL : " WITH MOVIL EQ " : Y.TELEFONO : " AND ESTADO EQ AUTORIZADO NO.APLICADA"
	CALL EB.READLIST(SELECT.CMD, LIST.ID, '', NO.REC, ERR.ITEMS)

	;*Validar Token asociado a Items
	LOOP
		REMOVE ID.ITEMS FROM LIST.ID SETTING POS
	WHILE ID.ITEMS NE ''
		Y.DOC.BENEF = ''
		Y.MONTO     = 0
		;*Obtener Registro del Items
		CALL F.READ(FN.ITEMS.MAZUL, ID.ITEMS, R.ITEMS, F.ITEMS.MAZUL, ERR.ITEMS)
		
		;*Generar Token según Items
		GOSUB VALID.TOKEN
		
		IF Y.MAZUL.MASIVO EQ 'S' THEN
			;*Se elimina validacion de monto, esto se hará en la autorizacion de carga de archivo
			;*Validar limites de monto y transacciones
*			Y.DOC.BENEF = R.ITEMS<EB.IMA.DOCUMENTO>
*			Y.MONTO     = R.ITEMS<EB.IMA.MONTO>
*			CALL SLV.V.AMT.BENEF.MAZUL(Y.DOC.BENEF, Y.MONTO, OUT.MENSUAL, OUT.DIARIO)
			BREAK
		END
	REPEAT
	IF Y.MAZUL.MASIVO EQ 'S' THEN
		IF Y.ERROR.TOKEN EQ '' THEN
			;*Se elimina validacion de monto, esto se hará en la autorizacion de carga de archivo		
			Y.RESTRINGIDO = 0
*			IF  OUT.MENSUAL EQ 0 OR OUT.DIARIO EQ 0 THEN
*				A.INFO = Y.TXT.LIMITE
*				Y.RESTRINGIDO = 1
*			END ELSE
				IF R.ITEMS<EB.IMA.ESTADO> EQ Y.TXT.NO.APLICA THEN
					A.INFO = Y.TXT.POLITICA
					Y.RESTRINGIDO = 1
				END
				IF R.ITEMS<EB.IMA.SCRENEO.AML> EQ "" OR R.ITEMS<EB.IMA.SCRENEO.AML> EQ 0 THEN
					A.INFO = Y.TXT.AML.SCR
					Y.RESTRINGIDO = 1
				END
				IF Y.RESTRINGIDO EQ 0 THEN
					;*Obtener registo de moneda azul masivo
					A.INFO = ID.ITEMS:'*':Y.SEEDS:'*':Y.TOKEN
					CALL SLV.E.ITEMS.MAZUL(A.INFO)
				END
*			END
		END ELSE
		   A.INFO = Y.ERROR.TOKEN
		END
	END
	;*Validar que exista Telefono Ingresado
	IF Y.MAZUL.MASIVO EQ "N" THEN
		;*Moneda Azul Individual
		CALL SLV.E.STM.INFO(A.INFO)
	END
	CRT A.INFO
RETURN
*-----------------------------------------------------------------------------	
VALID.TOKEN:
*-----------------------------------------------------------------------------	
	;*Obtener parametros para generacion de Token de registro de Items
	;*Obtener ID Master
	Y.SEEDS = ''
	Y.TOKEN.VERIFY = ''
	Y.ERROR.TOKEN  = ''
	Y.ID.MASTER = FIELDS(ID.ITEMS,".",1)
	CALL F.READ(FN.MAS.MAZUL, Y.ID.MASTER, R.MASTER, F.MAS.MAZUL, ERR.MASTER)
	Y.FECHA.FROM  = R.MASTER<EB.SLV44.FECHA.ULT.AUT>
	Y.TRAN.TIME   = R.ITEMS<EB.IMA.TRAN.TIME>
	Y.SEEDS  = Y.TELEFONO   : VM : R.ITEMS<EB.IMA.DOCUMENTO> : VM
	Y.SEEDS := Y.FECHA.FROM : VM : Y.TRAN.TIME
	CALL SLV.UTIL.STM.TOKEN(Y.ID.TOKEN, Y.SEEDS, Y.TOKEN.VERIFY, Y.ERROR.TOKEN)
	;*Validar si token ingresado existe para moneda azul masivo
	Y.TOKEN.VALIDATE = FIELD(Y.TOKEN.VERIFY,VM,1)
	IF Y.TOKEN EQ Y.TOKEN.VALIDATE THEN
		Y.MAZUL.MASIVO = 'S'
	END
RETURN

DESENCRIPTAR:
		TOKEN.NUEVO=FIELD(Y.TOKEN, @VM, 1)
		ORIGEN=SUBSTRINGS(TOKEN.NUEVO,1,1)
		TOKEN3=SUBSTRINGS(TOKEN.NUEVO,2,1)
		CEL8=SUBSTRINGS(TOKEN.NUEVO,3,1)
		CEL3=SUBSTRINGS(TOKEN.NUEVO,4,1)
		TOKEN1=SUBSTRINGS(TOKEN.NUEVO,5,1)
		CEL7=SUBSTRINGS(TOKEN.NUEVO,6,1)
		CEL2=SUBSTRINGS(TOKEN.NUEVO,7,1)
		TOKEN4=SUBSTRINGS(TOKEN.NUEVO,8,1)
		CEL5=SUBSTRINGS(TOKEN.NUEVO,9,1)
		CEL1=SUBSTRINGS(TOKEN.NUEVO,10,1)
		TOKEN2=SUBSTRINGS(TOKEN.NUEVO,11,1)
		CEL6=SUBSTRINGS(TOKEN.NUEVO,12,1)
		CEL4=SUBSTRINGS(TOKEN.NUEVO,13,1)
		
		TELEFONO=CEL1:CEL2:CEL3:CEL4:CEL5:CEL6:CEL7:CEL8

		
RETURN
END
