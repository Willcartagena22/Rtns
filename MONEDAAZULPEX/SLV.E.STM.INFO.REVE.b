*-----------------------------------------------------------------------------
* <Rating>-101</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.STM.INFO.REVE(A.INFO)
*-----------------------------------------------------------------------------
*
* Nombre: SLV.E.STM.INFO
* Descripción: Rutina para extraccion de informacion para el Pago STM
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* OCornejo	10.10.2017	Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.EB.SLV.STM.FAVORITES
$INSERT I_F.EB.ERROR
$INSERT I_F.ACCOUNT
$INSERT I_F.USER
$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.LOCK = 'F.AC.LOCKED.EVENTS$HIS'
		F.LOCK  = ''
		FN.FAV  = 'F.EB.SLV.STM.FAVORITES'
		F.FAV   = ''
		FN.ERR  = 'F.EB.ERROR'
		F.ERR   = ''	
		FN.ACC  = 'F.ACCOUNT'
		F.ACC   = ''	
		FN.USER = 'F.USER'
		F.USER  = ''
		FN.FT 	= 'F.FUNDS.TRANSFER'
		F.FT 	= ''
		Y.PROCESAR = 'N'
		
		
		;*Obtener Parametros desde el Enquiry
		LOCATE "TELEFONO" IN D.FIELDS<1> SETTING LN.POS THEN
	    	Y.TELEFONO = D.RANGE.AND.VALUE<LN.POS>
	    END
	    
	    LOCATE "TOKEN" IN D.FIELDS<1> SETTING LN.POS THEN
	    	Y.TOKEN = D.RANGE.AND.VALUE<LN.POS>
	    END
	    
	    ;*Y.TELEFONO = '79233215'
	    ;*Y.TOKEN    = '5264548322'
	RETURN
	
	OPENFILE:
		CALL OPF(FN.LOCK, F.LOCK)
		CALL OPF(FN.FAV, F.FAV)
		CALL OPF(FN.ERR, F.ERR)
		CALL OPF(FN.ACC, F.ACC)
		CALL OPF(FN.USER, F.USER)
		CALL OPF(FN.FT, F.FT)
	RETURN
	
	PROCESS:
		;*Buscar Favorito a partir del Numero de Telefono
		STMT.FAVORITOS = "SELECT " : FN.FAV : " WITH PHONE EQ " : Y.TELEFONO : " AND STATUS EQ ACTIVO"
		CALL EB.READLIST(STMT.FAVORITOS, LIST.FAVORITOS, '', NO.REC, SYSTEM.RETURN.CODE)
		
		;*Validar que exista Telefono Ingresado
		IF NO.REC EQ 0 THEN
			Y.ERROR = 'EB-SLV.STM.PHONE.NO.EXISTS'
			Y.REPLA = Y.TELEFONO
			GOSUB GET.DESCRIPT.ERROR
			RETURN
		END

		;*Iterar Lista de Clientes que tienen Asociado el Numero de Telefono
		LOOP
			REMOVE FAVORITO FROM LIST.FAVORITOS SETTING POS
		WHILE FAVORITO NE ''
			;*Obtener Registro del Favorito
			CALL F.READ(FN.FAV, FAVORITO, R.FAV, F.FAV, E.FAV)
			
			;*Bloqueos por STM
			GOSUB STMT.LOCKS.STM
			IF Y.PROCESAR EQ 'S' THEN
				BREAK
			END
		REPEAT
		
		;*Error en Generacion de Token para Validacion o en Token Ingresado
		IF Y.PROCESAR EQ 'N' THEN
			IF Y.ERROR EQ '' THEN
				Y.ERROR = 'EB-SLV.STM.TOKEN.NO.VALID'			
			END			
			Y.REPLA = Y.TOKEN
			GOSUB GET.DESCRIPT.ERROR
			RETURN
		END
		
		;*Obtener Cliente de la Cuenta
		CALL CACHE.READ(FN.ACC, R.LOCK<AC.LCK.ACCOUNT.NUMBER>, R.ACC, E.ACC)
		Y.CUS = R.ACC<AC.CUSTOMER>
		CALL SLV.UTIL.GET.NOM.CLIENTE(Y.CUS)
		
		;*Completar Datos de Enquiry con la informacion del Bloqueo
		A.INFO  = FIELD(LOCK.ID,";",1)                       : '*' 
		A.INFO := R.LOCK<AC.LCK.ACCOUNT.NUMBER> : '*'
		A.INFO := Y.CUS                         : '*'
		A.INFO := R.LOCK<AC.LCK.LOCKED.AMOUNT>  : '*'
		A.INFO := R.FAV<EB.SLV83.NAME>          : '*'
		A.INFO := R.FAV<EB.SLV83.PHONE>         : '*'
		A.INFO := R.LOCK<AC.LCK.FROM.DATE>      : '*'
		A.INFO := R.LOCK<AC.LCK.TO.DATE>        : '*'
		A.INFO := ID.FT.PE
	RETURN
	
	STMT.LOCKS.STM:
		;*Buscar Bloqueos a partir del Favorito 
		STMT.LOCK  = "SELECT " : FN.LOCK : " WITH RECORD.STATUS EQ REVE AND LF.ID.COL.MOD NE '' "
		STMT.LOCK := " AND LF.CHQ.BENEFICI EQ " : FAVORITO
		STMT.LOCK := " AND LF.MOTIVO.CHQ EQ STM "		
		CALL EB.READLIST(STMT.LOCK, LIST.LOCKS, '', NO.REC, SYSTEM.RETURN.CODE)
		
		TEXTO.ARCHIVO = 'SELECT LIST.LOCKS-> ':LIST.LOCKS
		GOSUB ESCRIBIR.ARCHIVO
		
		;*Iterar Lista de Bloqueos
		LOOP
			REMOVE LOCK.ID FROM LIST.LOCKS SETTING POS
		WHILE LOCK.ID NE ''
			;*Obtener Registro del Bloqueo
			CALL F.READ(FN.LOCK, LOCK.ID, R.LOCK, F.LOCK, E.LOCK)
				
			;* Obtener id de transaccion a punto xpress
			GOSUB GET.MAPE.FT
			 
			TEXTO.ARCHIVO = 'REGISTRO DEL BLOQUEO -> ':R.FT: ' ID FT':ID.FT.PE 
			GOSUB ESCRIBIR.ARCHIVO
			
			IF ID.FT.PE THEN
				;*Validar Datos del Bloqueo y Token
				GOSUB VALIDAR.LOCK.TOKEN
				
				TEXTO.ARCHIVO = 'Y.PROCESAR -> ':Y.PROCESAR  
				GOSUB ESCRIBIR.ARCHIVO
		
				IF Y.PROCESAR EQ 'S' THEN
					;*Obtener Agencia del Cajero
					CALL CACHE.READ(FN.USER, OPERATOR, R.USUARIO, E.USER)				
					R.LOCK<AC.LCK.CO.CODE> = R.USUARIO<EB.USE.COMPANY.CODE><1,1>
					CALL F.WRITE(FN.LOCK, LOCK.ID, R.LOCK)
					CALL JOURNAL.UPDATE(FN.LOCK)
					BREAK
				END
			END
			
		REPEAT
	RETURN
	
	VALIDAR.LOCK.TOKEN:
		;*Generar Token de Validacion
		CALL GET.LOC.REF ('AC.LOCKED.EVENTS', 'LF.TRAN.TIME', POS)
		Y.SEEDS  = R.FAV<EB.SLV83.PHONE> : VM : R.FAV<EB.SLV83.DOCUMENT> : VM
		Y.SEEDS := R.LOCK<AC.LCK.FROM.DATE> : VM : R.LOCK<AC.LCK.LOCAL.REF><1, POS>
		CALL SLV.UTIL.STM.TOKEN('STM', Y.SEEDS, Y.TOKEN.VERIFI, Y.ERROR)
		
		;*Validar si Token Ingresado Corresponde
		IF Y.TOKEN EQ Y.TOKEN.VERIFI THEN
			Y.PROCESAR = 'S'	;*Token Correcto
		END
	RETURN
	
	GET.MAPE.FT:
		;*Buscar Bloqueos a partir del Favorito 
		STMT.FT  = "SELECT " : FN.FT : " WITH ORDERING.CUST EQ ":FIELD(LOCK.ID,";",1)   	
		CALL EB.READLIST(STMT.FT, LIST.FT, '', NO.REC.FT, SYSTEM.RETURN.CODE)
			
			TEXTO.ARCHIVO = 'LISTA DE FT -> ':LIST.FT
			GOSUB ESCRIBIR.ARCHIVO
			
		IF NO.REC.FT EQ 1 THEN
			
			CALL F.READ(FN.FT,LIST.FT,R.FT,F.FT,E.FT)
				IF R.FT THEN 
					ID.FT.PE = LIST.FT
				END
		END
		
	RETURN
	
	
	GET.DESCRIPT.ERROR:
		;*Obtener Error
		CALL CACHE.READ(FN.ERR, Y.ERROR, R.ERR, E.ERR)
		IF R.ERR<EB.ERR.ERROR.MSG> EQ '' THEN
			R.ERR<EB.ERR.ERROR.MSG> = Y.ERROR
		END
		A.INFO = CHANGE(R.ERR<EB.ERR.ERROR.MSG>, '&', Y.REPLA) : '* * * * * * * *'
	RETURN
	
ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    ;*DIR.NAME='C:\Users\rramos\Documents\Temp'
    R.ID   = 'STM.REVE.PE.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
 END
    CLOSESEQ SEQ.PTR
    
RETURN

END
