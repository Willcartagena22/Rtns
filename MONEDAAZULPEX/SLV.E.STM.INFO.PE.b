*-----------------------------------------------------------------------------
* <Rating>233</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.STM.INFO.PE(A.INFO)
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
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
RETURN

	INIT:
		FN.LOCK = 'F.AC.LOCKED.EVENTS'
		F.LOCK  = ''
		FN.FAV  = 'F.EB.SLV.STM.FAVORITES'
		F.FAV   = ''
		FN.ERR  = 'F.EB.ERROR'
		F.ERR   = ''	
		FN.ACC  = 'F.ACCOUNT'
		F.ACC   = ''	
		FN.USER = 'F.USER'
		F.USER  = ''
		Y.PROCESAR = 'N'
		
		;*Obtener Parametros desde el Enquiry
		LOCATE "TELEFONO" IN D.FIELDS<1> SETTING LN.POS THEN
	    	Y.TELEFONO = D.RANGE.AND.VALUE<LN.POS>
	    END
	    
	    LOCATE "TOKEN" IN D.FIELDS<1> SETTING LN.POS THEN
	    	Y.TOKEN = D.RANGE.AND.VALUE<LN.POS>
	    END
	    
	    Y.TELEFONO = '79194550'
	    Y.TOKEN    = '2365854387'
	RETURN
	
	OPENFILE:
		CALL OPF(FN.LOCK, F.LOCK)
		CALL OPF(FN.FAV, F.FAV)
		CALL OPF(FN.ERR, F.ERR)
		CALL OPF(FN.ACC, F.ACC)
		CALL OPF(FN.USER, F.USER)
	RETURN
	
	PROCESS:
		;*Buscar Favorito a partir del Numero de Telefono
		STMT.FAVORITOS = "SELECT " : FN.FAV : " WITH PHONE EQ " : Y.TELEFONO : " AND STATUS EQ ACTIVO"
		CALL EB.READLIST(STMT.FAVORITOS, LIST.FAVORITOS, '', NO.REC, SYSTEM.RETURN.CODE)
		
		;*Validar que exista Telefono Ingresado
		IF NO.REC EQ 0 THEN
			Y.ERROR = '01' ;*'EB-SLV.STM.PHONE.NO.EXISTS'
			;*Y.REPLA = Y.TELEFONO
			GOSUB GET.DESCRIPT.ERROR
			RETURN
		END
		
		COUNT.BREAK = 1
		LIST.FAVORITO.SAVE = LIST.FAVORITOS
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
		
		COUNT.BREAK = COUNT.BREAK+1
		;*Si no se encuentra el bloqueo, entonces revisar en historico para respuesta de pagado o vencido
		IF Y.PROCESAR NE 'S' AND COUNT.BREAK EQ 2 THEN
			FN.LOCK = 'F.AC.LOCKED.EVENTS$HIS'
			CALL OPF(FN.LOCK, F.LOCK)
			GOSUB CONSULTA.HIS
		END
		
		;*Error en Generacion de Token para Validacion o en Token Ingresado
		IF Y.PROCESAR EQ 'N' THEN
			Y.ERROR = 'EB-SLV.STM.TOKEN.NO.VALID'			
			Y.REPLA = Y.TOKEN
			GOSUB GET.DESCRIPT.ERROR
			RETURN
		END
	
		IF Y.ERROR EQ '' THEN 	
				;*Obtener Cliente de la Cuenta
				CALL CACHE.READ(FN.ACC, R.LOCK<AC.LCK.ACCOUNT.NUMBER>, R.ACC, E.ACC)
				Y.CUS = R.ACC<AC.CUSTOMER>
				CALL SLV.UTIL.GET.NOM.CLIENTE(Y.CUS)
				
				;*Completar Datos de Enquiry con la informacion del Bloqueo
				A.INFO  = LOCK.ID                       : '*' 
				A.INFO := R.LOCK<AC.LCK.ACCOUNT.NUMBER> : '*'
				A.INFO := Y.CUS                         : '*'
				A.INFO := R.LOCK<AC.LCK.LOCKED.AMOUNT>  : '*'
				A.INFO := R.FAV<EB.SLV83.NAME>          : '*'
				A.INFO := R.FAV<EB.SLV83.PHONE>         : '*'
				A.INFO := R.LOCK<AC.LCK.FROM.DATE>      : '*'
				A.INFO := R.LOCK<AC.LCK.TO.DATE>        : '*'
				A.INFO := R.FAV<EB.SLV83.DOCUMENT.TYPE>	: '*'
				A.INFO := R.FAV<EB.SLV83.DOCUMENT>		: '*'
				CRT A.INFO
		END
	RETURN
	
	STMT.LOCKS.STM:
		;*Buscar Bloqueos a partir del Favorito 
		STMT.LOCK  = "SELECT " : FN.LOCK : " WITH "
		STMT.LOCK := " AND LF.CHQ.BENEFICI EQ " : FAVORITO
		STMT.LOCK := " AND LF.MOTIVO.CHQ EQ STM"		
		CALL EB.READLIST(STMT.LOCK, LIST.LOCKS, '', NO.REC, SYSTEM.RETURN.CODE)

		IF FN.LOCK EQ 'FBNK.AC.LOCKED.EVENTS$HIS' THEN
			GOSUB ORDER.HIS.ACLK
		END
		
		;*Iterar Lista de Bloqueos
		LOOP
			REMOVE LOCK.ID FROM LIST.LOCKS SETTING POS
		WHILE LOCK.ID NE ''
			;*Obtener Registro del Bloqueo

			LOCK.REVE.ID = LOCK.ID
			CALL F.READ(FN.LOCK, LOCK.REVE.ID, R.LOCK, F.LOCK, E.LOCK)
			
			;*Validar Datos del Bloqueo y Token
			GOSUB VALIDAR.LOCK.TOKEN
			
			IF Y.PROCESAR EQ 'S' THEN
				;*Obtener Agencia del Cajero
				CALL CACHE.READ(FN.USER, OPERATOR, R.USUARIO, E.USER)				
				R.LOCK<AC.LCK.CO.CODE> = R.USUARIO<EB.USE.COMPANY.CODE><1,1>
				
				;*Validar si esta pagado o vencido 
				IF FN.LOCK EQ 'FBNK.AC.LOCKED.EVENTS$HIS' THEN 
				
					;*Validar si registro esta pagado
					CALL GET.LOC.REF ('AC.LOCKED.EVENTS', 'LF.ID.CHQ.COL', ID.CHQ.COL)
					CANAL = R.LOCK<AC.LCK.LOCAL.REF><1, ID.CHQ.COL>
					CANAL.INI =  LEFT(CANAL,2)
					STATUS.ACLK = R.LOCK<AC.LCK.RECORD.STATUS>
					
					TEXT.ARCHIVO = 'CANAL -> ':CANAL:' STATUS':STATUS.ACLK:' CANAL.INI ':CANAL.INI
					GOSUB ESCRIBIR.ARCHIVO
										
					IF  STATUS.ACLK EQ 'REVE' AND CANAL.INI EQ 'SV' THEN
						Y.ERROR = '02'
						;*Y.REPLA = 'MONEDA YA PAGADO'
						GOSUB GET.DESCRIPT.ERROR
						BREAK;
					END
					
					;*Validar si esta vencido
					FECHA.VEN = R.LOCK<AC.LCK.TO.DATE>
					TEXT.ARCHIVO = 'FECHA.VEN -> ':FECHA.VEN
					GOSUB ESCRIBIR.ARCHIVO
					
					IF  FECHA.VEN LT TODAY AND STATUS.ACLK EQ 'MAT' THEN
						Y.ERROR = '03'
						;*Y.REPLA = 'MONEDA NO VALIDA POR VENCIMIENTO'
						GOSUB GET.DESCRIPT.ERROR
						BREAK;
					END
				END
								
				CALL F.WRITE(FN.LOCK, LOCK.ID, R.LOCK)
				CALL JOURNAL.UPDATE(FN.LOCK)
				BREAK
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
	
	GET.DESCRIPT.ERROR:
		;*Obtener Error
		CALL CACHE.READ(FN.ERR, Y.ERROR, R.ERR, E.ERR)
		IF R.ERR<EB.ERR.ERROR.MSG> EQ '' THEN
			R.ERR<EB.ERR.ERROR.MSG> = Y.ERROR
		END
		A.INFO = CHANGE(R.ERR<EB.ERR.ERROR.MSG>, '&', Y.REPLA) : '* * * * * * * *'
		CRT A.INFO
	RETURN
	
 	ORDER.HIS.ACLK:
 		
 		IF LIST.LOCKS THEN
 			LIST.LOCKS = SORT(LIST.LOCKS)
			M=0
 			FOR K=1 TO COUNT(LIST.LOCKS,FM)+1  	
 				ID.LIST.LOCKS = FIELD(LIST.LOCKS<K>,";",1)
 				ID.LIST.LAST = FIELD(LIST.LAST.ID<M>,";",1)
				IF ID.LIST.LOCKS EQ  ID.LIST.LAST THEN
 					LIST.LAST.ID<M> = LIST.LOCKS<K>
 				END	
 				 ELSE
 					M = M+1
 					LIST.LAST.ID<M> = LIST.LOCKS<K>	
 				END						
 			NEXT K
 		END
 		LIST.LOCKS = LIST.LAST.ID 
 	RETURN	
	
	CONSULTA.HIS:
	LIST.FAVORITOS = LIST.FAVORITO.SAVE
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
	RETURN	
	
ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    ;*DIR.NAME='C:\Users\rramos\Documents\Temp'
    R.ID   = 'CONSULTA.MONEDA.PE.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
 END
    CLOSESEQ SEQ.PTR
    
RETURN
	
END
