*-----------------------------------------------------------------------------
* <Rating>-97</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.ITEMS.MAZUL.PE(A.INFO)
*-----------------------------------------------------------------------------
* Nombre: SLV.E.ITEMS.MAZUL
* Descripción: Obtener información de Items Moneda Azul Masivo
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Version	Autor	Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0	   vburgos	30.07.19	Version inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.ERROR
$INSERT I_F.USER
$INSERT I_F.EB.SLV.ITEMS.MONEDA.AZUL
$INSERT I_F.EB.SLV.MASTER.MONEDA.AZUL
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
	FN.ERR  = 'F.EB.ERROR'
	F.ERR   = ''	
	FN.USER = 'F.USER'
	F.USER  = ''
	Y.PROCESAR = 'N'
	Y.MASIVO   = 'M'
	ID.TOKEN   = 'TMMA'		;*Transferencia Masivo Moneda Azul
	;*Obtener Parametros desde el Enquiry
	LOCATE "TELEFONO" IN D.FIELDS<1> SETTING LN.POS THEN
    	Y.TELEFONO = D.RANGE.AND.VALUE<LN.POS>
    END
    
    LOCATE "TOKEN" IN D.FIELDS<1> SETTING LN.POS THEN
    	Y.TOKEN = D.RANGE.AND.VALUE<LN.POS>
    END
    
    ;*Y.TELEFONO = '71188153'
    ;*Y.TOKEN    = '9829027905'
RETURN
*-----------------------------------------------------------------------------	
OPENFILE:
*-----------------------------------------------------------------------------	
	CALL OPF(FN.ITEMS.MAZUL, F.ITEMS.MAZUL)
	CALL OPF(FN.MAS.MAZUL, F.MAS.MAZUL)
	CALL OPF(FN.ERR, F.ERR)
	CALL OPF(FN.USER, F.USER)
RETURN
*-----------------------------------------------------------------------------	
PROCESS:
*-----------------------------------------------------------------------------
	;*Evaluar si A.INFO ya contiene el ID de Items
	IF A.INFO THEN
		ID.ITEMS = FIELD(A.INFO,'*',1)
		Y.SEEDS  = FIELD(A.INFO,'*',2)
		Y.TELEFONO = FIELD(Y.SEEDS,VM,1)
		Y.TOKEN  = FIELD(A.INFO,'*',3)
		;*Obtener Registro del Items
		CALL F.READ(FN.ITEMS.MAZUL, ID.ITEMS, R.ITEMS, F.ITEMS.MAZUL, ERR.ITEMS)
		;*Generar Token según Items
		GOSUB VALID.TOKEN
	END ELSE
		;*Buscar Favorito a partir del Numero de Telefono
		SELECT.CMD = "SELECT " : FN.ITEMS.MAZUL : " WITH MOVIL EQ " : Y.TELEFONO : " AND ESTADO EQ AUTORIZADO AND SCRENEO.AML EQ 1"
		CALL EB.READLIST(SELECT.CMD, LIST.ID, '', NO.REC, ERR.ITEMS)
		
		;*Validar que exista Telefono Ingresado
		IF NO.REC EQ 0 THEN
			Y.ERROR = 'EB-SLV.STM.PHONE.NO.EXISTS'
			Y.REPLA = Y.TELEFONO
			GOSUB GET.DESCRIPT.ERROR
			RETURN
		END
		
		;*Validar Token asociado a Items
		LOOP
			REMOVE ID.ITEMS FROM LIST.ID SETTING POS
		WHILE ID.ITEMS NE ''
			;*Obtener Registro del Items
			CALL F.READ(FN.ITEMS.MAZUL, ID.ITEMS, R.ITEMS, F.ITEMS.MAZUL, ERR.ITEMS)
			
			;*Generar Token según Items
			GOSUB VALID.TOKEN
			IF Y.PROCESAR EQ 'S' THEN
				BREAK
			END
		REPEAT
	END	
	;*Error en Generacion de Token para Validacion o en Token Ingresado
	IF ERR.ITEMS THEN
		A.INFO = ERR.ITEMS
		RETURN
	END
	IF Y.PROCESAR EQ 'N' THEN
		IF Y.ERROR EQ '' THEN
			Y.ERROR = 'EB-SLV.STM.TOKEN.NO.VALID'			
		END			
		Y.REPLA = Y.TOKEN
		GOSUB GET.DESCRIPT.ERROR
		RETURN
	END
	
	;*Obtener Cliente de la Cuenta
	CALL SLV.UTIL.GET.NOM.CLIENTE(Y.CUS)
	
	;*Completar Datos de Enquiry con la informacion del Bloqueo
	A.INFO  = ID.ITEMS	        			: '*' ;* Posicion 01
	A.INFO := Y.CUENTA 						: '*' ;* Posicion 02
	A.INFO := Y.CUS                         : '*' ;* Posicion 03
	A.INFO := R.ITEMS<EB.IMA.MONTO>  		: '*' ;* Posicion 04
	A.INFO := R.ITEMS<EB.IMA.NOMBRE>        : '*' ;* Posicion 05
	A.INFO := R.ITEMS<EB.IMA.MOVIL>         : '*' ;* Posicion 06
	A.INFO := Y.FECHA.FROM      			: '*' ;* Posicion 07
	A.INFO := Y.FECHA.TO        			: '*' ;* Posicion 08
	A.INFO := 'DUI'		      				: '*' ;* Posicion 09
	A.INFO := R.ITEMS<EB.IMA.DOCUMENTO>  
RETURN
*-----------------------------------------------------------------------------	
VALID.TOKEN:
*-----------------------------------------------------------------------------	
	Y.TOKEN.VERIFY = ''
	GOSUB GENERA.TOKEN
	;*Validar si Token Ingresado Corresponde
	IF Y.TOKEN EQ Y.TOKEN.VERIFY THEN
		Y.PROCESAR = 'S'	;*Token Correcto
	END
	
	IF Y.PROCESAR EQ 'S' THEN
		;*Obtener Agencia del Cajero
		CALL CACHE.READ(FN.USER, OPERATOR, R.USUARIO, E.USER)				
		R.ITEMS<EB.IMA.CO.CODE> = R.USUARIO<EB.USE.COMPANY.CODE><1,1>
		CALL F.WRITE(FN.ITEMS.MAZUL, ID.ITEMS, R.ITEMS)
		CALL JOURNAL.UPDATE(FN.ITEMS.MAZUL)
	END
RETURN
*-----------------------------------------------------------------------------	
GENERA.TOKEN:
*-----------------------------------------------------------------------------	
	;*Obtener parametros para generacion de Token de registro de Items
	;*Obtener ID Master
	Y.ID.MASTER = FIELDS(ID.ITEMS,".",1)
	CALL F.READ(FN.MAS.MAZUL, Y.ID.MASTER, R.MASTER, F.MAS.MAZUL, ERR.MASTER)
	Y.FECHA.FROM= R.MASTER<EB.SLV44.FECHA.ULT.AUT>
	Y.FECHA.TO  = R.MASTER<EB.SLV44.FECHA.FIN>	
	Y.TRAN.TIME = R.ITEMS<EB.IMA.TRAN.TIME>
	Y.CUENTA    = R.MASTER<EB.SLV44.CUENTA>
	Y.CUS  		= R.MASTER<EB.SLV44.ID.CUSTOMER>
	ID.LOCKED   = R.MASTER<EB.SLV44.ID.LOCK.EVENTS>
	Y.SEEDS  = Y.TELEFONO   : VM : R.ITEMS<EB.IMA.DOCUMENTO> : VM
	Y.SEEDS := Y.FECHA.FROM : VM : Y.TRAN.TIME
	
	CALL SLV.UTIL.STM.TOKEN(ID.TOKEN, Y.SEEDS, Y.TOKEN.VERIFY, Y.ERROR)
RETURN
*-----------------------------------------------------------------------------	
GET.DESCRIPT.ERROR:
*-----------------------------------------------------------------------------	
	;*Obtener Error
	CALL CACHE.READ(FN.ERR, Y.ERROR, R.ERR, E.ERR)
	IF R.ERR<EB.ERR.ERROR.MSG> EQ '' THEN
		R.ERR<EB.ERR.ERROR.MSG> = Y.ERROR
	END
	A.INFO = CHANGE(R.ERR<EB.ERR.ERROR.MSG>, '&', Y.REPLA) : '* * * * * * * *'
RETURN
END


WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT'
	R.ID = 'LOG.V.MAZUL' : '.txt' 
 
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END 
	CLOSESEQ SEQ.PTR 
RETURN
