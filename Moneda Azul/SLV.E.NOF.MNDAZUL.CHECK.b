*-----------------------------------------------------------------------------
* <Rating>-72</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.MNDAZUL.CHECK(ENQ.DATA)  
*-----------------------------------------------------------------------------
*Nombre: SLV.E.NOF.MNDAZUL.CHECK
*Descripcion: Rutina encargada de mostrar FILE CHECKER del archivo de Moneda Azul Masiva en TCE.
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		eescobar	22.03.19	Version inicial
* 1.1		iturcios	04.04.19	Agrega validacion de monto limite excedido.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.ITEMS.MONEDA.AZUL
$INSERT I_F.EB.SLV.MASTER.MONEDA.AZUL
$INSERT I_ENQUIRY.COMMON 
*-----------------------------------------------------------------------------
GOSUB INIT    
GOSUB PROCESS
RETURN  

INIT:
	LOCATE 'ID.MASTER.MNDAZUL' IN D.FIELDS<1> SETTING ITEM.POS THEN 
		ID.MASTER.MNDAZUL = D.RANGE.AND.VALUE<ITEM.POS> 
    END 
    
    ;*Debug
    ;*ID.MASTER.MNDAZUL = 'MNDAZL1553560546215'
     
    FN.ITEMS.MONEDA.AZUL = 'F.EB.SLV.ITEMS.MONEDA.AZUL'
    F.ITEMS.MONEDA.AZUL  = ''
    CALL OPF(FN.ITEMS.MONEDA.AZUL, F.ITEMS.MONEDA.AZUL)
    
    FN.MASTER.MONEDA.AZUL = 'F.EB.SLV.MASTER.MONEDA.AZUL$NAU'
    F.MASTER.MONEDA.AZUL  = ''
    CALL OPF(FN.MASTER.MONEDA.AZUL, F.MASTER.MONEDA.AZUL)
         
    ;*Constantes
    EQU ITEMS 	TO 'ITEMS'
	EQU TOTAL	TO 'TOTAL'
	EQU PENDIENTE TO 'PENDIENTE'
	EQU ERROR 	  TO 'ERROR'
	EQU CURRENCY.FORMAT TO 'L2,$'
	EQU LIMI.AMOUNT TO CHANGE('Monto límite excedido. Por favor contacta con el Banco.', 'í', CHAR(237))
		
	;*Contadores
	Y.COUNTER.PROC		= 0
	Y.COUNTER.ERROR		= 0	
	Y.COUNTER.TOTAL 	= 0	
	Y.SUMA.MONTO		= 0
RETURN 

PROCESS:    
	IF ID.MASTER.MNDAZUL THEN
		SELECT.ITEMS = "SELECT " : FN.ITEMS.MONEDA.AZUL : " WITH @ID LIKE '" : ID.MASTER.MNDAZUL : "...'"
		CALL EB.READLIST (SELECT.ITEMS, ID.ITEMS.MNDAZUL, '', NO.OF.RECS.MNDAZUL.ITEMS, ERR.MNDAZUL.ITEMS)
						    	     	
		IF NO.OF.RECS.MNDAZUL.ITEMS NE 0 THEN
			FOR I = 1 TO NO.OF.RECS.MNDAZUL.ITEMS
				CALL F.READ (FN.ITEMS.MONEDA.AZUL, ID.ITEMS.MNDAZUL<I>, REC.ITEM.MNDAZUL, F.ITEMS.MONEDA.AZUL, ERR.ITEM.MNDAZUL)
			    Y.SUMA.MONTO += REC.ITEM.MNDAZUL<EB.IMA.MONTO>
				GOSUB READ.WRITE.INFO.ITEMS	
			NEXT I		
			;*Contadores
			GOSUB READ.WRITE.CONTADORES.ITEMS
		END	
	END
RETURN

READ.WRITE.INFO.ITEMS:
	GOSUB CLEAR.VARS
	
	ID.ITEM.MNDAZUL		= ID.ITEMS.MNDAZUL<I>	
	NO.TEL 		   		= REC.ITEM.MNDAZUL<EB.IMA.MOVIL>
	NO.DOC 		   		= REC.ITEM.MNDAZUL<EB.IMA.DOCUMENTO>
	NOMBRE.CLIENTE 		= REC.ITEM.MNDAZUL<EB.IMA.NOMBRE>
	MONTO 	   			= REC.ITEM.MNDAZUL<EB.IMA.MONTO>
	ESTADO     			= REC.ITEM.MNDAZUL<EB.IMA.ESTADO>
		
	;*Construccion de arreglo
 	STR.ARR.ITEM	 = ''
 	
 	;*Ordenamiento
 	STR.ARR.ITEM	:= NO.TEL	  						: '*' 	;*1>Telefono
 	STR.ARR.ITEM	:= NO.DOC 		 					: '*'	;*2>No. de documento
 	STR.ARR.ITEM	:= NOMBRE.CLIENTE 		  			: '*'	;*3>Nombre de cliente
 	STR.ARR.ITEM	:= FMT(MONTO,CURRENCY.FORMAT)  		: '*'	;*4>Monto
 	STR.ARR.ITEM	:= ESTADO  							: '*'	;*5>Monto
 	STR.ARR.ITEM	:= ITEMS							: '*'	;*6>Tipo Respuesta
 	STR.ARR.ITEM	:= ''			 					 		;*7>Error
 	 	
 	ENQ.DATA<-1>	 = STR.ARR.ITEM
RETURN

READ.WRITE.CONTADORES.ITEMS:			
	;*Seleccionando Lista de Items en Estado PROCESADO
	SELECT.ITEMS.PTE = "SELECT " : FN.ITEMS.MONEDA.AZUL : " WITH @ID LIKE '" : ID.MASTER.MNDAZUL : "...'" : ' AND ESTADO EQ ' : PENDIENTE	
	CALL EB.READLIST(SELECT.ITEMS.PTE, ID.ITEMS.PTE, '' , NO.OF.RECS.PTE, ERR.ITEMS.PTE)	
	Y.COUNTER.PTE = NO.OF.RECS.PTE
	
	;*Contado de Items
	Y.COUNTER.TOTAL = Y.COUNTER.PTE 
	
	;*Obtener IVA y Comision
	CALL SLV.UTIL.CAL.COM.MAZUL(ID.MASTER.MNDAZUL,OUT.VAL.COM,OUT.VAL.IVA)
	Y.TOTAL.PAGO = Y.SUMA.MONTO + OUT.VAL.COM + OUT.VAL.IVA
	
	;*Validando Monto Limite.
	CALL F.READ(FN.MASTER.MONEDA.AZUL, ID.MASTER.MNDAZUL, REC.MASTER, F.MASTER.MONEDA.AZUL, ERR.MASTER)
	Y.ID.PJ = REC.MASTER<EB.SLV44.ID.CUSTOMER>
	
	Y.TXT = "VALIDACION MONTO LIMITE: REC.MASTER : " : REC.MASTER
	GOSUB WRITE_LOG_FILE
	Y.TXT = "VALIDACION MONTO LIMITE: Y.ID.PJ : " : Y.ID.PJ
	GOSUB WRITE_LOG_FILE
		
	CALL SLV.V.MNT.LIMITE.MND.AZL(Y.ID.PJ, Y.SUMA.MONTO, OUT.FLAG)
	
	Y.TXT = "VALIDACION MONTO LIMITE : " : OUT.FLAG 
	GOSUB WRITE_LOG_FILE
	
	IF OUT.FLAG EQ 0 THEN
		Y.ERROR = LIMI.AMOUNT
*		E = 'AI-PROTECTION.LIMIT' 
*		CALL ERR
	END
 
	;*Construccion de arreglo
 	STR.ARR.COUNTER	 = ''

 	STR.ARR.COUNTER	:= Y.COUNTER.TOTAL						: '*' ;*1>Total Pagos
 	STR.ARR.COUNTER	:= FMT(Y.SUMA.MONTO,CURRENCY.FORMAT)	: '*' ;*2>Monto a reservar 
 	STR.ARR.COUNTER	:= FMT(OUT.VAL.COM,CURRENCY.FORMAT)     : '*' ;*3>Comision
 	STR.ARR.COUNTER	:= FMT(OUT.VAL.IVA,CURRENCY.FORMAT)     : '*' ;*4>IVA
 	STR.ARR.COUNTER	:= FMT(Y.TOTAL.PAGO,CURRENCY.FORMAT)    : '*' ;*5>Total (Monto a reservar + Comision + IVA)
 	STR.ARR.COUNTER	:= TOTAL		    					: '*' ;*6>Tipo Respuesta
 	STR.ARR.COUNTER	:= Y.ERROR			  	  				 	  ;*7>Error Monto Limite Excedido.
 	
 	ENQ.DATA<-1>	 = STR.ARR.COUNTER
RETURN 

CLEAR.VARS: 
	ID.ITEM.MNDAZU = ''	
	NO.TEL = ''
	NO.DOC = ''
	NOMBRE.CLIENTE = ''
	MONTO = ''
	ESTADO = '' 		
RETURN

;*Archivo para Revisión de Errores
WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT'
	R.ID = 'LOG.FILE.CHECKER.MONEDA.AZUL' : '.txt'
 
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN

END
