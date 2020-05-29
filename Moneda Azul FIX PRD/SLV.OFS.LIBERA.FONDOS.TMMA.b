*-----------------------------------------------------------------------------
* <Rating>19</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.OFS.LIBERA.FONDOS.TMMA
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* NAME: SLV.OFS.LIBERA.FONDOS.TMMA
*
* DESCRIPTION: 
* 				Liberación parcial de fondos bloqueados según, según registro 
*				de pago realizado de moneda azul masivo (TMMA)
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		Jonas I.	28.03.2019	 Inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.ITEMS.MONEDA.AZUL
$INSERT I_F.EB.SLV.MASTER.MONEDA.AZUL
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------
;*INI Log de Control
GOSUB LOGGER
TEXTO.ARCHIVO = 'INICIO....SLV.OFS.LIBERA.FONDOS.TMMA-->':TIME()
GOSUB ESCRIBIR.ARCHIVO
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS
TEXTO.ARCHIVO = 'FIN....SLV.OFS.LIBERA.FONDOS.TMMA-->':TIME()
GOSUB ESCRIBIR.ARCHIVO

RETURN
*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------	
	FN.ITEMS.MAZUL  = 'F.EB.SLV.ITEMS.MONEDA.AZUL'
	F.ITEMS.MAZUL   = ''
	FN.MAS.MAZUL  	= 'F.EB.SLV.MASTER.MONEDA.AZUL'
	F.MAS.MAZUL   	= ''
	FN.LCK.EVE  	= 'F.AC.LOCKED.EVENTS'
	F.LCK.EVE   	= ''
	Y.ID.MASTER     = ''
	ID.PARAM.OFS.UPD  = 'OFS.LIBERA.FON'
 	ID.PARAM.OFS.REVE = 'OFS.REVE.FON'
 	;*Obtener registro de Items
 	Y.ID.ITEMS   = ID.NEW
RETURN
*-----------------------------------------------------------------------------	
OPENFILE:
*-----------------------------------------------------------------------------	
	CALL OPF(FN.ITEMS.MAZUL, F.ITEMS.MAZUL)
	CALL OPF(FN.MAS.MAZUL, F.MAS.MAZUL)
	CALL OPF(FN.LCK.EVE, F.LCK.EVE)
RETURN
*-----------------------------------------------------------------------------	
PROCESS:
*-----------------------------------------------------------------------------
* 	Y.ID.ITEMS   = 'MNDAZL1554419252902.18722201816165100'
 	TEXTO.ARCHIVO = 'Y.ID.ITEMS-->' : Y.ID.ITEMS
	GOSUB ESCRIBIR.ARCHIVO
 	
 	CALL F.READ(FN.ITEMS.MAZUL, Y.ID.ITEMS, R.ITEMS, F.ITEMS.MAZUL,ERR.ITEMS)
 	Y.ITEM.STAT = R.ITEMS<EB.IMA.ESTADO>
* 	Y.ITEM.STAT = 'AUTORIZADO'
 	TEXTO.ARCHIVO = 'Y.ITEM.STAT-->' : Y.ITEM.STAT 
	GOSUB ESCRIBIR.ARCHIVO
 	TEXTO.ARCHIVO = 'R.ITEMS-->' : R.ITEMS 
	GOSUB ESCRIBIR.ARCHIVO

 	;*Evaluar si el registro a pagar no está liberado, para enviar liberacion de fondos 
 	IF Y.ITEM.STAT NE Y.NEW.STAT THEN
	 	Y.ID.MASTER  = FIELD(Y.ID.ITEMS,".",1)
	 	Y.AMT.LIBERA = R.NEW(EB.IMA.MONTO)
	 	
	 	TEXTO.ARCHIVO = 'Y.ID.MASTER-->' : Y.ID.MASTER :'.....Y.AMT.LIBERA-->':Y.AMT.LIBERA 
		GOSUB ESCRIBIR.ARCHIVO

*	 	Y.AMT.LIBERA = 3662.36
	 	;*Obtener registro de master
	 	CALL F.READ(FN.MAS.MAZUL, Y.ID.MASTER, R.MASTER, F.MAS.MAZUL,ERR.MAS)
	 	TEXTO.ARCHIVO = 'R.MASTER-->' : R.MASTER 
		GOSUB ESCRIBIR.ARCHIVO
	 	
	 	;*Evaluar si registro existe
	 	IF R.MASTER THEN
		 	Y.ID.LCK.EVE = R.MASTER<EB.SLV44.ID.LOCK.EVENTS>
		 	TEXTO.ARCHIVO = 'Y.ID.LCK.EVE-->' : Y.ID.LCK.EVE
			GOSUB ESCRIBIR.ARCHIVO
		 	
		 	;*Obtener registro de AC.LOKED.EVENTS
		 	CALL F.READ(FN.LCK.EVE, Y.ID.LCK.EVE, R.LCK.EVE, F.LCK.EVE,ERR.LCK.EVE)
		 	TEXTO.ARCHIVO = 'R.LCK.EVE-->' : R.LCK.EVE
			GOSUB ESCRIBIR.ARCHIVO

		 	IF R.LCK.EVE THEN 	
			 	Y.NEW.AMT.LOCKED = R.LCK.EVE<AC.LCK.LOCKED.AMOUNT> - Y.AMT.LIBERA
*			 	R.LCK.EVE.UPDATE = ''
			 	VAL.OMG.ST.MSG='' 
			 	IF Y.NEW.AMT.LOCKED EQ 0 THEN
*			 		ID.PARAM.OFS = ID.PARAM.OFS.REVE				  	
					VAL.OMG.ST.MSG = "AC.LOCKED.EVENTS,SLV.LIBERA.TMMA/R///0/,///////,":Y.ID.LCK.EVE:",LOCKED.AMOUNT=": Y.NEW.AMT.LOCKED			 		
*			 						  AC.LOCKED.EVENTS,SLV.LIBERA.TMMA/R///0/,///////,ACLK1901906364,LOCKED.AMOUNT=200.00,
			 	END ELSE
*			 		ID.PARAM.OFS = ID.PARAM.OFS.UPD
					VAL.OMG.ST.MSG = "AC.LOCKED.EVENTS,SLV.LIBERA.TMMA,//,":Y.ID.LCK.EVE:",LOCKED.AMOUNT=": Y.NEW.AMT.LOCKED
*			 		R.LCK.EVE.UPDATE<AC.LCK.LOCKED.AMOUNT> = Y.NEW.AMT.LOCKED
			 		
			 	END
*			 	R.LCK.EVE.UPDATE<AC.LCK.CO.CODE> = ID.COMPANY
			 	GOSUB OFS.SEND
			END
		END
	END
RETURN
*-----------------------------------------------------------------------------
OFS.SEND:
*-----------------------------------------------------------------------------
	;*Enviando OFS para actualizar monto de fondo bloqueado en AC.LOCKED.EVENTS
	;*-----------------------------
	TEXTO.ARCHIVO = '********************ID.COMPANY ANTES....:' : ID.COMPANY
	GOSUB ESCRIBIR.ARCHIVO

	Y.OUT    = ''
	;*CALL SLV.OFS.UTIL.OL.TRX(Y.ID.LCK.EVE,R.LCK.EVE.UPDATE,ID.PARAM.OFS,Y.OUT)
	 OFS.SRC = "SLVOFS"
*   VAL.OMG.ST.MSG=''   	
*   VAL.OMG.ST.MSG = "AC.LOCKED.EVENTS,SLV.LIBERA.TMMA,//,":Y.ID.LCK.EVE:",LOCKED.AMOUNT=": Y.NEW.AMT.LOCKED

    CALL OFS.POST.MESSAGE(VAL.OMG.ST.MSG,'',OFS.SRC,'')

	
	
	TEXTO.ARCHIVO = '********************ID.COMPANY DESPUES....:' : ID.COMPANY
	GOSUB ESCRIBIR.ARCHIVO

	TEXTO.ARCHIVO = 'Liberacion de Fondos Y.OUT-->' : Y.OUT : '...Y.ID.LCK.EVE--->':Y.ID.LCK.EVE : '...ID.PARAM.OFS--->':ID.PARAM.OFS
	GOSUB ESCRIBIR.ARCHIVO
RETURN
*-----------------------------------------------------------------------------
LOGGER:
*-----------------------------------------------------------------------------
	FN.GBL       = 'F.EB.SLV.GLOBAL.PARAM'
	F.GBL        = ''
	CALL OPF(FN.GBL, F.GBL)
	
	;*Extraer Parametrizacion de Log
	CALL F.READ(FN.GBL, 'LOG.TMMA', R.GBL, F.GBL, E.GBL)
	DIR.NAME   = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',1)	;*Ruta de Salida del Archivo
	R.ID       = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',3) : '.' : TODAY : '.log'	;*Nombre de Archivo Log
	LOG.ACTIVO = FIELD(R.GBL<EB.SLV39.VALOR.PARAM>,'*',2)	;*Bandera de Log Activo/Inactivo
RETURN
*-----------------------------------------------------------------------------
ESCRIBIR.ARCHIVO:
*-----------------------------------------------------------------------------
	;*Si el parametro de Log esta Activo Escribir Archivo
	IF LOG.ACTIVO EQ 'Y' THEN
		OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
		WRITESEQ '[':DATE():'-' : LEFT(TIMEDATE(),8) : ']  ' : TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
		END
		CLOSESEQ SEQ.PTR
	END
RETURN
END
