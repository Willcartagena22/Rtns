*-----------------------------------------------------------------------------
* <Rating>-64</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.RESERVA.HALCASH(R.DATA)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.RESERVA.HALCASH
* Descripcion: Rutina Encargada para generar Enquiry para obtener montos reservados a partir del customer
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		rmoran	   09.10.17	   Version inicial
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_System
$INSERT I_ENQUIRY.COMMON  
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.EB.SLV.STM.FAVORITES
$INSERT I_F.EB.EXTERNAL.USER

*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCESS
*CRT R.DATA
RETURN
INIT:
;*  Debug
*  SMS.CUSTOMERS ='FBATRESBI' 
   
 

	LOCATE 'ID.EXT.USER' IN D.FIELDS<1> SETTING ITEM.POS THEN
   	SMS.CUSTOMERS = D.RANGE.AND.VALUE<ITEM.POS> 
    END 
    

	FN.STM.FAVORITES	= 'F.EB.SLV.STM.FAVORITES'
	F.STM.FAVORITES		= ''
	CALL OPF(FN.STM.FAVORITES, F.STM.FAVORITES)

	FN.LOCKED.EVENTS	= 'F.AC.LOCKED.EVENTS'
	F.LOCKED.EVENTS		= ''
	CALL OPF(FN.LOCKED.EVENTS, F.LOCKED.EVENTS)

   CALL GET.LOC.REF('AC.LOCKED.EVENTS','LF.CHQ.BENEFICI',POS.CR) 
   CALL GET.LOC.REF('AC.LOCKED.EVENTS','LF.USER.BANCA',POS.NS)
 
 
 
RETURN 

PROCESS:
*Consulta Eventos AC.LOCKED.EVENT a partir del usuario Banca
IF SMS.CUSTOMERS THEN
	SELECT.LOCKED.EVENTS = 'SELECT ' : FN.LOCKED.EVENTS : ' WITH LF.USER.BANCA EQ ' : SMS.CUSTOMERS	
	CALL EB.READLIST(SELECT.LOCKED.EVENTS, ID.AC.LOCKED.EVEN, '' , NO.OF.RECS.LK.EVENTS, ERR.LK.EVENTS)	
	    IF NO.OF.RECS.LK.EVENTS NE 0 THEN
	         FOR J = 1 TO NO.OF.RECS.LK.EVENTS
                CALL F.READ(FN.LOCKED.EVENTS,ID.AC.LOCKED.EVEN<J>, LOCKED.EVENTS.REC,F.LOCKED.EVENTS, ERR.BI)
	                 Y.ID= ID.AC.LOCKED.EVEN<J>
	                 Y.CUSTOMER     = LOCKED.EVENTS.REC<AC.LCK.LOCAL.REF><1,POS.NS> 
	                 Y.ID.FAVORITES = LOCKED.EVENTS.REC<AC.LCK.LOCAL.REF><1,POS.CR> 
	                 Y.AMOUNT       = LOCKED.EVENTS.REC<AC.LCK.LOCKED.AMOUNT>
	                 Y.FROMDATE     = LOCKED.EVENTS.REC<AC.LCK.FROM.DATE>
	                 Y.TODATE       = LOCKED.EVENTS.REC<AC.LCK.TO.DATE>        

				 GOSUB GETFAVORITES
				 GOSUB WRITE.PENDING.TX			 
             NEXT J	
	    END
      GOSUB SORT_RES ;*Se ordena la salida del ENQ
END
RETURN

GETFAVORITES:
*# Obtenemos informacion de favorito de cada transaccion AC.LOCKED.EVEN
	         CALL F.READ(FN.STM.FAVORITES,Y.ID.FAVORITES, FAVORITES.CUSTOMER.REC,F.STM.FAVORITES, ERR.BI)
	         Y.FAVORITE.NAME= FAVORITES.CUSTOMER.REC<EB.SLV83.NAME>	 
	         Y.FAVORITE.PHONE=FAVORITES.CUSTOMER.REC<EB.SLV83.PHONE>	
	         Y.FAVORITE.DOCUMENT=FAVORITES.CUSTOMER.REC<EB.SLV83.DOCUMENT>
RETURN

WRITE.PENDING.TX:

GOSUB CLEAR.VARS

	Y.OUT.CUSTOMER			 = Y.CUSTOMER
	Y.OUT.FAVORITE.NAME		 = Y.FAVORITE.NAME
	Y.OUT.PHONE				 = Y.FAVORITE.PHONE
	Y.OUT.DOCUMENT			 = Y.FAVORITE.DOCUMENT
	Y.OUT.FROMDATE			 = Y.FROMDATE
	Y.OUT.TODATE			 = Y.TODATE
	
	STR.ARR.TX  = ''
	STR.ARR.TX := Y.OUT.FROMDATE			: "#"
	
	STR.ARR.TX := Y.ID   					: "*" 
	STR.ARR.TX := Y.OUT.CUSTOMER			: "*" 
	STR.ARR.TX := Y.OUT.FAVORITE.NAME   	: "*" 
	STR.ARR.TX := Y.OUT.PHONE				: "*" 
	STR.ARR.TX := Y.AMOUNT					: "*" 
	STR.ARR.TX := Y.OUT.DOCUMENT			: "*" 
	STR.ARR.TX := Y.OUT.FROMDATE			: "*" 
	STR.ARR.TX := Y.OUT.TODATE		    	

 

ENQ.DATA<-1> = STR.ARR.TX
CRT STR.ARR.TX
RETURN 
SORT_RES:
	ENQ.DATA = SORT(ENQ.DATA)
	Y.COUNT = DCOUNT(ENQ.DATA, FM)
	FOR I = 1 TO Y.COUNT
		Y.ORDEN = FIELD(ENQ.DATA<I>,"#",2)
		R.DATA<-1> = Y.ORDEN 
	NEXT I	 
RETURN

CLEAR.VARS: 
RETURN    
	Y.OUT.ID  				 = ''
	Y.OUT.CUSTOMER			 = '' 
	Y.OUT.FAVORITE.NAME		 = ''
	Y.OUT.PHONE				 = ''
	Y.OUT.DOCUMENT			 = ''
	Y.OUT.FROMDATE			 = ''
	Y.OUT.TODATE			 = ''   

RETURN



END