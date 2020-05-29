*-----------------------------------------------------------------------------
* <Rating>-63</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.TC.WS.SALDOS(ENQ.DATA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.SLV.CREDIT.CARD
*-----------------------------------------------------------------------------

  	GOSUB INIT
    GOSUB PROCESS
    RETURN

*** <region name= INIT>
INIT:
	FN.CC='F.EB.SLV.CREDIT.CARD'
     F.CC=''
    
    CALL OPF(FN.CC,F.CC) 
*** <desc>Inicializar Valores de componentes </desc>
    TEXTO.ARCHIVO = CHARX(10) CHARX(13):'... INICIALIZANDO VARIABLES DE TRABAJO ...'
    
    LOCATE "CARD.NUMBER.ID" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
    	CARD.NUMBER = D.RANGE.AND.VALUE<PAR.POS.1> 
    END
****INI DEBUG    
****	CARD.NUMBER = '4007310000000040'
****END DEBUG 	
	
    
	GOSUB WLOG
	
RETURN
*** </region>

*** <region name= PROCESS>
PROCESS:
*** <desc>Procesamiento del programa </desc>
;*	LLAMADA A CALLJ

;*LLAMADA AL METODO CALLJ
     CALL F.READ(FN.CC, CARD.NUMBER, R.V, F.CC, CC.ERR) ;* Validacion de cliente existente.

		R.TC.LIMIT     = R.V<EB.SLV54.TC.LIMIT>
		R.TC.BALANCE   = R.V<EB.SLV54.TC.BALANCE>
		R.TC.PAY.CASH  = R.V<EB.SLV54.TC.PAY.CASH>
		R.TC.PAY.MIN   = R.V<EB.SLV54.TC.PAY.MIN>
        ;*R.DISPO.COMPRA = R.V<EB.SLV54.DISPO.COMPRA>
        ;*R.DISPO.RETIRO = R.V<EB.SLV54.DISPO.RETIRO>
 		R.FLOTANTE     = R.V<EB.SLV54.FLOTANTE>
 		R.MORADIAS     = R.V<EB.SLV54.MORA.DIAS>
 		R.MORATOTAL    = R.V<EB.SLV54.MORA.TOTAL>
		R.DATE.PAY    = R.V<EB.SLV54.TC.DATE.PAY>
		           
		GOSUB GET.BALANCE.AVAILABEL
		GOSUB GET.DATA
		           
    GOSUB WLOG  
    RETURN
*** </region>

GET.BALANCE.AVAILABEL:
	Y.SUCCESS = '01'
	RET.SP = 'ObtenerSaldosPagosTc'
	Y.DELIM = "%M"
	
	Y.PARAMETRO.ARGUMENT = RET.SP:'~':CARD.NUMBER
	
	THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "setExecuteProcedure"

    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
	

    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    ;*PRINT CALLJ.RESPONSE.CLT
    
	Y.COD    = FIELD(CALLJ.RESPONSE.CLT,Y.DELIM,2)

	TEXTO.ARCHIVO = CHARX(10) CHARX(13):'... RESULTADO WS  ...':CALLJ.RESPONSE.CLT
    GOSUB WLOG
	TEXTO.ARCHIVO = CHARX(10) CHARX(13):'... ERROR WS  ...':CALLJ.ERROR.CLT
    GOSUB WLOG

        
	IF Y.COD NE Y.SUCCESS THEN
	   CHANGE String1 TO String2 IN Variable
	   TEXTO.ARCHIVO = CHARX(10) CHARX(13):'Error  Al obtener informacion Prisma '
	   GOSUB WLOG
	   E = "Error  Al obtener informacion Prisma "
	   CALL ERR
	END ELSE
		Y.DISPONIBLE.COMPRA    = FIELD(CALLJ.RESPONSE.CLT,Y.DELIM,7)
		Y.DISPONIBLE.ADELAN    = FIELD(CALLJ.RESPONSE.CLT,Y.DELIM,8)
		TEXTO.ARCHIVO=Y.DISPONIBLE.COMPRA
    	GOSUB WLOG  
    	TEXTO.ARCHIVO=Y.DISPONIBLE.ADELAN
    	GOSUB WLOG  
    	
	END	
	
	 R.DISPO.COMPRA = Y.DISPONIBLE.COMPRA
     R.DISPO.RETIRO = Y.DISPONIBLE.ADELAN ;*Y.DISPONIBLE.COMPRA ;* ACTUALMENTE SON LO MISMO (ELARA - 2019.03.04)

RETURN 


GET.DATA:

	Y.PARAMETRO.ARGUMENT = 'spTCWSConsultaSaldos'
	Y.PARAMETRO.ARGUMENT := '~': CARD.NUMBER
	
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";
    THIS.METHOD.CLT= "setExecuteProcedure"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
    
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT, CALLJ.ARGUMENTS.CLT, CALLJ.RESPONSE.CLT, CALLJ.ERROR.CLT)
    DATA.EXTRME = CALLJ.RESPONSE.CLT
     
	TEXTO.ARCHIVO = CALLJ.RESPONSE.CLT
    GOSUB WLOG
    
    data = ''
	data = FMT(R.TC.LIMIT, 'R2,')   
	data :='*' : FMT((R.DISPO.COMPRA), 'R2,')
	data :='*' : FMT((R.DISPO.COMPRA), 'R2,') 
    data :='*' : FMT((R.DISPO.RETIRO), 'R2,')
    data :='*' : FMT(R.FLOTANTE      , 'R2,')
    data :='*' : FMT((R.TC.BALANCE)  , 'R2,')
 	data :='*' : FMT(R.MORADIAS      , 'R2,') 
 	data :='*' : FMT(R.MORATOTAL     , 'R2,') 
 	data :='*' : R.DATE.PAY

    Y.COUNT.FILE =  DCOUNT(DATA.EXTRME,'|')-1

    FOR I=1 TO Y.COUNT.FILE

        YBLOQUE.1 = FIELD(DATA.EXTRME,'|',I)
        Y.CAMPOS.B1 = CHANGE(YBLOQUE.1,'~',VM)

        PAG.MIN	= CHANGE(CHANGE(FIELD( Y.CAMPOS.B1, VM,  1),"\", ""),'"',"")
        PAG.CON	= FIELD( Y.CAMPOS.B1, VM,  2)
        
       	data := '*':FMT((CHANGE(CHANGE(PAG.MIN,",",""),".","")/100), 'R2,')
       	data := '*':FMT((CHANGE(CHANGE(PAG.CON,",",""),".","")/100), 'R2,')
       	
	    ENQ.DATA<-1> = data

    NEXT I

RETURN 

*** <region name= LOG FILE >
WLOG:
*** <desc>Write a text file </desc>
    DIR.NAME = 'VisaCreditLogs'
    R.ID     = 'SLV.TC.WS.SALDOS':TODAY:'.log'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN
*** </region>


END