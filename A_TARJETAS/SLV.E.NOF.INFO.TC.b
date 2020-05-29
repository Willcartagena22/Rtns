*-----------------------------------------------------------------------------
* <Rating>-48</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.INFO.TC(ENQ.DATA)
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
	GOSUB OPENF
    GOSUB PROCESS
    RETURN

*** <region name= INIT>
INIT:
	 FN.CREDIT.CARD='F.EB.SLV.CREDIT.CARD'
     F.CREDIT.CARD=''

*** <desc>Inicializar Valores de componentes </desc>
    TEXTO.ARCHIVO = CHARX(10) CHARX(13):'... INICIALIZANDO VARIABLES DE TRABAJO ...'
    GOSUB WLOG
    
	Y.SUCCESS = '01'
	RET.SP = 'ObtenerSaldosPagosTc'
	Y.DELIM = "%M"
	
	THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "setExecuteProcedure"

	CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
    
    R.CREDIT.CARD=""
	
RETURN
*** </region>

*** <region name= OPENF>
OPENF:
*** <desc>Apertura de Aplicaciones </desc>
    CALL OPF(FN.CREDIT.CARD,F.CREDIT.CARD)
    RETURN
*** </region>

*** <region name= PROCESS>
PROCESS:

	LOCATE "INFO.CUENTA" IN D.FIELDS<1> SETTING LOC.POS THEN
		CARD.NUMBER=D.RANGE.AND.VALUE<LOC.POS>
	END
	
	;*CARD.NUMBER="4007280000000359"
	
	TEXTO.ARCHIVO = CHARX(10) CHARX(13):'... NÚMERO DE TARJETA  ...':CARD.NUMBER
    GOSUB WLOG
    
	CALL F.READ(FN.CREDIT.CARD,CARD.NUMBER,R.CREDIT.CARD,F.CREDIT.CARD,ERR.CREDIT.CARD)
    
    Y.PARAMETRO.ARGUMENT = RET.SP:'~':CARD.NUMBER
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    
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
		TEXTO.ARCHIVO=Y.DISPONIBLE.COMPRA
    	GOSUB WLOG  
    	;*GOSUB SAVE.CREDITCARD
	END	
	
	STR.ARR  = "Producto*":R.CREDIT.CARD<EB.SLV54.TC.PRODUCT>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Segmento*":R.CREDIT.CARD<EB.SLV54.TC.SEGMET>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Tasa Inter":CHAR(XTD('E9')):"s*":FMT(R.CREDIT.CARD<EB.SLV54.TC.RATE>,'R2#10')
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Tasa Efectiva*":FMT(R.CREDIT.CARD<EB.SLV54.EFFECTIVE.RATE>,'R2#10')
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Fecha de Corte*":R.CREDIT.CARD<EB.SLV54.TC.CUTTING>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Fecha de Pago*":R.CREDIT.CARD<EB.SLV54.TC.DATE.PAY>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "L":CHAR(XTD('ED')):"mite*":FMT(R.CREDIT.CARD<EB.SLV54.TC.LIMIT>,'R2#10')
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Disponible*":FMT(Y.DISPONIBLE.COMPRA,'R2#10') ;*R.CREDIT.CARD<EB.SLV54.CUSTOMER>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Deuda Total*":FMT(R.CREDIT.CARD<EB.SLV54.TC.BALANCE>,'R2#10')
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Pago Contado*":FMT(R.CREDIT.CARD<EB.SLV54.TC.PAY.CASH>,'R2#10')
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Pago M":CHAR(XTD('ED')):"nimo*":FMT(R.CREDIT.CARD<EB.SLV54.TC.PAY.MIN>,'R2#10')
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "D":CHAR(XTD('ED')):"as Mora*":R.CREDIT.CARD<EB.SLV54.MORA.DIAS>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Total Mora*":FMT(R.CREDIT.CARD<EB.SLV54.MORA.TOTAL>,'R2#10')
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Tipo*":R.CREDIT.CARD<EB.SLV54.IS.ADDITIONAL>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Estado*":R.CREDIT.CARD<EB.SLV54.DES.ESTADO>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Estatus*":R.CREDIT.CARD<EB.SLV54.HABILITADA>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Nombre en Pl":CHAR(XTD('E1')):"stico*":R.CREDIT.CARD<EB.SLV54.NOMBRE.PLASTICO>
	ENQ.DATA<-1> = STR.ARR
	STR.ARR  = "Nombre del Vendedor*":R.CREDIT.CARD<EB.SLV54.VENDEDOR>
	ENQ.DATA<-1> = STR.ARR
	
    RETURN
    
*** <region name= LOG FILE >
WLOG:
*** <desc>Write a text file </desc> 
    DIR.NAME = 'VisaCreditLogs'
    ;*DIR.NAME = 'C:\Users\elara'
    R.ID     = 'EQN.SLV.INFO.TC_':TODAY:'.log'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN
*** </region>    
END
