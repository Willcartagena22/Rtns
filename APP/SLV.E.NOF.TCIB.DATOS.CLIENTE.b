*-----------------------------------------------------------------------------
* <Rating>47</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.TCIB.DATOS.CLIENTE(ENQ.DATA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_System 
$INSERT I_F.EB.EXTERNAL.USER
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCESS

RETURN

INIT:
	;* USE TABLES
	FN.EXT	= 'F.EB.EXTERNAL.USER'
	F.EXT	= ''
	CALL OPF(FN.EXT, F.EXT)	
    FN.CUST = 'F.CUSTOMER'
    F.CUST = ''	
    CALL OPF(FN.CUST, F.CUST)
    
    ;*CALLED TO  LOCAL TABLE
    CALL GET.LOC.REF ('CUSTOMER', 'LF.DUI', DOC.DUI)
    CALL GET.LOC.REF ('CUSTOMER', 'LF.NIT', DOC.NIT)
    CALL GET.LOC.REF('CUSTOMER', 'LF.TYPE.CUST', POSTypeCus)
    
    ;* INPUT TO ID USER
    LOCATE 'ID.USER' IN D.FIELDS<1> SETTING ITEM.POS THEN
   	Y.USER = D.RANGE.AND.VALUE<ITEM.POS> 
    END  
RETURN

PROCESS:
	GOSUB CLEAR.VAR
	
 	;*Y.USER = 'AALEMAN17'
	;* CALL TO RB.RXTERNAL.USER
	CALL F.READ(FN.EXT,Y.USER,ARR.EXT.USER,F.EXT,ERROR.EXT)	
	;* CREATING VIEW CUSTOMER
		Y.CUSTOMER 		= ARR.EXT.USER<EB.XU.CUSTOMER>
		Y.COMPANY		= ARR.EXT.USER<EB.XU.COMPANY>
		Y.CHANNEL		= ARR.EXT.USER<EB.XU.CHANNEL>
		Y.STATUS		= ARR.EXT.USER<EB.XU.STATUS>
		Y.ARRANGEMENT	= ARR.EXT.USER<EB.XU.ARRANGEMENT>
		Y.BANCA.TYPE	= ARR.EXT.USER<EB.XU.USER.TYPE>

	;* CALL TO CUSTOMER
	CALL F.READ(FN.CUST,Y.CUSTOMER,ARR.CUST,F.CUST,ERROR.CUST)
	
	;* CREATING VIEW EB.RXTERNAL.USER
		Y.NAME1 		= ARR.CUST<EB.CUS.NAME.1>
		Y.NAME2 		= ARR.CUST<EB.CUS.TEXT>
		Y.PHONE 		= ARR.CUST<EB.CUS.PHONE.1>
		Y.SMS 			= ARR.CUST<EB.CUS.SMS.1>
		Y.EMAIL 		= ARR.CUST<EB.CUS.EMAIL.1>
		Y.DUI			= ARR.CUST<EB.CUS.LOCAL.REF,DOC.DUI>
		Y.NIT		 	= ARR.CUST<EB.CUS.LOCAL.REF,DOC.NIT>
		Y.GEN1			= ARR.CUST<EB.CUS.GENDER>
		
		;*Formando Nombre Completo de cliente
		TIPO_CLIENTE = ARR.CUST<EB.CUS.LOCAL.REF, POSTypeCus>[1,3]		
		IF TIPO_CLIENTE EQ 'NAT' THEN		
        ;* Encontrando el nombre completo
        PRIMER.NOMBRE = ARR.CUST<EB.CUS.NAME.1>
        SEGUNDO.NOMBRE = ARR.CUST<EB.CUS.NAME.2>
        TERCER.NOMBRE = ARR.CUST<EB.CUS.GIVEN.NAMES>
        PRIMER.APELLIDO = ARR.CUST<EB.CUS.TEXT>
        SEGUNDO.APELLIDO = ARR.CUST<EB.CUS.FAMILY.NAME>
        APELLIDO.CASADA = ARR.CUST<EB.CUS.PREVIOUS.NAME>
        NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
        APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
        S_CUS_NAME = TRIM(NOMBRES : ' ' : APELLIDOS)
   	    END 
   	    
   	    ELSE IF TIPO_CLIENTE EQ 'JUR' THEN    
        ;* Encontrando la razon social
        CALL GET.LOC.REF('CUSTOMER', 'LF.RAZON.SOCIAL', POS.RAZON.SOCIAL)
        S_CUS_NAME = ARR.CUST<EB.CUS.LOCAL.REF, POS.RAZON.SOCIAL>
      	S_CUS_NAME = SWAP(ARR.CUST<EB.CUS.LOCAL.REF, POS.RAZON.SOCIAL>, @SM, ' ') 
    END
		
	GOSUB ARR.INFO
RETURN

ARR.INFO:

	STR.ARR.INFO   = ''
	;*VAR OF TABLE EB.EXTERNAL.USER
	STR.ARR.INFO	:= Y.CUSTOMER    	: "*" 
	STR.ARR.INFO	:= Y.COMPANY    	: "*" 
	STR.ARR.INFO	:= Y.CHANNEL    	: "*" 
	STR.ARR.INFO	:= Y.STATUS    		: "*" 
	STR.ARR.INFO	:= Y.ARRANGEMENT   	: "*" 
	STR.ARR.INFO	:= Y.BANCA.TYPE		: "*"	
    STR.ARR.INFO	:= Y.NAME1    		: "*" 
    STR.ARR.INFO	:= Y.NAME2			: "*"
    STR.ARR.INFO	:= Y.PHONE			: "*"
    STR.ARR.INFO	:= Y.SMS			: "*"
    STR.ARR.INFO	:= Y.EMAIL			: "*"
    STR.ARR.INFO	:= Y.DUI			: "*"
    STR.ARR.INFO	:= Y.NIT			: "*"
    STR.ARR.INFO	:= Y.GEN1			: "*"
    STR.ARR.INFO	:= S_CUS_NAME
    
    ;*CRT STR.ARR.INFO
    
    ENQ.DATA<-1>	 = STR.ARR.INFO    

RETURN

CLEAR.VAR:

	;*VAR OF TABLE EB.EXTERNAL.USER
	Y.CUSTOMER 		= ''
	Y.COMPANY		= ''
	Y.CHANNEL		= ''
	Y.STATUS		= ''
	Y.ARRANGEMENT	= ''
	Y.BANCA.TYPE	= ''
	;*VAR OF TABLE CUSTOMER
	Y.NAME1 		= ''
    Y.NAME2			= ''
	Y.PHONE 		= ''
    Y.SMS 			= ''
    Y.EMAIL 		= ''
    Y.DUI		 	= ''
    Y.NIT		 	= ''
    Y.GEN1			= ''
RETURN
END
