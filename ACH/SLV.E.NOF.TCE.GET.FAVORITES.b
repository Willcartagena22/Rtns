*-----------------------------------------------------------------------------
* <Rating>-102</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.TCE.GET.FAVORITES(ENQ.DATA) 
*-----------------------------------------------------------------------------
*Nombre: SLV.E.NOF.TCE.GET.FAVORITES
*Descripcion: Rutina encargada de devolver todos los favoritos por usuario (segun empresas asociadas) o de filtrar por empresa y por tipo de favorito. 
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	22.04.19	Version inicial
* 1.1		Jiraheta	18.11.19	Modificar asignacion de nombre de app de F.BENEFICIARY a FBNK.BENEFICIARY
* 1.2		PSANCHEZ	22.11.19	se anade beneficiario de transferencia ach (REG)
*						22.11.19	Integracion
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.BENEFICIARY
$INSERT I_ENQUIRY.COMMON
$INSERT I_System
$INSERT I_F.FT.TXN.TYPE.CONDITION
$INSERT I_F.EB.SLV.BANKS.DETAILS
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN


INIT:
	;*Tipo Favorito: INTER>INTERNACIONAL, LOCAL>LOCAL, ALL> TODOS LOS FAVORITOS
	LOCATE 'TIPO.FAVORITO' IN D.FIELDS<1> SETTING ITEM.POS THEN 
		TIPO.FAVORITO = D.RANGE.AND.VALUE<ITEM.POS> 
    END 
    
    ;*Id Empresa: Customer id de la empresa de los favoritos.
	LOCATE 'ID.EMPRESA' IN D.FIELDS<1> SETTING ITEM.POS THEN 
		ID.EMPRESA = D.RANGE.AND.VALUE<ITEM.POS> 
    END    
    
	FN.BENEFICIARY = 'FBNK.BENEFICIARY'
	F.BENEFICIARY  = ''
	CALL OPF(FN.BENEFICIARY, F.BENEFICIARY)
	
	FN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'
	F.TYPE.CONDITION  = ''
	CALL OPF(FN.TYPE.CONDITION, F.TYPE.CONDITION)
	
	FN.TABLE.BANK	=	'F.EB.SLV.BANKS.DETAILS'
	F.TABLE.BANK	=	''
	CALL OPF(FN.TABLE.BANK, F.TABLE.BANK)
	
	EQU TYPE.TXN.INT TO 'OTIB' ;*Tipo de transaccion Transf. Internacional 	 
	EQU TYPE.TXN.ACH TO 'OTCH' ;*Tipo de transaccion Transf. ACH
	EQU ALL   TO 'ALL'
	EQU INTER TO 'INTER'
	EQU LOCAL TO 'LOCAL'
	EQU REG	  TO 'REG' ;* Tipo Regional ACH
	EQU EXT.SMS.CUSTOMERS TO 'EXT.SMS.CUSTOMERS' ;*Customer ids juridicos asignados al usuario.
	
	;*Obteniendo posiciones locales 
	APPL.ARR ='BENEFICIARY' 
	FIELDNAME.ARR	='LF.SWIFT':VM:'LF.ABA':VM:'LF.IBAN':VM:'LF.BANK.INT':VM:'LF.SWIFT.INT':VM:'LF.ABA.INT':VM:'LF.IBAN.INT':VM:'LF.ADDR.INT':VM:'LF.TRAN.COD':VM:'LF.CODBANK':VM:'EMAIL'
	CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.ARR)
	;*Customer Ids Juridicos del usuario logeado
	CUSTOMER.IDS.PJ = System.getVariable(EXT.SMS.CUSTOMERS)
	IDS.PJ = SWAP(CUSTOMER.IDS.PJ, SM, ' ' )
		
	;*Debug
*	IDS.PJ = '101296' : ' ' : '101316' 	
*	TIPO.FAVORITO = REG 
*	ID.EMPRESA = '102587'
RETURN 

PROCESS:
	IF TIPO.FAVORITO EQ ALL THEN
		IF ID.EMPRESA THEN ;*Si viene lleno mostrarle todos los favoritos de esa empresa 
			;*Devolver todos los favoritos (de cualquier tipo) para una empresa
			SELECT.FAVORITES  = "SELECT " : FN.BENEFICIARY : " WITH OWNING.CUSTOMER EQ " : ID.EMPRESA		
		END ELSE
			;*Devolver todos los favoritos (de cualquier tipo) de todas las empresas
			SELECT.FAVORITES  = "SELECT " : FN.BENEFICIARY : " WITH OWNING.CUSTOMER EQ " : IDS.PJ
		END
	END  
	IF TIPO.FAVORITO EQ LOCAL THEN
		;*Devolver todos los favoritos locales.
		SELECT.FAVORITES   = "SELECT " : FN.BENEFICIARY : " WITH OWNING.CUSTOMER EQ '" : ID.EMPRESA : "'"
		SELECT.FAVORITES  := " AND TRANSACTION.TYPE NE '" : TYPE.TXN.INT : "'"
	END 
	IF TIPO.FAVORITO EQ INTER THEN 
		;*Devolver todos los favoritos Internacionales.
		SELECT.FAVORITES   = "SELECT " : FN.BENEFICIARY : " WITH OWNING.CUSTOMER EQ '" : ID.EMPRESA : "'"
		SELECT.FAVORITES  := " AND TRANSACTION.TYPE EQ '" : TYPE.TXN.INT : "'"
	END
	IF TIPO.FAVORITO EQ REG THEN 
		;*Devolver todos los favoritos ACH.
		SELECT.FAVORITES   = "SELECT " : FN.BENEFICIARY : " WITH OWNING.CUSTOMER EQ '" : ID.EMPRESA : "'"
		SELECT.FAVORITES  := " AND TRANSACTION.TYPE EQ '" : TYPE.TXN.ACH : "'"
	END
	
	CALL EB.READLIST (SELECT.FAVORITES, ID.FAVORITES, '', NO.OF.FAVORITES, ERR.FAVORITES)
	IF ID.FAVORITES THEN
		FOR AL = 1 TO NO.OF.FAVORITES
			CALL F.READ(FN.BENEFICIARY, ID.FAVORITES<AL>, REC.FAVORITES, F.BENEFICIARY, ERROR.FAVORITES)

			GOSUB READ.WRITE.INFO.FAVORITES
		NEXT AL
	END		 
RETURN

READ.WRITE.INFO.FAVORITES:
	GOSUB CLEAR.VARS
	
	Y.ID.FAVORITE 		= ID.FAVORITES<AL>	
	Y.NICKNAME   		= REC.FAVORITES<ARC.BEN.NICKNAME>
	Y.CUENTA.BENEF   	= REC.FAVORITES<ARC.BEN.BEN.ACCT.NO>
	Y.TIPO.TRANSACCION	= REC.FAVORITES<ARC.BEN.TRANSACTION.TYPE>
	GOSUB GET.NARRATIVE ;*Obtiene primeros 4 caracteres de la descripcion del TYPE.CONDITION
	Y.TXN.CODE   		= Y.TXN.TYPE
	Y.CUSTOMER.REF 		= REC.FAVORITES<ARC.BEN.CUSTOMER.REF>
	Y.NOMBRE.BENEF 		= REC.FAVORITES<ARC.BEN.BEN.SHORT.NAME>
	Y.BEN.ACCT.DESC		= '';*REC.FAVORITES<ARC.BEN>
	Y.LINK.TO.BENEF		= REC.FAVORITES<ARC.BEN.LINK.TO.BENEFICIARY>
	Y.OWNING.CUSTOMER   = REC.FAVORITES<ARC.BEN.OWNING.CUSTOMER>
	;*Inicio campos Fav. Internacional
	Y.PAIS.BENEF   		= REC.FAVORITES<ARC.BEN.COUNTRY>
	Y.BANCO.BENEF		= REC.FAVORITES<ARC.BEN.AC.WITH.BK.SHORT.NAME>
	Y.SWIFT.BENEF  		= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,1>> ;*LF.SWIFT
	Y.ABA.BENEF  		= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,2>> ;*<1,LF.ABA>
	Y.IBAN.BENEF   		= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,3>> ;*<1,LF.IBAN>
	Y.DIREC.BANK.BENEF	= REC.FAVORITES<ARC.BEN.BK.STREET.ADDR> 
	Y.DIREC.BENEF		= REC.FAVORITES<ARC.BEN.STREET.ADDR>
	Y.BANCO.INT  		= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,4>> ;*<1,LF.BANK.INT>
	Y.SWIFT.INT	  		= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,5>> ;*<1,LF.SWIFT.INT>
	Y.ABA.INT  			= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,6>> ;*<1,LF.ABA.INT>
	Y.IBAN.INT	   		= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,7>> ;*<1,LF.IBAN.INT>
	Y.DIREC.BANK.INT	= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,8>> ;*<1,LF.ADDR.INT> 
	;*Inicio campos Fav. Transferencias ACH
	Y.TRAN.CODE			= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,9>> ;*<1,LF.TRAN.CODE>
	Y.BANK.CODE			= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,10>> ;*<1,LF.CODBANK>
	IF Y.BANK.CODE THEN
		GOSUB GET.NAME.BANK
	END
	Y.EMAIL				= REC.FAVORITES<ARC.BEN.LOCAL.REF><1,POS.ARR<1,11>> ;*<1,EMAIL>
			
	;*Construccion de arreglo
 	STR.ARR.FAV	 = ''
 	
 	;*Ordenamiento
 	STR.ARR.FAV	:= Y.ID.FAVORITE		: '*' 	;*1>Id beneficiary
 	STR.ARR.FAV	:= Y.NICKNAME	 		: '*'	;*2>Nickname
 	STR.ARR.FAV	:= Y.CUENTA.BENEF	  	: '*'	;*3>No. Cuenta del Beneficiario
 	STR.ARR.FAV	:= Y.TIPO.TRANSACCION	: '*'	;*4>Tipo de Transaccion
 	STR.ARR.FAV	:= Y.TXN.CODE			: '*'	;*5>Tran/Pago: primeros 4 caracteres de la descripcion del type.condition
 	STR.ARR.FAV	:= Y.CUSTOMER.REF		: '*'	;*6>Referencia de cliente
 	STR.ARR.FAV	:= Y.NOMBRE.BENEF		: '*'	;*7>Nombre del beneficiario
 	STR.ARR.FAV	:= Y.BEN.ACCT.DESC		: '*'	;*8>Ben Acct Desc
 	STR.ARR.FAV	:= Y.LINK.TO.BENEF		: '*'	;*9>Link to beneficiary
 	STR.ARR.FAV	:= Y.OWNING.CUSTOMER	: '*'	;*10>Id Cliente al que pertenece el favorito.
 	STR.ARR.FAV	:= Y.PAIS.BENEF			: '*'	;*11>Pais de Destino del beneficiario
 	STR.ARR.FAV	:= Y.BANCO.BENEF		: '*'	;*12>Banco del beneficiario
 	STR.ARR.FAV	:= Y.SWIFT.BENEF		: '*'	;*13>Codigo SWIFT beneficiario
 	STR.ARR.FAV	:= Y.ABA.BENEF			: '*'	;*14>Codigo ABA beneficiario
 	STR.ARR.FAV	:= Y.IBAN.BENEF			: '*'	;*15>Codigo IBAN beneficiario
 	STR.ARR.FAV	:= Y.DIREC.BANK.BENEF	: '*'	;*16>Direccion banco beneficiario
 	STR.ARR.FAV	:= Y.DIREC.BENEF		: '*'	;*17>Direccion del beneficiario
 	STR.ARR.FAV	:= Y.BANCO.INT			: '*'	;*18>Nombre del banco intermediario 	
 	STR.ARR.FAV	:= Y.SWIFT.INT			: '*'	;*19>Codigo SWIFT del Banco intermediario
 	STR.ARR.FAV	:= Y.ABA.INT			: '*'	;*20>Codigo ABA del Banco intermediario
 	STR.ARR.FAV	:= Y.IBAN.INT			: '*'	;*21>Codigo IBAN del Banco intermediario
 	STR.ARR.FAV	:= Y.DIREC.BANK.INT		: '*'	;*22>Direccion del Banco intermediario
 	STR.ARR.FAV := Y.EMAIL				: '*'	;*23>EMAIL
 	STR.ARR.FAV := Y.BANK.CODE			: '*'	;*24>Codigo de Banco
 	STR.ARR.FAV := Y.TRAN.CODE			: '*'	;*25>Codigo de Transaccion
 	
 	ENQ.DATA<-1> = STR.ARR.FAV
 	;*CRT STR.ARR.FAV
RETURN

CLEAR.VARS: 
	Y.ID.FAVORITE 		= ''
	Y.NICKNAME   		= ''
	Y.CUENTA.BENEF 		= ''
	Y.TIPO.TRANSACCION	= ''	
	Y.TXN.CODE   		= ''
	Y.CUSTOMER.REF 		= ''
	Y.NOMBRE.BENEF 		= ''
	Y.BEN.ACCT.DESC		= '' 
	Y.LINK.TO.BENEF		= ''
	Y.OWNING.CUSTOMER	= ''
	Y.PAIS.BENEF   		= ''
	Y.BANCO.BENEF		= ''
	Y.SWIFT.BENEF  		= ''
	Y.ABA.BENEF  		= ''
	Y.IBAN.BENEF   		= ''
	Y.DIREC.BANK.BENEF	= '' 
	Y.DIREC.BENEF		= ''
	Y.BANCO.INT  		= ''
	Y.SWIFT.INT	  		= ''
	Y.ABA.INT  			= ''
	Y.IBAN.INT	   		= ''
	Y.DIREC.BANK.INT	= ''
	Y.EMAIL				= ''
	Y.TRAN.CODE			= ''
	Y.BANK.CODE			= ''	
RETURN

GET.NARRATIVE:	
	CALL F.READ(FN.TYPE.CONDITION, Y.TIPO.TRANSACCION, REC.TYPE.CONDITION, F.TYPE.CONDITION, ERR.TYPE.CONDITION)
	IF REC.TYPE.CONDITION THEN
		Y.TXN.TYPE = REC.TYPE.CONDITION<FT6.DESCRIPTION>[1,4]
		;*CRT Y.TXN.TYPE
	END
RETURN

GET.NAME.BANK:
	CALL F.READ(FN.TABLE.BANK, Y.BANK.CODE, REC.TABLE.BANK, F.TABLE.BANK, ERR.TABLE.BANK)
	IF REC.TABLE.BANK THEN
		Y.BANCO.BENEF = REC.TABLE.BANK<EB.SLV.BANK.NAME.BANK>
RETURN
END


