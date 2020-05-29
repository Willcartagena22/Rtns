*-----------------------------------------------------------------------------
* <Rating>64</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.DJ.DETALLE.CRG.MSV(DETALLE.PATH,ACCOUNT.ID, DETALLE.SPOOL,ITEM.ID,ID.MASTER.MASIVO,SS.CUSTOMER.ID)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
    $INSERT I_COMMON
	$INSERT I_EQUATE
	$INSERT I_F.USER

	$INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.ACCOUNT 
    $INSERT I_F.AA.CUSTOMER    
	$INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.PRODUCT   
      
    $INSERT I_F.ACCOUNT        
    $INSERT I_F.COUNTRY
    $INSERT I_F.COMPANY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.INDUSTRY
    $INSERT I_F.SECTOR
    $INSERT I_F.RELATION
    
    $INSERT I_F.EB.SLV.GLOBAL.PARAM 
	$INSERT I_F.EB.SLV.ITEMS.MASIVOS


*-----------------------------------------------------------------------------
	GOSUB INIT
    GOSUB OPENFILE
    GOSUB DIRECTORY
    GOSUB PROCESS
    RETURN   

*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
;* Aplicaciones a utilizar			
    FN.AA.PRODUCT = 'F.AA.PRODUCT'
    F.AA.PRODUCT = ''

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    FN.ARR.CUSTOMER = 'F.AA.ARR.CUSTOMER'
    F.ARR.CUSTOMER = ''
    FN.ARR.ACCOUNT = 'F.AA.ARR.ACCOUNT'
    F.ARR.ACCOUNT = ''
    FN.ARR.TERM.AMOUNT = 'F.AA.ARR.TERM.AMOUNT'
    F.ARR.TERM.AMOUNT = ''
    
    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT = ''
    
    
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
 
    FN.COMPANY = 'F.COMPANY'
    F.COMPANY = ''
    
    FN.TABLE.NAC = 'F.COUNTRY'
    F.TABLE.NAC = ''
    FN.TABLE.IND = 'F.INDUSTRY'
    F.TABLE.IND = ''
    FN.TABLE.SEC = 'F.SECTOR'
    F.TABLE.SEC = ''
    FN.RELATION = 'F.RELATION'
    F.RELATION = ''
    FN.TABLE.USR = 'F.USER'
    F.TABLE.USR = ''
    FN.TABLE.OCU = 'F.EB.CUS.OCUPACION.SSF'
    F.TABLE.OCU = ''
    FN.TABLE.DEP = 'F.EB.SLV.CUS.DEPARTAMENTO'
    F.TABLE.DEP = ''
    FN.TABLE.MUN = 'F.EB.SLV.CUS.MUNICIPIO'
    F.TABLE.MUN = ''
    FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = ''
    FN.TABLE.LOOK = 'F.EB.LOOKUP'
    F.TABLE.LOOK = ''
    FN.ITEM.MSV  = 'F.EB.SLV.ITEMS.MASIVOS'
    F.ITEM.MSV   = ''
       
    SS.ACCOUNT.ID = ACCOUNT.ID

    RETURN    
    
	
*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
;* Apertura de archivos a utilizar

    CALL OPF(FN.AA.PRODUCT, F.AA.PRODUCT)
    CALL OPF(FN.ARR.ACCOUNT, F.ARR.ACCOUNT)
    CALL OPF(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    CALL OPF(FN.ARR.CUSTOMER, F.ARR.CUSTOMER)
    
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    
    CALL OPF(FN.COMPANY, F.COMPANY)
    
    CALL OPF(FN.TABLE.NAC, F.TABLE.NAC)
    CALL OPF(FN.TABLE.IND, F.TABLE.IND)
    CALL OPF(FN.TABLE.SEC, F.TABLE.SEC)
    CALL OPF(FN.RELATION, F.RELATION)
    CALL OPF(FN.TABLE.USR, F.TABLE.USR)
    CALL OPF(FN.TABLE.OCU, F.TABLE.OCU)
    CALL OPF(FN.TABLE.DEP, F.TABLE.DEP)
    CALL OPF(FN.TABLE.MUN, F.TABLE.MUN)
    CALL OPF(FN.TABLE.PA, F.TABLE.PA)
    CALL OPF(FN.TABLE.LOOK, F.TABLE.LOOK)
    CALL OPF(FN.ITEM.MSV,F.ITEM.MSV)
    RETURN
      
	DIRECTORY:

	    RUTA.SPOOL.ID = 'RUTA.SPOOL.FILES'
	    CALL F.READ(FN.TABLE.PA, RUTA.SPOOL.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
	    DIR.NAME.SPOOL = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
	    
	   	RUTA.PDF.ID = 'RUTA.CONTRATO.PDF'
	    CALL F.READ(FN.TABLE.PA, RUTA.PDF.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
	    DIR.NAME.PDF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

	RETURN

	PROCESS:
	
		CALL F.READ(FN.ACCOUNT, SS.ACCOUNT.ID, R.ACCOUNT, F.ACCOUNT, F.ERR.ACC)
		SS.ARRANGEMENT.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
	    ;*CRT 'SS.ARRANGEMENT.ID ->':SS.ARRANGEMENT.ID
		;* Obtener ACCOUNT
   		CALL F.READ(FN.AA.ARRANGEMENT, SS.ARRANGEMENT.ID, R.AA.ARRANGEMENT, F.AA.ARRANGEMENT, F.ERR.AAR)
   		
   		;*Obtener Datos de Banca en Linea : OCORNEJO 22.03.2016
		;*-----------------------------------------------------
		AA.ACCOUNT.KEY = SS.ARRANGEMENT.ID : '-BALANCE-' : R.AA.ARRANGEMENT<AA.ARR.START.DATE> : '.1'
		CALL F.READ(FN.ARR.ACCOUNT, AA.ACCOUNT.KEY, R.ARR.ACCOUNT, F.ARR.ACCOUNT, ERR.ARR.ACCOUNT)
		CALL GET.LOC.REF ("AA.ARR.ACCOUNT", "LF.AML.USA.TCIB", P.LF.AML.USA.TCIB)
		CALL GET.LOC.REF ("AA.ARR.ACCOUNT", "LF.AML.AMT.TCIB", P.LF.AML.AMT.TCIB)
		CALL GET.LOC.REF ("AA.ARR.ACCOUNT", "LF.AML.MOT.TCIB", P.LF.AML.MOT.TCIB)
		USA.TCIB = 'NO'
		IF R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1, P.LF.AML.USA.TCIB> EQ 'Y' THEN
			USA.TCIB = 'SI'
		END
		MONTO.TCIB  = R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1, P.LF.AML.AMT.TCIB>
		MOTIVO.TCIB = R.ARR.ACCOUNT<AA.AC.LOCAL.REF><1, P.LF.AML.MOT.TCIB>
		;*-----------------------------------------------------
		BEGIN CASE
			CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'ACCOUNTS' 
				SS.TIPO.CUENTA = 'TIPO-CUENTA'
			CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS'
				SS.TIPO.CUENTA = 'TIPO-DEPOSITO'
			CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'LENDING' 
				SS.TIPO.CUENTA = 'TIPO-PRESTAMO'
		END CASE
		       	           		
	    CALL AA.GET.ARRANGEMENT.CONDITIONS(SS.ARRANGEMENT.ID, 'ACCOUNT', LS.PROPERTY.ACC, TODAY, R.ACCOUNT.ID, R.ACCOUNT.PPI, F.ERR.APPI)
	    LA.ACCOUNT.PPI = RAISE(R.ACCOUNT.PPI)
	    LS.ACCOUNT.ID = LA.ACCOUNT.PPI<AA.AC.ACCOUNT.REFERENCE>
	    
	    ;* Leer campo local monto depositos
	    CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.AML.DEP.PROY', POS)
	   	LN.DJ.MONTO.DEPOSITO = LA.ACCOUNT.PPI<AA.AC.LOCAL.REF, POS>       	
	           	
	    ;* Leer campo local monto retiros
	    CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.AML.RET.PROY', POS)
	   	LN.DJ.MONTO.RETIRO = LA.ACCOUNT.PPI<AA.AC.LOCAL.REF, POS>       	          		
	 	
	 	;* Leer campo local procedencia de los fondos
	    CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.AML.PROC.FON', POS)
	   	LS.DJ.PROC.FON = LA.ACCOUNT.PPI<AA.AC.LOCAL.REF, POS> 
	   	
	   	IF R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS' THEN
	   		;* Encontrando el monto del depósito
	    	CALL AA.GET.ARRANGEMENT.CONDITIONS(SS.ARRANGEMENT.ID, 'TERM.AMOUNT', 'COMMITMENT', TODAY, R.TERM.AMOUNT.ID, R.TERM.AMOUNT.PPI, F.ERR.TAM)
	    	LA.TERM.AMOUNT.PPI = RAISE(R.TERM.AMOUNT.PPI)
	
	 		LN.DJ.MONTO.APERTURA = LA.TERM.AMOUNT.PPI<AA.AMT.AMOUNT>
	 	END
		ELSE
			LN.DJ.MONTO.APERTURA = ''
	   	END      	          			
	 	
	 	CALL F.READ(FN.COMPANY, R.AA.ARRANGEMENT<AA.ARR.CO.CODE>, R.ARR.COMPANY, F.COMPANY, F.ERR.COM)
	 	LN.POS = DCOUNT(R.ARR.COMPANY<EB.COM.NAME.ADDRESS>, @VM)
	 	LS.LUGAR.EMISION = R.ARR.COMPANY<EB.COM.NAME.ADDRESS><1, LN.POS>
	 	
	 	
	 	BEGIN CASE
			CASE SS.TIPO.CUENTA EQ 'TIPO-CUENTA' 
				 	SS.TIPO.CUENTA.ENCABEZADO= 'NUMERO DE CUENTA'
				 	SS.TIPO.CUENTA.LEYENDA = ' '
				 	SS.TIPO.CUENTA.DETALLE = 'y los movimientos proyectados mensualmente de dep':CHAR(243):'sito son: US$ ':TRIM(FMT(LN.DJ.MONTO.DEPOSITO, 'R2,#15'))
				 	SS.TIPO.CUENTA.DETALLE :=', y retiros de: US$ ':TRIM(FMT(LN.DJ.MONTO.RETIRO, 'R2,#15')):' '
				 	
			CASE SS.TIPO.CUENTA EQ 'TIPO-PRESTAMO' 
				 	SS.TIPO.CUENTA.ENCABEZADO= 'NUMERO DE PRESTAMO'
				 	SS.TIPO.CUENTA.LEYENDA = ' para pagar el prestamo '
				 	SS.TIPO.CUENTA.DETALLE = 'y pagar':CHAR(225):' la cantidad mensual de: US$ ':TRIM(FMT(LN.DJ.MONTO.DEPOSITO, 'R2,#15')):' y adicionalmente a mi cuota '
				 	SS.TIPO.CUENTA.DETALLE :='establecida pretendo realizar pagos anticipados por monto de: US$ ':TRIM(FMT(LN.DJ.MONTO.RETIRO, 'R2,#15')) 
				 					 	
			CASE SS.TIPO.CUENTA EQ 'TIPO-DEPOSITO'
				 	SS.TIPO.CUENTA.ENCABEZADO= 'NUMERO DE DEPOSITO A PLAZO'
				 	SS.TIPO.CUENTA.LEYENDA = ' para la apertura del dep':CHAR(243):'sito a plazo '
				 	SS.TIPO.CUENTA.DETALLE = 'el monto de la apertura del dep':CHAR(243):'sito de plazo ser':CHAR(225):' de : US$ ':TRIM(FMT(LN.DJ.MONTO.APERTURA, 'R2,#15'))
		END CASE
		
		SS.CUSTOMER.ID=FIELD(DETALLE.SPOOL, @FM, 1) 
		LS.PERSONERIA=FIELD(DETALLE.SPOOL, @FM, 15)
		
		SS.SHORT.NAME=FIELD(DETALLE.SPOOL, @FM, 23)   	
	    SS.NOMBRE.DOCUMENTO=FIELD(DETALLE.SPOOL, @FM, 24)
				
		
		DATOSCT ='' 
		DATOSCT := 'CUENTANO=' : ACCOUNT.ID : ';'
		DATOSCT := 'NumCliente=' : SS.CUSTOMER.ID  : ';'
		DATOSCT := 'NombreNit=' : FIELD(DETALLE.SPOOL, @FM, 2) : ';'
		
		IF FIELD(DETALLE.SPOOL, @FM, 3) EQ '' OR FIELD(DETALLE.SPOOL, @FM, 3) EQ ' ' THEN
			TEXT.ACT='(NO POSEE DETALLE DE ACTIVIDAD ECONOMICA)'  
		END
		ELSE
		    TEXT.ACT = FIELD(DETALLE.SPOOL, @FM, 3)
		END
		
		DATOSCT := 'ActividadEconomica=' : TEXT.ACT : ';'
    	DATOSCT := 'DJEDADRL=' : FIELD(DETALLE.SPOOL, @FM, 4)  : ';'
    	DATOSCT := 'DJPROFRL=' : FIELD(DETALLE.SPOOL, @FM, 5)  : ';'
		DATOSCT := 'DJDOMIRL=' : FIELD(DETALLE.SPOOL, @FM, 6)  : ';'
		DATOSCT := 'DJTIPDRL=' : FIELD(DETALLE.SPOOL, @FM, 7)  : ';'
		DATOSCT := 'DJNUMDRL=' : FIELD(DETALLE.SPOOL, @FM, 8)  : ';'
		;*DATOSCT := 'DJORIGEN=' : FIELD(DETALLE.SPOOL, @FM, 9)  : ';'
		DATOSCT := 'DJORIGEN=' : LS.DJ.PROC.FON  : ';'
		DATOSCT := 'DJCIUDAD=' : FIELD(DETALLE.SPOOL, @FM, 10)  : ';'
		DATOSCT := 'DJDIADDD=' : FIELD(DETALLE.SPOOL, @FM, 11)  : ';'
		DATOSCT := 'DJMESMMM=' : FIELD(DETALLE.SPOOL, @FM, 12)  : ';'
		DATOSCT := 'DJANNOAA=' : FIELD(DETALLE.SPOOL, @FM, 13)  : ';'
		DATOSCT := 'DJMENORE=' : FIELD(DETALLE.SPOOL, @FM, 14)  : ';'
		DATOSCT := 'DJNITTRL=' : FIELD(DETALLE.SPOOL, @FM, 16)  : ';'
		DATOSCT := 'DJNOMBRL=' : FIELD(DETALLE.SPOOL, @FM, 17)  : ';'			
		DATOSCT := 'RAZONSOC=' : FIELD(DETALLE.SPOOL, @FM, 18)  : ';'
		DATOSCT := 'NOMBCOME=' : FIELD(DETALLE.SPOOL, @FM, 19)  : ';'
		DATOSCT := 'NUMERNIT=' : FIELD(DETALLE.SPOOL, @FM, 20)  : ';'
		DATOSCT := 'ACTIECON=' : FIELD(DETALLE.SPOOL, @FM, 21)  : ';'
		DATOSCT := 'FUENTEINGRESOS=' : FIELD(DETALLE.SPOOL, @FM, 22)  : ';'
		DATOSCT := 'DJCTEncabezado=' : SS.TIPO.CUENTA.ENCABEZADO : ';'
    	DATOSCT := 'DJCTTexto=' : SS.TIPO.CUENTA.LEYENDA: ';'
    	DATOSCT := 'DJCTDetalle=' : SS.TIPO.CUENTA.DETALLE: ';'
 
		GOSUB PROCESS.PERFIL
	 	;*GOSUB UPDATE.ITEM
	RETURN
	
UPDATE.ITEM:
    CALL F.READ(FN.ITEM.MSV,ITEM.ID,ITEM.R,F.ITEM.MSV,ITEM.ERR)
    ITEM.R<EB.SLV3.RESERVADO.3> = 'Declaracion jurada del cliente ':SS.CUSTOMER.ID:' Generada.'
    ITEM.R<EB.SLV3.RESERVADO.7> = R.ID.PDF
    CALL F.WRITE (FN.ITEM.MSV,ITEM.ID,ITEM.R)
RETURN

	
*-----------------------------------------------------------------------------
	PROCESS.PERFIL:
*-----------------------------------------------------------------------------
		IF LS.PERSONERIA EQ '1' THEN
	    	;*NAME.ID = 'DECJURPNCTMSV':'-': ID.MASTER.MASIVO:'-':SS.CUSTOMER.ID : '-' :  ACCOUNT.ID
	    	NAME.ID = 'DjMSV':'-': ID.MASTER.MASIVO:'-':SS.CUSTOMER.ID ;*: '-' :  ACCOUNT.ID
	    END ELSE
	    	NAME.ID = 'DECJURPJCT-' : SS.CUSTOMER.ID : '-' : ACCOUNT.ID : '-' : (TIMESTAMP() * 1000)
	    END
	    R.ID.SPOOL = NAME.ID : '.txt'
	    R.ID.TXT = NAME.ID : '.txt'
	    R.ID.PDF = NAME.ID : '.pdf'
	    
		DATOSCT = CHANGE(DATOSCT, 'Ñ', CHAR(465))
		DATOSCT = CHANGE(DATOSCT, 'ñ', CHAR(497))
		DATOSCT = CHANGE(DATOSCT, '=;', '= ;')
		
		;* Creando el archivo principal
	    ;*-----------------------------
	    
	    SPOOL.NAME = R.ID.SPOOL
	  
	    OPENSEQ DIR.NAME.SPOOL, R.ID.SPOOL TO SEQ.PTR 
	   	ELSE
	        CREATE SEQ.PTR 
	        ELSE
	          	ETEXT = 'No se puede crear el archivo.'
	      		CALL STORE.END.ERROR
	   		END
		END
	
		;* Escribiendo la cadena en el archivo principal
		;*----------------------------------------------
		WRITESEQ DATOSCT ON SEQ.PTR 
		ELSE
			ETEXT = 'No se pueden escribir los datos en el archivo.'
			CALL STORE.END.ERROR 
		END
	
		;* Cerrando el archivo
		;*--------------------
		CLOSESEQ SEQ.PTR 
		
	
		;* bloque adicionado para pruebas de rutina que valida si existe el pdf    
		PERFIL.ARCHIVO = DIR.NAME.PDF: R.ID.PDF
		
		;*Retornando Nombre del SPOOL
		;*---------------------------
		SPOOL.NAME = R.ID.SPOOL
		       
		CALL SLV.I.DOCUMENT.CONTROL(SPOOL.NAME, 'DECLARACION.JURADA')
	    Y.ID   = CHANGE(OPERATOR,'.','') : '.' : ID.COMPANY
		;*CALL JOURNAL.UPDATE(Y.ID)
		
		STR.ARR = SS.ACCOUNT.ID : "*"
		STR.ARR := SS.CUSTOMER.ID : "*"
		STR.ARR := SS.SHORT.NAME : "*"  	
		STR.ARR := SS.NOMBRE.DOCUMENTO : "*"
		STR.ARR := DIR.NAME.PDF : R.ID.PDF
		
		DETALLE.PATH<-1>=STR.ARR
		
		
	    RETURN	 	

END
