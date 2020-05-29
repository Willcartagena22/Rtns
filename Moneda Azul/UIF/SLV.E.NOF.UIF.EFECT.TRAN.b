*-----------------------------------------------------------------------------
* <Rating>6381</Rating>
*-----------------------------------------------------------------------------
* Nombre: 		SLV.E.NOF.UIF.EFECT.TRAN
* Descripcion: 	Rutina para devolver las transacciones mayores o iguales a un monto.
* EQUIRY:		SLV.E.UIF.TRA.EFE.DIA
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		David		06.02.15	Version inicial
* 1.1       Serge       07.02.15	Revisión y correcciones varias por campos vacios.
* 1.2								Separación de departamento y municipio para naturales.
* 1.3								Obtención del número de cuenta de la transacción.
* 1.4								Tratamiento Ñ, modificación en control de montos.
* 1.5		Jonás		17.02.15	Mostrar monto total de la transaccion para transaccioens TFS.
* 1.6		Jonás		18.02.15	Ordenar registros por cliente.
* 1.7		Jonás		23.02.15	Para FUNDS.TRANSFER obtener monto de campo LOC.AMT.DEBITED.
*            	        			Obtener cuenta del producto del cliente de TELLER>NARRATIVE.2,
*									cuando se escriba una cuenta interna en la alerta.
* 1.8		     		25.02.15	Mostrar concepto de transacción de aplicación TRANSACTION.
* 1.9		     		28.02.15	Excluir registros de transacciones mensuales.
* 1.10		     		03.03.15	Excluir registros no autorizados o reversados.
* 1.11		     		05.03.15	Revisión de registros reversados, obtener ultimo registro de historico.
* 1.12		     		06.05.15	Revisión de estado civil cuando es MARRIED.
*						06.05.15	Eliminar salto de linea en campo local LF.RAZON.SOCIAL.
*						14.05.15	No mostrar registros por duplicidad de alertas.
* 1.13					08.07.15	Obtener nombre de agencia de campo COMPANY>COMPANY.NAME.
* 1.14					13.07.15	Obtener municipio de campo COMPANY>NAME.ADDRESS posición 3.
*									Obtener departamento de campo COMPANY>NAME.ADDRESS posición 4.
* 2.0		Jonas		07.08.15	Evaluar campo multivalor SHORT.NAME		
* 2.1		CALFARO		23.08.18	agregar campo de pago de tarjeta de credito para descripcion
* 2.2		CALFARO		30.08.18	agregar Retiro de Tarjeta de Crédito
*----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.E.NOF.UIF.EFECT.TRAN(ENQ.DATA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
	$INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.ID
    $INSERT I_F.USER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.DEPT.ACCT.OFFICER
    $INSERT I_F.COMPANY
    $INSERT I_F.SLV.AML.ALERT.LOG
	$INSERT I_F.SLV.CUS.MUNICIPIO
    $INSERT I_F.SLV.CUS.DEPARTAMENTO
    $INSERT I_F.INDUSTRY
    $INSERT I_F.COUNTRY
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.FT.TXN.TYPE.CONDITION 
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.EB.SLV.TRX.LOG.HIST
    $INSERT I_F.SLV.AML.PROFILE.PARAMETER
    $INSERT I_F.TRANSACTION
    $INSERT I_F.TELLER.PARAMETER
    $INSERT I_F.TELLER.TRANSACTION
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN


*-----------------------------------------------------------------------------
      INIT:
*-----------------------------------------------------------------------------
    FN.FUNDS.TRANSFER= 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER = ''
    FN.TELLER 		 = 'F.TELLER$HIS'
    F.TELLER  		 = ''
    FN.COMPANY 		 = 'F.COMPANY'
    F.COMPANY  		 = ''
    FN.USER 		 = 'F.USER'
    F.USER  		 = ''
    FN.OFFICER 		 = 'F.DEPT.ACCT.OFFICER'
    F.OFFICER  		 = ''
    FN.ACCOUNT  	 = 'F.ACCOUNT'
    F.ACCOUNT  		 = ''
	FN.CUSTOMER 	 = 'F.CUSTOMER'
    F.CUSTOMER  	 = ''
    FN.AML.ALERT.LOG = 'F.SLV.AML.ALERT.LOG'
    F.AML.ALERT.LOG  = ''
    FN.TABLE.DEP 	 = 'F.EB.SLV.CUS.DEPARTAMENTO'
    F.TABLE.DEP 	 = ''
    FN.TABLE.MUN 	 = 'F.EB.SLV.CUS.MUNICIPIO'
    F.TABLE.MUN 	 = ''
    FN.TABLE.IND 	 = 'F.INDUSTRY'
    F.TABLE.IND 	 = ''  
    FN.TABLE.NAC 	 = 'F.COUNTRY'
    F.TABLE.NAC 	 = ''      
    FN.TFS 			 = 'F.TELLER.FINANCIAL.SERVICES'
    F.TFS 			 = ''      
    FN.ARR 			 = 'F.AA.ARRANGEMENT'
    F.ARR 			 = ''      
    FN.TXN 			 = 'F.FT.TXN.TYPE.CONDITION'
    F.TXN 			 = ''      
    FN.TELL.TXN		 = 'F.TELLER.TRANSACTION'
    F.TELL.TXN 		 = ''      
    FN.KEYS	   	     = 'F.EB.SLV.KEYS.PARAMS'
    F.KEYS		     = ''
    FN.TRX.LG.HIST   = 'F.EB.SLV.TRX.LOG.HIST'
    F.TRX.LG.HIST    = ''
    FN.TT            = 'F.TELLER'
    F.TT             = ''
    FN.FT            = 'F.FUNDS.TRANSFER'  
    F.FT             = ''
    FN.AML.PARAMETER = 'F.SLV.AML.PROFILE.PARAMETER'
    F.AML.PARAMETER = ''
    
       FN.TRANSACTION  	= 'F.TRANSACTION'
    F.TRANSACTION  	= ''
    
       FN.STMT.NARR.PARAM  	= 'F.STMT.NARR.PARAM'
    F.STMT.NARR.PARAM   	= ''
      FN.STMT.NARR.FORMAT  	= 'F.STMT.NARR.FORMAT'
    F.STMT.NARR.FORMAT   	= ''
	
	;*Definicion de variables
    V.CODIGO_DE_CLIENTE                     = ""
    V.NOMBRE_AGENCIA                        = ""
    V.MUNICIPIO_AGENCIA                     = ""
    V.DEPARTAMENTO_AGENCIA                  = ""
    V.CODIGO_COLABORADOR                    = ""
    V.CARGO_COLABORADOR                     = ""
    V.FECHA_DE_LA_TRANSACCION               = ""
    V.TIPO_DE_PERSONA                       = ""
    V.PRIMER_APELLIDO                       = ""
    V.SEGUNDO_APELLIDO                      = ""
    V.APELLIDO_CASADA                       = ""
    V.PRIMER_NOMBRE                         = ""
    V.SEGUNDO_NOMBRE                        = ""
    V.TERCER_NOMBRE                         = ""
    V.FECHA_DE_NACIMIENTO                   = ""
    V.LUGAR_DE_NACIMIENTO                   = ""
    V.NACIONALIDAD                          = ""
    V.ESTADO_CIVIL                          = ""
    V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD 	    = ""
    V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD      = ""
    V.DIRECCION_PERSONA             		= ""
    V.MUNICIPIO_PERSONA             	 	= ""
    V.DEPARTAMENTO_PERSONA			        = ""
    V.PROFESION                             = ""
    V.NOMBRE_JURIDICO_RAZON_SOCIAL          = ""
    V.DIRECCION_EMPRESA               		= ""
    V.MUNICIPIO_EMPRESA               		= ""
    V.DEPARTAMENTO_EMPRESA               	= ""        
    V.ACTIVIDAD_ECONOMICA                   = ""
    V.TIPO_DE_DOCUMENTO                     = ""
    V.NUMERO_DE_DOCUMENTO                   = ""
    V.NUMERO_DE_PRODUCTO                    = ""
    V.TIPO_DE_TRANSACCION                   = ""
    V.CODIGO_DE_TRANSACCION                 = ""
    V.MONTO_TRANSACCION                     = ""
    V.MONTO_MEDIO_DE_PAGO                   = ""
    V.CONCEPTO_DE_LA_TRANSACCION            = ""
    Y.CODIGO.NARRATIVE 						= ""	
    Y.TFS.PAG.IMP 							= ""
    Y.TFS.PAG.IMP.RET 						= ""
    LOCATE "FECHA.CONSULTA" IN D.FIELDS<1> SETTING POS THEN
    	V.FECHA.CONSULTA = D.RANGE.AND.VALUE<POS>
    END
       
    LOCATE "TIPO.REPORTE" IN D.FIELDS<1> SETTING POS THEN
    	V.TIPO.REPORTE = D.RANGE.AND.VALUE<POS>
    END   
       
    LOCATE "RANGO.EFECTIVO" IN D.FIELDS<1> SETTING POS THEN
    	V.RANGO.EFECTIVO = D.RANGE.AND.VALUE<POS>
    END

    APPL = 'TELLER.FINANCIAL.SERVICES'
        FIELDNAME = 'LF.TRX.VISA'
        
        FN.P.TELLER='TELLER'
        
*-----------------------------------------------------------------------------
* DEBUG
*-----------------------------------------------------------------------------
*  V.FECHA.CONSULTA = '20180108'
*   V.TIPO.REPORTE   = 'UIF'
*   V.RANGO.EFECTIVO = 1
*-----------------------------------------------------------------------------
       
	RETURN


*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    CALL OPF(FN.FUNDS.TRANSFER, F.FUNDS.TRANSFER)
    CALL OPF(FN.TELLER, F.TELLER)
    CALL OPF(FN.COMPANY, F.COMPANY)
    CALL OPF(FN.USER, F.USER)
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    CALL OPF(FN.OFFICER,F.OFFICER)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.AML.ALERT.LOG, F.AML.ALERT.LOG)
	CALL OPF(FN.TABLE.NAC, F.TABLE.NAC)
	CALL OPF(FN.TFS, F.TFS)
	CALL OPF(FN.ARR, F.ARR)
	CALL OPF(FN.TXN, F.TXN)
	CALL OPF(FN.KEYS, F.KEYS)
	CALL OPF(FN.TRX.LG.HIST, F.TRX.LG.HIST)
	CALL OPF(FN.TT,F.TT)
	CALL OPF(FN.FT,F.FT)
	CALL OPF(FN.AML.PARAMETER,F.AML.PARAMETER)
	CALL OPF(FN.STMT.NARR.PARAM, F.STMT.NARR.PARAM)
    CALL OPF(FN.STMT.NARR.FORMAT, F.STMT.NARR.FORMAT)
	CALL OPF(FN.TRANSACTION, F.TRANSACTION)
	RETURN

 
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	SELECT.AML.ALERT.LOG  = "SELECT " : FN.AML.ALERT.LOG : " WITH ALR.DATE EQ '" : V.FECHA.CONSULTA : "' AND ALR.ID EQ '" : V.TIPO.REPORTE: "'"
    SELECT.AML.ALERT.LOG := " AND ALR.DAILY.HIS.IND NE 'H'" 
    
    ;*SELECT.AML.ALERT.LOG := " AND ALR.CUSTOMER EQ '130839'"
    ;*SELECT.AML.ALERT.LOG := " AND ALR.CONTRACT EQ 'TT17312FS8P7'"

	CALL EB.READLIST(SELECT.AML.ALERT.LOG, LIST.AML.ALERT.LOG, '', AML.ALERT.LOG.SIZE, ERR.ALERT.LOG.SIZE)
	STR.ARR = ''
	;* Recorrer cada id de las actividades diarias
     FOR FOR.MOVS.TODAY = 1 TO AML.ALERT.LOG.SIZE
        Y.ID.KEYS = ''
    	CALL F.READ( FN.AML.ALERT.LOG, LIST.AML.ALERT.LOG<FOR.MOVS.TODAY>, R.AML.ALERT.LOG, F.AML.ALERT.LOG, ERR.AML.ALERT.LOG )
        V.CODIGO_DE_CLIENTE 	= R.AML.ALERT.LOG<SLV.ALERT.ALR.CUSTOMER> 
        ;*Se agrega validacion para no mostrar el cliente BANCO
        ;*-----------------------------------------------------
        IF V.CODIGO_DE_CLIENTE EQ '999999' THEN
         	CONTINUE
        END
        
        V.CODIGO_DE_TRANSACCION = FIELD(R.AML.ALERT.LOG<SLV.ALERT.ALR.CONTRACT>, "\", 1)  
        V.NUMERO_DE_PRODUCTO 	= R.AML.ALERT.LOG<SLV.ALERT.ALR.ACCOUNT>
       	V.TIPO_DE_TRANSACCION   = LEFT(V.CODIGO_DE_TRANSACCION,2)
		V.CREDITO_O_DEBITO      = R.AML.ALERT.LOG<SLV.ALERT.ALR.DEB.CRE.IND>
		;*Para busqueda en EB.READ.HISTORY.RE, ya que se altera el dato con el ID 
		V.REF.TRANSACCION = V.CODIGO_DE_TRANSACCION
		Y.ID.TT =   V.CODIGO_DE_TRANSACCION
		S.REG.VALIDO = 'S'  	
		      	
		
		V.CODIGO.DE.TRANSACCION = V.CODIGO_DE_TRANSACCION
	    V.NUMERO.DE.PRODUCTO    = V.NUMERO_DE_PRODUCTO
	    
	   	CALL F.READ(FN.TELLER, V.CODIGO.DE.TRANSACCION:';1', RECORD.TT, F.TELLER, ERROR.TT)
	   	Y.TFS.REFERENCE = RECORD.TT<TT.TE.THEIR.REFERENCE>
	    
	    Y.ACC.DEBIT 	= RECORD.TT<TT.TE.ACCOUNT.2>
	    Y.ACC.CREDIT 	= RECORD.TT<TT.TE.ACCOUNT.1>
	    Y.CUS.DEBIT     = RECORD.TT<TT.TE.CUSTOMER.2>
	    Y.CUS.CREDIT    = RECORD.TT<TT.TE.CUSTOMER.1>
	    ;* Obtener bandera de retiro=2
	    ;*CALL F.READ(FN.TT, V.CODIGO.DE.TRANSACCION, REC.TT, F.TT, ERR.TT)
	    CALL GET.LOC.REF (FN.P.TELLER,FIELDNAME,POS2) 
    	Y.TFS.PAG.IMP.RET = TRIM(FIELD(RECORD.TT<TT.TE.LOCAL.REF><1,POS2>,'-',1))
	    
	    IF RECORD.TT<TT.TE.TRANSACTION.CODE> EQ 97 THEN
	       	Y.ACC.DEBIT 	= RECORD.TT<TT.TE.ACCOUNT.1>
	    	Y.ACC.CREDIT 	= RECORD.TT<TT.TE.ACCOUNT.2>
	    	Y.CUS.DEBIT     = RECORD.TT<TT.TE.CUSTOMER.1>
	    	Y.CUS.CREDIT    = RECORD.TT<TT.TE.CUSTOMER.2>
	    END
	    
	    CALL F.READ(FN.TFS, Y.TFS.REFERENCE, RECORD.TFS, F.TFS, ERROR.TFS)
       	CALL GET.LOC.REF('TELLER.FINANCIAL.SERVICES', 'LF.TFS.LOAN.REF', LocTFSLoanRef)
       	Y.ARR.ID = RECORD.TFS<TFS.LOCAL.REF,LocTFSLoanRef>
       	;*para obtener la descripcion si es tarjeta de credito pago
        CALL GET.LOC.REF (APPL,FIELDNAME,POS)
         Y.TFS.PAG.IMP = TRIM(FIELD(RECORD.TFS<TFS.LOCAL.REF><1,POS>,'-',1))
       
         ;*PRINT Y.TFS.PAG.IMP
       	IF Y.ARR.ID EQ '' THEN
       	  	IF RECORD.TT<TT.TE.TRANSACTION.CODE> EQ 41 THEN
		       	Y.ACC.DEBIT 	= RECORD.TT<TT.TE.ACCOUNT.1>
		    	Y.ACC.CREDIT 	= RECORD.TT<TT.TE.ACCOUNT.2>
		    	Y.CUS.DEBIT     = RECORD.TT<TT.TE.CUSTOMER.1>
		    	Y.CUS.CREDIT    = RECORD.TT<TT.TE.CUSTOMER.2>
	    	END
       	END
	    
	    IF Y.ACC.DEBIT NE Y.ACC.CREDIT THEN
	     IF V.CREDITO_O_DEBITO EQ 'C' THEN
	      V.NUMERO_DE_PRODUCTO = Y.ACC.CREDIT
	      IF  V.TIPO.REPORTE EQ 'UIF.OTROS' THEN
	      	V.CODIGO_DE_CLIENTE =  Y.CUS.CREDIT
	      END
	     END
	     ELSE
	      V.NUMERO_DE_PRODUCTO = Y.ACC.DEBIT
	      IF  V.TIPO.REPORTE EQ 'UIF.OTROS' THEN
	      	V.CODIGO_DE_CLIENTE =  Y.CUS.DEBIT
	      END
	     END   
	  END
	  
	  CALL F.READ(FN.TFS, Y.TFS.REFERENCE, RECORD.TFS, F.TFS, ERROR.TFS)
      CALL GET.LOC.REF('TELLER.FINANCIAL.SERVICES', 'LF.TFS.LOAN.REF', LocTFSLoanRef)
      Y.ARR.ID = RECORD.TFS<TFS.LOCAL.REF,LocTFSLoanRef>
       
      	CALL F.READ(FN.ARR, Y.ARR.ID, RECORD.ARR, F.ARR, ERROR.ARR)
      	Y.NUMERO.ACC = RECORD.ARR<AA.ARR.LINKED.APPL.ID>
      	Y.PRODUCT.ID = RECORD.ARR<AA.ARR.PRODUCT.LINE>
      
;*Se realiza validacion si la txn es DAP, para asignar en la variable V.NUMERO_DE_PRODUCTO el numero de cuenta desde el ARRANGEMENT
;*---------------------------------------------------------------------------------------------------------------------------------
		CALL F.READ(FN.ACCOUNT, V.NUMERO.DE.PRODUCTO, RECORD.ACC, F.ACCOUNT, ERR.ACC)
		Y.CODIGO.CLIENTE =  RECORD.ACC<AC.CUSTOMER>
		
		IF 	Y.CODIGO.CLIENTE EQ V.CODIGO_DE_CLIENTE THEN
	        IF RECORD.ARR<AA.ARR.PRODUCT.GROUP> EQ 'DEPOSITOS.BANCO.AZUL' THEN
	           V.NUMERO_DE_PRODUCTO = Y.NUMERO.ACC
	        END
		END	

;*Se agrega IF donde verifica si el codigo de Cliente viene vacio para obtener la info de otra forma
;*------------------------------------------------------------------------------------------------		
		IF V.CODIGO_DE_CLIENTE EQ '' THEN
           V.CODIGO_DE_CLIENTE     = RECORD.ARR<AA.ARR.CUSTOMER>
           V.NUMERO_DE_PRODUCTO    = Y.NUMERO.ACC
		END
	
		;*Validar que la alerta no este duplicada
		;*Se agrega nueva valor a llave compuesta para que muestre ambas transacciones en el reporte
		;*CRT V.CODIGO_DE_CLIENTE:V.CODIGO_DE_TRANSACCIONN
		;*CRT STR.ARR
		;*CRT '--------------------------------------------------------------------------------'
		;*CRT V.CODIGO_DE_CLIENTE:V.CREDITO_O_DEBITO
		FINDSTR V.CODIGO_DE_CLIENTE:V.CODIGO_DE_TRANSACCION IN STR.ARR SETTING Ap1, Vp1 THEN
			Y.POS = 1
		END ELSE
			Y.POS = 0
		END
		
		IF Y.POS EQ 0 THEN		
			;*Control de referencias de transacciones para evitar duplicidad
			STR.ARR<-1>   = V.CODIGO_DE_CLIENTE:V.CODIGO_DE_TRANSACCION	
	      	IF V.TIPO_DE_TRANSACCION EQ 'FT' THEN
	      		GOSUB PROCESS.FT	
	      		IF Y.SALTAR EQ 'S' THEN
	      			CONTINUE
	      		END
	      	END
	
	      	IF V.TIPO_DE_TRANSACCION EQ 'TT' THEN
	      	    GOSUB PROCESS.TO.SHOW.ONE.TXN
	      	    IF Y.CONTINUE EQ 'S' THEN
	      	     	CONTINUE
	      	    END
	      	    ELSE
	      			GOSUB PROCESS.TT
	      		END		            
	      	END 
	      	
	      ;*	PRINT 'V.MONTO_MEDIO_DE_PAGO = ':V.MONTO_MEDIO_DE_PAGO
	      ;*	PRINT 'V.RANGO.EFECTIVO = ':V.RANGO.EFECTIVO
			IF V.MONTO_MEDIO_DE_PAGO LT V.RANGO.EFECTIVO OR S.REG.VALIDO = 'N' THEN
				CONTINUE
			END
			
			;*Se valida si es ACNA y si el monto esta por encima del limite
			;*-------------------------------------------------------------
			  
			;* Obtenemos la informacion de la compañia.
			CALL F.READ(FN.COMPANY, V.AGENCIA.CODE, RS.COMPANY, F.COMPANY, ERR.COMPANY)
			
			;* Obtenemos la informacion del colaborador
			CALL F.READ(FN.USER, V.CODIGO_COLABORADOR,  RS.USER,  F.USER, ERR.USER)
				              
			;* EJECUTIVO
			CALL F.READ(FN.OFFICER, RS.USER<EB.USE.DEPARTMENT.CODE>, R.OFFICER , F.OFFICER , ERR.FN.OFFICER)
			  
			;* INFORMACION DEL CLIENTE
			CALL F.READ(FN.CUSTOMER, V.CODIGO_DE_CLIENTE, R.CUSTOMER , F.CUSTOMER, ERR.CUSTOMER)
			                                                          
			CALL GET.LOC.REF('CUSTOMER', 'LF.LUG.NAC', LOCPOSLugarNacio)
			CALL GET.LOC.REF('CUSTOMER', 'LF.RAZON.SOCIAL', LOCPOSRasonSocial)
			CALL GET.LOC.REF('CUSTOMER', 'LF.TYPE.CUST', LOCPOEstadoCivil)                          
			CALL GET.LOC.REF('CUSTOMER', 'SEGMENT', LOCPOSSegment)
	                
	        ;* AGENCIA           T
	        ;*V.NOMBRE_AGENCIA = FIELD(RS.COMPANY<EB.COM.NAME.ADDRESS>, VM, 1)
	        V.NOMBRE_AGENCIA = UPCASE(EREPLACE(RS.COMPANY<EB.COM.COMPANY.NAME><1,1>, LEFT(RS.COMPANY<EB.COM.COMPANY.NAME><1,1>,10),""))       
	        V.MUNICIPIO_AGENCIA = UPCASE(FIELD(RS.COMPANY<EB.COM.NAME.ADDRESS>, VM, 4)) 
	        V.DEPARTAMENTO_AGENCIA = UPCASE(FIELD(RS.COMPANY<EB.COM.NAME.ADDRESS>, VM, 3)) 
	                      
	        V.CARGO_COLABORADOR = R.OFFICER<EB.DAO.NAME>
	          				
			;*Limpiar variables	
			GOSUB LIMPIAR.CAMPOS
	              
			IF R.CUSTOMER<EB.CUS.LOCAL.REF><1,LOCPOSSegment> LE 1 THEN
				SS.PERSONERIA = '1'
			END 
			ELSE
				SS.PERSONERIA = '2'
			END
			
			;* Obtener dirección del cliente
			GOSUB GET.DIRECCION
										
			IF SS.PERSONERIA = '1' THEN
				;*PERSONA NATURAL
	          	V.PRIMER_NOMBRE    		= R.CUSTOMER<EB.CUS.NAME.1>
	            V.SEGUNDO_NOMBRE   		= R.CUSTOMER<EB.CUS.NAME.2>
	            V.TERCER_NOMBRE    		= R.CUSTOMER<EB.CUS.GIVEN.NAMES>
	           	V.PRIMER_APELLIDO  		= R.CUSTOMER<EB.CUS.TEXT>
	            V.SEGUNDO_APELLIDO 		= R.CUSTOMER<EB.CUS.FAMILY.NAME>
	            V.APELLIDO_CASADA  		= R.CUSTOMER<EB.CUS.PREVIOUS.NAME>
	            V.FECHA_DE_NACIMIENTO 	= R.CUSTOMER<EB.CUS.DATE.OF.BIRTH>
	           	V.LUGAR_DE_NACIMIENTO 	= R.CUSTOMER<EB.CUS.LOCAL.REF><1,LOCPOSLugarNacio>
	           	V.PROFESION             = UPCASE(R.CUSTOMER<EB.CUS.OCCUPATION>)
	        	V.DIRECCION_PERSONA     = SS.PN.DIRECCION
	        	V.MUNICIPIO_PERSONA     = SS.PN.MUNICIPIO
	           	V.DEPARTAMENTO_PERSONA 	= SS.PN.DEPARTAMENTO
	        	V.TIPO_DE_PERSONA       = SS.PERSONERIA
	
	           	;* Recuperando el número y fecha de expedición del documento
			    FINDSTR 'DOCTO.UNICO.IDENT' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
			       	V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
			        V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD = 'DUI'
			    END
			    ELSE FINDSTR 'PASSPORT' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
			    	V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
			    	V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD = 'PASAPORTE'
			   	END
			    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
			    	V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
			    	V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD = 'CARNET RESIDENTE'
			   	END
				ELSE FINDSTR 'NUM.IDEN.TRIBUT' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
			    	V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
			    	V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD = 'NIT'
			   	END   
			   	
				;* Encontrando el estado civil
				MARITAL.STATUS = R.CUSTOMER<EB.CUS.MARITAL.STATUS>		
				BEGIN CASE
			    	CASE MARITAL.STATUS EQ 'ACOMPA'
			        	V.ESTADO_CIVIL = 'ACOMPAÑADO(A)'
			    	CASE MARITAL.STATUS EQ 'DIVORCED'
			        	V.ESTADO_CIVIL = 'DIVORCIADO(A)'
			    	CASE MARITAL.STATUS EQ 'MARRIED'
			        	V.ESTADO_CIVIL = 'CASADO(A)'
			    	CASE MARITAL.STATUS EQ 'PARTNER'
			        	V.ESTADO_aCIVIL = 'ESPOSO(A)'
			    	CASE MARITAL.STATUS EQ 'SINGLE'
			        	V.ESTADO_CIVIL = 'SOLTERO(A)'
			    	CASE MARITAL.STATUS EQ 'WIDOWED'
			        	V.ESTADO_CIVIL = 'VIUDO(A)'
			   		CASE MARITAL.STATUS EQ 'OTHER'
			        	V.ESTADO_CIVIL = 'OTRO'
			    	CASE 1
			        	V.ESTADO_CIVIL = 'N/A'
				END CASE	  
				
	       		;* Encontrando la nacionalidad
				CALL F.READ(FN.TABLE.NAC, R.CUSTOMER<EB.CUS.NATIONALITY>, R.TABLE.NAC, F.TABLE.NAC, F.ERR.NAC)
				V.NACIONALIDAD = UPCASE(R.TABLE.NAC<EB.COU.COUNTRY.NAME><1,2>)	   		
			END 
			ELSE
				;*Evaluar SHORT.NAME multivalor
				Y.SHORT.NAME = ''
				Y.COUNT = DCOUNT(R.CUSTOMER<EB.CUS.SHORT.NAME>, VM)

				FOR J=1 TO Y.COUNT
					Y.SHORT.NAME := R.CUSTOMER<EB.CUS.SHORT.NAME><1,J>
					IF J NE Y.COUNT THEN
						Y.SHORT.NAME := " / "
					END
 
				NEXT J
			
	            ;*PERSONA JURIDICA
	            V.NOMBRE_JURIDICO_RAZON_SOCIAL = Y.SHORT.NAME :" / ": CHANGE(R.CUSTOMER<EB.CUS.LOCAL.REF><1,LOCPOSRasonSocial>, @SM, " ")
	            V.DIRECCION_EMPRESA    	= SS.PJ.DIRECCION
	        	V.MUNICIPIO_EMPRESA    	= SS.PJ.MUNICIPIO
	           	V.DEPARTAMENTO_EMPRESA 	= SS.PJ.DEPARTAMENTO                 
	            V.TIPO_DE_PERSONA 		= SS.PERSONERIA
	            
				FINDSTR 'NUM.IDEN.TRIBUT' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
			    	V.NUMERO_DE_DOCUMENTO = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
			    	V.TIPO_DE_DOCUMENTO = 'NIT'
			   	END      
			   	
				;* Encontrando la actividad económica
				CALL F.READ(FN.TABLE.IND, R.CUSTOMER<EB.CUS.INDUSTRY>, R.TABLE.IND, F.TABLE.IND, F.ERR.IND)
				V.ACTIVIDAD_ECONOMICA = R.TABLE.IND<EB.IND.DESCRIPTION>		
			END
	                                                                                                
			;*Construccion de arreglo
	 		STR.MOV = ''
		    STR.MOV := V.CODIGO_DE_CLIENTE : "*"						;* 1
		    STR.MOV := V.NOMBRE_AGENCIA : "*"							;* 2
		    STR.MOV := V.MUNICIPIO_AGENCIA : "*"						;* 3
		    STR.MOV := V.DEPARTAMENTO_AGENCIA : "*"						;* 4
		    STR.MOV := V.CODIGO_COLABORADOR : "*"						;* 5
		    STR.MOV := V.CARGO_COLABORADOR : "*"						;* 6
		    STR.MOV := V.FECHA_DE_LA_TRANSACCION : "*"					;* 7
		    STR.MOV := V.TIPO_DE_PERSONA : "*"							;* 8
		    STR.MOV := V.PRIMER_APELLIDO : "*"							;* 9
		    STR.MOV := V.SEGUNDO_APELLIDO : "*"							;* 10
		    STR.MOV := V.APELLIDO_CASADA : "*"							;* 11
		    STR.MOV := V.PRIMER_NOMBRE : "*"							;* 12
		    STR.MOV := V.SEGUNDO_NOMBRE : "*"							;* 13
		    STR.MOV := V.TERCER_NOMBRE : "*"							;* 14
		    STR.MOV := V.FECHA_DE_NACIMIENTO : "*"						;* 15
		    STR.MOV := V.LUGAR_DE_NACIMIENTO : "*"						;* 16	
		    STR.MOV := V.NACIONALIDAD : "*"								;* 17
		    STR.MOV := V.ESTADO_CIVIL : "*"								;* 18
		    STR.MOV := V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD : "*"			;* 19
		    STR.MOV := V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD : "*"			;* 20	
		    STR.MOV := V.DIRECCION_PERSONA : "*"						;* 21
		    STR.MOV := V.MUNICIPIO_PERSONA : "*"						;* 22	
		    STR.MOV := V.DEPARTAMENTO_PERSONA : "*"						;* 23
		    STR.MOV := V.PROFESION : "*"								;* 24
		    STR.MOV := V.NOMBRE_JURIDICO_RAZON_SOCIAL : "*"				;* 25
		    STR.MOV := V.DIRECCION_EMPRESA : "*"						;* 26
		    STR.MOV := V.MUNICIPIO_EMPRESA : "*"						;* 27
		    STR.MOV := V.DEPARTAMENTO_EMPRESA : "*"        				;* 28
		    STR.MOV := V.ACTIVIDAD_ECONOMICA : "*"						;* 29
		    STR.MOV := V.TIPO_DE_DOCUMENTO : "*"						;* 30
	        STR.MOV := V.NUMERO_DE_DOCUMENTO : "*"						;* 31
	        STR.MOV := V.NUMERO_DE_PRODUCTO : "*"						;* 32
	        STR.MOV := V.TIPO_DE_TRANSACCION: "*"						;* 33
	        FINDSTR Y.ID.KEYS IN STR.ARR.TFS.TT SETTING POS THEN;*Y.ID.TFS
	  		  STR.MOV := Y.ID.KEYS : "*"					            ;* 34
  			END
  			ELSE
	        	STR.MOV := V.CODIGO_DE_TRANSACCION : "*"			    ;* 34
	        END
	        
	        STR.MOV := V.MONTO_TRANSACCION : "*"						;* 35
	        STR.MOV := V.MONTO_MEDIO_DE_PAGO : "*"						;* 36
	        
	        GOSUB DESCRIP.CHQ.PROPIO
	        GOSUB DESCRIP.PAGO.PRES
	        STR.MOV := V.CONCEPTO_DE_LA_TRANSACCION : "*"				;* 37	
	        
	         STR.MOV := Y.CODIGO.NARRATIVE	 : "*"						;* 38								
			;* Códificar caracteres especiales
			STR.MOV = CHANGE(STR.MOV, 'Ñ', CHAR(465))
	                    
			ENQ.DATA<-1> = STR.MOV
			;*PRINT STR.MOV
    	END         
	NEXT FOR.MOVS.TODAY

	ENQ.DATA = SORT(ENQ.DATA)
	;*PRINT STR.MOV
	RETURN
	
	PROCESS.TT:
		;*Obtener el ultimo registro del historico
		
  		CALL EB.READ.HISTORY.REC(F.TELLER, V.REF.TRANSACCION, R.TELLER, ERR.TELLER)
  		;*Debug
  		;*FN.TELLER = 'F.TELLER'
  		;*F.TELLER  = ''
  		;*CALL OPF(FN.TELLER, F.TELLER)
  		;*CALL F.READ(FN.TELLER, V.REF.TRANSACCION, R.TELLER, F.TELLER, ERR.TELLER)

        ;*Para obtener concepto de la transaccion
        CALL F.READ(FN.TELL.TXN, R.TELLER<TT.TE.TRANSACTION.CODE>, R.TELL.TXN, F.TELL.TXN, ERR.TELL.TXN)
        
        V.AGENCIA.CODE                  = R.TELLER<TT.TE.CO.CODE>
        V.MONTO_TRANSACCION             = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
        V.MONTO_MEDIO_DE_PAGO           = R.TELLER<TT.TE.AMOUNT.LOCAL.1>
        V.CONCEPTO_DE_LA_TRANSACCION    = R.TELL.TXN<TT.TR.TRANSACTION.CODE.2>
        V.THEIR.REFERENCE               = R.TELLER<TT.TE.THEIR.REFERENCE>
        
        GOSUB DESCRIP.CHQ.PROPIO
        ;*-------------------------------------------------------------------------
        V.CODIGO_COLABORADOR 			= FIELD(R.TELLER<TT.TE.INPUTTER>, "_", 2)
        V.FECHA_DE_LA_TRANSACCION		= R.TELLER<TT.TE.VALUE.DATE.1>
		
        
        IF LEFT(R.TELLER<TT.TE.THEIR.REFERENCE>,3) EQ 'TFS' THEN
        	CALL F.READ(FN.TFS, R.TELLER<TT.TE.THEIR.REFERENCE>, R.TFS, F.TFS, ERR.TFS)
        
        IF Y.TFS.PAG.IMP EQ 1 THEN 
          Y.CONCEPT.CODE  = '':V.CONCEPTO_DE_LA_TRANSACCION:''
            CALL F.READ(FN.TRANSACTION,Y.CONCEPT.CODE, R.TRANSACTION.TXN, F.TRANSACTION, ERR.TRANSACTION.TXN)
            Y.CODIGO.NARRATIVE =  R.TRANSACTION.TXN<18>
             CALL F.READ( FN.STMT.NARR.PARAM,Y.CODIGO.NARRATIVE , R.NARR.STMT, F.NARR.TXN, ERR.NARR.TXN)
          Y.TIPOCAMPO =  R.NARR.STMT<6>
               CALL F.READ( FN.STMT.NARR.FORMAT,Y.TIPOCAMPO, R.NARR.FORMAT.T, F.FORMAT.TXN, ERR.FORMAT.TXN)
           
            Y.CODIGO.NARRATIVE = R.NARR.FORMAT.T<3>
             Y.CODIGO.NARRATIVE = CHANGE(Y.CODIGO.NARRATIVE, '"', "")
        END
        
        IF Y.TFS.PAG.IMP.RET EQ 2 THEN 
        Y.CODIGO.NARRATIVE ='Retiro de Tarjeta de Crédito'
        END
        	
        	Y.NO.TXN 		= DCOUNT(R.TFS<TFS.TRANSACTION>,VM)
        	Y.ACC.PRIMARY 	=  R.TFS<TFS.PRIMARY.ACCOUNT>
        	;*Definiendo la descripcion cuando la TFS contiene varias TT de diferente TRANSACTION.CODE
        	;*Solo cambia descripcion cuando la TFS tiene que ver con deposito a cuenta (Corriente/Ahorro) : JHENRIQUEZ 05.12.2017
        	;*----------------------------------------------------------------------------------------------------------------
            IF Y.NO.TXN GT 1 THEN 
              Y.TIPO.TFS = Y.ID.TFS
              CALL SLV.UTIL.TFS.GET.NOM.COMP(Y.TIPO.TFS)
              FINDSTR 'REMESA EN CUENTA' IN Y.TIPO.TFS SETTING Ap,Vp THEN
                V.CONCEPTO_DE_LA_TRANSACCION = 51
              END
              ELSE ;*Se realiza busqueda en la aplicacion de historico cuando el reporte es de dias anteriores
                STMT.TRX.LOG.HIST = "SELECT ":FN.TRX.LG.HIST:" WITH @ID LIKE %":V.FECHA.CONSULTA
 				CALL EB.READLIST (STMT.TRX.LOG.HIST, LINK.STMT.TRX.LOG.HIST, '', NO.STMT.TRX.LOG.HIST, ERR.STMT.TRX.LOG.HIST)
 				FOR I=1 TO NO.STMT.TRX.LOG.HIST
			    	CALL F.READ(FN.TRX.LG.HIST, LINK.STMT.TRX.LOG.HIST<I>, RECORD.TRX.LOG.HIST, F.TRX.LG.HIST, ERR.STMT.TRX.LOG.HIST)
			    	FINDSTR Y.ID.TFS IN RECORD.TRX.LOG.HIST<EB.SLV56.REG.TRX> SETTING Ap,Vp THEN
			    	   	Y.VERSION = RECORD.TRX.LOG.HIST<EB.SLV56.VERSION,Vp>
		                IF Y.VERSION EQ 'TELLER.FINANCIAL.SERVICES,SLV.REMESA.CTA.CTE' THEN
		                  V.CONCEPTO_DE_LA_TRANSACCION = 51
		                END
		                ELSE IF Y.VERSION EQ 'TELLER.FINANCIAL.SERVICES,SLV.DEP.CTA.AHO' THEN
		                  V.CONCEPTO_DE_LA_TRANSACCION = 51
		                END
			    	END
                NEXT I
              END
            END
        	V.MONTO_TRANSACCION = R.TFS<TFS.RUNNING.TOTAL><1,1>
        	
        	;*Acumular Todas las Transacciones que no sean UIF.OTROS : OCORNEJO 04.12.2017
        	;*----------------------------------------------------------------------------
        	IF Y.ACC.PRIMARY EQ V.NUMERO_DE_PRODUCTO THEN
        	   GOSUB PROCESS.TFS
        	END
		
		;*Obtener cuenta del producto del cliente cuando en la alerta se escriba una cuenta interna
        IF LEFT(R.AML.ALERT.LOG<SLV.ALERT.ALR.ACCOUNT>,3) EQ 'USD' THEN
        	CALL F.READ(FN.ARR, FIELD(R.TELLER<TT.TE.NARRATIVE.2>,"-",1), R.ARR, F.ARR, ERR.ARR)
        	IF R.ARR NE '' THEN
        		V.NUMERO_DE_PRODUCTO = R.ARR<AA.ARR.LINKED.APPL.ID>
        	END	
        END

		;*Verificar si el registro tiene reversa o no está autorizado
		IF R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'REVE' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAU' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAO' OR R.TFS<TFS.RECORD.STATUS> EQ 'RNAU' OR R.TFS<TFS.RECORD.STATUS>EQ 'RNAO'  OR R.TFS<TFS.RECORD.STATUS>EQ 'REVE' THEN
			S.REG.VALIDO = 'N'
		END
	END
	RETURN

	DESCRIP.CHQ.PROPIO:
	 	;*Cuando sea Deposito con Cheque Propio colocar la TT.TR.TRANSACTION.CODE.1 
		;*sino sale concepto erroneo en UIF.OTROS : OCORNEJO 29.11.2017
	    ;*-------------------------------------------------------------------------
	    IF R.TELLER<TT.TE.TRANSACTION.CODE> EQ 44 THEN
	    	 V.CONCEPTO_DE_LA_TRANSACCION    = R.TELL.TXN<TT.TR.TRANSACTION.CODE.1>
	    	 IF V.CREDITO_O_DEBITO EQ 'D' THEN
	    	 	 V.CONCEPTO_DE_LA_TRANSACCION    = 42
	    	 END 
	    END
	RETURN	

	DESCRIP.PAGO.PRES:
	    
		    STM.FT = "SELECT ":FN.FUNDS.TRANSFER:" WITH ORDERING.CUST EQ ":V.THEIR.REFERENCE
		    CALL EB.READLIST (STM.FT, KEY.LIST, '', SELECTED, SYSTEM.RETURN.CODE)
		    
		    CALL F.READ(FN.FUNDS.TRANSFER, KEY.LIST, RECORD.FT, F.FUNDS.TRANSFER, ERR.FT)
		    V.FT.TXN.TYPE = RECORD.FT<FT.TRANSACTION.TYPE>
		    
		    IF V.FT.TXN.TYPE EQ 'ACRP' THEN
		     CALL F.READ(FN.TXN, RECORD.FT<FT.TRANSACTION.TYPE>, R.TXN, F.TXN, ERR.TXN)
		     V.CONCEPTO_DE_LA_TRANSACCION = R.TXN<FT6.TXN.CODE.DR>
		    END
	    
	RETURN

	PROCESS.TFS:
		;*Obtener Txn de Efectivo
		GOSUB GET.TXN.EFECTIVO
		
		;*Acumular todas las Transacciones que no sean Efectivo
		V.MONTO_MEDIO_DE_PAGO = 0
		;*Y.NO.TXN = DCOUNT(R.TFS<TFS.TRANSACTION>,VM)
		FOR TXN = 1 TO Y.NO.TXN
		;*Se agrega validacion  para que identifique los saldo a mostrar dependiendo del reporte
		IF V.TIPO.REPORTE EQ 'UIF.OTROS' THEN
		
			FIND R.TFS<TFS.TRANSACTION><1, TXN> IN Y.TXN.EFECTIVO SETTING Ap, Vp ELSE
			  V.MONTO_MEDIO_DE_PAGO += R.TFS<TFS.AMOUNT><1, TXN>
			END
			   
		END	
		ELSE 
		    FIND R.TFS<TFS.TRANSACTION><1, TXN> IN Y.TXN.EFECTIVO SETTING Ap, Vp THEN
				V.MONTO_MEDIO_DE_PAGO += R.TFS<TFS.AMOUNT><1, TXN>
			END
		END	
		NEXT TXN 
	RETURN
	
	GET.TXN.EFECTIVO:
		CALL CACHE.READ(FN.KEYS, 'UIF.TRX.TYPE', R.KEYS, E.KEYS)
		FIND 'EFECTIVO' IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
			Y.TXN.EFECTIVO = CHANGE(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, VM)
		END
	RETURN
	
	PROCESS.FT:
		;*Obtener el ultimo registro del historico
  		CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER, V.REF.TRANSACCION, R.FUNDS.TRANSFER, ERR.FUNDS.TRANSFER)
  		
  		;*Validar si la Txn es Pago de Prestamo y si viene de Caja ya que este registro debe
  		;*ignorarse pues la alerta que importa es la que genera la TT de la TFS : OCORNEJO 30.11.2017
  		;*-------------------------------------------------------------------------------------------
  		Y.SALTAR = 'N'
  		IF R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE> EQ 'ACRP' AND LEFT(R.FUNDS.TRANSFER<FT.ORDERING.CUST>,3) EQ 'TFS' THEN
  			Y.SALTAR = 'S'
  			RETURN
  		END
  		
  		CALL F.READ(FN.AML.PARAMETER,'SYSTEM',RECORD.AML.PARAMETER, F.AML.PARAMETER, ERROR.AML.PARAMETER)
  		Y.AMT.MAX.UIF.OTROS = RECORD.AML.PARAMETER<SLV.PROF.MAX.UIF.AMOUNT.OTR>
  		
  		IF R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE> EQ 'ACNA' AND R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT> LT Y.AMT.MAX.UIF.OTROS  THEN
  		   Y.SALTAR = 'S'
  		RETURN
  		END
  		;*-------------------------------------------------------------------------------------------
	
		;*Para obtener concepto de la transaccion
		CALL F.READ(FN.TXN, R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE>, R.TXN, F.TXN, ERR.TXN)
		;*IF R.AML.ALERT.LOG<SLV.ALERT.ALR.DEB.CRE.IND> EQ 'C' THEN
		IF V.CREDITO_O_DEBITO EQ 'C' THEN
			V.CONCEPTO_DE_LA_TRANSACCION    = R.TXN<FT6.TXN.CODE.DR>
		END ELSE
			V.CONCEPTO_DE_LA_TRANSACCION    = R.TXN<FT6.TXN.CODE.CR>
		END
		
		;*Se agregar IF porque modificar los TRANSACTION le pega a muchas mas 
		;*FT.TXN.TYPE.CONDITION y TELLER.TRANSACTION : OCORNEJO 06.12.2017
		IF R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE> EQ 'ACCP' THEN
			V.CONCEPTO_DE_LA_TRANSACCION    = R.TXN<FT6.DR.CHEQUE.TXN.CODE>
		END
		
		V.AGENCIA.CODE                  = R.FUNDS.TRANSFER<FT.CO.CODE>
		V.MONTO_TRANSACCION             = R.FUNDS.TRANSFER<FT.LOC.AMT.DEBITED>
		V.MONTO_MEDIO_DE_PAGO           = R.FUNDS.TRANSFER<FT.LOC.AMT.DEBITED>
		V.CODIGO_COLABORADOR 			= FIELD( R.FUNDS.TRANSFER<FT.INPUTTER>, "_", 2)
		V.FECHA_DE_LA_TRANSACCION		= R.FUNDS.TRANSFER<FT.PROCESSING.DATE>
		  
		;*Obtener monto total de transaccion del registro TFS
		IF LEFT(R.FUNDS.TRANSFER<FT.TFS.REFERENCE>,3) EQ 'TFS' THEN
        	CALL F.READ(FN.TFS, R.FUNDS.TRANSFER<FT.TFS.REFERENCE>, R.TFS, F.TFS, ERR.TFS)
        	V.MONTO_TRANSACCION = R.TFS<TFS.RUNNING.TOTAL><1,1>
  		END
		
		;*Verificar si el registro tiene reversa o no está autorizado
		IF R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'INAU' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'INAO' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'RNAU' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'RNAO' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'REVE' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAU' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAO' OR R.TFS<TFS.RECORD.STATUS> EQ 'RNAU' OR R.TFS<TFS.RECORD.STATUS>EQ 'RNAO' OR R.TFS<TFS.RECORD.STATUS>EQ 'REVE' THEN			
			S.REG.VALIDO = 'N'
		END
	RETURN

PROCESS.TO.SHOW.ONE.TXN:
;*Lo que realiza este GOSUB es, no permitir que la transaccion se muestre mas de una vez, por ejemplo si se realiza una TFS con 8 Depositos de cheque ajeno
;*El GOSUB solo permite mostrar un registro de esos 8 ya que los demas tendrian los mismos datos
;*---------------------------------------------------------------------------------------------------------------------------------------------------------
       CALL EB.READ.HISTORY.REC(F.TELLER, Y.ID.TT, R.TELLER, ERR.TELLER)
  		Y.ID.TFS 		= R.TELLER<TT.TE.THEIR.REFERENCE>
  		Y.AMOUNT.TXN 	= R.TELLER<TT.TE.AMOUNT.LOCAL.1>
  		Y.ACC1 			= R.TELLER<TT.TE.ACCOUNT.1> 
  		IF Y.ID.TFS EQ '' THEN
  		  Y.ID.TFS = Y.ID.TT
  		END
  		
  		Y.TXN.CODE = R.TELLER<TT.TE.TRANSACTION.CODE>
  		Y.ID.KEYS = Y.ID.TFS
  		
  		;*Y.ID = Y.ID.TFS:V.CREDITO_O_DEBITO
  		Y.ID =  Y.ID.TFS:Y.TXN.CODE:V.NUMERO_DE_PRODUCTO
  		FINDSTR Y.ID IN STR.ARR.TFS.TT SETTING Ap1, Vp1 THEN
  		  Y.CONTINUE = 'S'
  		  V.CODIGO_DE_TRANSACCION  = ''
  		  
  		  ;*Cuando la transaccion con lleva varios TT de las misma forma de pago se agrega apartado donde se busca la posicion en el arreglo se extrae 
  		  ;*el saldo que ya esta registrado y se le suma el saldo de la transcion que tiene la misma forma de pago, esto se realiza debido al proceso que
  		  ;*solicitaron, que una forma de pago se muestre en una sola linea con el monto sumado.
  		  ;*-----------------------------------------------------------------------------------------------------------------------------------------------
  	     IF Y.ACC1 NE V.NUMERO_DE_PRODUCTO THEN
	  	      FINDSTR  V.NUMERO_DE_PRODUCTO:'*':'TT':'*':Y.ID.TFS IN  ENQ.DATA SETTING Am, Vm THEN
	  		  	Y.ARR.ENQ.DATA =  ENQ.DATA<Am>
	  		  	;*PRINT Y.ARR.ENQ.DATA
	  		  	Y.ARR.VM = CHANGE(Y.ARR.ENQ.DATA,'*',VM)
	  		  	Y.SALDO.TXN.ACUM = Y.ARR.VM<1,36> 
	  		  	Y.SALDO.TXN.ACUM += Y.AMOUNT.TXN
	  		  	Y.ARR.VM<1,36> = Y.SALDO.TXN.ACUM
	  		  	Y.ARR.ENQ.DATA = CHANGE(Y.ARR.VM,VM,'*')
	  		  	ENQ.DATA<Am> = Y.ARR.ENQ.DATA
	  		  END
	  	  END  
  		END
  		ELSE
  			IF V.NUMERO_DE_PRODUCTO EQ '' THEN
  			  Y.CONTINUE = 'S'
  			END
  			ELSE
  			  	Y.CONTINUE = 'N'
  		  		STR.ARR.TFS.TT<-1>   = Y.ID;*:V.CODIGO_DE_TRANSACCION:V.CREDITO_O_DEBITO
  			END
  		END
  		
  		
RETURN
*-----------------------------------------------------------------------------
LIMPIAR.CAMPOS:
*-----------------------------------------------------------------------------
    R.TELLER = ''
	Y.ACC.DEBIT 	= ""
	Y.ACC.CREDIT 	= ""
	Y.CUS.DEBIT     = ""
	Y.CUS.CREDIT    = ""
	Y.SALTAR        = ""
	;*PERSONA NATURAL
   	V.PRIMER_NOMBRE = ""
   	V.SEGUNDO_NOMBRE = ""
   	V.TERCER_NOMBRE = ""
   	V.PRIMER_APELLIDO = ""
   	V.SEGUNDO_APELLIDO = ""
   	V.APELLIDO_CASADA = ""
   	V.FECHA_DE_NACIMIENTO = ""
   	V.LUGAR_DE_NACIMIENTO = ""
   	V.NACIONALIDAD = ""
   	V.ESTADO_CIVIL = ""
   	V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD = ""
   	V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD = ""
   	V.PROFESION = " "
   	V.DIRECCION_PERSONA = ""
   	V.MUNICIPIO_PERSONA = " "
   	V.DEPARTAMENTO_PERSONA = " "

	;*PERSONA JURICA
   	V.NOMBRE_JURIDICO_RAZON_SOCIAL = ""
   	V.TIPO_DE_DOCUMENTO = ""
   	V.ACTIVIDAD_ECONOMICA = ""
   	V.NUMERO_DE_DOCUMENTO = ""
   	V.NOMBRE_JURIDICO_RAZON_SOCIAL = ""
   	V.DIRECCION_EMPRESA = ""
   	V.MUNICIPIO_EMPRESA = ""
   	V.DEPARTAMENTO_EMPRESA = ""
		 
	RETURN
              
*-----------------------------------------------------------------------------
GET.DIRECCION:
*-----------------------------------------------------------------------------
	;* Encontrando la dirección
  	SS.PN.DIRECCION = ''
  	SS.PN.MUNICIPIO  = ''
  	SS.PN.DEPARAMENTO  = ''
  	SS.PJ.DIRECCION = ''
  	SS.PJ.MUNICIPIO  = ''
  	SS.PJ.DEPARAMENTO  = ''

  	;* es una PERSONA NATURAL
	IF SS.PERSONERIA EQ '' THEN
  		LS.FLD.COLONIA = 'LF.COLONIA.3'
  		LS.FLD.CALLE = 'LF.CALLE.3'
  		LS.FLD.AVENIDA = 'LF.AVENIDA.3'
  		LS.FLD.DEPARTAMENTO = 'LF.DEPTO.3'
  		LS.FLD.MUNICIPIO = 'LF.MUNICIPIO.3'
  	END
  	ELSE 
   		LS.FLD.COLONIA = 'LF.COLONIA'
  		LS.FLD.CALLE = 'LF.CALLE'
  		LS.FLD.AVENIDA = 'LF.AVENIDA'
  		LS.FLD.DEPARTAMENTO = 'LF.DEPARTAMENTO'
  		LS.FLD.MUNICIPIO = 'LF.MUNICIPIO' 	
  	END
  	
  	LS.DIRECCION = ''
	CALL GET.LOC.REF('CUSTOMER', LS.FLD.COLONIA, POS)
	IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
    	LS.DIRECCION := R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
	END

	CALL GET.LOC.REF('CUSTOMER', LS.FLD.CALLE, POS)
	IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
    	LS.DIRECCION := ', ' : R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
	END
	
	CALL GET.LOC.REF('CUSTOMER', LS.FLD.AVENIDA, POS)
	IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
    	LS.DIRECCION := ', ' : R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
	END

	;* Encontrando el municipio
	CALL GET.LOC.REF('CUSTOMER', LS.FLD.MUNICIPIO, POS)
	CALL F.READ(FN.TABLE.MUN, R.CUSTOMER<EB.CUS.LOCAL.REF, POS>, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)

	;* Encontrando el departamento
	CALL GET.LOC.REF('CUSTOMER', LS.FLD.DEPARTAMENTO, POS)
	CALL F.READ(FN.TABLE.DEP, R.CUSTOMER<EB.CUS.LOCAL.REF, POS>, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)

	IF SS.PERSONERIA EQ '1' THEN
		SS.PN.DIRECCION = LS.DIRECCION
        SS.PN.MUNICIPIO = R.TABLE.MUN<EB.SLV33.DESCRIPTION>
        SS.PN.DEPARTAMENTO	= R.TABLE.DEP<EB.SLV43.DESCRIPTION>
	END
	ELSE
		SS.PJ.DIRECCION = LS.DIRECCION
        SS.PJ.MUNICIPIO = R.TABLE.MUN<EB.SLV33.DESCRIPTION>
        SS.PJ.DEPARTAMENTO	= R.TABLE.DEP<EB.SLV43.DESCRIPTION>
	END

	RETURN
END
