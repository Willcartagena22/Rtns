*----------------------------------------------------------------------------------------------------
* <Rating>4287</Rating>
*----------------------------------------------------------------------------------------------------
* Nombre: 		SLV.E.NOF.UIF.ACUM
* Descripcion: 	Rutina para enquiry de alertas UIF acumuladas por mes y monto.
* EQUIRY:		SLV.E.UIF.TRA.ACUM
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		Jonás		08.02.15	Version inicial.
* 1.1								Tratamiento Ñ, modificación en control de montos.
* 1.2		Jonás		17.02.15	Mostrar monto total de la transaccion para transaccioens TFS.
* 1.3		     		18.02.15	Ordenar por cliente, revisión de columnas de ingresos y egresos.
* 1.4		     		25.02.15	Rediseño de reporte de acuerdo a funcionalidad de alertas 
*									acumuladas por día.
* V1		     		03.03.15	Excluir registros no autorizados o reversados.
* V2		     		07.03.15	Excluir registros de alertas no encontrados en historico de TELLER o FT.
* V3					14.05.15	Eliminar salto de linea en campo local LF.RAZON.SOCIAL.
* V4					08.07.15	Obtener nombre de agencia de campo COMPANY>COMPANY.NAME.
* 2.0		Jonas		07.08.15	Evaluar campo multivalor SHORT.NAME
* 2.1		OCornejo	28.03.16	Modificacion para excluir Operaciones por Medios Electronicos
*----------------------------------------------------------------------------------------------------

SUBROUTINE SLV.E.NOF.UIF.ACUM(ENQ.DATA)
*-----------------------------------------------------------------------------

* Modification History :
*-----------------------------------------------------------------------------
	$INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
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
    $INSERT I_F.SLV.AML.TRANS.IDS
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN


*-----------------------------------------------------------------------------
      INIT:
*-----------------------------------------------------------------------------
    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER  = ''
    FN.TELLER 	= 'F.TELLER$HIS'
    F.TELLER  	= ''
    FN.COMPANY 	= 'F.COMPANY'
    F.COMPANY  	= ''
    FN.USER 	= 'F.USER'
    F.USER  	= ''
    FN.OFFICER 	= 'F.DEPT.ACCT.OFFICER'
    F.OFFICER  	= ''
    FN.ACCOUNT 	= 'F.ACCOUNT'
    F.ACCOUNT  	= ''
	FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER  = ''
    FN.AML.ALERT.LOG= 'F.SLV.AML.ALERT.LOG'
    F.AML.ALERT.LOG = ''
    FN.TABLE.DEP 	= 'F.EB.SLV.CUS.DEPARTAMENTO'
    F.TABLE.DEP 	= ''
    FN.TABLE.MUN 	= 'F.EB.SLV.CUS.MUNICIPIO'
    F.TABLE.MUN 	= ''
    FN.TABLE.IND 	= 'F.INDUSTRY'
    F.TABLE.IND 	= ''    
    FN.TABLE.NAC 	= 'F.COUNTRY'
    F.TABLE.NAC 	= ''    
    FN.TFS 			= 'F.TELLER.FINANCIAL.SERVICES'
    F.TFS 			= ''      
    FN.AML.TRA.IDS 	= 'F.SLV.AML.TRANS.IDS'
    F.AML.TRA.IDS 	= ''      

        
	;*Definicion de variables
    V.CODIGO_DE_CLIENTE                     = ""
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
    V.TIPO_DE_TRANSACCION                   = ""
    V.CODIGO_DE_TRANSACCION                 = ""
 
    LOCATE "FECHA.DESDE" IN D.FIELDS<1> SETTING POS THEN
    	V.FECHA.DESDE = D.RANGE.AND.VALUE<POS>
    END

    LOCATE "FECHA.HASTA" IN D.FIELDS<1> SETTING POS THEN
    	V.FECHA.HASTA = D.RANGE.AND.VALUE<POS>
    END
  
    LOCATE "TIPO.REPORTE" IN D.FIELDS<1> SETTING POS THEN
    	V.TIPO.REPORTE = D.RANGE.AND.VALUE<POS>
    END   
       
    LOCATE "RANGO.EFECTIVO" IN D.FIELDS<1> SETTING POS THEN
    	V.RANGO.EFECTIVO = D.RANGE.AND.VALUE<POS>
    END
*-----------------------------------------------------------------------------
* DEBUG
*-----------------------------------------------------------------------------
    ;*V.FECHA.DESDE = '20180207'
 	;*V.FECHA.HASTA = '20180208'
	;*V.TIPO.REPORTE = 'UIF' 	
	;*V.RANGO.EFECTIVO = 1
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
	CALL OPF(FN.AML.TRA.IDS, F.AML.TRA.IDS)       

	RETURN


*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	SELECT.AML.ALERT.LOG  = "SELECT " : FN.AML.ALERT.LOG : " WITH ALR.DATE GE '" : V.FECHA.DESDE :"' AND ALR.DATE LE '" : V.FECHA.HASTA :"'"
	;*SELECT.AML.ALERT.LOG := " AND ALR.CUSTOMER EQ '114765'"
	SELECT.AML.ALERT.LOG := " AND ALR.DAILY.HIS.IND EQ 'H'" :" ORDER BY ALR.CUSTOMER"
    
	CALL EB.READLIST(SELECT.AML.ALERT.LOG, LIST.AML.ALERT.LOG, '', NO.OF.RECS, ERR.ALERT.LOG)
    CRT LIST.AML.ALERT.LOG
	;*Bloque para obtener las ultimas alertas del dia de Debito o Credito por cliente
	;*-----------------------------------------------------------------------------------------------------
	STR.ALERT = ''
	STR.ARR = ''

	FOR I = 1 TO NO.OF.RECS
		
		
		;*Agregar registro por cliente	
		CALL F.READ(FN.AML.ALERT.LOG, LIST.AML.ALERT.LOG<I>, R.AML, F.AML.ALERT.LOG, ERR.AML)
		
		Y.ID.TXN =  R.AML<SLV.ALERT.ALR.CONTRACT>
		
		;*Se agrega validacion cuando sea tipo de transaccion AC, no muestre el CREDITO/DEBITO cuando son cuentas contables
		;*-----------------------------------------------------------------------------------------------------------------
		CALL F.READ(FN.FUNDS.TRANSFER, Y.ID.TXN:';1', RECORD.FT, F.FUNDS.TRANSFER, ERROR.FT)
		Y.TYPE.TXN    = RECORD.FT<FT.TRANSACTION.TYPE>
		
	    Y.ACC.DEBIT 	= RECORD.FT<FT.DEBIT.ACCT.NO>
	    Y.ACC.CREDIT 	= RECORD.FT<FT.CREDIT.ACCT.NO>
	    
	    
	    CALL F.READ(FN.ACCOUNT, Y.ACC.DEBIT,  RECORD.ACC.DEBIT,  F.ACCOUNT, RECORD.ACC)
	    Y.CUS.DEBIT 	= RECORD.ACC.DEBIT<AC.CUSTOMER>
	    
	    CALL F.READ(FN.ACCOUNT, Y.ACC.CREDIT, RECORD.ACC.CREDIT, F.ACCOUNT, RECORD.ACC)
	    Y.CUS.CREDIT 	= RECORD.ACC.CREDIT<AC.CUSTOMER>
	    
	    
	    Y.TXN.SHOW = ''
	    IF Y.CUS.DEBIT EQ '999999' THEN
	       Y.TXN.SHOW = 'D'
	    END
	    ELSE IF Y.CUS.CREDIT EQ '999999' THEN
	       Y.TXN.SHOW = 'C'
	    END
	    
	    IF Y.TXN.SHOW NE '' THEN
	     	IF Y.TYPE.TXN EQ 'AC' AND  R.AML<SLV.ALERT.ALR.DEB.CRE.IND> EQ Y.TXN.SHOW THEN
		 		CONTINUE
			END
	    END
		
		;*Y.ACC.DBT.TXN = SUBSTRINGS(RECORD.FT<FT.DEBIT.ACCT.NO>, 1, 3) 
		
		;*Se agrega validacion para que no se tome cuenta contable ACNA en el reporte
		;*---------------------------------------------------------------------
		IF Y.TYPE.TXN EQ 'ACNA' AND  R.AML<SLV.ALERT.ALR.DEB.CRE.IND> EQ 'D' THEN
		 CONTINUE
		END
		
		;*Se agrega validacion para cuando sea tipo pago de prestamo solo muestre el ingreso
		;*----------------------------------------------------------------------------------
		IF Y.TYPE.TXN EQ 'ACRP' AND  R.AML<SLV.ALERT.ALR.DEB.CRE.IND> EQ 'D' THEN
		 CONTINUE
		END
		
		;*Excluir Operaciones de Medios Electronicos : OCORNEJO 28.03.16
  		;*--------------------------------------------------------------
  		VERSION = R.AML<SLV.ALERT.ALR.VERSION>
  		IF VERSION[16,3] EQ 'ATM' OR VERSION[16,4] EQ 'TCIB' THEN
  			CONTINUE
  		END 
  		;*--------------------------------------------------------------
		
		IF R.AML<SLV.ALERT.ALR.ID> EQ "UIF" OR R.AML<SLV.ALERT.ALR.ID> EQ "UIF.OTROS" THEN
			
			;*Obtener informacion de las referencias que generaron la alerta
			CALL F.READ(FN.AML.TRA.IDS, LIST.AML.ALERT.LOG<I>, R.AML.T.IDS, F.AML.T.IDS, ERR.AML.T.IDS)
			N.REG.T.IDS = DCOUNT(R.AML.T.IDS, @FM)

			FOR J = 1 TO N.REG.T.IDS
				LOCATE R.AML<SLV.ALERT.ALR.CUSTOMER>:R.AML<SLV.ALERT.ALR.DEB.CRE.IND>:R.AML<SLV.ALERT.ALR.ID>:R.AML.T.IDS<J,SLV.TRANS.AML.TRANS.ID> IN STR.ARR SETTING N.POS ELSE N.POS=''

				IF N.POS THEN
				END ELSE
					STR.ALERT<-1> = R.AML<SLV.ALERT.ALR.CUSTOMER>:"*":R.AML<SLV.ALERT.ALR.ID>:"*":R.AML<SLV.ALERT.ALR.DEB.CRE.IND>:"*":R.AML.T.IDS<J,SLV.TRANS.AML.TRANS.ID>
					STR.ARR<-1>   = R.AML<SLV.ALERT.ALR.CUSTOMER>:R.AML<SLV.ALERT.ALR.DEB.CRE.IND>:R.AML<SLV.ALERT.ALR.ID>:R.AML.T.IDS<J,SLV.TRANS.AML.TRANS.ID>
				END
			NEXT J
			
		END		
	NEXT I

	IF STR.ALERT NE '' THEN
	
		STR.ALERT = SORT(STR.ALERT)
		
		;*-----------------------------------------------------------------------------------------------------
	
		N.REGIS = DCOUNT(STR.ALERT, @FM)
	
		;*	Control de acumulados por cliente
		S.CODE.CLIENT = FIELD(STR.ALERT<1>,"*",1)
	
	   	V.CODIGO_DE_CLIENTE	= S.CODE.CLIENT
		V.TOTAL_MONTO_TRANSACCION = 0
		V.TOTAL_CREDIT = 0
		V.MONTO_TRANS_CREDIT = 0.00
		V.MONTO_REPOR_CREDIT = 0.00
		V.TOTAL_DEBIT = 0
		V.MONTO_TRANS_DEBIT = 0.00
		V.MONTO_REPOR_DEBIT = 0.00
	
		;* Recorrer arreglo con alertas generadas
		J = 1
		LOOP
		WHILE J LE N.REGIS
	
			IF S.CODE.CLIENT NE FIELD(STR.ALERT<J>,"*",1) THEN
				S.CODE.CLIENT = FIELD(STR.ALERT<J>,"*",1)
	    		V.CODIGO_DE_CLIENTE	= S.CODE.CLIENT
				V.TOTAL_MONTO_TRANSACCION = 0
				V.TOTAL_CREDIT = 0
				V.MONTO_TRANS_CREDIT = 0.00
				V.MONTO_REPOR_CREDIT = 0.00
	  			V.TOTAL_DEBIT = 0
				V.MONTO_TRANS_DEBIT = 0.00
				V.MONTO_REPOR_DEBIT = 0.00
			END
	
			;*Acumular totales por cliente
						 
			;* tipo de transacción para evaluar origen de tnx.
			V.CODIGO_DE_TRANSACCION = FIELD(STR.ALERT<J>,"*", 4)
			CRT V.CODIGO_DE_TRANSACCION
			V.TIPO_DE_TRANSACCION   = LEFT(V.CODIGO_DE_TRANSACCION, 2)		
			V.TOTAL_MONTO_TRANSACCION = 0
			R.TFS = ''
			S.REG.VALIDO = 'S'
			
			;* Acumular registro actual      	     	
	      	IF V.TIPO_DE_TRANSACCION EQ 'FT' AND  Y.TYPE.TXN NE 'ACRP' THEN
	      	
	      	    ;*Obtener el ultimo registro del historico
	  			CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER, FIELD(STR.ALERT<J>,"*", 4), R.FUNDS.TRANSFER, ERR.FUNDS.TRANSFER)
	      	
	            IF LEFT(R.FUNDS.TRANSFER<FT.TFS.REFERENCE>,3) EQ 'TFS' THEN
	            	CALL F.READ(FN.TFS, R.FUNDS.TRANSFER<FT.TFS.REFERENCE>, R.TFS, F.TFS, ERR.TFS)
	      		END
	
				;*Verificar si el registro tiene reversa o no está autorizado
				IF R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ '' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'INAU' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'INAO' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'RNAU' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'RNAO' OR R.FUNDS.TRANSFER<FT.RECORD.STATUS> EQ 'REVE' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAU' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAO' OR R.TFS<TFS.RECORD.STATUS> EQ 'RNAU' OR R.TFS<TFS.RECORD.STATUS>EQ 'RNAO' OR R.TFS<TFS.RECORD.STATUS>EQ 'REVE' THEN			
					S.REG.VALIDO = 'N'
				END 
	
	            IF S.REG.VALIDO = 'S' THEN
			    	IF FIELD(STR.ALERT<J>, "*", 3) EQ 'D' THEN
			    		IF V.TIPO.REPORTE EQ FIELD(STR.ALERT<J>,"*", 2) THEN 
				    		V.MONTO_REPOR_DEBIT += R.FUNDS.TRANSFER<FT.LOC.AMT.DEBITED>
			    		END
		    			
		    			V.TOTAL_DEBIT++
		    			IF V.TOTAL_MONTO_TRANSACCION NE 0 THEN
			    			V.MONTO_TRANS_DEBIT += V.TOTAL_MONTO_TRANSACCION
			    		END ELSE
			    			V.MONTO_TRANS_DEBIT += R.FUNDS.TRANSFER<FT.LOC.AMT.DEBITED>
			    		END
			    		
			    	END 
			    	ELSE
				    	IF V.TIPO.REPORTE EQ FIELD(STR.ALERT<J>,"*", 2) THEN
				    		V.MONTO_REPOR_CREDIT += R.FUNDS.TRANSFER<FT.LOC.AMT.CREDITED>
				    	END
	
			    		V.TOTAL_CREDIT++                                                                                                                                                                                                                                                                         
			    		IF V.TOTAL_MONTO_TRANSACCION NE 0 THEN
			    			V.MONTO_TRANS_CREDIT += V.TOTAL_MONTO_TRANSACCION
			    		END ELSE
			    			V.MONTO_TRANS_CREDIT += R.FUNDS.TRANSFER<FT.LOC.AMT.CREDITED>
			    		END	
			    	END
		    	END
	      	END
	
	      	IF V.TIPO_DE_TRANSACCION EQ 'TT' THEN
	   
	      	    ;*Obtener el ultimo registro del historico
	  			CALL EB.READ.HISTORY.REC (F.TELLER, FIELD(STR.ALERT<J>,"*", 4), R.TELLER, ERR.TELLER)
				
				V.TOTAL_MONTO_TRANSACCION = 0
	            IF LEFT(R.TELLER<TT.TE.THEIR.REFERENCE>,3) EQ 'TFS' THEN
	            	CALL F.READ(FN.TFS, R.TELLER<TT.TE.THEIR.REFERENCE>, R.TFS, F.TFS, ERR.TFS)
	      		END
	
				;*Verificar si el registro tiene reversa o no está autorizado
				IF R.TELLER<TT.TE.RECORD.STATUS> EQ '' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'INAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAU' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'RNAO' OR R.TELLER<TT.TE.RECORD.STATUS> EQ 'REVE' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAU' OR R.TFS<TFS.RECORD.STATUS> EQ 'INAO' OR R.TFS<TFS.RECORD.STATUS> EQ 'RNAU' OR R.TFS<TFS.RECORD.STATUS>EQ 'RNAO'  OR R.TFS<TFS.RECORD.STATUS> EQ 'REVE' THEN
					S.REG.VALIDO = 'N'
				END 
	
	
	            IF S.REG.VALIDO = 'S'  THEN
			    	IF FIELD(STR.ALERT<J>,"*", 3) EQ 'D' THEN
	
			    		V.TOTAL_DEBIT++
			    		IF V.TOTAL_MONTO_TRANSACCION NE 0 THEN	
			    			V.MONTO_TRANS_DEBIT += V.TOTAL_MONTO_TRANSACCION
			    		END ELSE
			    			V.MONTO_TRANS_DEBIT += R.TELLER<TT.TE.AMOUNT.LOCAL.2> 	;* siempre es TT.TE.AMOUNT.LOCAL.2 (lado del cliente)
			    		END
			    		IF V.TIPO.REPORTE EQ FIELD(STR.ALERT<J>,"*", 2) THEN
			    			V.MONTO_REPOR_DEBIT += R.TELLER<TT.TE.AMOUNT.LOCAL.2>
			    		END
			    	END ELSE
	
						V.TOTAL_CREDIT++
	
			    		IF V.TOTAL_MONTO_TRANSACCION NE 0 THEN	
			    			V.MONTO_TRANS_CREDIT += V.TOTAL_MONTO_TRANSACCION
			    		END ELSE
			    			V.MONTO_TRANS_CREDIT += R.TELLER<TT.TE.AMOUNT.LOCAL.2>	;* siempre es TT.TE.AMOUNT.LOCAL.2 (lado del cliente)
			    		END
			    		IF V.TIPO.REPORTE EQ FIELD(STR.ALERT<J>,"*", 2) THEN	
			    			V.MONTO_REPOR_CREDIT += R.TELLER<TT.TE.AMOUNT.LOCAL.2>
			    		END
			    	END
		    	END
	      	END
	
		    
			J += 1

			;*Generación de registros
			IF S.CODE.CLIENT NE FIELD(STR.ALERT<J>,"*",1) OR J GT N.REGIS THEN
				;* Registrar cliente con sus acumulados
				IF V.MONTO_REPOR_CREDIT LT V.RANGO.EFECTIVO THEN
					V.TOTAL_CREDIT		 = 0
					V.MONTO_TRANS_CREDIT = 0.00		;* No se deben reportar montos acumulados abajo del parámetro
					V.MONTO_REPOR_CREDIT = 0.00		;* No se deben reportar montos acumulados abajo del parámetro
				END
				
				IF V.MONTO_REPOR_DEBIT LT V.RANGO.EFECTIVO THEN
					V.TOTAL_DEBIT   	= 0
					V.MONTO_TRANS_DEBIT = 0.00	;* No se deben reportar montos acumulados abajo del parámetro
					V.MONTO_REPOR_DEBIT = 0.00	;* No se deben reportar montos acumulados abajo del parámetro
				END			
			
				IF V.MONTO_REPOR_DEBIT + V.MONTO_REPOR_CREDIT > 0 THEN
					GOSUB REGISTRAR.CLIENTE
				END
			END
			
		REPEAT
	
		;*Ordenar registros por cliente
		ENQ.DATA = SORT(ENQ.DATA)
	END


	RETURN

		  
*-----------------------------------------------------------------------------
REGISTRAR.CLIENTE:
*-----------------------------------------------------------------------------		    
	;* INFORMACION DEL CLIENTE
	CALL F.READ(FN.CUSTOMER, V.CODIGO_DE_CLIENTE, R.CUSTOMER , F.CUSTOMER, ERR.CUSTOMER)
	CALL GET.LOC.REF('CUSTOMER', 'LF.LUG.NAC', LOCPOSLugarNacio)
	CALL GET.LOC.REF('CUSTOMER', 'LF.RAZON.SOCIAL', LOCPOSRasonSocial)
	CALL GET.LOC.REF('CUSTOMER', 'LF.TYPE.CUST', LOCPOEstadoCivil)                          
	CALL GET.LOC.REF('CUSTOMER', 'SEGMENT', LOCPOSSegment)
            
    ;* AGENCIA
    ;*V.NOMBRE_AGENCIA = FIELD(RS.COMPANY<EB.COM.NAME.ADDRESS>, VM, 1)       
    V.NOMBRE_AGENCIA = RS.COMPANY<EB.COM.COMPANY.NAME><1,1>
    V.MUNICIPIO_AGENCIA = FIELD(RS.COMPANY<EB.COM.COMPANY.NAME>, VM, 5) 
    V.DEPARTAMENTO_AGENCIA = FIELD(RS.COMPANY<EB.COM.NAME.ADDRESS>, VM, 4) 
                   
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
	        	V.ESTADO_CIVIL = 'ESPOSO(A)'
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

		FOR Z=1 TO Y.COUNT
			Y.SHORT.NAME := R.CUSTOMER<EB.CUS.SHORT.NAME><1,Z>
			IF Z NE Y.COUNT THEN
				Y.SHORT.NAME := " / "
			END
		NEXT Z
	
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
    STR.MOV := V.TIPO_DE_PERSONA : "*"							;* 2
    STR.MOV := V.PRIMER_APELLIDO : "*"							;* 3
    STR.MOV := V.SEGUNDO_APELLIDO : "*"							;* 4
    STR.MOV := V.APELLIDO_CASADA : "*"							;* 5
    STR.MOV := V.PRIMER_NOMBRE : "*"							;* 6
    STR.MOV := V.SEGUNDO_NOMBRE : "*"							;* 7
    STR.MOV := V.TERCER_NOMBRE : "*"							;* 8
    STR.MOV := V.FECHA_DE_NACIMIENTO : "*"						;* 9
    STR.MOV := V.LUGAR_DE_NACIMIENTO : "*"						;* 10	
    STR.MOV := V.NACIONALIDAD : "*"								;* 11
    STR.MOV := V.ESTADO_CIVIL : "*"								;* 12
    STR.MOV := V.TIPO_DE_DOCUMENTO_DE_IDENTIDAD : "*"			;* 13
    STR.MOV := V.NUMERO_DE_DOCUMENTO_DE_IDENTIDAD : "*"			;* 14	
    STR.MOV := V.DIRECCION_PERSONA : "*"						;* 15
    STR.MOV := V.MUNICIPIO_PERSONA : "*"						;* 16	
    STR.MOV := V.DEPARTAMENTO_PERSONA : "*"						;* 17
    STR.MOV := V.PROFESION : "*"								;* 18
    STR.MOV := V.NOMBRE_JURIDICO_RAZON_SOCIAL : "*"				;* 19
    STR.MOV := V.DIRECCION_EMPRESA : "*"						;* 20
    STR.MOV := V.MUNICIPIO_EMPRESA : "*"						;* 21
    STR.MOV := V.DEPARTAMENTO_EMPRESA : "*"        				;* 22
    STR.MOV := V.ACTIVIDAD_ECONOMICA : "*"						;* 23
    STR.MOV := V.TIPO_DE_DOCUMENTO : "*"						;* 24
    STR.MOV := V.NUMERO_DE_DOCUMENTO : "*"						;* 25          
	STR.MOV := V.TOTAL_CREDIT : "*"								;* 26
	STR.MOV := V.MONTO_TRANS_CREDIT : "*"						;* 27
	STR.MOV := V.MONTO_REPOR_CREDIT : "*"						;* 28
	STR.MOV := V.TOTAL_DEBIT : "*"								;* 29
	STR.MOV := V.MONTO_TRANS_DEBIT : "*"						;* 30
	STR.MOV := V.MONTO_REPOR_DEBIT : "*"						;* 31
 
	;* Códificar caracteres especiales
	STR.MOV = CHANGE(STR.MOV, 'Ñ', CHAR(465)) 
    ;*Se agrega validacion para que no muestre el cliente Banco
    ;*---------------------------------------------------------
    IF V.CODIGO_DE_CLIENTE NE '999999' THEN 
		ENQ.DATA<-1> = STR.MOV
	END

	RETURN


*-----------------------------------------------------------------------------
LIMPIAR.CAMPOS:
*-----------------------------------------------------------------------------
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

	;*PERSONA JURIDICA
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
	IF SS.PERSONERIA EQ '1' THEN
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

