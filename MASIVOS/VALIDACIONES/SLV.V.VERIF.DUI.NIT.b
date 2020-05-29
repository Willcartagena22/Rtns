*-----------------------------------------------------------------------------
* <Rating>15</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.VERIF.DUI.NIT
*-----------------------------------------------------------------------------
* Extrae datos del cliente
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version
* RRENDON  	01.12.2014 		Initial Code
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER

*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    
    RETURN
 
 INIT:     
	 ;*Definiendo Tablas
	 FN.CUSTOMER 			= 'F.CUSTOMER'
	 F.CUSTOMER  			= ''
	 ;*Declarando Variables
	 S_NIT              = '' 
	 S_DUI              = '' 
	 S_FLAG_NIT         = 0
	 S_FLAG_DUI         = 0
	 ;**********************Set Multivalor
	 S_NO.DOCUMENTO     = ''
	 S_TIPO.DOCUMENT    = '' 
	 
	 
 RETURN 

 
 OPENFILES:
	  ;*Abriendo Archivos
	 CALL OPF(FN.CUSTOMER,F.CUSTOMER)
 	
 RETURN   
 
 
 PROCESS:
 
  ;*Obteniendo posiciones de campos locales
	 ;*********************LOCAL FIELD**************************************

	 CALL GET.LOC.REF('CUSTOMER','LF.NIT',POS.NIT)
	 CALL GET.LOC.REF('CUSTOMER','LF.DUI',POS.DUI)  
	 CALL GET.LOC.REF('CUSTOMER','LF.TYPE.CUST',POS.TYPE) 
	 

*	 CALL F.READ(FN.CUSTOMER,'100155',R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
	  
	     ;*****LOCAL FIELDS
	     ;** nit
		 S_NIT              = R.NEW(EB.CUS.LOCAL.REF)<1,POS.NIT> ;*1
		 ;**dui
		 S_DUI              = R.NEW(EB.CUS.LOCAL.REF)<1,POS.DUI> ;*2
		 ;*type
		 S_TYPE              = R.NEW(EB.CUS.LOCAL.REF)<1,POS.TYPE> ;*2

		 ;**********************Set Multivalor de documentos
		 FOR H = 1 TO DCOUNT(R.NEW(EB.CUS.LEGAL.ID),@VM)
			 S_NO.DOCUMENTO     = R.NEW(EB.CUS.LEGAL.ID)<1,H> ;*3
			 S_TIPO.DOCUMENT    = R.NEW(EB.CUS.LEGAL.DOC.NAME)<1,H> ;*4

			 IF 'NUM.IDEN.TRIBUT' EQ S_TIPO.DOCUMENT THEN
			    S_FLAG_NIT = 1
			 	IF S_NO.DOCUMENTO NE S_NIT THEN
			 	    AF = EB.CUS.LOCAL.REF 
			 	    AV = POS.NIT
			 		ETEXT ='Los numeros de NIT no coinciden ':S_NIT:' y ':S_NO.DOCUMENTO
					CALL STORE.END.ERROR
			 	END 
			 END 
			 IF (S_TYPE EQ 'NATSDO' AND LEN(S_DUI) NE 0) THEN ;* Si es cliente natural  valida DUI
				 IF 'DOCTO.UNICO.IDENT' EQ S_TIPO.DOCUMENT THEN
				    S_FLAG_DUI = 1
				 	IF S_NO.DOCUMENTO NE S_DUI THEN
				 	    AF = EB.CUS.LOCAL.REF 
			 	        AV = POS.DUI
				 		ETEXT ='Los numeros de DUI no coinciden ':S_DUI:' y ':S_NO.DOCUMENTO
						CALL STORE.END.ERROR
				 	END 
				 END 
			 END
		 NEXT H
		 
		 IF S_FLAG_NIT EQ 0 THEN
		    AF = EB.CUS.LEGAL.DOC.NAME
		 	ETEXT ='Falta digitar NIT en la pestaña de Documentos'
			CALL STORE.END.ERROR
		 END
		 
		 IF (S_TYPE EQ 'NATSDO' AND LEN(S_DUI) NE 0)   THEN ;* Si es cliente natural  valida DUI
			 IF S_FLAG_DUI EQ 0 THEN
			    AF = EB.CUS.LEGAL.DOC.NAME
			 	ETEXT ='Falta digitar DUI en la pestaña de Documentos'
				CALL STORE.END.ERROR		 
			 END
		 END
		 		 
		 ;*********************		 

	 
	


 RETURN
 
    
END
