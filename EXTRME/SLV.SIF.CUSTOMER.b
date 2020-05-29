*-----------------------------------------------------------------------------
* <Rating>2238</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.SIF.CUSTOMER
 *-----------------------------------------------------------------------------
* Extrae datos del cliente
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version
* RRENDON  	12.08.2014 		Initial Code
* Galfaro	12.06.2015		Se agrega logica para extraer socios de clientes juridico
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_F.SLV.CUS.NO.GARANT.LEY
$INSERT I_F.INDUSTRY
$INSERT I_F.SECTOR
$INSERT I_F.EB.SLV.NORMA.RELACION.CLIENTE
$INSERT I_F.EB.SLV.DESC.NO.GARAN.44
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB INITVAR
    GOSUB OPENFILES
    GOSUB PROCESS
    
    RETURN
 
 INIT:
     ;*Variables de trabajo
     edLine	='|'
     DIR.NAME= 'SIF.OUT'
*     DIR.NAME='C:\EXTRACT-CRISK\'
     NAME.FILE='_Customer.csv'
     NAME.FILE.SOC='_Sociedades.csv'
     AR.CUS=''
     
     ;*************************
     ;* debug
     ;*DIR.NAME= 'C:\temp'
     ;*************************
     
	 ;*Definiendo Tablas
	 FN.CUSTOMER 			  = 'F.CUSTOMER'
	 F.CUSTOMER  			  = ''
	 FN.INDUSTRY 			  = 'F.INDUSTRY'
	 F.INDUSTRY 			  = ''
	 FN.SECTOR                = 'F.SECTOR'
	 F.SECTOR                 = ''
	 FN.CUSTOMER.ACCOUNT      = 'F.CUSTOMER.ACCOUNT'
	 F.CUSTOMER.ACCOUNT       = ''
	 FN.SLV.CUS.NO.GARANT.LEY = 'F.SLV.CUS.NO.GARANT.LEY'
	 F.SLV.CUS.NO.GARANT.LEY  = ''
	 
	 FN.NORMA.REL.CLIE	= 'F.EB.SLV.NORMA.RELACION.CLIENTE' 
	 F.NORMA.REL.CLIE = '' 
	 FN.DES.GARAN.44 = 'F.EB.SLV.DESC.NO.GARAN.44'
	 F.DES.GARAN.44 = ''
	 
 RETURN 
 
 INITVAR:
  ;*Declarando Variables
	 S_FIRST.NAME       = ''
	 S_SECOND_NAME      = ''
	 S_THIRD_NAME       = ''
	 S_FIRST.LAST.NAME  = ''
	 S_SECOND.LAST.NAME = ''
	 S_MARITAL.LAST.NAME= '' 
	 S_RAZON.SOCIAL     = '' ;*Para Empresas
	 S_NOMBRE.COMERCIAL = '' ;*Para Empresas
	 D_FECHA.CONSTITUCI = '' ;*Para Empresas
	 S_MNEMONIC         = '' 
	 S_ALTER.NAME       = ''
	 S_NIT.NAME         = '' 
	 S_NIT              = '' 
	 S_LAST_NIT         = '' 
	 S_DUI              = '' 
	 S_SEX              = '' ;** Femenino/Masculino
	 S_MARITAL.STATUS   = '' 
	 S_AGENCIA          = '' ;*aqui se guarda la agencia donde se creo  el cliente
	 S_SECTOR.TYPE      = '' 
	 S_OCUPACION.SSF    = ''
	 S_STATUS.CUSTOMER  = ''
	 S_SECTOR           = ''
	 S_PLACE_BIRTH      = ''
	 S_NATIONALITY      = ''
	 S_INTERNAL.CATEG   = ''
	 S_DATE_BIRTH       = ''
	 S_SEGMENT          = ''
	 S_EXENT.TAX        = ''
	 S_ACTIVITY_ECON    = ''
	 S_RESIDENCE        = ''
	 S_EXTERN.CATEG     = ''
	 S_LANGUAGE         = ''
	 S_FREQ.REVIEW      = ''
	 S_ISR_TYPE         = ''
	 S_COUNTRY          = ''
	 S_ZONE             = ''
	 S_DEPARMENT        = ''
	 S_MUNICIPIO        = ''
	 S_CANTON           = ''
	 S_COLONIA          = ''
	 S_CALLE            = ''
	 S_AVENIDA          = ''
	 S_NUM_CASA         = ''
	 S_TELEFONO         = ''
	 S_MOVIL            = ''
	 S_TEL.OFICINA      = ''
	 S_FAX              = ''
	 ;**********************Set Multivalor
	 S_NO.DOCUMENTO     = ''
	 S_TIPO.DOCUMENT    = ''
	 S_LUGAR.EMISION    = ''
	 D_FECHA.EMISION    = ''
	 D_FECHA.VENCIMIENTO= ''
	 ;**********************
	 S_RELATION_TYPE    = ''
	 S_REL.CUSTOMER     = ''
	 ;**********************
	 S_TAMANIO_EMP      = ''
	 S_TIPO_CONTRIBU    = ''
	 S_SUB.SEGEMNTO     = ''
	 S_NIVEL.INGRESO    = ''
	 S.ESTADO.EMPLEO    = ''
	 S.OCUPACION.DUI    = ''
	 S.NOMBRE.EMPRESA   = ''
	 S.DIRECCION.EMPRESA= ''
	 S.ACTIVIDAD.EMPRESA= ''
	 D.FECHA.ENTRADA    = ''
	 N.SALARIO           = ''
	 N.OTROS.INGRESOS   = ''
	 S.CARGO            = ''
	 N.INGRESO.MENUSAL  = ''
	 N.EGRESO.MENUSAL   = ''
	 ;*********************
	 S_RESIDENCIA.ESTADO = ''
	 S_TIPO.RESIDENCIA   = ''
	 D_RESIDENCIA.DESDE  = ''
	 N_VALOR.RESIDENCIA  = ''
	 S_RZONE             = ''
	 S_RDEPARMENT        = ''
	 S_RMUNICIPIO        = ''
	 S_RCANTON           = ''
	 S_RCOLONIA          = ''
	 S_RCALLE            = ''
	 S_RAVENIDA          = ''
	 S_RNUM_CASA         = ''
	 ;**********************
	 S_INTERNET.BANKING  = ''
	 S_BANCA.MOVIL       = ''
	 ;**********************
	 D_CLIENTE.DESDE     = ''
	 N_NUMERO.DEPENDIENTE= ''
 
 RETURN

 
 OPENFILES:
	  ;*Abriendo Archivos
	 CALL OPF(FN.CUSTOMER,F.CUSTOMER)
	 CALL OPF(FN.SECTOR, F.SECTOR)
	 CALL OPF(FN.INDUSTRY, F.INDUSTRY)
	 
	   ;*eliminando archivo existente     
     DELETESEQ DIR.NAME,NAME.FILE THEN 
     END
        ;*eliminando archivo existente     
     DELETESEQ DIR.NAME,NAME.FILE.SOC THEN 
     END
    
     ;* Abriendo archivo para escritura
	 OPENSEQ DIR.NAME, NAME.FILE TO SEQ.PTR THEN
		WEOFSEQ NAME.FILE
	 END
	   ;* Abriendo archivo para escritura
	 OPENSEQ DIR.NAME, NAME.FILE.SOC TO SEQ.PTR.SOC THEN
		WEOFSEQ NAME.FILE.SOC
	 END
 	 
 	 
 	 ;*Statement de clientes con cuentas
 	 STMT.CUSTOMER = "SELECT ":FN.CUSTOMER:" WITH @ID NE '999999' AND @ID NE '999994' "
 	 ;*102707
*	 STMT.CUSTOMER = "SELECT ":FN.CUSTOMER:" WITH @ID EQ 100794"
 	 CALL EB.READLIST(STMT.CUSTOMER, CUSTOMER.LIST,'',NO.OF.CUSTOMER,Y.CUSTOMER.ERR)
 	
 RETURN   
 
 
 PROCESS:
   
 
  ;*Obteniendo posiciones de campos locales
	 ;*********************LOCAL FIELD**************************************
	 CALL GET.LOC.REF('CUSTOMER','LF.RAZON.SOCIAL',POS.RAZON.SOCIAL)
	 CALL GET.LOC.REF('CUSTOMER','LF.NOB.NIT',POS.NOMBRE.NIT)
	 CALL GET.LOC.REF('CUSTOMER','LF.NIT',POS.NIT)
	 CALL GET.LOC.REF('CUSTOMER','LF.NIT.ANTER',POS.NIT.ANTER)   
	 CALL GET.LOC.REF('CUSTOMER','LF.DUI',POS.DUI)  
	 CALL GET.LOC.REF('CUSTOMER','LF.TIPO.SECTOR',POS.TIPO.SECTOR)
	 CALL GET.LOC.REF('CUSTOMER','LF.OCUPACION',POS.OCUPACION)
	 CALL GET.LOC.REF('CUSTOMER','LF.LUG.NAC',POS.LUG.NAC)
	 CALL GET.LOC.REF('CUSTOMER','SEGMENT',POS.SEGMENT)
	 CALL GET.LOC.REF('CUSTOMER','LF.EXE.TAX',POS.EXE.TAX)
	 CALL GET.LOC.REF('CUSTOMER','LF.CAT.RIES.EXT',POS.RIES.EXT)
	 CALL GET.LOC.REF('CUSTOMER','LF.TYPE.CUST',POS.TYPE.CUST)
	 CALL GET.LOC.REF('CUSTOMER','LF.ZONA',POS.ZONA)
	 CALL GET.LOC.REF('CUSTOMER','LF.DEPARTAMENTO',POS.DEPARTAMENTO)
	 CALL GET.LOC.REF('CUSTOMER','LF.MUNICIPIO',POS.MUNICIPIO)
	 CALL GET.LOC.REF('CUSTOMER','LF.CANTON',POS.CANTON)
	 CALL GET.LOC.REF('CUSTOMER','LF.NUM.DEPTO',POS.NUM.DEPTO)
	 CALL GET.LOC.REF('CUSTOMER','LF.COLONIA',POS.COLONIA)
	 CALL GET.LOC.REF('CUSTOMER','LF.CALLE',POS.CALLE)
	 CALL GET.LOC.REF('CUSTOMER','LF.AVENIDA',POS.AVENIDA)
	 CALL GET.LOC.REF('CUSTOMER','LF.TAMA.CONT',POS.TAMA.CONT)
	 CALL GET.LOC.REF('CUSTOMER','LF.TIPO.CONT',POS.TIPO.CONT)
	 ;**Direccion Residencia
	 CALL GET.LOC.REF('CUSTOMER','LF.ZONA.3',POS.ZONA2)
	 CALL GET.LOC.REF('CUSTOMER','LF.DEPTO.3',POS.DEPARTAMENTO2)
	 CALL GET.LOC.REF('CUSTOMER','LF.MUNICIPIO.3',POS.MUNICIPIO2)
	 CALL GET.LOC.REF('CUSTOMER','LF.CANTON.3',POS.CANTON2)
	 CALL GET.LOC.REF('CUSTOMER','LF.NUM.DEPTO.3',POS.NUM.DEPTO2)
	 CALL GET.LOC.REF('CUSTOMER','LF.COLONIA.3',POS.COLONIA2)
	 CALL GET.LOC.REF('CUSTOMER','LF.CALLE.3',POS.CALLE2)
	 CALL GET.LOC.REF('CUSTOMER','LF.AVENIDA.3',POS.AVENIDA2)
	 CALL GET.LOC.REF('INDUSTRY','LF.COD.IND',POS.AC)
	 	 
	 CALL GET.LOC.REF('CUSTOMER','LF.FULL.NAME',POS.ACCFULLNAME)
	 CALL GET.LOC.REF('CUSTOMER','LF.NIT.TAX',POS.NITTAX)
	 CALL GET.LOC.REF('CUSTOMER','LF.AML.PER',POS.AMLPER)
	 CALL GET.LOC.REF('CUSTOMER','LF.CARGO.JUNTA',POS.JUNTA)
	  
*	 CALL MULTI.GET.LOC.REF('CUSTOMER':FM:'INDUSTRY','LF.AVENIDA.3':VM:'LF.CALLE.3':FM:'LF.COD.IND',POS.ALL)
*	 
*	 POS.1 = POS.ALL<1,1>
*	 POS.2 = POS.ALL<1,2>
*	 POS.3 = POS.ALL<2,1>	 
	 ;*Bucle de clientes
	 
	 
	 FOR I=1 TO NO.OF.CUSTOMER
	   
	     GOSUB INITVAR
	     CALL F.READ(FN.CUSTOMER,CUSTOMER.LIST<I>,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
	     
	     
	     S_FIRST.NAME = ''
	     S_SECOND_NAME = ''
	     ;*****FIELDS
	     IF R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.SEGMENT> = 1 THEN
			 S_FIRST.NAME       = R.CUSTOMER<EB.CUS.NAME.1> ;*1
			 S_SECOND_NAME      = R.CUSTOMER<EB.CUS.NAME.2> ;*2
			 S_THIRD_NAME       = R.CUSTOMER<EB.CUS.GIVEN.NAMES> ;*3
			 S_FIRST.LAST.NAME  = R.CUSTOMER<EB.CUS.TEXT> ;*4
			 S_SECOND.LAST.NAME = R.CUSTOMER<EB.CUS.FAMILY.NAME> ;*5
			 S_MARITAL.LAST.NAME= R.CUSTOMER<EB.CUS.PREVIOUS.NAME> ;*6
		 	 S_RAZON.SOCIAL     = '' ;*7
		 	 S_NOMBRE.COMERCIAL = '' ;*8
		 	 S_SEX              = R.CUSTOMER<EB.CUS.GENDER> ;*16
		 	 S_DATE_BIRTH       = R.CUSTOMER<EB.CUS.DATE.OF.BIRTH> ;*26
		 	 S_SEGMENT          = 1 ;*27
		 END
		 ELSE
			 S_FIRST.NAME       = '' ;*1
			 S_SECOND_NAME      = '' ;*2
			 S_THIRD_NAME       = '' ;*3
			 S_FIRST.LAST.NAME  = '' ;*4
			 S_SECOND.LAST.NAME = '' ;*5
			 S_MARITAL.LAST.NAME= '' ;*6	 
			 I_LENRS = 1
			 I_LENRS = DCOUNT(R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.RAZON.SOCIAL>,@SM)
			 S_RS = ''
			 FOR H = 1 TO I_LENRS
			 	IF H = 1 THEN
			 		S_RS = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.RAZON.SOCIAL,H>
			 	END 
			 	ELSE
			 		S_RS = S_RS:' ':R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.RAZON.SOCIAL,H>
			 	END
			 NEXT H
			 
		 	S_RAZON.SOCIAL     = S_RS ;*7
		 	
		 	S_NOMBRE.COMERCIAL = R.CUSTOMER<EB.CUS.SHORT.NAME> ;*8
		 	S_SEX              = 'N' ;*16
		 	S_DATE_BIRTH       = R.CUSTOMER<EB.CUS.BIRTH.INCORP.DATE> ;*26
		 	S_SEGMENT          = 2 ;*27
		 END
		 

		 D_FECHA.CONSTITUCI = R.CUSTOMER<EB.CUS.BIRTH.INCORP.DATE> ;*9
		 S_MNEMONIC         = R.CUSTOMER<EB.CUS.MNEMONIC> ;*10
		 S_ALTER.NAME       = R.CUSTOMER<EB.CUS.INTRODUCER> ;*11
		 S_NIT.NAME         = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.NOMBRE.NIT> ;*12
		 S_NIT              = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.NIT> ;*13
		 S_LAST_NIT         = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.NIT.ANTER> ;*14
		 S_DUI              = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.DUI> ;*15
		 S_MARITAL.STATUS   = R.CUSTOMER<EB.CUS.MARITAL.STATUS> ;*17
		 S_AGENCIA          = R.CUSTOMER<EB.CUS.COMPANY.BOOK>[3,7] ;*18
		 S_SECTOR.TYPE      = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.TIPO.SECTOR> ;*19 
		 S_OCUPACION.SSF    = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.OCUPACION>  ;*20
		 S_STATUS.CUSTOMER  = R.CUSTOMER<EB.CUS.CUSTOMER.TYPE> ;*21
		 S_SECTOR           = R.CUSTOMER<EB.CUS.SECTOR> ;*22
		 CALL F.READ (FN.SECTOR,S_SECTOR,R.SECTOR,F.SECTOR,ERR.SEC)
		 I_LENSECTOR = 0
		 I_LENSECTOR = LEN(R.SECTOR<EB.SEC.SHORT.NAME>)
		 S_SECTOR = R.SECTOR<EB.SEC.SHORT.NAME>[2,I_LENSECTOR]
		 S_PLACE_BIRTH      = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.LUG.NAC> ;*23 
		 S_NATIONALITY      = R.CUSTOMER<EB.CUS.NATIONALITY> ;*24
		 S_INTERNAL.CATEG   = R.CUSTOMER<EB.CUS.CUSTOMER.RATING> ;*25
		 S_EXENT.TAX        = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.EXE.TAX> ;*28
		 S_ACTIVITY_ECON    = R.CUSTOMER<EB.CUS.INDUSTRY> ;*29
		 CALL F.READ(FN.INDUSTRY,S_ACTIVITY_ECON,R.INDUSTRY,F.INDUSTRY,INDUSTRY.ERR)
		 S_ACTIVITY_ECON = R.INDUSTRY<EB.IND.LOCAL.REF><1,POS.AC>
		 S_RESIDENCE        = R.CUSTOMER<EB.CUS.RESIDENCE> ;*30
		 S_EXTERN.CATEG     = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.RIES.EXT> ;*31
		 S_LANGUAGE         = R.CUSTOMER<EB.CUS.LANGUAGE> ;*32
		 S_FREQ.REVIEW      = R.CUSTOMER<EB.CUS.REVIEW.FREQUENCY> ;*33
		 S_ISR_TYPE         = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.TYPE.CUST> ;*34
		 S_COUNTRY          = R.CUSTOMER<EB.CUS.COUNTRY> ;*35
		 S_ZONE             = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.ZONA> ;*36
		 S_DEPARMENT        = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.DEPARTAMENTO> ;*37
		 S_MUNICIPIO        = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.MUNICIPIO> ;*38
		 S_CANTON           = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.CANTON> ;*39
		 S_COLONIA          = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.COLONIA> ;*40
		 S_CALLE            = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.CALLE> ;*41
		 S_AVENIDA          = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.AVENIDA> ;*42
		 S_NUM_CASA         = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.NUM.DEPTO> ;*43
		 S_TELEFONO         = R.CUSTOMER<EB.CUS.PHONE.1,1> ;*44
		 S_MOVIL            = R.CUSTOMER<EB.CUS.SMS.1,1> ;*45
		 S_TEL.OFICINA      = R.CUSTOMER<EB.CUS.OFF.PHONE,1> ;*46
		 S_FAX              = R.CUSTOMER<EB.CUS.FAX.1,1> ;*47
		 ;**********************Set Multivalor
		 S_NO.DOCUMENTO     = R.CUSTOMER<EB.CUS.LEGAL.ID,1> ;*48
		 S_TIPO.DOCUMENT    = R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME,1> ;*49
		 S_LUGAR.EMISION    = R.CUSTOMER<EB.CUS.LEGAL.ISS.AUTH,1> ;*50 
		 D_FECHA.EMISION    = R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE,1> ;*51
		 D_FECHA.VENCIMIENTO= R.CUSTOMER<EB.CUS.LEGAL.EXP.DATE,1> ;*52
		 ;**********************
		 
		 ;* Se busca el tipo de relacion en la aplicación local
		 ;*CALL F.READ(FN.SLV.CUS.NO.GARANT.LEY,CUSTOMER.LIST<I>,R.TP,FN.SLV.CUS.NO.GARANT.LEY,TP.ERR)
		 ;*Y.CODE = R.TP<SLV.CUS.GRNT.CODE>
		 ;*IF Y.CODE THEN
		 ;*	S_RELATION_TYPE    = Y.CODE ;*53
		 ;*END
		 ;*ELSE
		 ;*	S_RELATION_TYPE    = '0' ;*53
		 ;*END 
		 		 
		 
		 ;*obtener cliente relacionado con su tipo de relación. 
		 CALL F.READ(FN.NORMA.REL.CLIE,CUSTOMER.LIST<I>,R.TP,F.NORMA.REL.CLIE,TP.ERR)
		 Y.CODE = R.TP<EB.TIP.TIPO.REL>
		 ;* ejemplo de recuperacion de valor por posicion, utilizar unicamente para chequear si el valor se identifica por posicion en caso falla por nombre 
		 ;*Y.CODE.CUS.CHK = R.TP<1>
		 ;*Y.CODE.CDE.CHK = R.TP<2>		 
		 Y.CODE.1 = R.TP<EB.TIP.CURR.NO>
		 IF Y.CODE THEN 
		 	S_RELATION_TYPE = Y.CODE ;*53
		 END
		 ELSE
		 	S_RELATION_TYPE = '0' ;*53 
		 END 
		 
		 S_REL.CUSTOMER     = R.CUSTOMER<EB.CUS.REL.CUSTOMER> ;*54	;* valor cliente relacionado 
		 
		 
		 ;**********************
		 S_TAMANIO_EMP      = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.TAMA.CONT> ;*55
		 S_TIPO_CONTRIBU    = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.TIPO.CONT>  ;*56
		 S_SUB.SEGEMNTO     = R.CUSTOMER<EB.CUS.CUSTOMER.STATUS> ;*57
		 S_NIVEL.INGRESO    = R.CUSTOMER<EB.CUS.TARGET> ;*58
		 S.ESTADO.EMPLEO    = R.CUSTOMER<EB.CUS.EMPLOYMENT.STATUS> ;*59
		 S.OCUPACION.DUI    = R.CUSTOMER<EB.CUS.OCCUPATION> ;*60
		 S.NOMBRE.EMPRESA   = R.CUSTOMER<EB.CUS.EMPLOYERS.NAME> ;*61
		 S.DIRECCION.EMPRESA= R.CUSTOMER<EB.CUS.EMPLOYERS.ADD> ;*62
		 S.ACTIVIDAD.EMPRESA= R.CUSTOMER<EB.CUS.EMPLOYERS.BUSS> ;*63
		 D.FECHA.ENTRADA    = R.CUSTOMER<EB.CUS.EMPLOYMENT.START> ;*64
		 N.SALARIO          = R.CUSTOMER<EB.CUS.SALARY> ;*65
		 N.OTROS.INGRESOS   = R.CUSTOMER<EB.CUS.ANNUAL.BONUS> ;*66
		 S.CARGO            = R.CUSTOMER<EB.CUS.INTERESTS,1> ;*67
		 N.INGRESO.MENUSAL  = R.CUSTOMER<EB.CUS.NET.MONTHLY.IN> ;*68
		 N.EGRESO.MENUSAL   = R.CUSTOMER<EB.CUS.NET.MONTHLY.OUT> ;*69
		 ;*********************
		 S_RESIDENCIA.ESTADO = R.CUSTOMER<EB.CUS.RESIDENCE.STATUS> ;*70
		 S_TIPO.RESIDENCIA   = R.CUSTOMER<EB.CUS.RESIDENCE.TYPE> ;*71
		 D_RESIDENCIA.DESDE  = R.CUSTOMER<EB.CUS.RESIDENCE.SINCE> ;*72
		 N_VALOR.RESIDENCIA  = R.CUSTOMER<EB.CUS.RESIDENCE.VALUE> ;*73
		 S_RZONE             = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.ZONA2> ;*74
		 S_RDEPARMENT        = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.DEPARTAMENTO2> ;*75
		 S_RMUNICIPIO        = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.MUNICIPIO2> ;*76
		 S_RCANTON           = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.CANTON2> ;*77
		 S_RCOLONIA          = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.COLONIA2> ;*78
		 S_RCALLE            = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.CALLE2> ;*79
		 S_RAVENIDA          = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.AVENIDA2> ;*80
		 S_RNUM_CASA         = R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.NUM.DEPTO2> ;*81
		 
		
		 
		 ;**********************
		 S_INTERNET.BANKING  = R.CUSTOMER<EB.CUS.INTERNET.BANKING.SERVICE> ;*82
		 S_BANCA.MOVIL       = R.CUSTOMER<EB.CUS.MOBILE.BANKING.SERVICE> ;*83
		 ;**********************
		 D_CLIENTE.DESDE     = R.CUSTOMER<EB.CUS.CUSTOMER.SINCE,1> ;*84
		 N_NUMERO.DEPENDIENTE= R.CUSTOMER<EB.CUS.NO.OF.DEPENDENTS,1> ;*85
		 
		 ;**********************
		 ;*INICIO-SOCIOS SOCIEDADES Y JUNTA DIRECTIVA
		 ;**********************
				 	;*Credencia de la junta directiva
				 	N_NOCREDENCIAL	=''
				 	D_FECHAINICIAL	=''
				 	D_FECHAFINAL	=''
				 	S_NITDEUDOR		=''
					S_NAMESOCIOS	=''
					S_NITSOCIOS		=''
					S_PORCENTSOCIOS	=''
					S_CARGOSOCIOS	=''
					NITDEUDOR		=''
					NITMIEMBRO		=''
					CODCARGO		=''
					FECHAINICIALJD	=''
					FECHAFINALJD	=''
					NUMEROCREDENCIAL=''
					PORCENTEJEPARTIC=''
				 
				 	
				 	NO_TNX	=	DCOUNT(R.CUSTOMER<EB.CUS.LEGAL.ID>,VM) 		 	
				    FOR Y=1 TO NO_TNX	 	 		
						IF R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME><1,Y> EQ 'CREDENCIAL' THEN
							N_NOCREDENCIAL 	=R.CUSTOMER<EB.CUS.LEGAL.ID><1,Y>
							D_FECHAINICIAL	=R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE><1,Y>
							D_FECHAFINAL	=R.CUSTOMER<EB.CUS.LEGAL.EXP.DATE><1,Y>						
						END
					NEXT Y
					
					;*Socios de la junta directiva
					S_NITDEUDOR		=S_NIT
					S_NAMESOCIOS	=R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.ACCFULLNAME>
			      	S_NITSOCIOS		=R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.NITTAX>
			     	S_PORCENTSOCIOS	=R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.AMLPER>
			      	S_CARGOSOCIOS	=R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.JUNTA>
										
				 	NO_SOC =	DCOUNT(S_NAMESOCIOS,SM)
				 	FOR YS=1 TO NO_SOC
					 	NITDEUDOR		=S_NITDEUDOR
					 	NITMIEMBRO		=FIELD(S_NITSOCIOS,@SM,YS)
					 	CODCARGO		=FIELD(S_CARGOSOCIOS,@SM,YS)
					 	FECHAINICIALJD	=D_FECHAINICIAL
					 	FECHAFINALJD	=D_FECHAFINAL
					 	NUMEROCREDENCIAL=N_NOCREDENCIAL
				 		PORCENTEJEPARTIC=FIELD(S_PORCENTSOCIOS,@SM,YS)
						
						SOCIOLINE		=	NITDEUDOR:";":NITMIEMBRO:";":CODCARGO:";":FECHAINICIALJD:";":FECHAFINALJD:";":NUMEROCREDENCIAL:";":PORCENTEJEPARTIC:edLine
						ARR_SOCIOS<-1>	=SOCIOLINE
				 	NEXT YS
		 	
			;**********************
		 ;*FIN -  SOCIOS SOCIEDADES Y JUNTA DIRECTIVA
		 ;**********************	 
		 
		 
		 

		 	AR.CUS<I> = CUSTOMER.LIST<I>:";":S_FIRST.NAME:";":S_SECOND_NAME:";":S_THIRD_NAME:";":S_FIRST.LAST.NAME:";":S_SECOND.LAST.NAME:";":S_MARITAL.LAST.NAME:";":S_RAZON.SOCIAL:";":S_NOMBRE.COMERCIAL:";":D_FECHA.CONSTITUCI:";":S_MNEMONIC:";":S_ALTER.NAME:";":S_NIT.NAME:";":S_NIT:";":S_LAST_NIT:";":S_DUI:";":S_SEX:";":S_MARITAL.STATUS:";":S_AGENCIA:";":S_SECTOR.TYPE:";":S_OCUPACION.SSF:";":S_STATUS.CUSTOMER:";":S_SECTOR:";":S_PLACE_BIRTH:";":S_NATIONALITY:";":S_INTERNAL.CATEG:";":S_DATE_BIRTH:";":S_SEGMENT:";":S_EXENT.TAX:";":S_ACTIVITY_ECON:";":S_RESIDENCE:";":S_EXTERN.CATEG:";":S_LANGUAGE:";":S_FREQ.REVIEW:";":S_ISR_TYPE:";":S_COUNTRY:";":S_ZONE:";":S_DEPARMENT:";":S_MUNICIPIO:";":S_CANTON:";":S_COLONIA:";":S_CALLE:";":S_AVENIDA:";":S_NUM_CASA:";":S_TELEFONO:";":S_MOVIL:";":S_TEL.OFICINA:";":S_FAX:";":S_NO.DOCUMENTO:";":S_TIPO.DOCUMENT:";":S_LUGAR.EMISION:";":D_FECHA.EMISION:";":D_FECHA.VENCIMIENTO:";":S_RELATION_TYPE:";":S_REL.CUSTOMER:";":S_TAMANIO_EMP:";":S_TIPO_CONTRIBU:";":S_SUB.SEGEMNTO:";":S_NIVEL.INGRESO:";":S.ESTADO.EMPLEO:";":S.OCUPACION.DUI:";":S.NOMBRE.EMPRESA:";":S.DIRECCION.EMPRESA:";":S.ACTIVIDAD.EMPRESA:";":D.FECHA.ENTRADA:";":N.SALARIO:";":N.OTROS.INGRESOS:";":S.CARGO:";":N.INGRESO.MENUSAL:";":N.EGRESO.MENUSAL:";":S_RESIDENCIA.ESTADO:";":S_TIPO.RESIDENCIA:";":D_RESIDENCIA.DESDE:";":N_VALOR.RESIDENCIA:";":S_RZONE:";":S_RDEPARMENT:";":S_RMUNICIPIO:";":S_RCANTON:";":S_RCOLONIA:";":S_RCALLE:";":S_RAVENIDA:";":S_RNUM_CASA:";":S_INTERNET.BANKING:";":S_BANCA.MOVIL:";":D_CLIENTE.DESDE:";":N_NUMERO.DEPENDIENTE:edLine
		   CRT AR.CUS<I>
			 ;*-----------------------------------1-----------------2-----------------3------------------4--------------------5------------------------6----------------------7----------------8-----------------------9-------------------10--------------11---------------12----------13-----------14----------15--------16------------17-----------------18---------------------19------------------20------------------21-------------------22-------------23---------------24--------------25-------------------26-------------27-------------28--------------29------------------30-------------------31-------------32--------------33--------------34-------------35----------36-----------37-----------------38----------39------------40-----------41-----------42------------43--------------44-----------45------------46--------------47-----------48---------------49--------------------50-----------------51-----------------------52----------------53-----------------------54---------------55----------------56--------------57---------------------58--------------------59----------------60-----------------61-------------------62----------------------63----------------------64----------------65----------------66--------------67-------------68--------------------69------------------------70----------------------71-------------------72-------------------73----------------74----------------75-------------76------------77--------------78-----------79-------------80----------------81-------------------82--------------83---------------------84-----------------85--------------
			 IF NO.OF.CUSTOMER > I THEN			     		
			     WRITESEQ AR.CUS<I> APPEND TO SEQ.PTR THEN ;*con retorno de carro
			     END
			 END		       		
			 IF NO.OF.CUSTOMER=I THEN		 
				 WRITEBLK AR.CUS<I> ON SEQ.PTR THEN ;*sin retorno de carro
				 END	
			END
*		END
	 NEXT I
	 
*SOCIOS SOCIEDADES-JUNTA DIRECTIVA       
*----------------------------------------------------------        
 N_NOSC=DCOUNT(ARR_SOCIOS,FM)
 CRT N_NOSC
 FOR RSC=1 TO N_NOSC
 		IF RSC< N_NOSC THEN		
		            WRITESEQ ARR_SOCIOS<RSC> APPEND TO SEQ.PTR.SOC THEN ;*con retorno de carro
		            
		        	END		       
		    	END
		    IF RSC=N_NOSC THEN
		        WRITEBLK ARR_SOCIOS<RSC> ON SEQ.PTR.SOC THEN ;*sin retorno de carro
		        
		    	END
		 	END
 NEXT RSC
	 
	 
     CLOSESEQ SEQ.PTR
    CLOSESEQ SEQ.PTR.SOC
 RETURN
END
