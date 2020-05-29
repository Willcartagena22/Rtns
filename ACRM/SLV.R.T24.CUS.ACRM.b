*----------------------------------------------------------------------------------------------------
* <Rating>4848</Rating>
*----------------------------------------------------------------------------------------------------
* Nombre: SLV.R.T24.CUS.ACRM.b
* Descripcion: Genera archivo csv con informacion clientes T24 Para ACRM.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		Gerson			01.01.16	Version inicial
*			gerson			Revision	
* 2.0		Jonas			03.02.17	Agregar campo LF.OTHER.INDUST en generacion de archivo
* 2.1		Samuel Fusillo	21.03.2017  Se agregan campos solicitados para Declaracion Jurada FII
* 2.2       RFlamenco       20.09.2017  Correccion de Email
* 2.3       magarcia		07.12.2018	Se agregan campos solicitados para Transferecias Internacionales PN y PJ 
* 2.4		GAMARTINEZ		28.05.2019	Se modifica para envio de jms por medio de colas OSB
* 2.5       GAMARTINEZ 		02.07.2019 	modificacion para devolver json
*----------------------------------------------------------------------------------------------------

    SUBROUTINE SLV.R.T24.CUS.ACRM
*----------------------------------------------------------------------------------------------------
*
*----------------------------------------------------------------------------------------------------
* Modification History :
*----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
*    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
	$INSERT I_F.EB.LOOKUP
	$INSERT I_F.EB.SLV.CUSTOMER.TRAN.INT
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
   
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN


*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
    FN.CUS = 'F.CUSTOMER'
    F.CUS  = ''

	FN.LOOKUP = 'F.EB.LOOKUP'
    F.LOOKUP  = ''

    FN.CUST.TI   = 'FBNK.EB.SLV.CUSTOMER.TRAN.INT'
    F.CUST.TI    = ''
    EndLine = '|'	;*Separador de linea

;*Definicion de variables

    DIR.NAME		= 'MON_AML'
*    DIR.NAME='MON_AML_TI' ;*modo debug									   
    ;*NAME.FILE		= '_ACRM505Clientes.csv'
    
    
    ;*fecha y hora
    FECHAYHORA		= TIMEDATE()
	HORA 			= FECHAYHORA[1,8]:'.000'
	;*D_DATENOW		=	OCONV(DATE(),'D')
	;*D_DATENOW 		= 	OCONV(ICONV(D_DATENOW,"DMDY"),"D-E[2,A3,2]")
	D_DATENOW       =   OCONV(DATE(),"D/")
	;*UsLocGetDate	=	D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2]:' ':HORA
	UsLocGetDate	=	D_DATENOW[7,4]:D_DATENOW[1,2]:D_DATENOW[4,2]:'T':HORA
    
    ;*Bloque para Transferencias Internacionales
	FtcTintUtil = ''
	FtcTintCant = ''
	FtcTintMntp = ''
	FtcCobieUtil = ''
	FtcCobieCant = ''
	FtcCobieMntp = ''
	FtcLndUtil = ''
	FtcLndCant = ''
	FtcLndMntp = '-'  ;*Agregar caracter para que ultimo campo de arreglo no lleve vacio																					 
    RETURN

DELETE_AND_OPEN:
		;* Eliminando archivo existente
		;*------------------------------
		;*GAMARTINEZ (2.3)
*	    DELETESEQ DIR.NAME,NAME.FILE THEN
*	    END
	
		;* Abriendo archivo para escritura
		;*---------------------------------
		;*GAMARTINEZ (2.3)
*	    OPENSEQ DIR.NAME,NAME.FILE TO SEQ.PTR THEN
*	        WEOFSEQ NAME.FILE
*	    END
	RETURN
	
*-----------------------------------------------------------------------------
OPENFILE:
*-----------------------------------------------------------------------------
    CALL OPF(FN.CUS, F.CUS)
    CALL OPF(FN.LOOKUP, F.LOOKUP)
    CALL OPF(FN.CUST.TI,F.CUST.TI)								  
    RETURN


*-----------------------------------------------------------------------------
PROCESS:

   TEXTO.ARCHIVO = 'TIMEDATE() >':TIMEDATE()
   GOSUB ESCRIBIR.ARCHIVO
   
   TEXTO.ARCHIVO = 'DATE() >':DATE()
   GOSUB ESCRIBIR.ARCHIVO
   
   TEXTO.ARCHIVO = 'D_DATENOW (1) >':OCONV(DATE(),'D')
   GOSUB ESCRIBIR.ARCHIVO
   
   TEXTO.ARCHIVO = 'D_DATENOW (2) >':OCONV(ICONV(D_DATENOW,"DMDY"),"D-E[2,A3,2]")
   GOSUB ESCRIBIR.ARCHIVO
   
   TEXTO.ARCHIVO = 'UsLocGetDate >':D_DATENOW[7,4]:D_DATENOW[4,2]:D_DATENOW[1,2]:' ':HORA
   GOSUB ESCRIBIR.ARCHIVO
   
*-----------------------------------------------------------------------------
;*Obtener posicion de campo local
	CALL GET.LOC.REF("CUSTOMER","LF.RAZON.SOCIAL",RAZ.POS)	
	CALL GET.LOC.REF("CUSTOMER","LF.NOB.NIT",NNIT.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.NIT",NIT.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.DUI",DUI.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.TIPO.SECTOR",TPS.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.OCUPACION",OCU.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.LUG.NAC",LUGN.POS)
	CALL GET.LOC.REF("CUSTOMER","SEGMENT",SEG.POS)	
	CALL GET.LOC.REF('CUSTOMER',"LF.EXE.TAX",ETAX.POS)						
	CALL GET.LOC.REF('CUSTOMER',"LF.TYPE.CUST",TYPC.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.ZONA",ZONA.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.DEPARTAMENTO",DPT.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.MUNICIPIO",MUN.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.CANTON",CAN.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.COLONIA",COL.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.CALLE",CALL.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.AVENIDA",AVE.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.NUM.DEPTO",NDEP.POS)
    CALL GET.LOC.REF('CUSTOMER',"LF.TAMA.CONT",TAME.POS)
    CALL GET.LOC.REF('CUSTOMER',"LF.TIPO.CONT",TIPO.POS)
    CALL GET.LOC.REF("CUSTOMER","LF.COLONIA.2",COL2.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.CALLE.2",CALL2.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.AVENIDA.2",AVE2.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.ZONA.2",ZONA2.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.DEPTO.2",DPT2.POS)
	CALL GET.LOC.REF('CUSTOMER','LF.MUNICIPIO.2',MUN2.POS)	
	
	
	CALL GET.LOC.REF('CUSTOMER',"LF.ZONA.3",ZONA3.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.DEPTO.3",DPT3.POS)
	CALL GET.LOC.REF('CUSTOMER','LF.MUNICIPIO.3',MUN3.POS)
	CALL GET.LOC.REF("CUSTOMER","LF.CANTON.3",CAN3.POS)	
	CALL GET.LOC.REF("CUSTOMER","LF.COLONIA.3",COL3.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.CALLE.3",CALL3.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.AVENIDA.3",AVE3.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.NUM.DEPTO.3",NDEP3.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.EMP.SELEC.ID", EMPSELEC.POS)
	
	CALL GET.LOC.REF('CUSTOMER',"LF.NEG.PROP",NEG.PROP.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.NOM.FUNC.PUB",NOM.FUNCPUB.POS)
	CALL GET.LOC.REF('CUSTOMER',"LF.EMP.SELEC.ID",EMP.SELEC.ID)
	CALL GET.LOC.REF('CUSTOMER',"LF.NUM.EMPLES",NUM.EMPLES)
	CALL GET.LOC.REF('CUSTOMER',"LF.FLUJ.MES.APR",FLUJ.EFEC.MENS)
	
	CALL GET.LOC.REF('CUSTOMER',"LF.TOT.PASIVOS",POS.TOT.PASIVOS)
	CALL GET.LOC.REF('CUSTOMER',"LF.CLASS.CUS",POS.CLASS.CUS)
	CALL GET.LOC.REF('CUSTOMER',"LF.EXTERNAL.ACC",POS.EXTERNAL.ACC)
	CALL GET.LOC.REF('CUSTOMER',"LF.QT.DP.MTH.CT",POS.QT.DP.MTH.CT)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.DP.MTH.CT",POS.MT.DP.MTH.CT)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.DP.DL.AH",POS.MT.DP.DL.AH)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.DP.MTH.AH",POS.MT.DP.MTH.AH)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.DL.CTE.DO",POS.MT.DL.CTE.DO)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.MTH.CT.DO",POS.MT.MTH.CT.DO)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.DLY.AH.DO",POS.MT.DLY.AH.DO)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.MTH.AH.DO",POS.MT.MTH.AH.DO)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.TX.INT.RE",POS.MT.TX.INT.RE)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.TX.INT.EN",POS.MT.TX.INT.EN)
	CALL GET.LOC.REF('CUSTOMER',"LF.TXN.INT.REC",POS.TXN.INT.REC)
	CALL GET.LOC.REF('CUSTOMER',"LF.TXN.INT.ENV",POS.TXN.INT.ENV)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.SOBRE.AUT",POS.MT.SOBRE.AUT)
	
	CALL GET.LOC.REF('CUSTOMER',"LF.YN.TX.INTER",POS.YN.TX.INTER)
	CALL GET.LOC.REF('CUSTOMER',"LF.YN.DP.CSH",POS.YN.DP.CSH)
	
 	;*banca en linea
	CALL GET.LOC.REF('CUSTOMER',"LF.N.TRANS.BE",POS.NO.TXN.BANCEL)
	CALL GET.LOC.REF('CUSTOMER',"LF.MT.MNTH.BE",POS.MTO.MTH.BANCEL)
	
	;*banca en linea nuevos campos
	CALL GET.LOC.REF('CUSTOMER',"LF.AML.USA.TCIB",POS.AML.USA.TCIB) 
	CALL GET.LOC.REF('CUSTOMER',"LF.AML.AMT.TCIB",POS.AML.AMT.TCIB)
	CALL GET.LOC.REF('CUSTOMER',"LF.AML.MOT.TCIB",POS.AML.MOT.TCIB)
	
	;*CAMPOS
	CALL GET.LOC.REF('CUSTOMER',"LF.FUNC.PUB",POS.FN.PL.FL)
	CALL GET.LOC.REF('CUSTOMER',"LF.NOM.FUNC.SOC",POS.FN.PL.SOC)
	CALL GET.LOC.REF('CUSTOMER',"LF.CARGOFUNCION",POS.FN.PL.CR)
	CALL GET.LOC.REF('CUSTOMER',"LF.PAR.FUNC.PUB",POS.FN.PL.PR.FL)
	CALL GET.LOC.REF('CUSTOMER',"LF.NOM.FUNC.PUB",POS.FN.PL.PR.NM)
	CALL GET.LOC.REF('CUSTOMER',"LF.OTHER.INDUST",POS.OTHER.IND)
	
	;*Declaracion Jurada FII
	CALL GET.LOC.REF('CUSTOMER',"LF.ACTIVIT.CIIU", ACTIVIT.CIIU.POS) 
	CALL GET.LOC.REF('CUSTOMER',"LF.PRO.FON.N", PRO.FON.POS) 
	CALL GET.LOC.REF('CUSTOMER',"LF.NEG.OTRO", NEG.OTRO.POS) 
	CALL GET.LOC.REF('CUSTOMER',"LF.MON.EST.DEP", MON.EST.DEP.POS) 
	CALL GET.LOC.REF('CUSTOMER',"LF.MON.EST.ING", MON.EST.ING.POS)

	;*Agregar campos remesa familiar
	POS.ACT = ''
	POS.MON = ''
	APPL.ARR='CUSTOMER'
	FIELDNAME.REME='LF.REME.ACTIVA':VM:'LF.REME.MONTO'
	CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.REME,POS.REME)
	POS.ACT = POS.REME<1,1>
	POS.MON = POS.REME<1,2>					
	;*direccoin laboral
	DirLaboral = 'Z-':R.NEW(EB.CUS.LOCAL.REF)<1, ZONA2.POS>:' '
	DirLaboral := 'D-':R.NEW(EB.CUS.LOCAL.REF)<1, DPT2.POS>:' '
	DirLaboral := 'M-':R.NEW(EB.CUS.LOCAL.REF)<1, MUN2.POS>:' '
	DirLaboral :=  R.NEW(EB.CUS.LOCAL.REF)<1, COL2.POS>:' '
	DirLaboral := R.NEW(EB.CUS.LOCAL.REF)<1, CALL2.POS>:' '
	DirLaboral := R.NEW(EB.CUS.LOCAL.REF)<1, AVE2.POS>	
	
	;*Obtener ultimo valor de campo OVERRIDE
	OVER = DCOUNT(R.NEW(EB.CUS.OVERRIDE), SM)
	IF OVER EQ 0 THEN
		OVER = 1
	END	
		
	Y.DATETIME = LEFT(R.NEW(EB.CUS.DATE.TIME),6):" ":LEFT(RIGHT(R.NEW(EB.CUS.DATE.TIME),4),2):":":RIGHT(RIGHT(R.NEW(EB.CUS.DATE.TIME),4),2)

	;*Contruccion de variables
	CustomerId 		= ID.NEW 								;*ID cliente
	
	
	;* Abrir Archivo a Escribir
	;*--------------------------
	NAME.FILE		= '_T24Customer.' : CustomerId : '.csv'
	;*GAMARTINEZ (2.3)
*	GOSUB DELETE_AND_OPEN
	
	
	FirstName 		= R.NEW(EB.CUS.NAME.1) 					;*Primer nombre
	SecondName 		= R.NEW(EB.CUS.NAME.2) 					;*Segundo nombre 
	ThirdName 		= R.NEW(EB.CUS.GIVEN.NAMES) 			;*Tercer nombre
	FirstLastName 	= R.NEW(EB.CUS.TEXT)					;*Primer apellido
	SecondLastName 	= R.NEW(EB.CUS.FAMILY.NAME) 			;*Segundo apellido
	MaritalLastName = R.NEW(EB.CUS.PREVIOUS.NAME)  			;*Apellido de Casada
	SocialReason 	= R.NEW(EB.CUS.LOCAL.REF)<1, RAZ.POS>   ;*Razon social
	;*Tradename 		= R.NEW(EB.CUS.SHORT.NAME) 			 	;*Nombre Comercial
	
	TradeName = ''
	COUNT.TRADENAME		= DCOUNT(R.NEW<EB.CUS.SHORT.NAME>,VM)
	FOR TC = 1 TO COUNT.TRADENAME
		TradeName  = TradeName : R.NEW<EB.CUS.SHORT.NAME, TC>	;*Nombre Comercial
	NEXT TC
	
	ConstitutionDate= R.NEW(EB.CUS.BIRTH.INCORP.DATE) 	 	;*Fecha Constitucion
	Mnemonic 		= R.NEW(EB.CUS.MNEMONIC)				;*Mnemonic - DUI
	NitName 		= R.NEW(EB.CUS.LOCAL.REF)<1, NNIT.POS>	;*Nombre NIT
	Nit 			= R.NEW(EB.CUS.LOCAL.REF)<1, NIT.POS>	;*Numero NIT
	Dui 			= R.NEW(EB.CUS.LOCAL.REF)<1, DUI.POS>	;*Numero DUI
	
	NoTranBancel 	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.NO.TXN.BANCEL>	;*numero de transacciones en banca en linea
	MtoMonthBancel	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MTO.MTH.BANCEL>	;*monto de transacciones mensuales banca en linea
	
	FechaExpediDui   = ''
	Pasaporte		 = ''
	FechaExpediPassp = ''		
	NO_MVDOCS	=	DCOUNT(R.NEW<EB.CUS.LEGAL.ID>,VM)
    FOR Y=1 TO NO_MVDOCS
        BEGIN CASE
            CASE R.NEW<EB.CUS.LEGAL.DOC.NAME><1,Y> EQ 'DOCTO.UNICO.IDENT'
               ;* Dui 				= R.NEW<EB.CUS.LEGAL.ID><1,Y>
    			FechaExpediDui 		= LEFT(R.NEW<EB.CUS.LEGAL.ISS.DATE><1,Y>,4) : '-' : LEFT(RIGHT(R.NEW<EB.CUS.LEGAL.ISS.DATE><1,Y>,4),2) : '-' : RIGHT(R.NEW<EB.CUS.LEGAL.ISS.DATE><1,Y>,2)
            CASE R.NEW<EB.CUS.LEGAL.DOC.NAME><1,Y> EQ 'PASSPORT'
                Pasaporte			= R.NEW<EB.CUS.LEGAL.ID><1,Y>
                FechaExpediPassp 	= LEFT(R.NEW<EB.CUS.LEGAL.ISS.DATE><1,Y>,4) : '-' : LEFT(RIGHT(R.NEW<EB.CUS.LEGAL.ISS.DATE><1,Y>,4),2) : '-' : RIGHT(R.NEW<EB.CUS.LEGAL.ISS.DATE><1,Y>,2)
        END CASE
    NEXT Y 
	
	Gender 			= R.NEW(EB.CUS.GENDER) 					;*Genero
	MaritalStatus	= R.NEW(EB.CUS.MARITAL.STATUS) 			;*Estado civil
	AccountOfficer	= R.NEW(EB.CUS.ACCOUNT.OFFICER) 		;*DAO
	SectorType 		= R.NEW(EB.CUS.LOCAL.REF)<1, TPS.POS>	;*Tipo sector
	OcupationSSF 	= R.NEW(EB.CUS.LOCAL.REF)<1, OCU.POS>  	;*Ocupacion SSF
	StatusCustomer	= R.NEW(EB.CUS.CUSTOMER.STATUS)			;*Estado cliente
	Sector 			= R.NEW(EB.CUS.SECTOR)					;*Sector cliente
	PlaceBirth 		= R.NEW(EB.CUS.LOCAL.REF)<1, LUGN.POS>  ;*Lugar de nacimiento
	Nationality 	= R.NEW(EB.CUS.NATIONALITY)				;*Nacionalidad
	DateBirth 		= R.NEW(EB.CUS.DATE.OF.BIRTH)			;*Fecha cumpleaños
	Segment 		= R.NEW(EB.CUS.LOCAL.REF)<1, SEG.POS>   ;*Tipo de persona
	ExentTax 		= R.NEW(EB.CUS.LOCAL.REF)<1, ETAX.POS>	;*Exento Impuesto
	ActivityEcon 	= R.NEW(EB.CUS.INDUSTRY)				;*Actividad economica
	Residence 		= R.NEW(EB.CUS.RESIDENCE)				;*Residencia del Cliente
	IsrType 		= R.NEW(EB.CUS.LOCAL.REF)<1, TYPC.POS>	;*Tipo de Contribuyente
	Country 		= R.NEW(EB.CUS.COUNTRY)					;*Pais
	Zone 			= R.NEW(EB.CUS.LOCAL.REF)<1, ZONA.POS>	;*Zona
	Department 		= R.NEW(EB.CUS.LOCAL.REF)<1, DPT.POS>	;*Departamento
	Municipio 		= R.NEW(EB.CUS.LOCAL.REF)<1, MUN.POS>	;*Municipio
	Canton 			= R.NEW(EB.CUS.LOCAL.REF)<1, CAN.POS>	;*Canton
	Colonia 		= R.NEW(EB.CUS.LOCAL.REF)<1, COL.POS>	;*Colonia
	Calle 			= R.NEW(EB.CUS.LOCAL.REF)<1, CALL.POS>	;*Calle
	Avenida 		= R.NEW(EB.CUS.LOCAL.REF)<1, AVE.POS>	;*Avenida
	NumHouse 		= R.NEW(EB.CUS.LOCAL.REF)<1, NDEP.POS>	;*Numero casa
	Telephone 		= R.NEW(EB.CUS.PHONE.1)<1,1>			;*Telefono fijo
	MobilPhone 		= R.NEW(EB.CUS.SMS.1)<1,1>				;*Telefono movil
	Email			= R.NEW(EB.CUS.EMAIL.1)<1,1>			;*Correo	
	PhoneOffice 	= R.NEW(EB.CUS.OFF.PHONE)<1,1>			;*Telefono oficina
	Fax 			= R.NEW(EB.CUS.FAX.1)<1,1>				;*Numero fax
	NoDocument 		= R.NEW(EB.CUS.LEGAL.ID)<1,1>			;*Numero documento
	DocumentType 	= R.NEW(EB.CUS.LEGAL.DOC.NAME)<1,1>		;*Tipo documento
	PlaceIssue 		= R.NEW(EB.CUS.LEGAL.ISS.AUTH)<1,1>		;*Lugar emision
	DateIssueDoc 	= R.NEW(EB.CUS.LEGAL.ISS.DATE)<1,1> 	;*Fecha emision
	DueDate 		= R.NEW(EB.CUS.LEGAL.EXP.DATE)<1,1>		;*Fecha expiracion
	TamanioEmp 		= R.NEW(EB.CUS.LOCAL.REF)<1, TAME.POS>	;*Tamaño empresa
	SubSegment 		= R.NEW(EB.CUS.CUSTOMER.STATUS)			;*Sub-segmento
	IncomeLevel 	= R.NEW(EB.CUS.TARGET)					;*Nivel de ingreso
	StatusEmployment= R.NEW(EB.CUS.EMPLOYMENT.STATUS)<1,1>		;*Situacion laboral
	
	V.CODLOOKUP='EMPLOYMENT.STATUS*':StatusEmployment
	CALL F.READ(FN.LOOKUP,V.CODLOOKUP,R.LOOKUP,F.LOOKUP,Y.ERR)
	StatusEmployDescript=R.LOOKUP<EB.LU.DESCRIPTION><1,2>
	
	OccupationDui 	= R.NEW(EB.CUS.OCCUPATION)<1,1>				;*Ocupacion segun DUI
	CompanyName 	= R.NEW(EB.CUS.EMPLOYERS.NAME)<1,1>			;*Nombre empresa
	
	
	CompanyAddress	= DirLaboral							;*Direccion empresa
	CompanyActivity = R.NEW(EB.CUS.EMPLOYERS.BUSS)<1,1>				;*Actividad economica empresa
	DateOfAdmission = R.NEW(EB.CUS.EMPLOYMENT.START)<1,1>		;*Fecha ingreso
	Salary 			= R.NEW(EB.CUS.SALARY)<1,1>						;*Salario
	OtherIncomes 	= R.NEW(EB.CUS.ANNUAL.BONUS)<1,1>				;*Otros ingresos 
	Position 		= R.NEW(EB.CUS.INTERESTS)<1,1>				;*Cargo
	MonthIncome 	= R.NEW(EB.CUS.NET.MONTHLY.IN)			;*Ingreso mensual PJ
	IF Segment EQ 1 THEN
	    CALL GET.LOC.REF('CUSTOMER',"LF.MON.EST.ING",ING.MENS.PN.POS)
		;*MonthIncome 	= REC.CUSTOMER<EB.CUS.LOCAL.REF><1, ING.MENS.PN.POS>	;*Ingreso mensual PN
		MonthIncome = R.NEW(EB.CUS.LOCAL.REF)<1,ING.MENS.PN.POS>
	END 
	StatusResidence = R.NEW(EB.CUS.RESIDENCE.STATUS)		;*Estado de residencia 
   	ResidenceType 	= R.NEW(EB.CUS.RESIDENCE.TYPE)			;*Tipo de residencia
	ResidenceFrom 	= R.NEW(EB.CUS.RESIDENCE.SINCE)			;*Residencia desde
	RZone 			= R.NEW(EB.CUS.LOCAL.REF)<1, ZONA3.POS>	;*Zona residencia
	RDepartment 	= R.NEW(EB.CUS.LOCAL.REF)<1, DPT3.POS>	;*Departamento residencia
	RMunicipio		= R.NEW(EB.CUS.LOCAL.REF)<1, MUN3.POS>	;*Municipio residencia
	RCanton 		= R.NEW(EB.CUS.LOCAL.REF)<1, CAN3.POS>	;*Canton residencia
	RColonia 		= R.NEW(EB.CUS.LOCAL.REF)<1, COL3.POS>	;*Colonia residencia
	RCalle 			= R.NEW(EB.CUS.LOCAL.REF)<1, CALL3.POS>	;*Calle residencia
	RAvenida 		= R.NEW(EB.CUS.LOCAL.REF)<1, AVE3.POS>	;*Avenida residencia
	RNumCasa 		= R.NEW(EB.CUS.LOCAL.REF)<1, NDEP3.POS>	;*Numero  residencia
	NumDependents	= R.NEW(EB.CUS.NO.OF.DEPENDENTS)		;*Numero dependientes
	EmpresaEmpleado = R.NEW(EB.CUS.LOCAL.REF)<1, EMPSELEC.POS> ;*Id de la empresa del empleado
	Inputter		= FIELD(R.NEW(EB.CUS.INPUTTER),"_",2)	;*Digitador

	;*-----------------------------------------------------------------	
	
	DateTime		= UsLocGetDate							;*Fecha y hora del registro
	Authoriser		= FIELD(R.NEW(EB.CUS.AUTHORISER),"_",2)	;*Codigo del autorizador
	Company			= R.NEW(EB.CUS.COMPANY.BOOK)					;*Canal - Agencia
	CurrNum 		= R.NEW(EB.CUS.CURR.NO)					;*Numero Actual del Registro
	Override 		= R.CUS<EB.CUS.OVERRIDE><1, OVER>		;*Ultimo override

	;*AGREGAR
	NegProp 		= R.NEW(EB.CUS.LOCAL.REF)<1, NEG.PROP.POS>	;*Negocio propio
	NameFunPub		= R.NEW(EB.CUS.LOCAL.REF)<1, NOM.FUNCPUB.POS> ;*Nombre funcionario publico
	EmpSelec		= R.NEW(EB.CUS.LOCAL.REF)<1, EMP.SELEC.ID>		;*Empresa seleccionada 
	NumEmpleados	= R.NEW(EB.CUS.LOCAL.REF)<1, NUM.EMPLES>	;*Numero Empleados
	NumEmpleados	= R.NEW(EB.CUS.LOCAL.REF)<1, NUM.EMPLES>	;*Numero Empleados
	 IF R.NEW(EB.CUS.LOCAL.REF)<1, FLUJ.EFEC.MENS> NE '' THEN
		ManejaDepositoEfectivo = 'S'	;*Flujo de efectivo mensual
	 END ELSE
	  	ManejaDepositoEfectivo = 'N'
	END
	TotalPasivos 	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.TOT.PASIVOS> ;*total pasivos	
	ClaseClientes	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.CLASS.CUS>	  ;*Clase clientes
	CuentaExterior	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.EXTERNAL.ACC> ;*Cuentas en el Exterior
	DepEFECtaCteNoTxMen = R.NEW(EB.CUS.LOCAL.REF)<1, POS.QT.DP.MTH.CT> ;*Dep. efe. cta cte no tx
	DepEFECtaCteMtoMen	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.DP.MTH.CT> ;*Dep. efe. cta.cte monto mensual
	DepEFECtaAhoMtoDia	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.DP.DL.AH> ;*Dep. efe.  cta aho. mto diario
	DepEFECtaAhoMtoMen	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.DP.MTH.AH>;*Dep efe cta aho mto mensual	
	DepDtoCtaCteMtoDia	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.DL.CTE.DO>;*Dep documento cta cte mto diario
	DepDtoCtaCteMtoMen	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.MTH.CT.DO>;*Dep documento cta cte mto mensual
	DepDtoCtaAhoMtoDia	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.DLY.AH.DO>;*dep documento cta aho mto diario
	DepDtoCtasAhoMtoMen	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.MTH.AH.DO>;*dep documento cta ah mto mensual
	MtoTxInterRecibidas	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.TX.INT.RE>;*mto transacciones internacionales recibidas mensuales
	MtoTxInterEnviadas	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.TX.INT.EN>;*mto transacciones internaciones enviadas mensuales
	RecTxInterMensual	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.TXN.INT.REC>;*numero de transacciones internacioneles recibidas mensuales
	EnvTxInterMensual	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.TXN.INT.ENV>	;*Numero de transaccones internacionales enviadas mensulaes
	MtoSobrAutorizado	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.MT.SOBRE.AUT>	;*Monto de sobregiro autorizado
	ManejaOpInter		= R.NEW(EB.CUS.LOCAL.REF)<1, POS.YN.TX.INTER>	;*maneja operaciones internacionales
	ManejaDepEfectivo	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.YN.DP.CSH>		;*meneja depositos en efectivo
	
	FuncPubl            = R.NEW(EB.CUS.LOCAL.REF)<1, POS.FN.PL.FL>		;*Si es funcionario publico o tienen socios que lo sean
	FuncPublSocio       = R.NEW(EB.CUS.LOCAL.REF)<1, POS.FN.PL.SOC>		;*Nombre del socio que es funcionario publico
	FuncPublCargoTmp    = R.NEW(EB.CUS.LOCAL.REF)<1, POS.FN.PL.CR>		;*Cargo del funcionario publico
	DetActivEconom		= R.NEW(EB.CUS.LOCAL.REF)<1, POS.OTHER.IND>		;*Detalle de actividad economica del cliente
	
	LOOP
	REMOVE PART.CARGO FROM FuncPublCargoTmp SETTING POS.CARGO 
	WHILE PART.CARGO NE '' 
		FuncPublCargo:= PART.CARGO : ', ' 
	REPEAT 
		
	FuncPublPari        = R.NEW(EB.CUS.LOCAL.REF)<1, POS.FN.PL.PR.FL>	;*Pariente de funcionario publico
	FuncPublPariNomb    = R.NEW(EB.CUS.LOCAL.REF)<1, POS.FN.PL.PR.NM>	;*Nombre Pariente que es funcionario publico

	UtilizaBancaEl    	= R.NEW(EB.CUS.LOCAL.REF)<1, POS.AML.USA.TCIB>
	MontoDiarioBancaEl  = R.NEW(EB.CUS.LOCAL.REF)<1, POS.AML.AMT.TCIB>
	MotivoTnxBancaEl    = R.NEW(EB.CUS.LOCAL.REF)<1, POS.AML.MOT.TCIB>
	
	;*-----------------------------------------------------------------
	;*Declaracion Jurada
	ActividadEconimicaCiiu 		= R.NEW(EB.CUS.LOCAL.REF)<1, ACTIVIT.CIIU.POS>	
	ProcOtrosIngresos			= R.NEW(EB.CUS.LOCAL.REF)<1, PRO.FON.POS> 
	NombreNegocio 				= R.NEW(EB.CUS.LOCAL.REF)<1, NEG.OTRO.POS> 
	IngresosMenNegocio 			= R.NEW(EB.CUS.LOCAL.REF)<1, MON.EST.DEP.POS> 
	FlujoEfectMensual			= R.NEW(EB.CUS.LOCAL.REF)<1, FLUJ.EFEC.MENS>
	
	SourceID			= R.NEW(EB.CUS.INPUTTER)	

	;*Informacion de remesa familiar
    RecibeRemesa	= R.NEW(EB.CUS.LOCAL.REF)<1,POS.ACT>
    MontoMensualRem = R.NEW(EB.CUS.LOCAL.REF)<1,POS.MON>
	IF NumEmpleados EQ '' THEN
		NumEmpleados = 0	
	END

	
*	;*Generar arreglo con informacion
	
	STR.ARR = "{"

	STR.ARR := '"customerId":"': CustomerId:'",'
	STR.ARR := '"firstName":"': LEFT(TRIM(FirstName, ";","A"),99):'",'
	STR.ARR := '"secondName":"': LEFT(TRIM(SecondName, ";","A"),39):'",'
	STR.ARR := '"thirdName":"': LEFT(TRIM(ThirdName, ";","A"),39):'",'
	STR.ARR := '"firstLastName":"': LEFT(TRIM(FirstLastName, ";","A"),39):'",'
	STR.ARR := '"secondLastName":"': LEFT(TRIM(SecondLastName, ";","A"),39):'",'
	STR.ARR := '"maritalLastName":"': LEFT(TRIM(MaritalLastName, ";","A"),39):'",'
	STR.ARR := '"socialReason":"': LEFT(TRIM(SocialReason, ";","A"),149):'",'
	STR.ARR := '"tradename":"': LEFT(TRIM(Tradename, ";","A"),149):'",'
	IF ConstitutionDate EQ '' THEN 
		ConstitutionDate = '"1900-01-01"'
	END ELSE
		ConstitutionDate ='"':ConstitutionDate[1,4]:'-':ConstitutionDate[5,2]:'-':ConstitutionDate[7,2]:'"'
	END	
	STR.ARR := '"constitutionDate":':  ConstitutionDate  :','
	STR.ARR := '"mnemonic":"': LEFT(TRIM(Mnemonic, ";","A"),15):'",'
	STR.ARR := '"nitName":"': LEFT(TRIM(NitName, ";","A"),149):'",'
	STR.ARR := '"nit":"': Nit:'",'
	STR.ARR := '"dui":"': Dui:'",'
	STR.ARR := '"fechaExpediDui":"': FechaExpediDui:'",'
	STR.ARR := '"pasaporte":"': Pasaporte:'",'
	STR.ARR := '"fechaExpediPassp":"': FechaExpediPassp:'",'	
	STR.ARR := '"gender":"': Gender:'",'
	STR.ARR := '"martialStatus":"': MaritalStatus:'",'
	STR.ARR := '"accountOfficer":"': AccountOfficer:'",'
	STR.ARR := '"sectorType":"': SectorType:'",'
	STR.ARR := '"ocupationSsf":"': OcupationSSF:'",'
	STR.ARR := '"statusCustomer":"': StatusCustomer:'",'
	STR.ARR := '"sector":"': Sector:'",'
	STR.ARR := '"placeBirth":"': LEFT(TRIM(PlaceBirth, ";","A"),199):'",'
	STR.ARR := '"nationality":"': Nationality:'",'
	IF DateBirth EQ '' THEN 
		DateBirth = '"1900-01-01"'
	END ELSE
		DateBirth ='"':DateBirth[1,4]:'-':DateBirth[5,2]:'-':DateBirth[7,2]:'"'
	END
	STR.ARR := '"dateBirth":': DateBirth:','
	STR.ARR := '"segment":"': Segment:'",'
	STR.ARR := '"exentTax":"': ExentTax:'",'
	STR.ARR := '"activityEcon":"': ActivityEcon:'",'
	STR.ARR := '"residence":"': Residence:'",'
	STR.ARR := '"isrType":"': IsrType:'",'
	STR.ARR := '"country":"': Country:'",'
	STR.ARR := '"zone":"': Zone:'",'
	STR.ARR := '"department":"': Department:'",'
	STR.ARR := '"municipio":"': Municipio:'",'
	STR.ARR := '"canton":"': LEFT(TRIM(Canton, ";","A"),99):'",'
	STR.ARR := '"colonia":"': LEFT(TRIM(Colonia, ";","A"),99):'",'
	STR.ARR := '"calle":"': LEFT(TRIM(Calle, ";","A"),99):'",'
	STR.ARR := '"avenida":"': LEFT(TRIM(Avenida, ";","A"),99):'",'
	STR.ARR := '"numHouse":"': LEFT(TRIM(NumHouse, ";","A"),19):'",'
	STR.ARR := '"telephone":"': LEFT(TRIM(Telephone, ";","A"),20):'",'
	STR.ARR := '"mobilePhone":"': LEFT(TRIM(MobilPhone, ";","A"),20):'",'
	STR.ARR := '"email":"': LEFT(TRIM(Email, ";","A"),99):'",'
	STR.ARR := '"phoneOffice":"': LEFT(TRIM(PhoneOffice, ";","A"),20):'",'
	STR.ARR := '"fax":"': LEFT(TRIM(Fax, ";","A"),20):'",'
	STR.ARR := '"noDocument":"': LEFT(TRIM(NoDocument, ";","A"),49):'",'
	STR.ARR := '"documentType":"': DocumentType:'",'
	STR.ARR := '"placeIssue":"': LEFT(TRIM(PlaceIssue, ";","A"),49):'",'
	STR.ARR := '"dateIssue":"': DateIssue:'",'
	IF DueDate EQ '' THEN 
		DueDate = '"1900-01-01"'
	END ELSE
		DueDate ='"':DueDate[1,4]:'-':DueDate[5,2]:'-':DueDate[7,2]:'"'
	END
	STR.ARR := '"dueDate":': DueDate :','
	STR.ARR := '"temanioEmp":"': TamanioEmp:'",'
	STR.ARR := '"subSegment":"': SubSegment:'",'
	IF IncomeLevel EQ '' THEN IncomeLevel = '0'
	STR.ARR := '"incomeLevel":': IncomeLevel:','
	STR.ARR := '"statusEmployment":"': StatusEmployment:'",'
	STR.ARR := '"occupationDui":"': LEFT(TRIM(OccupationDui, ";","A"),49):'",'
	STR.ARR := '"companyName":"': LEFT(TRIM(CompanyName, ";","A"),199):'",'
	STR.ARR := '"companyAddress":"': LEFT(TRIM(CompanyAddress, ";","A"),199):'",'
	STR.ARR := '"companyActivity":"': LEFT(TRIM(CompanyActivity, ";","A"),49):'",'
	IF DateOfAdmission EQ '' THEN 
		DateOfAdmission = '"1900-01-01"'
	END ELSE
		DateOfAdmission ='"':DateOfAdmission[1,4]:'-':DateOfAdmission[5,2]:'-':DateOfAdmission[7,2]:'"'
	END	
	STR.ARR := '"dateOfAdmission":': DateOfAdmission:','
	IF Salary EQ '' THEN Salary = '0'
	STR.ARR := '"salary":': Salary:','
	IF OtherIncomes EQ '' THEN OtherIncomes = '0'
	STR.ARR := '"otherIncomes":': OtherIncomes:','
	STR.ARR := '"position":"': LEFT(TRIM(Position, ";","A"),99):'",'
	IF MonthIncome EQ '' THEN MonthIncome = '0'
	STR.ARR := '"monthIncome":': MonthIncome:','
	STR.ARR := '"statusResidence":"': StatusResidence:'",'
	STR.ARR := '"residenceType":"': ResidenceType:'",'
	STR.ARR := '"residenceFrom":"': ResidenceFrom:'",'
	STR.ARR := '"rZone":"': RZone:'",'
	STR.ARR := '"rDepartment":"': RDepartment:'",'
	STR.ARR := '"rMunicipio":"': RMunicipio:'",'
	STR.ARR := '"rCanton":"': LEFT(TRIM(RCanton, ";","A"),99):'",'
	STR.ARR := '"rColonia":"': LEFT(TRIM(RColonia, ";","A"),99):'",'
	STR.ARR := '"rCalle":"': LEFT(TRIM(RCalle, ";","A"),199):'",'
	STR.ARR := '"rAvenida":"': LEFT(TRIM(RAvenida, ";","A"),199):'",'
	STR.ARR := '"rNumCasa":"': LEFT(TRIM(RNumCasa, ";","A"),19):'",'
	IF NumDependents EQ '' THEN NumDependents = '0'
	STR.ARR := '"numDependents":': NumDependents:','
	STR.ARR := '"empresaEmpleado":"': EmpresaEmpleado: '",'
	STR.ARR := '"Inputter":"': LEFT(TRIM(Inputter, ";","A"),49):'",'
	STR.ARR := '"DateAndTime":"':   DateTime[1,4]:'-':DateTime[5,2]:'-':DateTime[7,2]: DateTime[9,LEN(DateTime)]    :'-06:00",'
	STR.ARR := '"Authorizer":"': LEFT(TRIM(Authoriser, ";","A"),49):'",'
	STR.ARR := '"Company":"': Company:'",'
	IF CurrNum EQ '' THEN CurrNum = '0'
	STR.ARR := '"CurrNum":': CurrNum:','
	STR.ARR := '"Override":"': LEFT(TRIM(Override, ";","A"),299):'",'
	;*AGREGAR
	STR.ARR := '"NegocioPropio":"': NegProp:'",'
	STR.ARR := '"NombreFuncPublico":"': LEFT(TRIM(NameFunPub, ";","A"),199):'",'
	IF EmpSelec EQ '' THEN EmpSelec = '0'
	STR.ARR := '"EmpresaSeleccionada":': EmpSelec:','
	IF NumEmpleados EQ '' THEN NumEmpleados = '0'
	STR.ARR := '"NumEmpleados":': NumEmpleados:','	
	STR.ARR := '"ManejaDepositoEfectivo":"': ManejaDepositoEfectivo:'",'	
	IF TotalPasivos EQ '' THEN TotalPasivos = '0'
	STR.ARR := '"TotalPasivos":':TotalPasivos:',' 		
	STR.ARR := '"ClaseClientes":"':ClaseClientes:'",'		
	STR.ARR := '"CuentaExterior":"':CuentaExterior:'",'
	IF DepEFECtaCteNoTxMen EQ '' THEN DepEFECtaCteNoTxMen = '0'		
	STR.ARR := '"DepEFECtaCteNoTxMen":':DepEFECtaCteNoTxMen:',' 
	IF DepEFECtaCteMtoMen EQ '' THEN DepEFECtaCteMtoMen = '0'
	STR.ARR := '"DepEFECtaCteMtoMen":':DepEFECtaCteMtoMen:','	
	IF DepEFECtaAhoMtoDia EQ '' THEN DepEFECtaAhoMtoDia = '0'
	STR.ARR := '"DepEFECtaAhoMtoDia":':DepEFECtaAhoMtoDia:','
	IF DepEFECtaAhoMtoMen EQ '' THEN DepEFECtaAhoMtoMen = '0'	
	STR.ARR := '"DepEFECtaAhoMtoMen":':DepEFECtaAhoMtoMen:','	
	IF DepDtoCtaCteMtoDia EQ '' THEN DepDtoCtaCteMtoDia = '0'
	STR.ARR := '"DepDtoCtaCteMtoDia":':DepDtoCtaCteMtoDia:','
	IF DepDtoCtaCteMtoMen EQ '' THEN DepDtoCtaCteMtoMen = '0'	
	STR.ARR := '"DepDtoCtaCteMtoMen":':DepDtoCtaCteMtoMen:','	
	IF DepDtoCtaAhoMtoDia EQ '' THEN DepDtoCtaAhoMtoDia = '0'
	STR.ARR := '"DepDtoCtaAhoMtoDia":':DepDtoCtaAhoMtoDia:','	
	IF DepDtoCtasAhoMtoMen EQ '' THEN DepDtoCtasAhoMtoMen = '0'
	STR.ARR := '"DepDtoCtasAhoMtoMen":':DepDtoCtasAhoMtoMen:','
	IF MtoTxInterRecibidas EQ '' THEN MtoTxInterRecibidas = '0'
	STR.ARR := '"MtoTxInterRecibidas":':MtoTxInterRecibidas:','
	IF MtoTxInterEnviadas EQ '' THEN MtoTxInterEnviadas = '0'
	STR.ARR := '"MtoTxInterEnviadas":':MtoTxInterEnviadas:','
	IF RecTxInterMensual EQ '' THEN RecTxInterMensual = '0'	
	STR.ARR := '"RecTxInterMensual":':RecTxInterMensual:','	
	IF EnvTxInterMensual EQ '' THEN EnvTxInterMensual = '0'
	STR.ARR := '"EnvTxInterMensual":':EnvTxInterMensual:','
	IF MtoSobrAutorizado EQ '' THEN MtoSobrAutorizado = '0'	
	STR.ARR := '"MtoSobrAutorizado":':MtoSobrAutorizado:','	
	STR.ARR := '"ManejaOpInter":"':ManejaOpInter:'",'	
	STR.ARR := '"ManejaDepEfectivo":"':ManejaDepEfectivo:'",'
	
	;*AGREGAR CAMPOS BANCA ELECTRONICA
	IF NoTranBancel EQ '' THEN NoTranBancel = '0'	
	STR.ARR := '"NoTranBancel":':NoTranBancel:','	
	IF MtoMonthBancel EQ '' THEN MtoMonthBancel = '0'			
	STR.ARR := '"MtoMonthBancel":':MtoMonthBancel:','
	
	;*AGREGAR CAMPOS 
	STR.ARR := '"FuncPubl":"':FuncPubl:'",'			
	STR.ARR := '"FuncPublSocio":"':FuncPublSocio:'",'
	STR.ARR := '"FuncPublCargo":"':LEFT(TRIM(FuncPublCargo, ";","A"),200):'",'			
	STR.ARR := '"FuncPublPari":"':FuncPublPari:'",'
	STR.ARR := '"FuncPublPariNomb":"':LEFT(TRIM(FuncPublPariNomb, ";","A"),200):'",'
	
	;*campos para banca en linea
	STR.ARR := '"UtilizaBancaEl":"':UtilizaBancaEl:'",'
	IF MontoDiarioBancaEl EQ '' THEN MontoDiarioBancaEl = '0'
	STR.ARR := '"MontoDiarioBancaEl":':MontoDiarioBancaEl:','
	STR.ARR := '"MotivoTnxBancaEl":"':LEFT(TRIM(MotivoTnxBancaEl, ";","A"),499):'",'
	STR.ARR := '"EconActivDetail":"':LEFT(TRIM(DetActivEconom, ";","A"),69):'",'
	STR.ARR := '"SourceID":"':LEFT(TRIM(SourceID, ";","A"),199):'",'
	
	;*Declaracion Jurada FII
	STR.ARR := '"ActividadEconimicaCiiu":"': ActividadEconimicaCiiu		:'",'
	STR.ARR := '"ProcOtrosIngresos":"': LEFT(TRIM(ProcOtrosIngresos, ";","A"),200)    		:'",'
	STR.ARR := '"NombreNegocio":"': LEFT(TRIM(NombreNegocio, ";","A") ,200)	    		:'",'
	IF IngresosMenNegocio EQ '' THEN IngresosMenNegocio = '0'
	STR.ARR := '"IngresosMenNegocio":': IngresosMenNegocio    		:','
	IF FlujoEfectMensual EQ '' THEN FlujoEfectMensual = '0'
	STR.ARR := '"FlujoEfectMensual":': FlujoEfectMensual			:','
	STR.ARR := '"StatusEmployDescript":"': StatusEmployDescript 		:'",'

	;*Obtener campos de Transferencias Internacionales
	GOSUB TI.GET.VALUES.PLUS
	STR.ARR := '"FtcTintUtil":"': FtcTintUtil:'",'  
	STR.ARR := '"FtcTintCant":"': FtcTintCant :'",'
	STR.ARR := '"FtcTintMntp":"': FtcTintMntp :'",'
	STR.ARR := '"FtcCobieUtil":"': FtcCobieUtil :'",'
	STR.ARR := '"FtcCobieCant":"': FtcCobieCant :'",'
	STR.ARR := '"FtcCobieMntp":"': FtcCobieMntp :'",'
	STR.ARR := '"FtcLndUtil":"':  FtcLndUtil :'",'
	STR.ARR := '"FtcLndCant":"':  FtcLndCant :'",'
 
	STR.ARR := '"FtcLndMntp":"': FtcLndMntp	:'"'


	
	STR.ARR := "}"
    
;* Generacion de csv array para escritura de en csv
*-----------------------------------------------------------------------------
    ;*Agregar arreglo al archivo

	;*GAMARTINEZ (2.3)
*	WRITEBLK STR.ARR ON SEQ.PTR THEN ;*sin retorno de carro
*    END


    
*----------------------------------------------------------------------------------------------------
   ;*GAMARTINEZ (2.3)
*    CLOSESEQ SEQ.PTR

		;*GAMARTINEZ (2.3)
		TEXTO.ARCHIVO = STR.ARR
		GOSUB ESCRIBIR.ARCHIVO
		JMS.MSJ = STR.ARR
		GOSUB SEND.JMS		

    RETURN
    
    ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MON_AML'
*    DIR.NAME= 'MON_AML_TI' ;*modo debug										
    R.ID   = 'DATE':TODAY:'.txt'
;* hacer que escriba un archivo
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN
    
	;* GAMARTINEZ (2.3)
	SEND.JMS:										   
	
		JMS.RESPONSE=''
		JMS.ERROR=''
		JMS.APP = 'ACRM_CUSTOMER'
		JMS.METHOD = 'send'
		
		CALL SLV.B.SEND.JMS(JMS.METHOD,JMS.APP:"##":JMS.MSJ,JMS.RESPONSE,JMS.ERROR)
		
		CRT JMS.RESPONSE
		CRT JMS.ERROR
		
		TEXTO.ARCHIVO = JMS.RESPONSE:' - ': JMS.ERROR
		GOSUB ESCRIBIR.ARCHIVO
	
	RETURN    
 
*Obtener campos de Transferencias Internacionales
TI.GET.VALUES.PLUS:
	CALL F.READ(FN.CUST.TI,CustomerId,R.CUST.TI,F.CUST.TI,ERR.CUST.TI)	
	FtcTintUtil = R.CUST.TI<EB.SLV5.FTC.TINT.UTIL>
	FtcTintCant = R.CUST.TI<EB.SLV5.FTC.TINT.CANT>
	FtcTintMntp = R.CUST.TI<EB.SLV5.FTC.TINT.MNTP>
	
	FtcCobieUtil = R.CUST.TI<EB.SLV5.FTC.COBIE.UTIL>
	FtcCobieCant = R.CUST.TI<EB.SLV5.FTC.COBIE.CANT>
	FtcCobieMntp = R.CUST.TI<EB.SLV5.FTC.COBIE.MNTP>

	FtcLndUtil = R.CUST.TI<EB.SLV5.FTC.LND.UTIL>
	FtcLndCant = R.CUST.TI<EB.SLV5.FTC.LND.CANT>

	;*Agregar caracter para que ultimo campo de arreglo no lleve vacio
	IF NOT(R.CUST.TI<EB.SLV5.FTC.LND.MNTP>) THEN
	   FtcLndMntp = "-"
	END ELSE
	   FtcLndMntp = R.CUST.TI<EB.SLV5.FTC.LND.MNTP>
	END	
RETURN
 
    
    END
