*-----------------------------------------------------------------------------
* <Rating>16006</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.E.NOF.ACC.DJ(ENQ.DATA)
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.ACC.DJ
* Descripci�n: Rutina NOFILE para enquiry de emisi�n declaraci�n jurada.
*-----------------------------------------------------------------------------
* Version	Paquete								Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0											Serge		16.12.14	Versi�n inicial.
* 2.0											Serge		02.01.15	Declaraci�n Jurada por producto y no cliente.
* 2.1       									Serge       08.01.15	Asignar direcci�n de residencia y no de contacto.
* 2.2       									Serge       12.01.15    Corregir direcci�n de ubicaci�n de empresa para cliente jur�dico.
* 2.3		AZUL001-161214.JURADA.SV_FIX1		Serge		21.01.15	Mostrar CARNET RESIDENTE cuando para extranjeros cuando aplique.
* 2.4											Serge		28.01.15	Ajustar el tipo de cliente a utilizar valor Segment
* 2.5											R.Garay		03.02.15	Se ajust� condicion de tipo de clientes y garantizar el envio de direccion y municipio en persona juridica.
* 2.6		20150210-AZUL002-RUTINAS_TILDES-FIX1R.Garay		04.02.15	Se adiciono tratamiento de las � asi como tambien manejo multivalor de campo LF.PRO.FON.N
* 2.7		20150219-AZUL001-CORR239-FIX1		R.Garay		19.02.15	Ahora se obtiene la actividad economica del campo LF.ACT.ECON de la aplicacion INDUSTRY
* 2.8		20150220-AZUL001-CORR244-RELEASE1	R.Garay		20.02.15	Se adiciono llamada a rutina SLV.E.NOF.GEN.FILEEXIST para validar creacion del pdf
* 2.9											Galfaro		28.02.15	asignar valores a variable de estado del arreglo y evaluacion de lennding
* 3.0											R.Garay		14.04.15	adicionar los campos Cargo.Desempeno y Nombre.Funcionario de PEP
* 3.1											R.Garay		21.04.15	obtener fuente de ingresos por valor de descripcion y no por CASE quemado
* 3.2											R.Garay		18.05.15	Tratamiento de multivalor a Cargo de empresa, GB Pa�s y short.name, antes de ser problema por multivalor
* 3.3											O.Cornejo	06.10.15	Se agrega impresion de arreglos cuando los clientes son OTHER.PARTY (mancomunados) en Perfil y DJ
* 3.4											R.Garay		27.11.15	Se adiciona tratamiento de multivalor para cargo desempeniado en declaracion jurada
* 3.5											O.Cornejo	22.03.15	Modificacion por Datos de Banca en Linea
* 3.6                                           R.Flamenco  16.05.16    Agregar campos banca en linea para customer
* 3.7                                           D.Montes    24.08.16    Cambio APDF para geneara pdf.
* 3.8                                           r.ortiz     23.12.16    Cambio para agregar campos de FATCA fase I
* 3.8                                           Jonas       21.01.18    Agregar informacion de remesa familiar.
* 3.9                                           Jonas       14.05.18    Eliminar espacio en cadena de texto.
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
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

    $INSERT I_F.CUS.OCUPACION.SSF
    $INSERT I_F.SLV.CUS.DEPARTAMENTO
    $INSERT I_F.SLV.CUS.MUNICIPIO

    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.EB.LOOKUP

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB LOAD
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
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    FN.CUSTOMER = 'F.CUSTOMER'
    F.ACCOUNT = ''

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

    LOCATE "CUSTOMER.ID" IN D.FIELDS<1> SETTING PAR.POS.1 THEN
    SS.CUSTOMER.ID = D.RANGE.AND.VALUE<PAR.POS.1>
    END

    LOCATE "ACCOUNT.ID" IN D.FIELDS<1> SETTING PAR.POS.2 THEN
    SS.ACCOUNT.ID = D.RANGE.AND.VALUE<PAR.POS.2>
    END
*-----------------------------------------------------------------------------
* Datos de prueba
*-----------------------------------------------------------------------------
;* DEBUG
*    SS.CUSTOMER.ID = '103552'				;* DBK
*    SS.ACCOUNT.ID = '10000000008689'    ;* natural
*    SS.ACCOUNT.ID = '10000000057159'	 ;* natural menor de edad
*    SS.ACCOUNT.ID = '10000000036461'		;* juridica
*-----------------------------------------------------------------------------
    IF SS.ACCOUNT.ID NE '' THEN
        SN.GENERA.DJ = 1
        SS.NOMBRE.DOCUMENTO = "Perfil del Cliente y Declaracion Jurada"
    END
    ELSE
    SN.GENERA.DJ = 0
    SS.NOMBRE.DOCUMENTO = "Perfil del Cliente"
    END

	;*Remesa Familiar
    REMESA.ACT = ''
    REMESA.PAI = ''
    REMESA.MON = ''
    REMESA.MOT = ''

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

    RETURN
*-----------------------------------------------------------------------------
LOAD:
*-----------------------------------------------------------------------------
;* Tiempo de espera para generaci�n de PDF de VIPO
*    CALL F.READ(FN.TABLE.PA, 'CONT.VIPO.DELAY', R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
*    VIPO.DELAY = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

;* Obtienendo la fecha actual con el formato adecuado
    MES.EMISION = TODAY[5,2]
    MES.EMISION = CHANGE(MES.EMISION, '01', 'enero')
    MES.EMISION = CHANGE(MES.EMISION, '02', 'febrero')
    MES.EMISION = CHANGE(MES.EMISION, '03', 'marzo')
    MES.EMISION = CHANGE(MES.EMISION, '04', 'abril')
    MES.EMISION = CHANGE(MES.EMISION, '05', 'mayo')
    MES.EMISION = CHANGE(MES.EMISION, '06', 'junio')
    MES.EMISION = CHANGE(MES.EMISION, '07', 'julio')
    MES.EMISION = CHANGE(MES.EMISION, '08', 'agosto')
    MES.EMISION = CHANGE(MES.EMISION, '09', 'septiembre')
    MES.EMISION = CHANGE(MES.EMISION, '10', 'octubre')
    MES.EMISION = CHANGE(MES.EMISION, '11', 'noviembre')
    MES.EMISION = CHANGE(MES.EMISION, '12', 'diciembre')
    FECHA.EMISION = TODAY[7,2] : ' de ': MES.EMISION : ' de ' :TODAY[1,4]

    RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
;* Si la emisi�n es por cuenta, verificar estado del acuerdo
    IF SN.GENERA.DJ EQ 1 THEN
        ;* Obtener id de arrangement
        CALL F.READ(FN.ACCOUNT, SS.ACCOUNT.ID, R.ACCOUNT, F.ACCOUNT, F.ERR.ACC)
        SS.ARRANGEMENT.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>

        ;* Obtener ACCOUNT
        CALL F.READ(FN.AA.ARRANGEMENT, SS.ARRANGEMENT.ID, R.AA.ARRANGEMENT, F.AA.ARRANGEMENT, F.ERR.AAR)

        BEGIN CASE
            CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'ACCOUNTS'
                SS.TIPO.CUENTA = 'TIPO-CUENTA'
            CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS'
                SS.TIPO.CUENTA = 'TIPO-DEPOSITO'
            CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'LENDING'
                SS.TIPO.CUENTA = 'TIPO-PRESTAMO'
        END CASE

        ;* Buscando a los propietarios de las cuentas
        AA.CUSTOMER.KEY = SS.ARRANGEMENT.ID : '-CUSTOMER-' : R.AA.ARRANGEMENT<AA.ARR.START.DATE> : '.1'

        ESTADO.ARREGLO =	R.AA.ARRANGEMENT<AA.ARR.ARR.STATUS>

        ;* adicionar estado Current para cuando ya fue desembolsado
        IF ESTADO.ARREGLO EQ 'AUTH' OR ESTADO.ARREGLO EQ 'CURRENT' THEN
            FN.ARR.CUSTOMER.TO.READ = 'F.AA.ARR.CUSTOMER'
        END
        ELSE
        FN.ARR.CUSTOMER.TO.READ = 'F.AA.ARR.CUSTOMER$NAU'
    END
*		CRT FN.ARR.CUSTOMER.TO.READ:'ABDC'
    CALL F.READ(FN.ARR.CUSTOMER.TO.READ, AA.CUSTOMER.KEY, R.ARR.CUSTOMER, F.ARR.CUSTOMER, F.ERR.ACU)

;* recorriendo los titulares
    LA.TITULARES = ''
    LA.TITULARES<-1> = R.AA.ARRANGEMENT<AA.ARR.CUSTOMER>
    LA.OWNER = R.AA.ARRANGEMENT<AA.ARR.CUSTOMER>
    CALL GET.LOC.REF('AA.ARR.CUSTOMER', 'LF.CON.INDIS', POS)
    LS.CONDICION = R.ARR.CUSTOMER<AA.CUS.LOCAL.REF, POS>

    IF (LS.CONDICION EQ 'Y' OR LS.CONDICION EQ 'O') OR R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'LENDING' THEN
        LN.CONTADOR = DCOUNT(R.ARR.CUSTOMER<AA.CUS.OTHER.PARTY>, @VM)
        FOR I1 = 1 TO LN.CONTADOR
            LA.TITULARES<-1> = R.ARR.CUSTOMER<AA.CUS.OTHER.PARTY><1,I1>
        NEXT I1
    END
    END
    ELSE
    LA.TITULARES = ''
    LA.TITULARES<-1> = SS.CUSTOMER.ID
    END

;* Generar datos para cada titular
    LN.CONTADOR = DCOUNT(LA.TITULARES, @FM)

    FOR I2 = 1 TO LN.CONTADOR

        SS.CUSTOMER.ID = LA.TITULARES<I2>

        CALL F.READ(FN.CUSTOMER, SS.CUSTOMER.ID, R.CUSTOMER, F.CUSTOMER, F.ERR.CUS)
        CALL GET.LOC.REF('CUSTOMER', 'LF.OTHER.INDUST', POS)
        LS.RL.ACTIVIDAD.ECONOMICA = R.CUSTOMER.RL<EB.CUS.LOCAL.REF, POS>

        IF LS.RL.ACTIVIDAD.ECONOMICA NE 'NO ESPECIFICADO' THEN

            GOSUB PROCESS.TITULAR
            GOSUB PROCESS.PERFIL
        END

    NEXT I2
    RETURN
*-----------------------------------------------------------------------------
PROCESS.TITULAR:
*-----------------------------------------------------------------------------
;* Leyendo el archivo de clientes
    CALL F.READ(FN.CUSTOMER, SS.CUSTOMER.ID, R.CUSTOMER, F.CUSTOMER, F.ERR.CUS)

    APPL.ARR='CUSTOMER'
    FIELDNAME.ARR='LF.FUNC.PUB':VM:'LF.PAR.FUNC.PUB':VM:'LF.CARGOFUNCION':VM:'LF.NOM.FUNC.PUB':VM:'LF.PRO.FON.N':VM:'LF.PRO.FON.J'
    FIELDNAME.ARR:=VM:'LF.FULL.NAME':VM:'LF.NIT.TAX':VM:'LF.AML.PER':VM:'LF.CARGO.JUNTA':VM:'LF.JUN.TXT':VM:'SEGMENT':VM:'LF.RAZON.SOCIAL'
    FIELDNAME.ARR:=VM:'LF.NUM.EMPLES':VM:'LF.NUM.PROV':VM:'LF.NUM.SUC':VM:'LF.OTHER.INDUST'
    FIELDNAME.ARR:=VM:'LF.REA.TRANS':VM:'LF.N.TRANS':VM:'LF.MONTO.MNTH':VM:'LF.MOTIVO.TRANS':VM:'LF.NOB.NIT':VM:'LF.LUG.NAC':VM:'LF.TIPO.SECTOR':VM:'LF.DEPTO.3'
    FIELDNAME.ARR:=VM:'LF.NEG.PROP':VM:'LF.MON.EST.ING':VM:'LF.MON.EST.DEP':VM:'LF.FLUJ.MES.APR'
    FIELDNAME.ARR:=VM:'LF.AML.USA.TCIB':VM:'LF.AML.AMT.TCIB':VM:'LF.AML.MOT.TCIB':VM:'LF.OCUPACION'
    FIELDNAME.ARR:=VM:'LF.REME.ACTIVA':VM:'LF.REME.PAIS':VM:'LF.REME.MONTO':VM:'LF.REME.MOTIVO'

    POS.ARR = ''

    CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.ARR)
    POS.LFUNC.PUB = POS.ARR<1,1>
    POS.LPAR.FUNC.PUB = POS.ARR<1,2>
    POS.LCARGOFUNCION = POS.ARR<1,3>
    POS.LNOM.FUNC.PUB = POS.ARR<1,4>
    POS.LPRO.FON.N = POS.ARR<1,5>
    POS.LPRO.FON.J = POS.ARR<1,6>
    POS.LFULL.NAME=POS.ARR<1,7>
    POS.LNIT.TAX=POS.ARR<1,8>
    POS.LAML.PER=POS.ARR<1,9>
    POS.LCARGO.JUNTA=POS.ARR<1,10>
    POS.LJUN.TXT=POS.ARR<1,11>
    POS.SEGMENT=POS.ARR<1,12>
    POS.LRAZON.SOCIAL=POS.ARR<1,13>
    POS.LNUM.EMPLES=POS.ARR<1,14>
    POS.LNUM.PROV=POS.ARR<1,15>
    POS.LNUM.SUC=POS.ARR<1,16>
    POS.LOTHER.INDUST=POS.ARR<1,17>
    POS.LREA.TRANS=POS.ARR<1,18>
    POS.LN.TRANS=POS.ARR<1,19>
    POS.LMONTO.MNTH=POS.ARR<1,20>
    POS.LMOTIVO.TRANS=POS.ARR<1,21>
    POS.LNOB.NIT=POS.ARR<1,22>
    POS.LLUG.NAC=POS.ARR<1,23>
    POS.LTIPO.SECTOR=POS.ARR<1,24>
    POS.LDEPARTAMENTO=POS.ARR<1,25>

    POS.LF.NEG.PROP=POS.ARR<1,26>
    POS.LF.MON.EST.ING=POS.ARR<1,27>
    POS.LF.MON.EST.APR=POS.ARR<1,28>
    POS.LF.FLUJ.MES.APR=POS.ARR<1,29>


    P.LF.AML.USA.TCIB=POS.ARR<1,30>
    P.LF.AML.AMT.TCIB=POS.ARR<1,31>
    P.LF.AML.MOT.TCIB=POS.ARR<1,32>
    P.LF.OCUPACION=POS.ARR<1,33>
    
    POS.REME.ACT=POS.ARR<1,34> ;*LF.REME.ACTIVA
    POS.REME.PAI=POS.ARR<1,35> ;*LF.REME.PAIS
    POS.REME.MON=POS.ARR<1,36> ;*LF.REME.MONTO
    POS.REME.MOT=POS.ARR<1,37> ;*LF.REME.MOTIVO

;* determinando la personeria del cliente
    LS.PERSONERIA = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.SEGMENT>
    SS.SHORT.NAME = SWAP(TRIM(R.CUSTOMER<EB.CUS.SHORT.NAME>), VM, ' ')

;*Obtener Datos de Banca en Linea : OCORNEJO 22.03.2016
;*-----------------------------------------------------
    USA.TCIB = 'NO'
    CRT '>':R.CUSTOMER<EB.CUS.LOCAL.REF><1, P.LF.AML.USA.TCIB>
    IF R.CUSTOMER<EB.CUS.LOCAL.REF><1, P.LF.AML.USA.TCIB> EQ 'SI' THEN
        USA.TCIB = 'SI'
    END
    MONTO.TCIB  = R.CUSTOMER<EB.CUS.LOCAL.REF><1, P.LF.AML.AMT.TCIB>
    MOTIVO.TCIB = SWAP(TRIM(R.CUSTOMER<EB.CUS.LOCAL.REF><1, P.LF.AML.MOT.TCIB>), SM, ' ')
;*-----------------------------------------------------
;* Informaci�n a recuperar dependiendo el tipo de cliente
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN
        ;* Extracci�n de los nombres
        PRIMER.NOMBRE = TRIM(R.CUSTOMER<EB.CUS.NAME.1>)
        SEGUNDO.NOMBRE = TRIM(R.CUSTOMER<EB.CUS.NAME.2>)
        TERCER.NOMBRE = TRIM(R.CUSTOMER<EB.CUS.GIVEN.NAMES>)
        PRIMER.APELLIDO = TRIM(R.CUSTOMER<EB.CUS.TEXT>)
        SEGUNDO.APELLIDO = TRIM(R.CUSTOMER<EB.CUS.FAMILY.NAME>)
        APELLIDO.CASADA = TRIM(R.CUSTOMER<EB.CUS.PREVIOUS.NAME>)
        CONOCIDO.POR = TRIM(R.CUSTOMER<EB.CUS.INTRODUCER>)

        REMESA.ACT =	R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.REME.ACT>
        IF REMESA.ACT EQ "SI" THEN
	        REMESA.PAI =	R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.REME.PAI>
	        REMESA.MON =	R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.REME.MON>
	        REMESA.MOT =	R.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.REME.MOT>
	        Y.COUN.PAI = DCOUNT(REMESA.PAI,SM)
        	FOR I = 1 TO Y.COUN.PAI
        		Y.PAIS = FIELD(REMESA.PAI,SM,I)
        		CALL F.READ(FN.TABLE.NAC, Y.PAIS, R.PAIS.REME, F.TABLE.NAC, ERR.COU)
        		Y.NOMBRE.PAIS := R.PAIS.REME<EB.COU.COUNTRY.NAME><1,4>
        		IF Y.COUN.PAI GT 1 AND I NE Y.COUN.PAI THEN
        			Y.NOMBRE.PAIS :=", "
        		END
        	NEXT I
        	REMESA.PAI = Y.NOMBRE.PAIS
        END

********************************************************************************************************
        * Recuperacion de campos banca en linea para clientes
        *        CALL GET.LOC.REF('CUSTOMER', 'LF.REA.TRANS', POS)
        *    	TRANS = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
********************************************************************************************************
    END
;* es una PERSONA JURIDICA
    ELSE
;* Encontrando la razon social X
;*CALL GET.LOC.REF('CUSTOMER', 'LF.RAZON.SOCIAL', POS)
    RAZON.SOCIAL = SWAP(R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LRAZON.SOCIAL>, @SM, ' ')

;* Nombre comercial X
    NOMBRE.COMERCIAL = SS.SHORT.NAME

;* Encontrando fecha de constitucion
    FECHA.CONSTITUCION = R.CUSTOMER<EB.CUS.BIRTH.INCORP.DATE>
    FECHA.CONSTITUCION = FECHA.CONSTITUCION[7,2] : '/' : FECHA.CONSTITUCION[5,2] : '/' : FECHA.CONSTITUCION[1,4]

;* Encontrando otra informaci�n de la empresa
;*CALL GET.LOC.REF('CUSTOMER', 'LF.NUM.EMPLES', POS)
    NUM.EMPLEADOS = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LNUM.EMPLES>

;*CALL GET.LOC.REF('CUSTOMER', 'LF.NUM.PROV', POS)
    NUM.PROVEEDOR = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LNUM.PROV>

;*CALL GET.LOC.REF('CUSTOMER', 'LF.NUM.SUC', POS)
    NUM.SUCURSAL = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LNUM.SUC>
    END

;* Obtener datos del representante legal (Empresas y menores de edad)
;******************************************************************************************************
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN
        ;*CRT 'PERSONERIA -> ':LS.PERSONERIA
        CALL GET.LOC.REF('CUSTOMER', 'LF.APO.LEGAL', POS)

        IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> EQ '' THEN
            LS.RL.CUSTOMER.ID = SS.CUSTOMER.ID
        END
        ELSE
        LS.RL.CUSTOMER.ID = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END
    END
    ELSE
;*CRT 'PERSONERIA -> ':LS.PERSONERIA
    CALL GET.LOC.REF('CUSTOMER', 'LF.LEGAL.REP', POS)
    LS.RL.CUSTOMER.ID = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END

;*CRT 'LS.RL.CUSTOMER.ID -->':LS.RL.CUSTOMER.ID:' POS -->':POS

    IF LS.RL.CUSTOMER.ID NE '' THEN
        CALL F.READ(FN.CUSTOMER, LS.RL.CUSTOMER.ID, R.CUSTOMER.RL, F.CUSTOMER, F.ERR.CRL)

        LS.RL.NAME = R.CUSTOMER.RL<EB.CUS.SHORT.NAME>
        ;*CRT 'LS.RL.NAME --> ':LS.RL.NAME

        ;* Determinar si es extranjero
        IF R.CUSTOMER.RL<EB.CUS.NATIONALITY> = 'SV' THEN
            LS.RL.EXTRANJERO = 'NO'
        END
        ELSE
        LS.RL.EXTRANJERO = 'SI'
    END

;* Encontrando ocupaci�n seg�n DUI
    LS.RL.OCUPACION = R.CUSTOMER.RL<EB.CUS.OCCUPATION>
;*CRT 'LS.RL.OCUPACION --> ':LS.RL.OCUPACION

;* Nit del representante legal
    FINDSTR 'NUM.IDEN.TRIBUT' IN R.CUSTOMER.RL<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        LS.NIT.RL = R.CUSTOMER.RL<EB.CUS.LEGAL.ID, VP>
    END ELSE
        LS.NIT.RL = '____________________'
    END

;* Encontrando la actividad econ�mica representante legal
    CALL F.READ(FN.TABLE.IND, R.CUSTOMER<EB.CUS.INDUSTRY>, R.TABLE.IND, F.TABLE.IND, F.ERR.IND)
    DBG.INDUSTRY.IDENT = R.CUSTOMER<EB.CUS.INDUSTRY>

    LS.ACTIVIDAD.ECONO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LOTHER.INDUST>

    CALL GET.LOC.REF ('INDUSTRY', 'LF.ACT.ECON', POS)
    MULTI.ACT.ECON = R.TABLE.IND<EB.IND.LOCAL.REF, POS>
    MULTI.ACT.CONT = DCOUNT(MULTI.ACT.ECON, @SM)

    FOR I = 1 TO MULTI.ACT.CONT
        LS.ACTIVIDAD.ECONO.PJ = LS.ACTIVIDAD.ECONO.PJ : "" : FIELD(MULTI.ACT.ECON, @SM, I)
    NEXT I

;* Recuperando el n�mero y fecha de expedici�n del documento
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.CUSTOMER.RL<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        LS.RL.TIPO.DOCUMENTO = 'DUI'
        LS.RL.NUMERO.DOCUMENTO = R.CUSTOMER.RL<EB.CUS.LEGAL.ID, VP>
    END
    ELSE FINDSTR 'PASSPORT' IN R.CUSTOMER.RL<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    LS.RL.TIPO.DOCUMENTO = 'PASAPORTE'
    LS.RL.NUMERO.DOCUMENTO = R.CUSTOMER.RL<EB.CUS.LEGAL.ID, VP>
    END
    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.CUSTOMER.RL<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    LS.RL.TIPO.DOCUMENTO = 'CARNET DE RESIDENTE'
    LS.RL.NUMERO.DOCUMENTO = R.CUSTOMER.RL<EB.CUS.LEGAL.ID, VP>
    END
    ELSE
    LS.RL.NUMERO.DOCUMENTO = '_______________'
    LS.RL.TIPO.DOCUMENTO = '_______________'
    END

;* Encontrando el domicilio representante legal
    IF R.CUSTOMER.RL<EB.CUS.LOCAL.REF, POS.LDEPARTAMENTO> NE '' THEN
        CODIGO.DEP = R.CUSTOMER.RL<EB.CUS.LOCAL.REF, POS.LDEPARTAMENTO>
    END

    CALL F.READ(FN.TABLE.DEP, CODIGO.DEP, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    LS.RL.DOMICILIO = R.TABLE.DEP<EB.SLV43.DESCRIPTION>

;* Calculando la edad del representante legal

    LS.DATE.OF.BIRTH = R.CUSTOMER.RL<EB.CUS.DATE.OF.BIRTH>
;*CRT 'LS.DATE.OF.BIRTH -->':LS.DATE.OF.BIRTH
    LN.RL.EDAD = SUBSTRINGS(TODAY, 1, 4) - LS.DATE.OF.BIRTH[1, 4] - 1

;* Si el mes de nacimiento ya paso sumar 1
    IF SUBSTRINGS(TODAY, 5, 2) - LS.DATE.OF.BIRTH[5, 2] > 0 THEN
        LN.RL.EDAD = LN.RL.EDAD + 1
    END
    ELSE
;* Si es el mismo mes de nacimiento, sumar 1 si el d�a de nacimiento ya paso
    IF SUBSTRINGS(TODAY, 5, 2) - LS.DATE.OF.BIRTH[5, 2] = 0 THEN
        IF SUBSTRINGS(TODAY, 7, 2) - LS.DATE.OF.BIRTH[7, 2] > 0 THEN
            LN.RL.EDAD = LN.RL.EDAD + 1
        END
    END
    END

;* Encontrando el parentesco del representante legal
    LS.RL.PARENTESCO = ''
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN

        LS.RELATION.CODE = ''
        FINDSTR '70' IN R.CUSTOMER<EB.CUS.RELATION.CODE> SETTING AP, VP THEN	;* Padre
            LS.RELATION.CODE = '70'
        END
        ELSE FINDSTR '72' IN R.CUSTOMER<EB.CUS.RELATION.CODE> SETTING AP, VP THEN	;* Madre
        LS.RELATION.CODE = '72'
    END
    ELSE FINDSTR '74' IN R.CUSTOMER<EB.CUS.RELATION.CODE> SETTING AP, VP THEN	;* Tutor
    LS.RELATION.CODE = '74'
    END

    IF LS.RELATION.CODE NE '' THEN
        CALL F.READ(FN.RELATION, LS.RELATION.CODE, R.RELATION, F.RELATION, F.ERR.REL)
        LS.RL.PARENTESCO = R.RELATION<EB.REL.DESCRIPTION>
    END
    ELSE
    LS.RL.PARENTESCO = '____________________'
    END
    END

;* Encontrando el periodo del representante legal
;* El campo TEXT se ocupa como periodo de nombramiento en cliente JURIDICO, para NATURAL se usa como parte del nombre.
;* es una persona juridica
    ELSE
    LS.RL.PERIODO.NOMBRAMIENTO = TRIM(R.CUSTOMER<EB.CUS.TEXT>)

;* Encontrando credencial de rl
    FINDSTR 'CREDENCIAL' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        LS.RL.CREDENCIAL = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
    END ELSE
        LS.RL.CREDENCIAL = '____________________'
    END

;* Encontrando fecha de modificaci�n al pacto social
    FINDSTR 'MODIF.PACTO.SOCIAL' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        LS.FECHA.MOD.PACTO = R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>[7,2] : '/' : R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>[5,2] : '/' : R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>[1,4]
    END ELSE
        LS.FECHA.MOD.PACTO = '____________________'
    END

;* Encontrando num. inscripci�n y fecha de inscripci�n
    FINDSTR 'FECHA.INSCR' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        LS.NUM.INSCR =  R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
        LS.FECHA.INSCR = R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>[7,2] : '/' : R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>[5,2] : '/' : R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>[1,4]
    END ELSE
        LS.NUM.INSCR = '____________________'
        LS.FECHA.INSCR = '____________________'
    END

;* Encontrando folio y libro de inscripci�n
    FINDSTR 'NUM.FOLIO' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        LS.FOLIO = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
    END ELSE
        LS.FOLIO = '_______'
    END

;* Encontrando el n�mero de NIT
    FINDSTR 'NUM.LIBRO' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        LS.LIBRO = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
    END ELSE
        LS.LIBRO = '_______'
    END
    LS.NUM.FOLIO.LIBRO = 'FOLIO ' : LS.FOLIO : ', LIBRO ' : LS.LIBRO
    END
    END

;* Recuperando el n�mero y fecha de expedici�n del documento
    FINDSTR 'DOCTO.UNICO.IDENT' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NUMERO.DOC = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
        LUGAR.DOC = R.CUSTOMER<EB.CUS.LEGAL.ISS.AUTH, VP>
        DOC.INICIO = R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>
        DOC.INICIO = DOC.INICIO[7,2] : '/' : DOC.INICIO[5,2] : '/' : DOC.INICIO[1,4]
        DOC.FIN = R.CUSTOMER<EB.CUS.LEGAL.EXP.DATE, VP>
        DOC.FIN = DOC.FIN[7,2] : '/' : DOC.FIN[5,2] : '/' : DOC.FIN[1,4]
        TIPO.DOCUMENTO = 'DUI'
    END
    ELSE FINDSTR 'PASSPORT' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    NUMERO.DOC = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
    LUGAR.DOC = R.CUSTOMER<EB.CUS.LEGAL.ISS.AUTH, VP>
    DOC.INICIO = R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>
    DOC.INICIO = DOC.INICIO[7,2] : '/' : DOC.INICIO[5,2] : '/' : DOC.INICIO[1,4]
    DOC.FIN = R.CUSTOMER<EB.CUS.LEGAL.EXP.DATE, VP>
    DOC.FIN = DOC.FIN[7,2] : '/' : DOC.FIN[5,2] : '/' : DOC.FIN[1,4]
    TIPO.DOCUMENTO = 'PASAPORTE'
    END
    ELSE FINDSTR 'CARNET.RESIDENTE' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
    NUMERO.DOC = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
    LUGAR.DOC = R.CUSTOMER<EB.CUS.LEGAL.ISS.AUTH, VP>
    DOC.INICIO = R.CUSTOMER<EB.CUS.LEGAL.ISS.DATE, VP>
    DOC.INICIO = DOC.INICIO[7,2] : '/' : DOC.INICIO[5,2] : '/' : DOC.INICIO[1,4]
    DOC.FIN = R.CUSTOMER<EB.CUS.LEGAL.EXP.DATE, VP>
    DOC.FIN = DOC.FIN[7,2] : '/' : DOC.FIN[5,2] : '/' : DOC.FIN[1,4]
    TIPO.DOCUMENTO = 'CARNET RESIDENTE'
    END
    ELSE
    NUMERO.DOC = '_______________'
    LUGAR.DOC = '_______________'
    DOC.INICIO = '_______________'
    DOC.FIN = '_______________'
    TIPO.DOCUMENTO = '_______________'
    END

;* Encontrando el n�mero de NIT
    FINDSTR 'NUM.IDEN.TRIBUT' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NUMERO.NIT = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
    END ELSE
        NUMERO.NIT = '______________'
    END

;* Encontrando el nombre seg�n NIT
;*CALL GET.LOC.REF('CUSTOMER', 'LF.NOB.NIT', POS)
    NOMBRE.NIT = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LNOB.NIT>

;* Encontrando la nacionalidad
    CALL F.READ(FN.TABLE.NAC, R.CUSTOMER<EB.CUS.NATIONALITY>, R.TABLE.NAC, F.TABLE.NAC, F.ERR.NAC)
    NACIONALIDAD = R.TABLE.NAC<EB.COU.COUNTRY.NAME><1,2>

;* Encontrando pa�s de residencia
    CALL F.READ(FN.TABLE.NAC, R.CUSTOMER<EB.CUS.RESIDENCE>, R.TABLE.NAC, F.TABLE.NAC, F.ERR.NAC)
    PAIS.RESIDENCIA = R.TABLE.NAC<EB.COU.COUNTRY.NAME><1,2>

;* Lugar, fecha de nacimiento, edad del cliente
;******************************************************************************************************
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN
        ;* Lugar y fecha de nacimiento
        ;*CALL GET.LOC.REF('CUSTOMER', 'LF.LUG.NAC', POS)
        LS.LUGAR.NAC = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LLUG.NAC>

        LS.CUS.DATE.OF.BIRTH = R.CUSTOMER<EB.CUS.DATE.OF.BIRTH>
        LS.FECHA.NAC = LS.CUS.DATE.OF.BIRTH[7,2] : '/' : LS.CUS.DATE.OF.BIRTH[5,2] : '/' : LS.CUS.DATE.OF.BIRTH[1,4]

        ;* Calculando la edad del cliente principal
        LN.EDAD.CL = SUBSTRINGS(TODAY, 1, 4) - LS.CUS.DATE.OF.BIRTH[1, 4] - 1

        ;* Si el mes de nacimiento ya paso sumar 1
        IF SUBSTRINGS(TODAY, 5, 2) - LS.CUS.DATE.OF.BIRTH[5, 2] > 0 THEN
            LN.EDAD.CL = LN.EDAD.CL + 1
        END
        ELSE
        ;* Si es el mismo mes de nacimiento, sumar 1 si el d�a de nacimiento ya paso
        IF SUBSTRINGS(TODAY, 5, 2) - LS.CUS.DATE.OF.BIRTH[5, 2] = 0 THEN
            IF SUBSTRINGS(TODAY, 7, 2) - LS.CUS.DATE.OF.BIRTH[7, 2] > 0 THEN
                LN.EDAD.CL = LN.EDAD.CL + 1
            END
        END
    END

;* Determinando si es menor de edad
    IF LN.EDAD.CL < 18 THEN
        LS.MENOR.EDAD = 'SI'
    END
    ELSE
    LS.MENOR.EDAD = 'NO'
    END
    END
;* persona juridica
    ELSE
    LS.LUGAR.NAC = ''
    LS.FECHA.NAC = ''
    LS.EDAD.CL = ''
    END

;* Encontrando el estado civil
    MARITAL.STATUS = R.CUSTOMER<EB.CUS.MARITAL.STATUS>
    BEGIN CASE
        CASE MARITAL.STATUS EQ 'ACOMPA'
            ESTADO.CIVIL = 'ACOMPA�ADO(A)'
        CASE MARITAL.STATUS EQ 'DIVORCED'
            ESTADO.CIVIL = 'DIVORCIADO(A)'
        CASE MARITAL.STATUS EQ 'MARRIED'
            ESTADO.CIVIL = 'CASADO(A)'
        CASE MARITAL.STATUS EQ 'PARTNER'
            ESTADO.CIVIL = 'ESPOSO(A)'
        CASE MARITAL.STATUS EQ 'SINGLE'
            ESTADO.CIVIL = 'SOLTERO(A)'
        CASE MARITAL.STATUS EQ 'WIDOWED'
            ESTADO.CIVIL = 'VIUDO(A)'
        CASE MARITAL.STATUS EQ 'OTHER'
            ESTADO.CIVIL = 'OTRO'
        CASE 1
            ESTADO.CIVIL = 'N/A'
    END CASE

;* Encontrando la ocupaci�n
;*CALL GET.LOC.REF('CUSTOMER', 'LF.OCUPACION', POS)
    CALL F.READ(FN.TABLE.OCU, R.CUSTOMER<EB.CUS.LOCAL.REF, P.LF.OCUPACION>, R.TABLE.OCU, F.TABLE.OCU, F.ERR.OCU)
    OCUPACION = R.TABLE.OCU<EB.CUS17.DESCRIPTION>

;* Encontrando el g�nero
    IF R.CUSTOMER<EB.CUS.GENDER> EQ 'MALE' THEN
        GENERO = 'MASCULINO'
    END
    ELSE IF R.CUSTOMER<EB.CUS.GENDER> EQ 'FEMALE' THEN
    GENERO = 'FEMENINO'
    END
    ELSE
    GENERO = 'N/A'
    END

;* Encontrando la actividad econ�mica
    CALL F.READ(FN.TABLE.IND, R.CUSTOMER<EB.CUS.INDUSTRY>, R.TABLE.IND, F.TABLE.IND, F.ERR.IND)
    DBG.INDUSTRY.IDENT = R.CUSTOMER<EB.CUS.INDUSTRY>

;* Encontrando el tipo de sector (p�blico/privado)
;*CALL GET.LOC.REF('CUSTOMER', 'LF.TIPO.SECTOR', POS)
    LS.TIPO.SECTOR = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LTIPO.SECTOR>

;* Encontrando el sector econ�mico
    CALL F.READ(FN.TABLE.SEC, R.CUSTOMER<EB.CUS.SECTOR>, R.TABLE.SEC, F.TABLE.SEC, F.ERR.SEC)
    LS.SECTOR.ECONOMICO = R.TABLE.SEC<EB.SEC.DESCRIPTION>

;* Encontrando el n�mero de registro
    FINDSTR 'REGISTRO.FISCAL.IVA' IN R.CUSTOMER<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
        NUMERO.REGISTRO = R.CUSTOMER<EB.CUS.LEGAL.ID, VP>
    END
    ELSE
    NUMERO.REGISTRO = '______________'
    END

;* Encontrando el tipo de contribuyente

;*CALL GET.LOC.REF('CUSTOMER', 'LF.TIPO.CONT', POS)
    TIPO.CONTRIBUYENTE = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LTIPO.CONT>

    BEGIN CASE
        CASE TIPO.CONTRIBUYENTE EQ '01'
            TIPO.CONTRIBUYENTE = 'GRAN CONTRIBUYENTE'
        CASE TIPO.CONTRIBUYENTE EQ '02'
            TIPO.CONTRIBUYENTE = 'MEDIANO CONTRIBUYENTE'
        CASE TIPO.CONTRIBUYENTE EQ '03'
            TIPO.CONTRIBUYENTE = 'PEQUE�O CONTRIBUYENTE'
        CASE TIPO.CONTRIBUYENTE EQ '99'
            TIPO.CONTRIBUYENTE = 'NO CONTRIBUYENTE'
        CASE 1
            TIPO.CONTRIBUYENTE = 'N/A'
    END CASE

;* Encontrando la direcci�n
;******************************************************************************************************
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN
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

    CALL GET.LOC.REF('CUSTOMER', LS.FLD.COLONIA, POS)
    IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
        DIRECCION = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END

    CALL GET.LOC.REF('CUSTOMER', LS.FLD.CALLE, POS)
    IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
        DIRECCION = DIRECCION : ', ' : R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END
    CALL GET.LOC.REF('CUSTOMER', LS.FLD.AVENIDA, POS)
    IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
        DIRECCION = DIRECCION : ', ' : R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END
    DIRECCION = TRIM(SWAP(DIRECCION, ', ,', ', '))

;* Encontrando el departamento
    CALL GET.LOC.REF('CUSTOMER', LS.FLD.DEPARTAMENTO, POS)
    IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
        CODIGO.DEP = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END
    CALL F.READ(FN.TABLE.DEP, CODIGO.DEP, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    DEPARTAMENTO = R.TABLE.DEP<EB.SLV43.DESCRIPTION>

;* Encontrando el municipio
    CALL GET.LOC.REF('CUSTOMER', LS.FLD.MUNICIPIO, POS)
    CODIGO.MUN = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>

    CALL F.READ(FN.TABLE.MUN, CODIGO.MUN, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
    MUNICIPIO = R.TABLE.MUN<EB.SLV33.DESCRIPTION>

;* Encontrando telefono, fax, email
    TELEFONO.FIJO = R.CUSTOMER<EB.CUS.PHONE.1>
    TELEFONO.CELULAR = R.CUSTOMER<EB.CUS.SMS.1>
    TELEFONO.FAX = R.CUSTOMER<EB.CUS.FAX.1>
    TELEFONO.OFICINA = R.CUSTOMER<EB.CUS.OFF.PHONE>
    CORREO.ELECTRONICO = R.CUSTOMER<EB.CUS.EMAIL.1>

;* Encontrando ocupaci�n seg�n DUI
    OCUPACION.DUI = R.CUSTOMER<EB.CUS.OCCUPATION>

;* Encontrando el estado del empleado
    EMPLOYMENT.STATUS = R.CUSTOMER<EB.CUS.EMPLOYMENT.STATUS>

    STR.EMPLOYMENT.ID = 'EMPLOYMENT.STATUS*':EMPLOYMENT.STATUS

    CALL F.READ(FN.TABLE.LOOK, STR.EMPLOYMENT.ID, R.EMP.LOOK, F.TABLE.LOOK, F.ERR.LOOK)
    ESTADO.EMPLEO = R.EMP.LOOK<EB.LU.DESCRIPTION><1,2>

;* Encontrando nombre del funcionario publico socio
    CALL GET.LOC.REF('CUSTOMER', 'LF.NOM.FUNC.SOC', POS)
    LS.NOM.FUNC.SOCIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>

;* Encontrando el nombre de la empresa
    NOMBRE.EMPRESA = R.CUSTOMER<EB.CUS.EMPLOYERS.NAME>

;* Encontrando nombre del negocio
    CALL GET.LOC.REF('CUSTOMER', 'LF.NEG.OTRO', POS)
    LS.NOMBRE.NEGOCIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>

;* Encontrando tipo de negocio
    CALL GET.LOC.REF('CUSTOMER', 'LF.NEG.PROP', POS)
    LS.TIPO.NEGOCIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>

;* Encontrando flujo de efectivo
*	CALL GET.LOC.REF('CUSTOMER', 'LF.MON.EST.TRA', POS)
*	LN.FLUJO.EFECTIVO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    CALL GET.LOC.REF('CUSTOMER', 'LF.FLUJ.MES.APR', POS)
    LN.FLUJO.EFECTIVO = FMT(R.CUSTOMER<EB.CUS.LOCAL.REF, POS>,',')

;* Encontrando la actividad economica de la empresa
    ACTIVIDAD.EMPRESA = R.CUSTOMER<EB.CUS.EMPLOYERS.BUSS>

;* Encontrando el cargo del empleado
    CARGO.EMPLEADO = ''
    CARGO.SINGLE = R.CUSTOMER<EB.CUS.INTERESTS><1,1>
    CARGO.EMPLEADO = CARGO.SINGLE

;* Encontrando la direcci�n de la empresa
    CALL GET.LOC.REF('CUSTOMER', 'LF.COLONIA.2', POS)
    IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
        DIRECCION.EMPRESA = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END

    CALL GET.LOC.REF('CUSTOMER', 'LF.CALLE.2', POS)
    IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
        DIRECCION.EMPRESA := ', ' : R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END

    CALL GET.LOC.REF('CUSTOMER', 'LF.AVENIDA.2', POS)
    IF R.CUSTOMER<EB.CUS.LOCAL.REF, POS> NE '' THEN
        DIRECCION.EMPRESA := ', ' : R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    END
    DIRECCION.EMPRESA = TRIM(SWAP(DIRECCION.EMPRESA, ', ,', ', '))

;* Encontrando el departamento de la empresa
    CALL GET.LOC.REF('CUSTOMER', 'LF.DEPTO.2', POS)
    CODIGO.DEP = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.DEP, CODIGO.DEP, R.TABLE.DEP, F.TABLE.DEP, F.ERR.DEP)
    DEPTO.EMP = R.TABLE.DEP<EB.SLV43.DESCRIPTION>

;* Encontrando el municipio de la empresa
    CALL GET.LOC.REF('CUSTOMER', 'LF.MUNICIPIO.2', POS)
    CODIGO.MUN = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
    CALL F.READ(FN.TABLE.MUN, CODIGO.MUN, R.TABLE.MUN, F.TABLE.MUN, F.ERR.MUN)
    MUNIC.EMP = R.TABLE.MUN<EB.SLV33.DESCRIPTION>

;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN

        ;* Encontrando el salario
        LN.SALARIO = R.CUSTOMER<EB.CUS.SALARY>

        ;* Encontrando otros ingresos
        OTROS.INGRESOS = R.CUSTOMER<EB.CUS.ANNUAL.BONUS>

        ;* Encontrando ingresos mensuales negocio
        CALL GET.LOC.REF('CUSTOMER', 'LF.MON.EST.DEP', POS)
        LN.INGRESOS.MENSUALES.NEGOCIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>

        ;* Encontrando total de intresos
        CALL GET.LOC.REF('CUSTOMER', 'LF.MON.EST.ING', POS)
        LN.TOTAL.INGRESOS = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>

        LN.PN.TIPO.NEGOCIO=R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LF.NEG.PROP>
        LN.PN.ING.MENSUAL=R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LF.MON.EST.ING>
        LN.PN.INI.NEGOCIO=R.CUSTOMER<EB.CUS.EMPLOYMENT.START>
        LN.PN.EFEC.MENSUAL=R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LF.FLUJ.MES.APR>
        LN.PN.EFEC.FLUJO=R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LF.MON.EST.APR>
    END
    ELSE ;* es persona juridica
;* Encontrando la cantidad de dinero a mover anualmente
    LN.MOVIMIENTO.ANUAL = R.CUSTOMER<EB.CUS.NET.MONTHLY.IN>
    END

;* Perioricidad de ingresos
    FRECUENCIA.INGRESO = R.CUSTOMER<EB.CUS.SALARY.DATE.FREQ>
    FINDSTR 'DAILY' IN FRECUENCIA.INGRESO SETTING AP.INGRESO, VP.INGRESO THEN
        FRECUENCIA.INGRESO = 'DIARIO'
    END
    ELSE FINDSTR 'WEEK1' IN FRECUENCIA.INGRESO SETTING AP.INGRESO, VP.INGRESO THEN
    FRECUENCIA.INGRESO = 'SEMANAL'
    END
    ELSE FINDSTR 'WEEK' IN FRECUENCIA.INGRESO SETTING AP.INGRESO, VP.INGRESO THEN
    FRECUENCIA.INGRESO = 'CADA ' : CHANGE(FRECUENCIA.INGRESO, 'WEEK', '') : ' SEMANAS'
    END
    ELSE FINDSTR 'TWMTH' IN FRECUENCIA.INGRESO SETTING AP.INGRESO, VP.INGRESO THEN
    FRECUENCIA.INGRESO = 'QUINCENAL'
    END
    ELSE
    FRECUENCIA.INGRESO = 'MENSUAL'
    END

;* Encontrando el inicio de operaciones
    INICIO.EMP = R.CUSTOMER<EB.CUS.EMPLOYMENT.START>
    IF INICIO.EMP NE '' THEN
        INICIO.EMP = R.CUSTOMER<EB.CUS.EMPLOYMENT.START>
        INICIO.EMP = INICIO.EMP[7,2] : '/' : INICIO.EMP[5,2] : '/' : INICIO.EMP[1,4]
    END

;* Encontrando el tama�o de la empresa
    CALL GET.LOC.REF('CUSTOMER', 'LF.TAMA.CONT', POS)
    TAMANIO.EMP.TEMP=R.CUSTOMER<EB.CUS.LOCAL.REF, POS>

    BEGIN CASE
        CASE TAMANIO.EMP.TEMP EQ 'GE'
            TAMANIO.EMP = 'GRAN EMPRESA'
        CASE TAMANIO.EMP.TEMP EQ 'GO'
            TAMANIO.EMP = 'GOBIERNO'
        CASE TAMANIO.EMP.TEMP EQ 'ME'
            TAMANIO.EMP = 'MEDIANA EMPRESA'
        CASE TAMANIO.EMP.TEMP EQ 'MI'
            TAMANIO.EMP = 'MICRO EMPRESA'
        CASE TAMANIO.EMP.TEMP EQ 'PA'
            TAMANIO.EMP = 'CUENTA PROPIA'
        CASE TAMANIO.EMP.TEMP EQ 'PE'
            TAMANIO.EMP = 'PEQUE�A EMPRESA'
        CASE 1
            TAMANIO.EMP = 'N/A'
    END CASE

;* Encontrando si es funcionario
;*CALL GET.LOC.REF('CUSTOMER', 'LF.FUNC.PUB', POS)
    ES.FUNCIONARIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LFUNC.PUB>

;* Encontrando si es funcionario
;*CALL GET.LOC.REF('CUSTOMER', 'LF.PAR.FUNC.PUB', POS)
    PARIENTE.FUNCIONARIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LPAR.FUNC.PUB>

;* se adicionan los campos Cargo Desempenado y Nombre del Funcionario Publico
;* para la seccion Persona Expuesta Politicamente
;*CALL GET.LOC.REF('CUSTOMER', 'LF.CARGOFUNCION', POS)
*	CARGO.FUNCIONARIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
;* RGARAY - 20151127 - adicionado para dar tratamiento al multivalor del cargo desempeniado del funcionario en le campo LF.CARGOFUNCION
    CARGO.FUNCIONARIO = ''
    MULTI.CARGO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LCARGOFUNCION>
    LOOP
        REMOVE PART.CARGO FROM MULTI.CARGO SETTING POS.CARGO
    WHILE PART.CARGO NE ''
        CARGO.FUNCIONARIO := PART.CARGO : ' '
    REPEAT

;*CALL GET.LOC.REF('CUSTOMER', 'LF.NOM.FUNC.PUB', POS)
    NOMBRE.FUNCIONARIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LNOM.FUNC.PUB>

;* Procedencia de los otros ingresos (LF.PRO.FON.N o LF.PRO.FON.J)
    PROCEDENCIA.OTROS = ''
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN

        ;*CALL GET.LOC.REF('CUSTOMER', 'LF.PRO.FON.N', POS)
        ;*PROCEDENCIA.OTROS = R.CUSTOMER<EB.CUS.LOCAL.REF, POS>
        MULTI.FONDOS.OTROS = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LPRO.FON.N>
        ;*CRT 'MULTI.FONDOS.OTROS -> ' : MULTI.FONDOS.OTROS
        ;* dado que LF.PRO.FON.N es un listado de valores
        ;* se obtiene el multivalor y se recorre para sus elementos
        LOOP
            REMOVE ID.FOND.OTROS FROM MULTI.FONDOS.OTROS SETTING POS.OTROS
        WHILE ID.FOND.OTROS NE ''
            ;*CRT 'ID.FOND.OTROS -> ' : ID.FOND.OTROS
            PROCEDENCIA.OTROS := ID.FOND.OTROS : ' '
        REPEAT
    END
    ELSE
;*CALL GET.LOC.REF('CUSTOMER', 'LF.PRO.FON.J', POS)
    PROCEDENCIA.OTROS = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LPRO.FON.J>
    END

;* es una PERSONA juridica
    IF LS.PERSONERIA NE '1' THEN
        ;* Informaci�n de los socios
        ;*CALL GET.LOC.REF('CUSTOMER', 'LF.FULL.NAME', POS)
        NOMBRE.SOCIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LFULL.NAME>

        ;*CALL GET.LOC.REF('CUSTOMER', 'LF.NIT.TAX', POS)
        TAX.SOCIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LNIT.TAX>

        ;*CALL GET.LOC.REF('CUSTOMER', 'LF.AML.PER', POS)
        PORCENTAJE.SOCIO = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LAML.PER>

        ;*----------------------------------------------------------------------------------------
        ;*FATCA
        ;*Author: Ronald Ortiz
        ;*Date: 20161220
        CALL GET.LOC.REF('CUSTOMER','LF.CUS.FIN',POS.TAX.ID.USA)
        TAXID.SOCIOS = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.TAX.ID.USA>

        ;* Informaci�n de junta directiva
        ;*CALL GET.LOC.REF('CUSTOMER', 'LF.CARGO.JUNTA', POS)
        CARGO.JUNTA =  R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LCARGO.JUNTA>

        ;*CALL GET.LOC.REF('CUSTOMER', 'LF.JUN.TXT', POS)
        NOMBRE.JUNTA = R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LJUN.TXT>
    END

;* Obtener propiedad ACCOUNT, seg�n el producto tienen diferente nombre
;******************************************************************************************************
    IF SN.GENERA.DJ EQ 1 THEN
        BEGIN CASE
            CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'ACCOUNTS'
                LS.PROPERTY.ACC = 'BALANCE'
            CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS'
                LS.PROPERTY.ACC = 'ACCOUNT'
            CASE R.AA.ARRANGEMENT<AA.ARR.PRODUCT.LINE> EQ 'LENDING'
                LS.PROPERTY.ACC = 'ACCOUNT'
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
            ;* Encontrando el monto del dep�sito
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
    END

;*-----------------------------------------------------------------------------------
;* Obtener Datos Banca en Linea para Customer   --- 16-06-16
;*-----------------------------------------------------------------------------------
;* Leer campo local realiza transacciones
;* CALL GET.LOC.REF('CUSTOMER', 'LF.REA.TRANS', POS)
;*REALIZA.TRANS =  R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LREA.TRANS>

;* Leer campo local numero transacciones
;* CALL GET.LOC.REF('CUSTOMER', 'LF.N.TRANS', POS)
;*NUM.TRANS =  R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LN.TRANS>

;* Leer campo local monto mensual
;* CALL GET.LOC.REF('CUSTOMER', 'LF.MONTO.MNTH', POS)
;*MONTO.MENSUAL =  R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LMONTO.MNTH>

;* Leer campo local motivo
;* CALL GET.LOC.REF('CUSTOMER', 'LF.MOTIVO.TRANS', POS)
;*MOTIVO =  R.CUSTOMER<EB.CUS.LOCAL.REF, POS.LMOTIVO.TRANS>
;*------------------------------------------------------------------------------------
;* Obtener cuentas y transaccionalidad por cada producto.
;******************************************************************************************************
    DJ.TEXT=''
    DJ.TEXT<-1>=SS.CUSTOMER.ID
    DJ.TEXT<-1>=NOMBRE.NIT
    DJ.TEXT<-1>=LS.ACTIVIDAD.ECONO

    IF LS.PERSONERIA EQ '1' THEN
        DJ.TEXT<-1>=LN.EDAD.CL
    END
    ELSE
    DJ.TEXT<-1>=LN.RL.EDAD
    END

    DJ.TEXT<-1>=LS.RL.OCUPACION
    DJ.TEXT<-1>=LS.RL.DOMICILIO
    DJ.TEXT<-1>=LS.RL.TIPO.DOCUMENTO
    DJ.TEXT<-1>=LS.RL.NUMERO.DOCUMENTO
    DJ.TEXT<-1>=LS.DJ.PROC.FON
    DJ.TEXT<-1>=LS.LUGAR.EMISION
    DJ.TEXT<-1>=TODAY[7,2]
    DJ.TEXT<-1>=MES.EMISION
    DJ.TEXT<-1>=TODAY[1,4]
    DJ.TEXT<-1>=LS.MENOR.EDAD
    DJ.TEXT<-1>=LS.PERSONERIA
    DJ.TEXT<-1>=LS.NIT.RL
    DJ.TEXT<-1>=LS.RL.NAME
    DJ.TEXT<-1>=RAZON.SOCIAL
    DJ.TEXT<-1>=NOMBRE.COMERCIAL
    DJ.TEXT<-1>=NUMERO.NIT
    DJ.TEXT<-1>=LS.ACTIVIDAD.ECONO.PJ
    DJ.TEXT<-1>=ESTADO.EMPLEO
    DJ.TEXT<-1>=SS.SHORT.NAME
    DJ.TEXT<-1>='Declaracion Jurada'

    GOSUB PROCESS.PRODUCTOS

;* Armando la cadena de datos
;******************************************************************************************************
    DATOS = ''

    DATOS := 'Fecha=' : FECHA.EMISION : ';'
    DATOS := 'NumCliente=' : SS.CUSTOMER.ID : ';'
    DATOS := 'CUENTATI=' : SS.TIPO.CUENTA : ';'
    DATOS := 'CUENTANO=' : SS.ACCOUNT.ID : ';'
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN
        DATOS := 'TIPOCLIE=NAT' : ';'
    END
;* persona juridica
    ELSE
    DATOS := 'TIPOCLIE=JUR' : ';'
    END
;* es una PERSONA NATURAL
    IF LS.PERSONERIA EQ '1' THEN
        DATOS := 'PrimerNombre=' : PRIMER.NOMBRE : ';'
        DATOS := 'SegundoNombre=' : SEGUNDO.NOMBRE : ';'
        DATOS := 'TercerNombre=' : TERCER.NOMBRE : ';'
        DATOS := 'PrimerApellido=' : PRIMER.APELLIDO : ';'
        DATOS := 'SegundoApellido=' : SEGUNDO.APELLIDO : ';'
        DATOS := 'ApellidoCasada=' : APELLIDO.CASADA : ';'
        DATOS := 'ConocidoPor=' : CONOCIDO.POR : ';'
        DATOS := 'TipoDoc=' : TIPO.DOCUMENTO : ';'
        DATOS := 'NumDoc=' : NUMERO.DOC : ';'
        DATOS := 'FechaVtoDoc=' : DOC.FIN : ';'
        DATOS := 'LugarFechaDoc=' : LUGAR.DOC : ', ' : DOC.INICIO : ';'
        DATOS := 'OcupacionDui=' : OCUPACION.DUI : ';'
        DATOS := 'NombreNit=' : NOMBRE.NIT : ';'
        DATOS := 'NumNit=' : NUMERO.NIT : ';'
        DATOS := 'Nacionalidad=' : NACIONALIDAD : ';'
        DATOS := 'PaisResidente=' : PAIS.RESIDENCIA : ';'
        DATOS := 'LugarFechaNac=' : LS.LUGAR.NAC : ', ' : LS.FECHA.NAC : ';'
        DATOS := 'EstadoCivil=' : ESTADO.CIVIL : ';'
        DATOS := 'Profesion=' : OCUPACION : ';'
        DATOS := 'Sexo=' : GENERO : ';'

        DATOS := 'ProcedenciaOtrosIngresos=' : PROCEDENCIA.OTROS : ';'
        DATOS := 'ActividadEconomica=' : LS.ACTIVIDAD.ECONO : ';'
        DATOS := 'IngresosMesNegocio=' : TRIM(FMT(LN.INGRESOS.MENSUALES.NEGOCIO, 'R2,#15')) : ';'
        DATOS := 'OtrosIngresos=' : TRIM(FMT(OTROS.INGRESOS, 'R2,#15')) : ';'
        DATOS := 'PERIOTRO=' : FRECUENCIA.INGRESO : ';'
        DATOS := 'NumNrc=' : NUMERO.REGISTRO : ';'
        DATOS := 'TipoContribuyente=' : TIPO.CONTRIBUYENTE : ';'
        DATOS := 'Direccion=' : DIRECCION : ';'
        DATOS := 'Departamento=' : DEPARTAMENTO : ';'
        DATOS := 'Municipio=' : MUNICIPIO : ';'
        DATOS := 'TelefonoRes=' : TELEFONO.FIJO : ';'
        DATOS := 'TelefonoCel=' : TELEFONO.CELULAR : ';'
        DATOS := 'Email=' : CORREO.ELECTRONICO : ';'
        DATOS := 'ESTAEMPL=' : ESTADO.EMPLEO : ';'
        DATOS := 'TipoNegocio=' : LS.TIPO.NEGOCIO : ';'
        DATOS := 'FlujoEfectivo=' : LN.FLUJO.EFECTIVO : ';'
        DATOS := 'EmpresaLabora=' : NOMBRE.EMPRESA : ';'
        DATOS := 'NombreNegocio=' : LS.NOMBRE.NEGOCIO : ';'
        DATOS := 'DireccionNegocio=' : DIRECCION.EMPRESA : ';'
        DATOS := 'Cargo=' : CARGO.EMPLEADO : ';'
        DATOS := 'DepTrabajo=' : DEPTO.EMP : ';'
        DATOS := 'MunTrabajo=' : MUNIC.EMP : ';'
        DATOS := 'TelTrabajo=' : TELEFONO.OFICINA : ';'
        DATOS := 'SALARIOE=' : TRIM(FMT(LN.SALARIO, 'R2,#15')) : ';'
        DATOS := 'INICIOEM=' : INICIO.EMP : ';'
        DATOS := 'TOTALING=' : TRIM(FMT(LN.TOTAL.INGRESOS, 'R2,#15')) : ';'
        DATOS := 'TAMAEMPR=' : TAMANIO.EMP : ';'
        DATOS := 'ESFUNCIO=' : ES.FUNCIONARIO : ';'
        DATOS := 'PARIFUNC=' : PARIENTE.FUNCIONARIO : ';'
        DATOS := 'CARGDESE=' : CARGO.FUNCIONARIO : ';'
        DATOS := 'NOMBFUNC=' : NOMBRE.FUNCIONARIO : ';'


        DATOS := 'TipoNegocio=':LN.PN.TIPO.NEGOCIO : ';'
        DATOS := 'IngresosMensuales=':LN.PN.ING.MENSUAL : ';'
        DATOS := 'FechaInicioNegTrab=' :LN.PN.INI.NEGOCIO[7,2] : '/' : LN.PN.INI.NEGOCIO[5,2] : '/' : LN.PN.INI.NEGOCIO[1,4]: ';'
        DATOS := 'FlujoEfectivo=':LN.PN.EFEC.FLUJO: ';'
        DATOS := 'IngresosMesNegocio=':LN.PN.EFEC.MENSUAL: ';'
        DATOS := 'OtrosIngresos=':OTROS.INGRESOS: ';'
        DATOS := 'FrecuenciaOtrosIngresos=':FRECUENCIA.INGRESO: ';'
        DATOS := 'Ingresos=':LN.SALARIO: ';'
        DATOS := 'FuenteIngresos=':ESTADO.EMPLEO: ';'

        ;*--------------------------------------
        ;*FATCA
        GOSUB FATCA.GET.VALUES.NATURAL
        DATOS := 'FATCACIUDSI=':FATCACIUDSI:';'
        DATOS := 'FATCACIUDNO=':FATCACIUDNO:';'
        DATOS := 'TINUSA=':TIN.USA:';'
        DATOS := 'FATCARESDSI=':FATCARESDSI:';'
        DATOS := 'FATCARESDNO=':FATCARESDNO:';'
        DATOS := 'FATCASSN=':SSN.USA:';'
        DATOS := 'FATCANACSI=':FATCANACSI:';'
        DATOS := 'FATCANACNO=':FATCANACNO:';'
        DATOS := 'FATCATAXID=':SSN.USA:';'
        DATOS := 'FATCACONTRISI=':FATCACONTRISI:';'
        DATOS := 'FATCACONTRINO=':FATCACONTRINO:';'
        DATOS := 'FATCAOTHERNACISI=':FATCAOTHERNACISI:';'
        DATOS := 'FATCAOTHERNACINO=':FATCAOTHERNACINO:';'
        DATOS := 'FATCAPAIS=':VAL.NAC:';'
        DATOS := 'FATCADIRSI=':FATCADIRSI:';'
        DATOS := 'FATCADIRNO=':FATCADIRNO:';'
        DATOS := 'FATCAPHONESI=':FATCAPHONESI:';'
        DATOS := 'FATCAPHONENO=':FATCAPHONENO:';'
        DATOS := 'FATCAPODERSI=':FATCAPODERSI:';'
        DATOS := 'FATCAPODERNO=':FATCAPODERNO:';'

        ;*--------------------------------------
        ;*Remesa Familiar
        DATOS := 'REMESACTIVA=':REMESA.ACT:';'
        ;*Jonas       14.05.18    Eliminar espacio en cadena de texto.
        DATOS := 'REMESAPAIS=':REMESA.PAI:';'
        DATOS := 'REMESAMONTO=':REMESA.MON:';'
        DATOS := 'REMESAMOTIV=':REMESA.MOT:';'
    END
    ELSE
;*     Bloque para personas jur�dicas
*-----------------------------------------------------------------------------
    DATOS := 'RAZONSOC=' : RAZON.SOCIAL:';'
    DATOS := 'NOMBCOME=' : NOMBRE.COMERCIAL : ';'
    DATOS := 'NACIONAL=' : NACIONALIDAD : ';'
    DATOS := 'FECHCONS=' : FECHA.CONSTITUCION : ';'
    DATOS := 'NUMERNIT=' : NUMERO.NIT : ';'
    DATOS := 'NOMBRNIT=' : NOMBRE.NIT : ';'
    DATOS := 'NUMEREGI=' : NUMERO.REGISTRO : ';'
    DATOS := 'TIPOCONT=' : TIPO.CONTRIBUYENTE : ';'
    DATOS := 'TIPOSECT=' : LS.TIPO.SECTOR  : ';'
    DATOS := 'SECTINST=' : LS.SECTOR.ECONOMICO : ';'
    DATOS := 'FECHPACT=' : LS.FECHA.MOD.PACTO:';'
    DATOS := 'NUMINSCR=' : LS.NUM.INSCR :';'
    DATOS := 'FECHINSC=' : LS.FECHA.INSCR : ';'
    DATOS := 'NUMFOLIO=' : LS.NUM.FOLIO.LIBRO : ';'
    DATOS := 'ACTIECON=' : LS.ACTIVIDAD.ECONO : ';'
    DATOS := 'MOVANUAL=' : TRIM(FMT(LN.MOVIMIENTO.ANUAL, 'R2,#15')) : ';'
    DATOS := 'PROCFOND=' : PROCEDENCIA.OTROS : ';'
    DATOS := 'TELEFON1=' : TELEFONO.FIJO : ';'
    DATOS := 'TELEFON2=' : TELEFONO.OFICINA : ';'
    DATOS := 'NUMERFAX=' : TELEFONO.FAX : ';'
    DATOS := 'CORREOEL=' : CORREO.ELECTRONICO : ';'
    DATOS := 'DIRECCIO=' : DIRECCION : ';'
    DATOS := 'DEPARTAM=' : DEPARTAMENTO : ';'
    DATOS := 'MUNICIPI=' : MUNICIPIO : ';'
    DATOS := 'NUMEMPLE=' : NUM.EMPLEADOS : ';'
    DATOS := 'NUMPROVE=' : NUM.PROVEEDOR : ';'
    DATOS := 'NUMSUCUR=' : NUM.SUCURSAL : ';'
    DATOS := 'ESFUNCIO=' : ES.FUNCIONARIO : ';'
    DATOS := 'PARIFUNC=' : PARIENTE.FUNCIONARIO : ';'
    DATOS := 'CARGDESE=' : CARGO.FUNCIONARIO : ';'
    DATOS := 'NOMFUNSO=' : LS.NOM.FUNC.SOCIO : ';'
    DATOS := 'FLUJOEFE=' : LN.FLUJO.EFECTIVO : ';'
    DATOS := 'NOMBFUNC=' : NOMBRE.FUNCIONARIO : ';'
    DATOS := 'CREDENRL=' : LS.RL.CREDENCIAL : ';'
    DATOS := 'PERIONOM=' : LS.RL.PERIODO.NOMBRAMIENTO : ';'
    DATOS := 'ReTipoDoc=' : LS.RL.TIPO.DOCUMENTO : ';'
    DATOS := 'ReNumDocumento=' : LS.RL.NUMERO.DOCUMENTO : ';'
    DATOS := 'ReNombre=' : LS.RL.NAME: ';'

;* Obteniendo socios
    NMCOUNT=DCOUNT(NOMBRE.SOCIO,SM)
    I = 1
    LOOP
        REMOVE SOC.ID FROM NOMBRE.SOCIO SETTING POS
    WHILE SOC.ID NE ''
        IF I LE NMCOUNT THEN
            ;*SOC.DETALLE:= "TblSocios":SOC.ID:"=":PORCENTAJE.SOCIO<1,1,I>:"*":TAX.SOCIO<1,1,I>:"*_______________":';'
            SOC.DETALLE:= "TblSocios":I:"=":NOMBRE.SOCIO<1,1,I>:"*":PORCENTAJE.SOCIO<1,1,I>:"*":TAX.SOCIO<1,1,I>:'*':TAXID.SOCIOS<1,1,I>:';'

        END
        I= I+1
    REPEAT

;* Obteniendo junta directiva
    LOCATE 'PRESIDENTE' IN CARGO.JUNTA<1,1,1> SETTING LN.POS ELSE LN.POS = ''
    IF LN.POS NE '' THEN
        ;*DATOS := "JDIRECTI!" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
        DATOS := "PRESIDENTE=" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
    END ELSE
        DATOS := 'PRESIDENTE= ;'
    END

    LOCATE 'VICEPRESIDENTE' IN CARGO.JUNTA<1,1,1> SETTING LN.POS ELSE LN.POS = ''
    IF LN.POS NE '' THEN
        ;*DATOS := "JDIRECTI!" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
        DATOS := "VICEPRESIDENTE=" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
    END ELSE
        DATOS := 'VICEPRESIDENTE= ;'
    END

    LOCATE 'SECRETARIO' IN CARGO.JUNTA<1,1,1> SETTING LN.POS ELSE LN.POS = ''
    IF LN.POS NE '' THEN
        ;*DATOS := "JDIRECTI!" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
        DATOS := "SECRETARIO=" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
    END ELSE
        DATOS := 'SECRETARIO= ;'
    END

    LOCATE 'DIRECTOR.PROPIETARIO' IN CARGO.JUNTA<1,1,1> SETTING LN.POS ELSE LN.POS = ''
    IF LN.POS NE '' THEN
        ;*DATOS := "JDIRECTI!" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
        DATOS := "DIRECTOR.PROPIETARIO=" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
    END ELSE
        DATOS := 'DIRECTOR.PROPIETARIO= ;'
    END

    LOCATE 'TESORERO' IN CARGO.JUNTA<1,1,1> SETTING LN.POS ELSE LN.POS = ''
    IF LN.POS NE '' THEN
        ;*DATOS := "JDIRECTI!" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
        DATOS := "TESORERO=" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
    END ELSE
        DATOS := 'TESORERO= ;'
    END

    LOCATE 'APODERADO' IN CARGO.JUNTA<1,1,1> SETTING LN.POS ELSE LN.POS = ''
    IF LN.POS NE '' THEN
        ;*DATOS := "JDIRECTI!" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
        DATOS := "APODERADO=" : NOMBRE.JUNTA<1,1,LN.POS> : ';'
    END ELSE
        DATOS := 'APODERADO= ;'
    END
    END

    DATOS := 'DJEDADCL=' : LN.EDAD.CL : ';'
    DATOS := 'DJNOMBRL=' : LS.RL.NAME : ';'
    DATOS := 'DJEXTRRL=' : LS.RL.EXTRANJERO : ';'
    DATOS := 'DJPARERL=' : LS.RL.PARENTESCO : ';'
    DATOS := 'DJEDADRL=' : LN.RL.EDAD : ';'
    DATOS := 'DJPROFRL=' : LS.RL.OCUPACION : ';'
    DATOS := 'DJDOMIRL=' : LS.RL.DOMICILIO : ';'
    DATOS := 'DJTIPDRL=' : LS.RL.TIPO.DOCUMENTO : ';'
    DATOS := 'DJNUMDRL=' : LS.RL.NUMERO.DOCUMENTO : ';'
    DATOS := 'DJNITTRL=' : LS.NIT.RL : ';'
    DATOS := 'DJACTIRL=' : LS.RL.ACTIVIDAD.ECONOMICA : ';'
    DATOS := 'DJORIGEN=' : LS.DJ.PROC.FON  : ';'
    DATOS := 'DJDEPOSI=' : TRIM(FMT(LN.DJ.MONTO.DEPOSITO, 'R2,#15')) : ';'
    DATOS := 'DJRETIRO=' : TRIM(FMT(LN.DJ.MONTO.RETIRO, 'R2,#15')) : ';'
    DATOS := 'DJAPERTU=' : TRIM(FMT(LN.DJ.MONTO.APERTURA, 'R2,#15')) : ';'
    DATOS := 'DJCIUDAD=' : LS.LUGAR.EMISION : ';'
    DATOS := 'DJDIADDD=' : TODAY[7,2] : ';'
    DATOS := 'DJMESMMM=' : MES.EMISION : ';'
    DATOS := 'DJANNOAA=' : TODAY[1,4] : ';'
    DATOS := 'DJMENORE=' : LS.MENOR.EDAD : ';'

;* agregar transaccionalidad, la variable SS.CUENTAST ya es de l�neas compuestas y ya lleva el marcador CUENTAST y el CHAR(10) al final
    IF SS.CUENTAST NE '' THEN
        DATOS := SS.CUENTAST
    END ELSE
        DATOS := "TblCuentas= ;"
    END

    IF SS.PRESTAMT NE '' THEN
        DATOS := SS.PRESTAMT
    END ELSE
        DATOS := "TblPrestamos= ;"
    END
    IF SOC.DETALLE NE '' THEN
        DATOS := SOC.DETALLE
    END ELSE
        DATOS := "TblSocios= ;"
    END

;* Informaci�n del ejecutivo
    DATOS = DATOS : 'AGENCIAS=' : LS.LUGAR.EMISION : ';'
    DATOS = DATOS : 'USUARIOS=' : R.USER<EB.USE.USER.NAME> : ';'

;*CRT "TIPO -> ":SS.TIPO.CUENTA
;*CRT "CUENTANO ->":SS.ACCOUNT.ID

    BEGIN CASE
        CASE SS.TIPO.CUENTA EQ 'TIPO-CUENTA'
            SS.TIPO.CUENTA.ENCABEZADO= 'N�MERO DE CUENTA'
            SS.TIPO.CUENTA.TEXTO = ' '
            SS.TIPO.CUENTA.DETALLE = 'y los movimientos proyectados mensualmente de dep�sito son: US$ ':TRIM(FMT(LN.DJ.MONTO.DEPOSITO, 'R2,#15'))
            SS.TIPO.CUENTA.DETALLE :=', y retiros de: US$ ':TRIM(FMT(LN.DJ.MONTO.RETIRO, 'R2,#15'))

        CASE SS.TIPO.CUENTA EQ 'TIPO-PRESTAMO'
            SS.TIPO.CUENTA.ENCABEZADO= 'N�MERO DE PR�STAMO'
            SS.TIPO.CUENTA.LEYENDA = ' para pagar el pr�stamo '
            SS.TIPO.CUENTA.DETALLE = 'y pagar� la cantidad mensual de: US$ ':TRIM(FMT(LN.DJ.MONTO.DEPOSITO, 'R2,#15')):' y adicionalmente a mi cuota '
            SS.TIPO.CUENTA.DETALLE :='establecida pretendo realizar pagos anticipados por monto de: US$ ':

        CASE SS.TIPO.CUENTA EQ 'TIPO-DEPOSITO'
            SS.TIPO.CUENTA.ENCABEZADO= 'N�MERO DE DEP�SITO A PLAZO'
            SS.TIPO.CUENTA.TEXT = ' para la apertura del dep�sito a plazo '
            SS.TIPO.CUENTA.DETALLE = ', el monto de la apertura del dep�sito de plazo ser� de : US$ ':TRIM(FMT(LN.DJ.MONTO.APERTURA, 'R2,#15'))
    END CASE

;*Datos para Banca en Linea : OCORNEJO 22.02.2016
;*-----------------------------------------------
;* se cambiaron por los cambios de Customer
*    DATOS := 'TCIBTRAN!' : USA.TCIB : CHAR(10)
*    DATOS := 'TCIBAMNT!' : TRIM(FMT(MONTO.TCIB, 'R2,#15')) : CHAR(10)
*    DATOS := 'TCIBMTVO!' : MOTIVO.TCIB : CHAR(10)

;*----------------------------------------------------------------------------------------
;*DATOS BANCA EN LINEA  PARA CUSTOMER  16-05-16
;*----------------------------------------------------------------------------------------
    DATOS := 'DJCTEncabezado=' : SS.TIPO.CUENTA.ENCABEZADO : ';'
    DATOS := 'DJCTTexto=' : SS.TIPO.CUENTA.TEXTO: ';'
    DATOS := 'DJCTDetalle=' : SS.TIPO.CUENTA.DETALLE: ';'

;*DATOS := 'TcibTransf=' : REALIZA.TRANS : ';'
;*DATOS := 'TcibNumero=' : NUM.TRANS : ';'
;*DATOS := 'TcibMonto=' : TRIM(FMT(MONTO.MENSUAL, 'R2,#15')) : ';'
;*DATOS := 'TcibMotivo=' : MOTIVO : ';'


    DATOS := 'TcibTransf=' : USA.TCIB : ';'
    DATOS := 'TcibNumero= ;'
    DATOS := 'TcibMonto=' : TRIM(FMT(MONTO.TCIB, 'R2,#15')) : ';'
    DATOS := 'TcibMotivo=' : MOTIVO.TCIB : ';'


    DATOS := 'Funcionario=' : ES.FUNCIONARIO : ';'
    DATOS := 'CargoFuncionario=' : CARGO.FUNCIONARIO : ';'
    DATOS := 'ParienteFuncionario=' : PARIENTE.FUNCIONARIO  : ';'
    DATOS := 'NombreFuncionario=' : NOMBRE.FUNCIONARIO : ';'
    DATOS := 'Ejecutivo=' : R.USER<EB.USE.USER.NAME> : ';'
    DATOS := 'NombreNit=' : NOMBRE.NIT : ';'

;*--------------------------------------------------------------------------------------------------------------------
;* Se agregan campos Fatca
    GOSUB FATCA.GET.VALUES.JURIDICO

    DATOS := 'FATCATRIB=':IDENTIFICACION.TRIBUTARIA:';'
    DATOS := 'FATCAENTSI=':FATCAENTSI:';'
    DATOS := 'FATCAENTNO=':FATCAENTNO:';'
    DATOS := 'FATCAACCSI=':FATCAACCSI:';'
    DATOS := 'FATCAACCNO=':FATCAACCNO:';'
    DATOS := 'FATCASOCSI=':FATCASOCSI:';'
    DATOS := 'FATCASOCNO=':FATCASOCNO:';'
    DATOS := 'FATCAEIN=':NUMERO.EIN.USA:';'
    DATOS := CLIENTES
    DATOS := PROVEEDORES
;*CRT DATOS

    RETURN

;*-------------------------------------------------------------------------------------------------------------------
;*Author:Ronald Ortiz
;*Date: 20161221
;*Subproceso que nos servira para obtener los valores especificos de clientes naturales
FATCA.GET.VALUES.NATURAL:

;* �Es ciudadano de EE.UU?
    CALL GET.LOC.REF('CUSTOMER','LF.CDO.USA',POS.CDO.USA)
    CDO.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.CDO.USA>

    IF CDO.USA EQ 'SI' THEN
        FATCACIUDSI = 'X'
        FATCACIUDNO = ' '
    END
    ELSE

    IF CDO.USA EQ 'NO' THEN
        FATCACIUDSI = ' '
        FATCACIUDNO = 'X'
    END
    ELSE
    FATCACIUDSI = ' '
    FATCACIUDNO = ' '
    END

    END

;*Campo con Taxpayer Identification Number (TIN)
    CALL GET.LOC.REF('CUSTOMER','LF.TIN.USA',POS.TIN.USA)
    TIN.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.TIN.USA>

;*�Es residente EE.UU permanente o temporal?
    CALL GET.LOC.REF('CUSTOMER','LF.RES.USA',POS.RES.USA)
    RES.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.RES.USA>

    IF RES.USA EQ 'SI' THEN
        FATCARESDSI = 'X'
        FATCARESDNO = ' '
    END
    ELSE
    IF RES.USA EQ 'NO' THEN
        FATCARESDSI = ' '
        FATCARESDNO = 'X'
    END
    ELSE
    FATCARESDSI = ' '
    FATCARESDNO = ' '
    END

    END

;*�Naci� en Estados Unidos?
    CALL GET.LOC.REF('CUSTOMER','LF.CDO.BIRTH',POS.CDO.BIRTH)
    CDO.BIRTH = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.CDO.BIRTH>

    IF CDO.BIRTH EQ 'SI' THEN
        FATCANACSI = 'X'
        FATCANACNO = ' '
    END
    ELSE
    IF CDO.BIRTH EQ 'NO' THEN
        FATCANACSI = ' '
        FATCANACNO = 'X'
    END
    ELSE
    FATCANACSI = ' '
    FATCANACNO = ' '
    END

    END

;*�Es Contribuyente del impuesto de Renta en EE.UU?
    CALL GET.LOC.REF('CUSTOMER','LF.CONTRI.USA',POS.CONTRI.USA)
    CONTRI.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.CONTRI.USA>

    IF CONTRI.USA EQ 'SI' THEN
        FATCACONTRISI = 'X'
        FATCADIRNO    = ''
    END
    ELSE
    IF CONTRI.USA EQ 'NO' THEN
        FATCACONTRISI = ''
        FATCADIRNO    = 'X'
    END
    ELSE
    FATCACONTRISI = ' '
    FATCADIRNO    = ' '
    END

    END

;*�Posee doble nacionalidad?
    CALL GET.LOC.REF('CUSTOMER','LF.CDO.NAC',POS.CDO.NAC)
    CDO.NAC = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.CDO.NAC>

    IF CDO.NAC EQ 'SI' THEN
        FATCAOTHERNACISI = 'X'
        FATCAOTHERNACINO = ''
    END
    ELSE
    IF CDO.NAC EQ 'NO' THEN
        FATCAOTHERNACISI = ''
        FATCAOTHERNACINO = 'X'
    END
    ELSE
    FATCAOTHERNACISI = ' '
    FATCAOTHERNACINO = ' '
    END

    END

;*Especifique la segunda nacionalidad
    CALL GET.LOC.REF('CUSTOMER','LF.VAL.NAC',POS.VAL.NAC)
    VAL.NAC = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.VAL.NAC>

;*�Cuenta con alguna direcci�n o apartado postal en Estados Unidos?
    CALL GET.LOC.REF ('CUSTOMER','LF.ADRS.USA',POS.ADRS.USA)
    ADRS.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.ADRS.USA>

    IF ADRS.USA EQ 'SI' THEN
        FATCADIRSI = 'X'
        FATCADIRNO = ''
    END
    ELSE
    IF ADRS.USA EQ 'NO' THEN
        FATCADIRSI = ''
        FATCADIRNO = 'X'
    END
    ELSE
    FATCADIRSI = ' '
    FATCADIRNO = ' '
    END

    END

;*�Posee alg�n n�mero de tel�fono en Estados Unidos?
    CALL GET.LOC.REF('CUSTOMER','LF.PHO.USA',POS.PHO.USA)
    PHO.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.PHO.USA>

    IF PHO.USA EQ 'SI' THEN
        FATCAPHONESI = 'X'
        FATCAPHONENO = ' '
    END
    ELSE
    IF PHO.USA EQ 'NO' THEN
        FATCAPHONESI = ''
        FATCAPHONENO = 'X'
    END
    ELSE
    FATCAPHONESI = ' '
    FATCAPHONENO = ' '
    END
    END

;*�Posee alg�n poder de representaci�n o de firma otorgado a su favor por alguna persona que tenga direcci�n en Estados Unidos?
    CALL GET.LOC.REF('CUSTOMER','LF.FIRMA.USA',POS.FIRMA.USA)
    FIRMA.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.FIRMA.USA>

    IF FIRMA.USA EQ 'SI' THEN
        FATCAPODERSI = 'X'
        FATCAPODERNO = ''
    END
    ELSE
    IF FIRMA.USA EQ 'NO' THEN
        FATCAPODERSI = ''
        FATCAPODERNO = 'X'
    END
    ELSE
    FATCAPODERSI = ' '
    FATCAPODERNO = ' '
    END

    END

;*TAX ID o el ID del Seguro Social (SSN)
    CALL GET.LOC.REF('CUSTOMER','LF.SSN.USA',POS.SSN.USA)
    SSN.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.SSN.USA>

    RETURN

;*--------------------------------------------------------------------------------------------------------------------
;*Author:Ronald Ortiz
;*Date: 20161220
;*Subproceso que nos servira para obtener los valores especificos de clientes Juridicos
FATCA.GET.VALUES.JURIDICO:

;*Campo donde se indica el n�mero de identificaci�n tributaria del pa�s donde se constituy� la empresa.
    CALL GET.LOC.REF('CUSTOMER','LF.BUSS.COUN',POS.BUSS.COUN)
    IDENTIFICACION.TRIBUTARIA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.BUSS.COUN>

;*Si la empresa se constituy� en E.E.U.U. proporcionar "Employer Identification Number" (EIN)
    CALL GET.LOC.REF('CUSTOMER','LF.EIN.USA',POS.EINUSA)
    NUMERO.EIN.USA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.EINUSA>

;*Campo que identifica si al entidad es financiera o de seguros.
    CALL GET.LOC.REF('CUSTOMER','LF.TYPE.BUSS',POS.TYPE.BUSS)
    TIPO.ENTIDAD = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.TYPE.BUSS>

    IF TIPO.ENTIDAD EQ 'SI' THEN
        FATCAENTSI = 'X'
        FATCAENTNO = ' '
    END
    ELSE
        IF TIPO.ENTIDAD EQ 'NO' THEN
            FATCAENTSI = ' '
            FATCAENTNO = 'X'
        END
        ELSE
            FATCAENTSI = ' '
            FATCAENTNO = ' '
        END
    END

;*Campo que contiene el valor si la entidad solicitante posee accionistas estadounidenses que ejercen control o participaci�n igual o superior al 10% hasta el �ltimo nivel de beneficiario
    CALL GET.LOC.REF('CUSTOMER','LF.ACCI.USA',POS.ACCIONISTA.AMERICANO)
    ACCIONISTA.AMERICANO = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.ACCIONISTA.AMERICANO>

    IF ACCIONISTA.AMERICANO EQ 'SI' THEN
        FATCAACCSI = 'X'
        FATCAACCNO = ' '
    END
    ELSE
        IF ACCIONISTA.AMERICANO EQ 'NO' THEN
           FATCAACCSI = ' '
           FATCAACCNO = 'X'
        END
        ELSE
           FATCAACCSI = ' '
           FATCAACCNO = ' '
        END
    END

;*Posee accionistas o socios comerciales Off-Shore (Paraisos Fiscales).
    CALL GET.LOC.REF('CUSTOMER','LF.PARTNERS.USA',POS.SOC.PARAISOS.FISCALES)
    SOCIOS.PARAISOS.FISCALES = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.SOC.PARAISOS.FISCALES>

    IF SOCIOS.PARAISOS.FISCALES EQ 'SI' THEN
        FATCASOCSI = 'X'
        FATCASOCNO = ' '
    END
    ELSE
     IF SOCIOS.PARAISOS.FISCALES EQ 'NO' THEN
        FATCASOCSI = ' '
        FATCASOCNO = 'X'
     END
     ELSE
        FATCASOCSI = ' '
        FATCASOCNO = ' '
     END
    END

;*Campo que contiene los nombres de los principales 3 clientes.
    CALL GET.LOC.REF('CUSTOMER','LF.CUS.NAME',POS.LISTA.CLIENTES)
    LST.CLIENTES = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.LISTA.CLIENTES>

;*Campos que contiene los nombres de los principales 3 proveedores.
    CALL GET.LOC.REF('CUSTOMER','LF.PROV.NAME',POS.LISTA.PROVEEDOR)
    LST.PROVEEDOR = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.LISTA.PROVEEDOR>

    X=1
    Y=1
;*Procedemos a recorrer los 3 principales clientes
    LOOP
        REMOVE LIST.CUSTOMER FROM LST.CLIENTES SETTING POSIC.CUST
    WHILE LIST.CUSTOMER:POSIC.CUST
        CLIENTES := 'TblClientes':X:'=':LIST.CUSTOMER:';'
        X++
    REPEAT
;*Procedemos a recorrer los 3 principales proveedores.
    LOOP
        REMOVE LIST.PROVIDER FROM LST.PROVEEDOR SETTING POSIC.PROV
    WHILE LIST.PROVIDER:POSIC.PROV
        PROVEEDORES := 'TblProveedores':Y:'=':LIST.PROVIDER:';'
        Y++
    REPEAT

    IF CLIENTES EQ '' THEN
        CLIENTES = 'TblClientes1=;'
    END

    IF PROVEEDORES EQ '' THEN
        PROVEEDORES = 'TblProveedores1=;'
    END
    RETURN


DIRECTORY:

    RUTA.SPOOL.ID = 'RUTA.SPOOL.FILES'
    CALL F.READ(FN.TABLE.PA, RUTA.SPOOL.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.SPOOL = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    RUTA.PDF.ID = 'RUTA.CONTRATO.PDF'
    CALL F.READ(FN.TABLE.PA, RUTA.PDF.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    DIR.NAME.PDF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>


;*CRT DIR.NAME.SPOOL:"#"

;*CALL F.READ(FN.GLB.PARAM, 'RUTA.SPOOL.FILES', R.GLB.PARAM, F.GLB.PARAM, ERR.GLB.PARAM)
;*DIR.NAME.SPOOL = R.GLB.PARAM<EB.SLV39.VALOR.PARAM>

;*CRT DIR.NAME.SPOOL: "-- TEST --"
;* DIR.NAME.SPOOL="C:\APDF\spool\"
;*DIR.NAME.PDF="C:\APDF\pdf\"

;*CRT DIR.NAME.SPOOL:" - ":DIR.NAME.PDF

    RETURN
*-----------------------------------------------------------------------------
PROCESS.PERFIL:
*-----------------------------------------------------------------------------
    IF LS.PERSONERIA EQ '1' THEN
        NAME.ID = 'DECJURPNPR-': SS.CUSTOMER.ID : '-' : (TIMESTAMP() * 1000)
    END ELSE
        NAME.ID = 'DECJURPJPR-': SS.CUSTOMER.ID : '-' : (TIMESTAMP() * 1000)
    END

    R.ID.SPOOL = NAME.ID : '.txt'
    R.ID.TXT = NAME.ID : '.txt'
    R.ID.PDF = NAME.ID : '.pdf'


;* Nombre de la plantilla a utilizar
;*CALL F.READ(FN.TABLE.PA, NOMBRE.PLANTILLA, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
;*PLANTILLA  = '(' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : ') STARTLM'
;*PLANTILLA  = '(' : NOMBRE.PLANTILLA : ') STARTLM'

;* Codificar caracteres especiales
    DATOS = CHANGE(DATOS, '�', CHAR(465))
    DATOS = CHANGE(DATOS, '�', CHAR(497))

;*CRT "� - ":CHAR(465)

;* tratamiento de letras tildadas o semejantes
    DATOS = CHANGE(DATOS, CHAR(193), 'A')
    DATOS = CHANGE(DATOS, CHAR(201), 'E')
    DATOS = CHANGE(DATOS, CHAR(205), 'I')
    DATOS = CHANGE(DATOS, CHAR(211), 'O')
    DATOS = CHANGE(DATOS, CHAR(218), 'U')
    DATOS = CHANGE(DATOS, CHAR(192), 'A')
    DATOS = CHANGE(DATOS, CHAR(200), 'E')
    DATOS = CHANGE(DATOS, CHAR(204), 'I')
    DATOS = CHANGE(DATOS, CHAR(210), 'O')
    DATOS = CHANGE(DATOS, CHAR(217), 'U')
    DATOS = CHANGE(DATOS, CHAR(220), 'U')
    DATOS = CHANGE(DATOS, CHAR(219), 'U')
    DATOS = CHANGE(DATOS, CHAR(214), 'O')
    DATOS = CHANGE(DATOS, CHAR(212), 'O')
    DATOS = CHANGE(DATOS, CHAR(207), 'I')
    DATOS = CHANGE(DATOS, CHAR(206), 'I')
    DATOS = CHANGE(DATOS, CHAR(203), 'E')

    DATOS = CHANGE(DATOS, CHAR(225), 'A')
    DATOS = CHANGE(DATOS, CHAR(233), 'E')
    DATOS = CHANGE(DATOS, CHAR(237), 'I')
    DATOS = CHANGE(DATOS, CHAR(243), 'O')
    DATOS = CHANGE(DATOS, CHAR(250), 'U')

    DATOS = CHANGE(DATOS, '=;', '= ;')

;* Creando el archivo principal
;*-----------------------------
;*CALL SLV.I.DOCUMENT.CONTROL('SPOOL.NAME', 'DECLARACION.JURADA')

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
    WRITESEQ DATOS ON SEQ.PTR
    ELSE
    ETEXT = 'No se pueden escribir los datos en el archivo.'
    CALL STORE.END.ERROR
    END

;* Cerrando el archivo
;*--------------------
    CLOSESEQ SEQ.PTR

***********************************************************************
;* bloque adicionado para pruebas de rutina que valida si existe el pdf
    PERFIL.ARCHIVO = DIR.NAME.PDF: R.ID.PDF


    STR.ARR = SS.ACCOUNT.ID : "*"
    STR.ARR := SS.CUSTOMER.ID : "*"
    STR.ARR := SS.SHORT.NAME : "*"
    STR.ARR := "Perfil del Cliente": "*"
    STR.ARR := DIR.NAME.PDF : R.ID.PDF
;*STR.ARR := DIR.NAME.PDF : R.ID.PDF

***********************************************************************
;*Retornando Nombre del SPOOL
;*---------------------------
    SPOOL.NAME = R.ID.SPOOL

;*CRT SPOOL.NAME
;*CRT STR.ARR

    CALL SLV.I.DOCUMENT.CONTROL(SPOOL.NAME, 'PERFIL.CLIENTE')
    Y.ID   = CHANGE(OPERATOR,'.','') : '.' : ID.COMPANY
    CALL JOURNAL.UPDATE(Y.ID)

    Y.TASK = 'ENQ SLV.E.NOF.FIND.DOCUMENT USUARIO EQ ': R.USER<EB.USE.USER.NAME>
    CALL EB.SET.NEW.TASK(Y.TASK)

;*CRT STR.ARR
    ENQ.DATA<-1>=STR.ARR


    RETURN
*-----------------------------------------------------------------------------
PROCESS.PRODUCTOS:
*-----------------------------------------------------------------------------
    SS.CUENTAST = ""
    SS.PRESTAMT = ""

;* Obtener los productos de un cliente y su transaccionalidad declarada
;*STMT.ARRANGEMENT = "SELECT " : FN.AA.ARRANGEMENT
    IF SS.CUSTOMER.ID EQ LA.OWNER THEN
        STMT.ARRANGEMENT = "SELECT " : FN.AA.ARRANGEMENT:" WITH OWNER EQ ":SS.CUSTOMER.ID
    END
    ELSE
    STMT.ARRANGEMENT = "SELECT " : FN.AA.ARRANGEMENT:" WITH OWNER EQ ":SS.CUSTOMER.ID:" OR LINKED.APPL.ID EQ ":SS.ACCOUNT.ID
    END

;*CRT SS.CUSTOMER.ID
;* Extrayendo ids
    CALL EB.READLIST(STMT.ARRANGEMENT, ARRANGEMENT.LIST, '', NO.OF.RECS, Y.ARRANGEMENT.ERR1)
;*CRT ARRANGEMENT.LIST
;* Recorrer ARRANGEMENT

    IF NO.OF.RECS GT 0 THEN

        CALL F.READ(FN.CUSTOMER, SS.CUSTOMER.ID, R.CUSTOMER, F.CUSTOMER, F.ERR.CUS)
        CALL GET.LOC.REF('CUSTOMER','LF.FLG.UPDATE',POSFLG)
        FLAG.UPDATED=R.CUSTOMER<EB.CUS.LOCAL.REF,POSFLG>
        R.CUSTOMER<EB.CUS.LOCAL.REF,POSFLG> = 'SI'
        CALL F.WRITE (FN.CUSTOMER, SS.CUSTOMER.ID, R.CUSTOMER)

    END

    FOR K=1 TO NO.OF.RECS

        CALL F.READ(FN.AA.ARRANGEMENT, ARRANGEMENT.LIST<K>, R.ARR.PPI, F.AA.ARRANGEMENT, Y.ARRANGEMENT.ERR2)

        ;* Funcion para extraccion de data por arrangement
        IF R.ARR.PPI<AA.ARR.ARR.STATUS> EQ 'UNAUTH' OR R.ARR.PPI<AA.ARR.ARR.STATUS> EQ 'AUTH' OR R.ARR.PPI<AA.ARR.ARR.STATUS> EQ 'CURRENT' THEN

            ;* Leer propiedad CUSTOMER
            CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<K>, 'CUSTOMER', 'CUSTOMER', TODAY, LS.ID.PPI, R.CUSTOMER.PPI, RETURN.ERROR)

            ;*Buscar Mancomunados : OCORNEJO 06.10.2015
            ;*-----------------------------------------
            CUS_MC = ''
            FOR OP = 1 TO DCOUNT(R.CUSTOMER.PPI<1, AA.CUS.OTHER.PARTY>,SM)
                IF FIELD(R.CUSTOMER.PPI<1, AA.CUS.OTHER.PARTY>,SM,OP) EQ SS.CUSTOMER.ID THEN
                    CUS_MC = FIELD(R.CUSTOMER.PPI<1, AA.CUS.OTHER.PARTY>,SM,OP)
                    BREAK
                END
            NEXT OP
            ;*Fin Buscar Mancomunados
            ;*-----------------------

            ;*Se sustituye variable R.CUSTOMER.PPI<1, AA.CUS.OTHER.PARTY> ya que contenia mas de un cliente y en su lugar
            ;*se usar CUS_MC que ya trae identificado el cliente Mancomunado encontrado un paso antes : OCORNEJO 06.10.2015
            ;*-------------------------------------------------------------------------------------------------------------
            IF R.CUSTOMER.PPI<1, AA.CUS.PRIMARY.OWNER> EQ SS.CUSTOMER.ID OR CUS_MC EQ SS.CUSTOMER.ID THEN
                ;*CRT R.CUSTOMER.PPI<1, AA.CUS.PRIMARY.OWNER>:" --- ":SS.CUSTOMER.ID:" --- ":CUS_MC:" --- ":SS.CUSTOMER.ID
                LS.PRODUCTO = R.ARR.PPI<AA.ARR.PRODUCT>
                CALL F.READ(FN.AA.PRODUCT, LS.PRODUCTO, R.AA.PRODUCT, F.AA.PRODUCT, F.ERR.APR)
                LS.NOMBRE.PRODUCTO = R.AA.PRODUCT<AA.PDT.DESCRIPTION>

                ;* Obtener propiedad ACCOUNT, seg�n el producto tienen diferente nombre
                BEGIN CASE
                    CASE R.ARR.PPI<AA.ARR.PRODUCT.LINE> EQ 'ACCOUNTS'
                        LS.PROPERTY.ACC = 'BALANCE'
                    CASE R.ARR.PPI<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS'
                        LS.PROPERTY.ACC = 'ACCOUNT'
                    CASE R.ARR.PPI<AA.ARR.PRODUCT.LINE> EQ 'LENDING'
                        LS.PROPERTY.ACC = 'ACCOUNT'
                END CASE

                CALL AA.GET.ARRANGEMENT.CONDITIONS(ARRANGEMENT.LIST<K>, 'ACCOUNT', LS.PROPERTY.ACC, TODAY, R.ACCOUNT.ID, R.ACCOUNT.PPI, F.ERR.APPI)
                LA.ACCOUNT.PPI = RAISE(R.ACCOUNT.PPI)
                LS.ACCOUNT.ID = LA.ACCOUNT.PPI<AA.AC.ACCOUNT.REFERENCE>

                ;* Leer campo local monto depositos
                CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.AML.DEP.PROY', POS)
                LN.MONTO.DEPOSITO = LA.ACCOUNT.PPI<AA.AC.LOCAL.REF, POS>

                ;* Leer campo local monto retiros
                CALL GET.LOC.REF('AA.ARR.ACCOUNT', 'LF.AML.RET.PROY', POS)
                LN.MONTO.RETIRO = LA.ACCOUNT.PPI<AA.AC.LOCAL.REF, POS>
                E.PATH=''
                IF R.ARR.PPI<AA.ARR.PRODUCT.LINE> EQ 'LENDING' THEN
                    ;* Se crea un l�nea con el marcador PRESTAMT para cada producto
                    SS.DEPOSITO= TRIM(FMT(LN.MONTO.DEPOSITO, 'R2,#15'))
                    SS.RETIRO= TRIM(FMT(LN.MONTO.RETIRO, 'R2,#15'))
                    SS.PRESTAMT := "TblPrestamos":K:"="
                    SS.PRESTAMT := LS.NOMBRE.PRODUCTO
                    SS.PRESTAMT := "*" : LS.ACCOUNT.ID
                    SS.PRESTAMT := "*" : SS.DEPOSITO
                    SS.PRESTAMT := "*" : SS.RETIRO:";"
                    IF FLAG.UPDATED EQ 'SI' THEN
                        IF LS.ACCOUNT.ID EQ SS.ACCOUNT.ID THEN
                            CALL SLV.E.DJ.DETALLE(E.PATH,LS.ACCOUNT.ID,DJ.TEXT)
                            ENQ.DATA<-1>=E.PATH
                        END
                    END
                    ELSE
                    CALL SLV.E.DJ.DETALLE(E.PATH,LS.ACCOUNT.ID,DJ.TEXT)
                    ENQ.DATA<-1>=E.PATH
                END
            END
            ELSE IF R.ARR.PPI<AA.ARR.PRODUCT.LINE> EQ 'ACCOUNTS' OR R.ARR.PPI<AA.ARR.PRODUCT.LINE> EQ 'DEPOSITS' THEN
            SS.DEPOSITO= TRIM(FMT(LN.MONTO.DEPOSITO, "R2,#15"))
            SS.RETIRO= TRIM(FMT(LN.MONTO.RETIRO, "R2,#15"))
            SS.CUENTAST := "TblCuentas":K:"="
            SS.CUENTAST := LS.NOMBRE.PRODUCTO
            SS.CUENTAST := "*" : LS.ACCOUNT.ID
            SS.CUENTAST := "*" : SS.DEPOSITO
            SS.CUENTAST := "*" : SS.RETIRO:";"
            IF FLAG.UPDATED EQ 'SI' THEN
                IF LS.ACCOUNT.ID EQ SS.ACCOUNT.ID THEN
                    CALL SLV.E.DJ.DETALLE(E.PATH,LS.ACCOUNT.ID,DJ.TEXT)
                    ENQ.DATA<-1>=E.PATH
                END
            END
            ELSE
            CALL SLV.E.DJ.DETALLE(E.PATH,LS.ACCOUNT.ID,DJ.TEXT)
            ENQ.DATA<-1>=E.PATH
        END
    END
    END
    END
    NEXT K
    RETURN
*-----------------------------------------------------------------------------
REMESA.GET.VALUE:
*-----------------------------------------------------------------------------
	POS.ACT = ''
	POS.PAI = ''
	POS.MON = ''
	POS.MOT = ''
	APPL.ARR='CUSTOMER'
	FIELDNAME.REME='LF.REME.ACTIVA':VM:'LF.REME.PAIS':VM:'LF.REME.MONTO':VM:'LF.REME.MOTIVO'
	CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.REME)
	POS.ACT = POS.REME<1,1>
	POS.PAI = POS.REME<1,2>
	POS.MON = POS.REME<1,3>
	POS.MOT = POS.REME<1,4>
    REMESACTIVA	= R.CUSTOMER<EB.CUS.LOCAL.REF><1, POS.ACT>
    REMESAPAIS	= R.CUSTOMER<EB.CUS.LOCAL.REF><1, POS.PAI>
    REMESAMONTO = R.CUSTOMER<EB.CUS.LOCAL.REF><1, POS.MON>
    REMESAMOTIV = R.CUSTOMER<EB.CUS.LOCAL.REF><1, POS.MOT>

RETURN
    
END