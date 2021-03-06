*------------------------------------------------------------------------------------------
* <Rating>1058</Rating>
*------------------------------------------------------------------------------------------
    SUBROUTINE SLV.VAL.CLIENT.SEG
*------------------------------------------------------------------------------------------
* RUTINA QUE REALIZA VALIDACIONES PARA LA FUNCIONALIDAD DE SEGUROS
*------------------------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version.
* SFUSILLO 	11.09.2017 		Initial Code
* SFUSILLO 					Esta rutina se utiliza para la funcionalidad de SEGUROS
*							y realiza validaciones a la pantalla de Afiliacion de SEGUROS
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.ALTA.SEGURO
    $INSERT I_F.CUSTOMER
    $INSERT I_F.SLV.CUS.ZONA.PAIS
    $INSERT I_F.SLV.CUS.MUNICIPIO
    $INSERT I_F.SLV.CUS.DEPARTAMENTO
    $INSERT I_F.USER
    $INSERT I_F.EB.SLV.KEYS.PARAMS
    $INSERT I_F.ACCOUNT


    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN

INIT:
;*ARCHIVOS
    FN.ALTA.SEG 	= 'F.EB.SLV.ALTA.SEGURO'
    F.ALTA.SEG	= ''
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER	= ''
    FN.ZONE				='F.EB.SLV.CUS.ZONA.PAIS'
    F.ZONE				=''
    FN.MUNI				='F.EB.SLV.CUS.MUNICIPIO'
    F.MUNI				=''
    FN.DPTO				='F.EB.SLV.CUS.DEPARTAMENTO'
    F.DPTO				=''
    FN.USER				='F.USER'
    F.USER				=''

    N.CUSTOMER.ID=R.NEW(EB.SEG.CUSTOMER.ID)
;*N.CUSTOMER.ID=100116

    N_ZON_ID			=''
    N_DPT_ID			=''
    N_MUN_ID			=''
    S_ZON				=''
    S_DPT				=''
    S_MUN				=''

    ARR_LOCAL_FIELD_CUS	=''
    Y.VERSION = APPLICATION:PGM.VERSION

    FN.KEYS.PARAMS = 'F.EB.SLV.KEYS.PARAMS'
    F.KEYS.PARAMS  = ''


;*-----------------------------------

;*CONSTANTES
;*-----------------------------------



;*PARAMETROS
;*-----------------------------------

    RETURN

*APERTURA DE ARCHIVOS A USAR
OPENFILE:
    CALL OPF(FN.ALTA.SEG,F.ALTA.SEG)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.ZONE,F.ZONE)
    CALL OPF(FN.DPTO,F.DPTO)
    CALL OPF(FN.MUNI,F.MUNI)
    CALL OPF(FN.USER,F.USER)
    CALL OPF(FN.KEYS.PARAMS, F.KEYS.PARAMS)
    RETURN


PROCESS:

    CALL GET.LOC.REF('CUSTOMER','LF.DUI',LOCPOSDui)
    CALL GET.LOC.REF('CUSTOMER','LF.NIT',LOCPOSNit)
    CALL GET.LOC.REF('CUSTOMER','LF.ZONA.3',LOCPOSZona)
    CALL GET.LOC.REF('CUSTOMER','LF.DEPTO.3',LOCPOSDepto)
    CALL GET.LOC.REF('CUSTOMER','LF.MUNICIPIO.3',LOCPOSMuni)
    CALL GET.LOC.REF('CUSTOMER','LF.CANTON.3',LOCPOSCanton)
    CALL GET.LOC.REF('CUSTOMER','LF.COLONIA.3',LOCPOSColonia)
    CALL GET.LOC.REF('CUSTOMER','LF.CALLE.3',LOCPOSCalle)
    CALL GET.LOC.REF('CUSTOMER','LF.AVENIDA.3',LOCPOSAvenida)
    CALL GET.LOC.REF('CUSTOMER','LF.NUM.DEPTO.3',LOCPOSNroCasa)
;*registo del cliente
    CALL F.READ (FN.CUSTOMER,N.CUSTOMER.ID,R_CUS,F.CUSTOMER,CUS_ERR)
    ARR_LOCAL_FIELD_CUS = (R_CUS)

    VAR.PROCEDER=''
    VAR.PROCEDER='S'
    VAR.VALID.FECHA.PAGO=R.NEW(EB.SEG.FECHA.PAGO)
    VAR.VALID.REFERENCIA=R.NEW(EB.SEG.NO.TARJETA)
    VAR.VALID.CUENTA=R.NEW(EB.SEG.REFERENCIA)
    VAR.VALID.MEDIO.PAGO=R.NEW(EB.SEG.MEDIO.PAGO)
    VAR.VALID.BANCO.EMISOR=R.NEW(EB.SEG.BANCO.EMISOR)
    VAR.VALID.FECHA.VENCIMIENTO=R.NEW(EB.SEG.FECHA.VENCIMIENTO)
    VAR.VALID.ASEGURADO=R.NEW(EB.SEG.ASEGURADO)
    VAR.VALID.CUSTOMER.ID=R.NEW(EB.SEG.CUSTOMER.ID)
    VAR.VALID.PARENTESCO=R.NEW(EB.SEG.PARENTESCO.PAG)
    VAR.VALID.PREGUNA.A=R.NEW(EB.SEG.PREGUNTA.A)
    VAR.VALID.PREGUNA.B=R.NEW(EB.SEG.PREGUNTA.B)
    VAR.VALID.PREGUNA.C=R.NEW(EB.SEG.PREGUNTA.C)
    VAR.VALID.PREGUNA.D=R.NEW(EB.SEG.PREGUNTA.D)
    VAR.VALID.EXPLICACION=R.NEW(EB.SEG.EXPLICACION)
    VAR.VALID.PORCENTAJE.TOTAL=SUM(R.NEW(EB.SEG.PORCENTAJE))
    VAR.VALID.NOMBRES=R.NEW(EB.SEG.NOMBRES)
    VAR.VALID.APELLIDOS=R.NEW(EB.SEG.APELLIDOS)
    VAR.VALID.PORCENTAJE=R.NEW(EB.SEG.PORCENTAJE)
    VAR.VALID.CONTAR.BENEF=COUNT(R.NEW(EB.SEG.NOMBRES),VM)+1
    VAR.VALID.PARENTESCO.BENEF=R.NEW(EB.SEG.PARENTESCO)
    VAR.VALID.FECUENCIA.PAGO=R.NEW(EB.SEG.FECUENCIA.PAGO)
    VAR.SEGURO.ALTA.EJECUTIVO=R.NEW(EB.SEG.NOMBRE.EJECUTIVO)

    BEGIN CASE
        CASE Y.VERSION EQ 'EB.SLV.ALTA.SEGURO,SLV.INPUT.ALTA'
            CALL F.READ (FN.CUSTOMER,VAR.VALID.CUSTOMER.ID,R_CUS,F.CUSTOMER,CUS_ERR)
            CALL GET.LOC.REF('CUSTOMER','SEGMENT',LOCPOSSegment)
            VAR.VALID.TIPO.PERSONA.PAGADOR=R_CUS<EB.CUS.LOCAL.REF><1,LOCPOSSegment>
            GOSUB VALIDACIONES.PANTALLA
    END CASE


    ID.CURRENT=ID.NEW


;*Validando si cambia la fecha FN.ALTA.SEG,F.ALTA.SEG
    CALL F.READ(FN.ALTA.SEG,ID.CURRENT,R.AA,F.ALTA.SEG,ERR.ALTA)
    FECHA.ANT.PAGO= R.AA<EB.SEG.FECHA.PAGO>
;*obtener Cuenta del IVA
    VAR.DATE.TODAY2=''
    VAR.VALID.TODAY.NUM2=''
    VAR.DATE.TODAY2=TODAY
;*Formato: FECHA.PAGO DD/MM/YYYY y tipo calendario
    VAR.VALID.FPAGO.NUM=OCONV(VAR.VALID.FECHA.PAGO,'DI')
    VAR.VALID.TODAY.NUM2=OCONV(VAR.DATE.TODAY2,'DI')

    PROCESS.DATE = TODAY
    DAY.COUNT = "+30C"
    CALL CDT('', PROCESS.DATE, DAY.COUNT)


    IF  VAR.VALID.FECHA.PAGO GT PROCESS.DATE THEN

        V_NAME_FIELD= EB.SEG.FECHA.PAGO
        STRERR ='EB-SEG015'
        GOSUB CRT_ERROR

    END


    IF FECHA.ANT.PAGO NE VAR.VALID.FECHA.PAGO THEN

        IF VAR.VALID.FECHA.PAGO NE '' OR VAR.VALID.FECHA.PAGO NE ' ' THEN

            IF VAR.VALID.FPAGO.NUM EQ '' OR VAR.VALID.FPAGO.NUM EQ ' ' THEN
                VAR.VALID.FPAGO.NUM=0
            END
            BEGIN CASE

                CASE VAR.VALID.FPAGO.NUM LT VAR.VALID.TODAY.NUM2

                    V_NAME_FIELD= EB.SEG.FECHA.PAGO
                    STRERR ='EB-SEG003'
                    GOSUB CRT_ERROR


            END CASE


        END
    END




    BEGIN CASE
        CASE UPCASE(VAR.VALID.MEDIO.PAGO) EQ UPCASE('TCO') AND (LEN(VAR.VALID.REFERENCIA) GT 16 OR ISDIGIT( VAR.VALID.REFERENCIA) EQ 0 OR LEN(VAR.VALID.REFERENCIA) LT 16)
            V_NAME_FIELD= EB.SEG.NO.TARJETA
            STRERR ='EB-SEG004':FM:V.MENSAJE
            GOSUB CRT_ERROR
        CASE (UPCASE(VAR.VALID.MEDIO.PAGO) EQ UPCASE('CAA') OR UPCASE(VAR.VALID.MEDIO.PAGO) EQ UPCASE('CCA')) AND (ISDIGIT(VAR.VALID.CUENTA) EQ 0 OR VAR.VALID.CUENTA EQ '')
            V_NAME_FIELD= EB.SEG.REFERENCIA
            STRERR ='EB-SEG009':FM:V.MENSAJE
            GOSUB CRT_ERROR
    END CASE

    BEGIN CASE
        CASE (UPCASE(VAR.VALID.MEDIO.PAGO) EQ UPCASE('TCO') OR VAR.VALID.MEDIO.PAGO EQ UPCASE('TCA')) AND VAR.VALID.BANCO.EMISOR EQ ''
            V_NAME_FIELD= EB.SEG.BANCO.EMISOR
            STRERR ='EB-SEG009':FM:V.MENSAJE
            GOSUB CRT_ERROR
    END CASE

    VAR.DATE.TODAY=TODAY
;*Formato: MM/YYYY - Solo debe permitir meses válidos del 01 al 12
    BEGIN CASE
        CASE VAR.VALID.FECHA.VENCIMIENTO NE '' AND ((SUBSTRINGS(VAR.VALID.FECHA.VENCIMIENTO,1,2) GT 12 OR SUBSTRINGS(VAR.VALID.FECHA.VENCIMIENTO,1,2) LT 1))
            V_NAME_FIELD=EB.SEG.FECHA.VENCIMIENTO
            STRERR ='EB-SEG005'
            GOSUB CRT_ERROR

        CASE VAR.VALID.FECHA.VENCIMIENTO NE '' AND SUBSTRINGS(VAR.VALID.FECHA.VENCIMIENTO,4,4) LT SUBSTRINGS(VAR.DATE.TODAY,1,4)
            V_NAME_FIELD= EB.SEG.FECHA.VENCIMIENTO
            STRERR ='EB-SEG006':FM:V.MENSAJE
            GOSUB CRT_ERROR

        CASE VAR.VALID.FECHA.VENCIMIENTO NE '' AND (SUBSTRINGS(VAR.VALID.FECHA.VENCIMIENTO,4,4) LE SUBSTRINGS(VAR.DATE.TODAY,1,4) AND SUBSTRINGS(VAR.VALID.FECHA.VENCIMIENTO,1,2) LT SUBSTRINGS(VAR.DATE.TODAY,5,2))
            V_NAME_FIELD= EB.SEG.FECHA.VENCIMIENTO
            STRERR ='EB-SEG007':FM:V.MENSAJE
            GOSUB CRT_ERROR
        CASE VAR.VALID.FECHA.VENCIMIENTO NE '' AND (NUM(SUBSTRINGS(VAR.VALID.FECHA.VENCIMIENTO,4,4)) EQ 0 OR SUBSTRINGS(VAR.VALID.FECHA.VENCIMIENTO,3,1) NE '/' OR LEN(VAR.VALID.FECHA.VENCIMIENTO) NE 7)
            V_NAME_FIELD= EB.SEG.FECHA.VENCIMIENTO
            STRERR ='EB-SEG014':FM:V.MENSAJE
            GOSUB CRT_ERROR
    END CASE

    BEGIN CASE
        CASE VAR.VALID.FECHA.VENCIMIENTO EQ '' AND UPCASE(VAR.VALID.MEDIO.PAGO) EQ UPCASE('TCO')
            V_NAME_FIELD= EB.SEG.FECHA.VENCIMIENTO
            STRERR =' Favor completar campo.'
            GOSUB CRT_ERROR
    END CASE

    BEGIN CASE
        CASE (VAR.VALID.PREGUNA.A EQ 'SI' OR VAR.VALID.PREGUNA.B EQ 'SI' OR VAR.VALID.PREGUNA.C EQ 'SI' OR VAR.VALID.PREGUNA.D EQ 'SI') AND VAR.VALID.EXPLICACION EQ ''
            V_NAME_FIELD= EB.SEG.EXPLICACION
            STRERR ='EB-SEG009':FM:V.MENSAJE
            GOSUB CRT_ERROR
    END CASE
;*VALIDAR SI NO HAY PREGUNTAR AFIRMATIVAS
    BEGIN CASE
        CASE VAR.VALID.PREGUNA.A NE 'NO' OR VAR.VALID.PREGUNA.B NE 'NO' OR VAR.VALID.PREGUNA.C NE 'NO' OR VAR.VALID.PREGUNA.D NE 'NO'
            V_NAME_FIELD= DCOUNT(R.NEW(EB.SEG.OVERRIDE),VM) + 1
            STRERR ='SEG-PREG.ASE'
            GOSUB CRT_OVERRIDE
    END CASE

    BEGIN CASE
        CASE VAR.VALID.CONTAR.BENEF GT 4
            V_NAME_FIELD= EB.SEG.NOMBRES
            STRERR ='EB-SEG013':FM:V.MENSAJE
            GOSUB CRT_ERROR
        CASE VAR.VALID.CONTAR.BENEF LE 4 AND EREPLACE(VAR.VALID.PORCENTAJE,VM,'') EQ '' AND EREPLACE(VAR.VALID.PARENTESCO.BENEF,VM,'') EQ '' AND EREPLACE(VAR.VALID.NOMBRES,VM,'') EQ '' AND EREPLACE(VAR.VALID.APELLIDOS,VM,'') EQ ''
            V_NAME_FIELD= EB.SEG.NOMBRES
            STRERR ='EB-SEG008':FM:V.MENSAJE
            V_CORR_POS=1
            GOSUB CRT_ERROR
        CASE 1
            B=1
            LOOP WHILE B <= VAR.VALID.CONTAR.BENEF  DO
                BEGIN CASE
                    CASE VAR.VALID.NOMBRES<1,B> EQ ''
                        V_NAME_FIELD= EB.SEG.NOMBRES
                        STRERR ='EB-SEG009':FM:V.MENSAJE
                        V_CORR_POS=B
                        GOSUB CRT_ERROR
                END CASE
                BEGIN CASE
                    CASE VAR.VALID.APELLIDOS<1,B> EQ ''
                        V_NAME_FIELD= EB.SEG.APELLIDOS
                        STRERR ='EB-SEG009':FM:V.MENSAJE
                        V_CORR_POS=B
                        GOSUB CRT_ERROR
                END CASE
                BEGIN CASE
                    CASE VAR.VALID.PARENTESCO.BENEF<1,B> EQ ''
                        V_NAME_FIELD= EB.SEG.PARENTESCO
                        STRERR ='EB-SEG009':FM:V.MENSAJE
                        V_CORR_POS=B
                        GOSUB CRT_ERROR
                END CASE
                BEGIN CASE
                    CASE VAR.VALID.PORCENTAJE<1,B> EQ '' OR VAR.VALID.PORCENTAJE<1,B> EQ 0
                        V_NAME_FIELD= EB.SEG.PORCENTAJE
                        STRERR ='EB-SEG009'
                        V_CORR_POS=B
                        GOSUB CRT_ERROR
                    CASE 1
                        BEGIN CASE
                            CASE (VAR.VALID.PORCENTAJE.TOTAL GT 100)
                                V_NAME_FIELD= EB.SEG.PORCENTAJE
                                STRERR ='EB-SEG011':FM:V.MENSAJE
                                V_CORR_POS=1
                                GOSUB CRT_ERROR
                            CASE (VAR.VALID.PORCENTAJE.TOTAL LT 100)
                                V_NAME_FIELD= EB.SEG.PORCENTAJE
                                STRERR ='EB-SEG011':FM:V.MENSAJE
                                V_CORR_POS=1
                                GOSUB CRT_ERROR
                            CASE VAR.VALID.PORCENTAJE<1,B> LT 1 OR ISDIGIT(VAR.VALID.PORCENTAJE<1,B>) EQ 0
                                V_NAME_FIELD= EB.SEG.PORCENTAJE
                                STRERR ='EB-SEG010':FM:V.MENSAJE
                                V_CORR_POS=B
                                GOSUB CRT_ERROR
                        END CASE
                END CASE

                B=B+1
            REPEAT

            BEGIN CASE
                CASE VAR.VALID.MEDIO.PAGO EQ 'AVACIO'
                    V_NAME_FIELD= EB.SEG.MEDIO.PAGO
                    STRERR ='EB-SEG009':FM:V.MENSAJE
                    V_CORR_POS=1
                    GOSUB CRT_ERROR
            END CASE

            BEGIN CASE
                CASE VAR.VALID.FECUENCIA.PAGO EQ 'AAVACIO'
                    V_NAME_FIELD= EB.SEG.FECUENCIA.PAGO
                    STRERR ='EB-SEG009':FM:V.MENSAJE
                    V_CORR_POS=1
                    GOSUB CRT_ERROR
            END CASE

    END CASE



    BEGIN CASE
        CASE VAR.VALID.ASEGURADO NE '' AND VAR.VALID.CUSTOMER.ID NE '' AND VAR.VALID.CUSTOMER.ID NE VAR.VALID.ASEGURADO AND UPCASE(VAR.VALID.MEDIO.PAGO) EQ 'TCO'
            V_NAME_FIELD=EB.SEG.MEDIO.PAGO
            STRERR ='EB-SEG012'
            GOSUB CRT_ERROR
    END CASE



    CALL F.READ (FN.CUSTOMER,VAR.VALID.ASEGURADO,R_CUS,F.CUSTOMER,CUS_ERR)
    CALL GET.LOC.REF('CUSTOMER','SEGMENT',LOCPOSSegment)

*Validación Sexo para Seguro Vida Plena Mujer
    TIPO.SEGURO=R.NEW(EB.SEG.TIPO.SEGURO)
    TIPO.SEGURO=TIPO.SEGURO[1,16]

    VAR.SEXO=R_CUS<EB.CUS.GENDER>


    BEGIN CASE
        CASE TIPO.SEGURO EQ 'Vida Plena Mujer' AND  VAR.SEXO EQ 'MALE'

            STRERR ='EB-SEG100':FM:V.MENSAJE
            GOSUB CRT_ERROR
    END CASE




    VAR.VALID.TIPO.PERSONA= R_CUS<EB.CUS.LOCAL.REF><1,LOCPOSSegment>

    BEGIN CASE
        CASE VAR.VALID.TIPO.PERSONA EQ 2
            V_NAME_FIELD= EB.SEG.ASEGURADO
            STRERR ='EB-SEG002':FM:V.MENSAJE
            GOSUB CRT_ERROR
    END CASE





    VAR.VALID.FECHA.NAC=R_CUS<EB.CUS.DATE.OF.BIRTH>
    VAR.DATE.TODAY=TODAY
    VAR.VALID.ANIO=OCONV(OCONV(VAR.VALID.FECHA.NAC,'DI'),'DY')
    VAR.VALID.FECHA.NAC.NUM=OCONV(VAR.VALID.FECHA.NAC,'DI')
    VAR.DATE.TODAY.YEAR=OCONV(OCONV(VAR.DATE.TODAY,'DI'),'DY')
    VAR.DATE.TODAY.NUM=OCONV(VAR.DATE.TODAY, 'DI')
    VAR.MES.DIA=RIGHT ('00':OCONV(VAR.VALID.FECHA.NAC.NUM,'DM'),2):RIGHT ('00':OCONV(VAR.VALID.FECHA.NAC.NUM,'DD'),2)
    VAR.DATE=(OCONV(VAR.DATE.TODAY.YEAR:VAR.MES.DIA,'DI'))
    BEGIN CASE
        CASE VAR.DATE.TODAY.NUM GE VAR.DATE
            VAR.YEARS.OLD=VAR.DATE.TODAY.YEAR-VAR.VALID.ANIO
        CASE 1
            VAR.YEARS.OLD=VAR.DATE.TODAY.YEAR-VAR.VALID.ANIO-1
    END CASE

    BEGIN CASE
        CASE VAR.YEARS.OLD LT 18
            V_NAME_FIELD= EB.SEG.FECHA.NAC
            STRERR ='EB-SEG001'
            GOSUB CRT_ERROR
    END CASE

    RETURN


VALIDACIONES.PANTALLA:

;* solo para pantalla usuario
*BEGIN CASE
*	CASE VAR.VALID.FECHA.PAGO GT 30 OR VAR.VALID.FECHA.PAGO LT 1
*	V_NAME_FIELD= EB.SEG.FECHA.PAGO
*	STRERR ='EB-SEG003':FM:V.MENSAJE
*	GOSUB CRT_ERROR
*END CASE

    BEGIN CASE
        CASE VAR.VALID.ASEGURADO NE VAR.VALID.CUSTOMER.ID AND VAR.VALID.PARENTESCO EQ '' AND VAR.VALID.TIPO.PERSONA.PAGADOR EQ 1
            V_NAME_FIELD= EB.SEG.PARENTESCO.PAG
            STRERR ='EB-SEG009':FM:V.MENSAJE
            GOSUB CRT_ERROR
    END CASE


    V.RANGO.INF=''
    V.RANGO.SUP=''
    VAR.KEY.PARAM.ID='SEG.DAO.RANGO'
    FECHA.ACTUAL=TODAY

    VAR.USER.ID=OPERATOR
    CALL F.READ (FN.USER,VAR.USER.ID,R_USER,F.USER,ERR)
    VAR.DAO=R_USER<EB.USE.DEPARTMENT.CODE>
    GOSUB CONVERT.KEY.PARAM
    V.RANGO.INF.OP=V.RANGO.INF
    V.RANGO.SUP.OP=V.RANGO.SUP


    K.STMT.ARRANGEMENT 	= "SELECT " : FN.USER : " WITH USER.NAME EQ '" : VAR.SEGURO.ALTA.EJECUTIVO :"' AND END.DATE.PROFILE GE ":FECHA.ACTUAL:" AND DEPARTMENT.CODE GE ":V.RANGO.INF.OP:" AND DEPARTMENT.CODE LE ":V.RANGO.SUP.OP
    CALL EB.READLIST(K.STMT.ARRANGEMENT, ARRANGEMENT.LIST, R.EJEC, NO.OF.RECS, Y.ARRANGEMENT.ERR1)
    VAR.SEGURO.ALTA.USER=ARRANGEMENT.LIST<1>
    CALL F.READ(FN.USER,VAR.SEGURO.ALTA.USER,R.USR,F.USER,Y.ERR)
    VAR.DAO=R.USR<EB.USE.DEPARTMENT.CODE>
    GOSUB CONVERT.KEY.PARAM

    V.RANGO.INF.EJEC=V.RANGO.INF
    V.RANGO.SUP.EJEC=V.RANGO.SUP



    BEGIN CASE
        CASE V.RANGO.INF.EJEC NE V.RANGO.INF OR V.RANGO.SUP.EJEC NE V.RANGO.SUP OR (V.RANGO.INF.EJEC EQ '' AND V.RANGO.INF EQ '')
            V_NAME_FIELD= EB.SEG.NOMBRE.EJECUTIVO
            STRERR =' El ejecutivo no pertenece al area del usuario'
            GOSUB CRT_ERROR
    END CASE

    BEGIN CASE
        CASE UPCASE(VAR.VALID.MEDIO.PAGO) EQ UPCASE('CAA') OR UPCASE(VAR.VALID.MEDIO.PAGO) EQ UPCASE('CCA') AND VAR.VALID.CUENTA NE ''

            FN.ACC	= 'F.ACCOUNT'
            F.ACC	= ''
            CALL OPF(FN.ACC, F.ACC)
            CALL F.READ(FN.ACC, VAR.VALID.CUENTA, R.ACC, F.ACC, ERR.ACC)
            VAR.CAT=R.ACC<AC.CATEGORY>
            VAR.CLIENTE.CTA.ID=R.ACC<AC.CUSTOMER>

            IF VAR.VALID.MEDIO.PAGO EQ 'CAA' THEN
                VAR.RANGO.INF=6000
                VAR.RANGO.SUP=6499
            END
            IF VAR.VALID.MEDIO.PAGO EQ 'CCA' THEN
                VAR.RANGO.INF=1000
                VAR.RANGO.SUP=1999
            END

            BEGIN CASE
                CASE VAR.VALID.CUSTOMER.ID NE VAR.CLIENTE.CTA.ID
                    V_NAME_FIELD= EB.SEG.REFERENCIA
                    STRERR =' Esta cuenta no es del cliente'
                    GOSUB CRT_ERROR
                CASE VAR.VALID.CUSTOMER.ID EQ VAR.CLIENTE.CTA.ID AND (VAR.CAT GT VAR.RANGO.SUP OR VAR.CAT LT VAR.RANGO.INF)
                    V_NAME_FIELD= EB.SEG.REFERENCIA
                    STRERR =' Esta cuenta no pertenece al medio de pago seleccionado'
                    GOSUB CRT_ERROR
            END CASE

    END CASE



    RETURN


CONVERT.KEY.PARAM:
    CALL F.READ(FN.KEYS.PARAMS,VAR.KEY.PARAM.ID,R.KEY.PARAM,F.KEYS.PARAMS,Y.ERR)
    V.CONTAR.KEY=COUNT(R.KEY.PARAM<EB.SLV18.PARAM.ID>,VM)+1
    j=1
    LOOP WHILE j LE V.CONTAR.KEY  DO
        VAR.VALID.RANG.INF=R.KEY.PARAM<EB.SLV18.VALOR><1,j,1>
        VAR.VALID.RANG.SUP=R.KEY.PARAM<EB.SLV18.VALOR><1,j,2>

        IF VAR.DAO GE VAR.VALID.RANG.INF AND VAR.DAO LE VAR.VALID.RANG.SUP THEN
            V.RANGO.INF=R.KEY.PARAM<EB.SLV18.VALOR><1,j,3>
            V.RANGO.SUP=R.KEY.PARAM<EB.SLV18.VALOR><1,j,4>
            RETURN
        END
        j=j+1
    REPEAT
    RETURN

;*Lazadores de error
*-----------------------------------------------------------------------------------------------------------------------
CRT_ERROR:
    AF  = V_NAME_FIELD
    AV 	= V_CORR_POS
    ETEXT = STRERR
    CALL STORE.END.ERROR
    RETURN

CRT_OVERRIDE:
    TEXT =STRERR
    CURR.NO =V_NAME_FIELD
    CALL STORE.OVERRIDE(CURR.NO)
    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MonedaAzul'
    R.ID   = 'Moneda_Azul ':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END



