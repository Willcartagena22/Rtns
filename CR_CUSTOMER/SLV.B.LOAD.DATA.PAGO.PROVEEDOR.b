*-----------------------------------------------------------------------------
* <Rating>665</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.LOAD.DATA.PAGO.PROVEEDOR
*-----------------------------------------------------------------------------
* Rutina que se encarga de validar y subir la data del reporte de PHARO al CORE
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0		Jorge H	  19.06.2018	 Inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.ACCOUNT
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.EB.SLV.PAGO.PROVEEDORES
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------
GOSUB INI
GOSUB OPENFILE
GOSUB PROCESS
RETURN


*** <region name= Inicializando Variables>
*** <desc> </desc>
INI:
 FN.PAGO.PROVEE = 'FBNK.EB.SLV.PAGO.PROVEEDORES'
 F.PAGO.PROVEE = ''
 
 FN.ACC = 'F.ACCOUNT'
 F.ACC = ''
 
 FN.ARR = 'F.AA.ARRANGEMENT'
 F.ARR = ''
 
 FN.CUS = 'F.CUSTOMER'
 F.CUS = ''
 
 FN.GLOBAL = 'F.EB.SLV.GLOBAL.PARAM'
 F.GLOBAL = ''
 
 V.NUM.LINE = 1
 V.ACC.CLIENTE = ''
 V.ID.APP.LOCAL = ''
 V.PRODUCT.LINE = ''
 V.ESTADO.CUENTA = ''
RETURN
*** </region>

*** <region name= Creando conexion hacia la Aplicacion>
*** <desc> </desc>clea
OPENFILE:
CALL OPF(FN.PAGO.PROVEE, F.PAGO.PROVEE)
CALL OPF(FN.ACC, F.ACC)
CALL OPF(FN.ARR, F.ARR)
CALL OPF(FN.CUS, F.CUS)
CALL OPF(FN.GLOBAL, F.GLOBAL)

V.CUENTA.CLIENTE = ''
RETURN
*** </region>


*** <region name= Load Data>
*** <desc>Leer el archivo excel que genera PHARO </desc>
PROCESS:
    CALL F.READ(FN.GLOBAL, 'CONTENER.PAGO.PROVEEDOR', RECORD.GLOBAL, F.GLOBAL, ERR.GLOBAL)
    V.NAME.FOLDER = RECORD.GLOBAL<EB.SLV39.VALOR.PARAM>
    V.NAME.ARCHIVE = BATCH.DETAILS<3,1,1>
    
 	Path    = V.NAME.FOLDER;*'PAGO.PROVEDOR.PHARO' CONTENER.PAGO.PROVEEDOR
 	Archive = V.NAME.ARCHIVE:'.csv';*BATCH.DETAILS<3,1,1>:'.csv'
 	
 	
    ;*Y.END.DATE = BATCH.DETAILS<3,1,2>
    ;*Archive   = 'pagos140618.csv'
    
    ;*Se verifica si el .csv existe para continuar con el proceso
    ;*-----------------------------------------------------------
    OPENSEQ Path,Archive TO MyPath ELSE
       	E = 'EB-SLV.CSV.PAGO.PROVEEDOR'
        CALL ERR
        RETURN
    END
    
    LOOP
        READSEQ Line FROM MyPath ELSE EXIT
        
        ;*Obtener  data del .csv
        ;*Se realiza la validacion si la variable es mayor a 1 para que no tome el encabezado del .csv
        ;*-----------------------
        IF V.NUM.LINE GT 1 THEN
            V.ID.APP.LOCAL = ''
            V.ID.APP.LOCAL = FIELD(Line,';',11) :'-':TODAY:'-':TIME()
            
            V.CUENTA.CLIENTE = FIELD(Line,';',11)
            V.NIT.PHARO = EREPLACE(FIELD(Line,';',12),'-','')
            GOSUB VALIDAR.ACC.CLIENTE 
            
            ;*Se realiza validacion si la cuenta del cliente existe para ejecutar el OFS y no detener el proceso
            ;*--------------------------------------------------------------------------------------------------
            IF V.CONTINUAR EQ 'N' THEN
             CONTINUE
            END
            R.PAY.PROVE = ''
	        R.PAY.PROVE<EB.SLV44.CUENTA.INTERNA> = FIELD(Line,';',1)
	        R.PAY.PROVE<EB.SLV44.FECHA.GEN.PHARO> = FIELD(Line,';',3)
	        R.PAY.PROVE<EB.SLV44.NOMBRE.PROVEEDOR> = S_CUS_NAME;*FIELD(Line,';',4)
	        
	        ;*Se agrega logica cuando la descripcion supera el limite maximo de caracteres
	        ;*----------------------------------------------------------------------------
	        ;*R.PAY.PROVE<EB.SLV44.DESCRIPCION> = FIELD(Line,';',5)
	        V.CONCEPTO = FIELD(FIELD(Line,';',5),':',1)
	        R.PAY.PROVE<EB.SLV44.DESCRIPCION,1> = V.CONCEPTO :" :"
    		V.DETALLE =  CHANGE(FIELD(FIELD(Line,';',5),':',2),' ', VM)
    		V.COUNT.DET = DCOUNT(V.DETALLE,VM) - 2
    
    		FOR I = 1 TO V.COUNT.DET
    	 		R.PAY.PROVE<EB.SLV44.DESCRIPCION,I+1> = FIELD(V.DETALLE,VM,I+1)
    		NEXT I
	        ;*----------------------------------------------------------------------------
	        
	        R.PAY.PROVE<EB.SLV44.MONTO> = FIELD(Line,';',6)
	        R.PAY.PROVE<EB.SLV44.NUM.PARTIDA> = FIELD(Line,';',7)
	        R.PAY.PROVE<EB.SLV44.CUENTA.CLIENTE> = FIELD(Line,';',11)
	      
	        ;*Ejecutar Rutina para envio de OFS.OL
	        ;*-----------------------------------
	        Y.ID = ''
    		ID.PARAM.OFS = 'OFS.LOAD.PAGO.PROVEEDOR'
    		TRANS.ID      = V.ID.APP.LOCAL
    		
    		CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID, R.PAY.PROVE, ID.PARAM.OFS, Y.OUT)

	    END
	    V.NUM.LINE = V.NUM.LINE + 1
    REPEAT
    CLOSESEQ MyPath
    
RETURN
*** </region>

*** <region name= Validar ACC Cliente>
*** <desc> </desc>
VALIDAR.ACC.CLIENTE:
  V.CONTINUAR = 'Y'
  CALL F.READ(FN.ACC, V.CUENTA.CLIENTE, RECORD.ACC, F.ACC, ERR.ACC)
  V.ARR.ID = RECORD.ACC<AC.ARRANGEMENT.ID>
  V.USUARIO = RECORD.ACC<AC.CUSTOMER>
  
  CALL F.READ(FN.ARR, V.ARR.ID, RECORD.ARR, F.ARR, ERR.ARR)
  V.PRODUCT.LINE = RECORD.ARR<AA.ARR.PRODUCT.LINE>
  V.RESTRICCION = RECORD.ACC<AC.POSTING.RESTRICT>
  
  CALL F.READ(FN.CUS, V.USUARIO, RECORD.CUS, F.CUS, ERR.CUS)
  V.NAME.CUS = RECORD.CUS<EB.CUS.SHORT.NAMEOR>
  Y.APP = 'CUSTOMER'
  Y.FLD = 'LF.NIT'
  
  CALL MULTI.GET.LOC.REF(Y.APP, Y.FLD, Y.POS)
  PosNitProvee = Y.POS<1,1>
  V.NIT = RECORD.CUS<EB.CUS.LOCAL.REF><1,PosNitProvee>
  
  GOSUB NAME.CUSTOMER	
 
  ;*Obtener Estado de la Cuenta
  ;*---------------------------
  CALL GET.LOC.REF ('ACCOUNT', 'LF.ESTADO.CTA', POS)
  V.ESTADO.CUENTA = RECORD.ACC<AC.LOCAL.REF><1,POS>
  
  ;*Se valida si la cuenta como tal existe
  ;*--------------------------------------
  IF NOT(RECORD.ACC) THEN
   V.CONTINUAR = 'N'
	   TEXTO.ARCHIVO = "Error en cargar registro ":V.CUENTA.CLIENTE:" , Cuenta no encontrada"
	   GOSUB ESCRIBIR.ARCHIVO
  END
  ELSE
     IF V.PRODUCT.LINE NE 'ACCOUNTS' THEN
        V.CONTINUAR = 'N'
        TEXTO.ARCHIVO = "Error en cargar registro ":V.CUENTA.CLIENTE:" , Cuenta no es tipo ACCOUNT"
	    GOSUB ESCRIBIR.ARCHIVO
     END
     ELSE IF V.RESTRICCION GT 0 THEN
	       V.CONTINUAR = 'N'
	       TEXTO.ARCHIVO = "Error en cargar registro ":V.CUENTA.CLIENTE:" , Cuenta posee Restriccion"
	       GOSUB ESCRIBIR.ARCHIVO
	 END
	 ELSE  IF (V.ESTADO.CUENTA NE 'ACT' AND V.ESTADO.CUENTA NE '')  THEN
       V.CONTINUAR = 'N'
       TEXTO.ARCHIVO = "Error en cargar registro ":V.CUENTA.CLIENTE:" , Cuenta no Activa"
	   GOSUB ESCRIBIR.ARCHIVO
     END
     ELSE IF V.NIT.PHARO NE V.NIT THEN
 	   V.CONTINUAR = 'N'
 	   TEXTO.ARCHIVO = "Error en cargar registro ":V.CUENTA.CLIENTE:" , Numero de NIT no coincide con T24"
	   GOSUB ESCRIBIR.ARCHIVO
 	END
  END
RETURN
*** </region>

*** <region name= Name Customer>
*** <desc> </desc>
NAME.CUSTOMER:
    CALL GET.LOC.REF('CUSTOMER', 'LF.TYPE.CUST', POSTypeCus)
 	TIPO_CLIENTE ='NAT'
    TIPO_CLIENTE = RECORD.CUS<EB.CUS.LOCAL.REF, POSTypeCus>[1,3]

    IF TIPO_CLIENTE EQ 'NAT' THEN
        ;* Encontrando el nombre completo
        PRIMER.NOMBRE = RECORD.CUS<EB.CUS.NAME.1>
        SEGUNDO.NOMBRE = RECORD.CUS<EB.CUS.NAME.2>
        TERCER.NOMBRE = RECORD.CUS<EB.CUS.GIVEN.NAMES>
        PRIMER.APELLIDO = RECORD.CUS<EB.CUS.TEXT>
        SEGUNDO.APELLIDO = RECORD.CUS<EB.CUS.FAMILY.NAME>
        APELLIDO.CASADA = RECORD.CUS<EB.CUS.PREVIOUS.NAME>
        NOMBRES = PRIMER.NOMBRE : ' ' : SEGUNDO.NOMBRE : ' ' : TERCER.NOMBRE
        APELLIDOS = PRIMER.APELLIDO : ' ' : SEGUNDO.APELLIDO : ' ' : APELLIDO.CASADA
        S_CUS_NAME = TRIM(NOMBRES : ' ' : APELLIDOS)
    END ELSE IF TIPO_CLIENTE EQ 'JUR' THEN
    
        ;* Encontrando la razon social
        CALL GET.LOC.REF('CUSTOMER', 'LF.RAZON.SOCIAL', POS.RAZON.SOCIAL)
        S_CUS_NAME = RECORD.CUS<EB.CUS.LOCAL.REF, POS.RAZON.SOCIAL>
      	;*Se le da tratamiento a multivalor(definir una rubroutine general para tratamiento de campos y caracteres)
      	S_CUS_NAME = SWAP(RECORD.CUS<EB.CUS.LOCAL.REF, POS.RAZON.SOCIAL>, @SM, ' ') 
    END
RETURN
*** </region>

*** <region name= Auditoria>
*** <desc> </desc>
ESCRIBIR.ARCHIVO:
	    DIR.NAME= V.NAME.FOLDER
	    R.ID   = "ErrorCarga_":V.NAME.ARCHIVE:'.txt'
	    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
	    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
	    END
	    CLOSESEQ SEQ.PTR
    RETURN
*** </region>



END


