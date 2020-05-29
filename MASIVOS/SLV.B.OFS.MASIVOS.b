*-----------------------------------------------------------------------------
* <Rating>32</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.OFS.MASIVOS
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.ACCOUNT
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_GTS.COMMON
$INSERT I_F.EB.SLV.OFS.PARAM.BUILD
*-----------------------------------------------------------------------------
GOSUB INI
GOSUB OPENFILE
GOSUB PROCESS
RETURN


*** <region name= Inicializando Variables>
*** <desc> </desc>
INI:

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
CALL OPF(FN.ACC, F.ACC)
CALL OPF(FN.ARR, F.ARR)
CALL OPF(FN.CUS, F.CUS)
CALL OPF(FN.GLOBAL, F.GLOBAL)

V.CUENTA.CLIENTE = ''
RETURN

PROCESS:

 	Path    = 'C:\Users\calvarado\Desktop\Proyectos\Creacion masiva de clientes\Pruebas';
 	Archive = 'CSVCLIENTE.csv';
 	
    ;*Se verifica si el .csv existe para continuar con el proceso
    ;*-----------------------------------------------------------
    OPENSEQ Path,Archive TO MyPath ELSE
       	E = 'EB-SLV.CSV.MASIVOS'
        CALL ERR
        RETURN
    END
    
    LOOP
    ;*Crear string de ecabezados y detalle de los datos extraídos del .csv
       READSEQ Line FROM MyPath ELSE EXIT
       
       DATOS= Line
       GOSUB CARACTERES
       Line = DATOS
       
	    IF V.NUM.LINE EQ 1 THEN
        	CAMPO1 = FIELD(Line,';',V.NUM.LINE)
        	 ENCABEZADOS = Line
        	 CONT = LEN(ENCABEZADOS)
        	 ENCABEZADOS=ENCABEZADOS[4,CONT]
	    END	    
        IF V.NUM.LINE EQ 2 THEN
        	CAMPO1 = FIELD(Line,';',V.NUM.LINE)
        	 DATOS = Line
	    END
	    V.NUM.LINE = V.NUM.LINE + 1 
    REPEAT
    CLOSESEQ MyPath
    
GOSUB CONSTRUIR.OFS
 TEXTO.ARCHIVO=OFS
	 GOSUB ESCRIBIR.ARCHIVO
    
RETURN



CONSTRUIR.OFS:
 
		FOR I = 1 TO 150
		  
		   DET = FIELD(DATOS,';',I)
		   DET= CHANGE(DET, CHAR(205), 'I')
		   
		   IF DET THEN
		       ENC = FIELD(ENCABEZADOS,';',I)
			   IF ENC EQ 'PRODUCT:1' OR ENC EQ 'LF.BENEFICIARIO:1' OR ENC EQ 'LF.PARENTESCO:1' OR ENC EQ 'LF.PORCENTAJE:1' THEN 
			   CUENTA= CUENTA:ENC: ':1=' : DET : ','
		   END
		   ELSE
		   OFS = OFS : ENC: ':1=' : DET : ','
		   END
		   
		   END
		   
		   
		NEXT I

			OFS='CUSTOMER,SLV.INPUT/I/PROCESS//0,/,,CUSTOMER.CURRENCY:1:1=USD,':OFS
			TOT=LENGTH(OFS)
			OFS= OFS[1,TOT-1]
			ERR.OFS.PAY=''
			Y.OPTIONS='SLVOFSPS'
			OFS.ID.LOG=''
*			CALL OFS.CALL.BULK.MANAGER(Y.OPTIONS, OFS, Y.RESPONSE,TXN.RESULT)
*			CALL OFS.POST.MESSAGE(OFS,OFS.ID.LOG,Y.OPTIONS, ERR.OFS.PAY)
CRT OFS
			CALL OFS.CALL.BULK.MANAGER(Y.OPTIONS, OFS, Y.RESPONSE,TXN.RESULT)
        	CALL JOURNAL.UPDATE("")
			CRT 'RESPUESTA OFS : ': Y.RESPONSE
			CRT 'ERROR OFS : ': TXN.RESULT
	
	
RETURN



ESCRIBIR.ARCHIVO:
	    DIR.NAME='C:\Users\calvarado\Desktop\Proyectos\Creacion masiva de clientes\Pruebas'
	    R.ID   = 'PruebasMasivos.txt'
	    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
	    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
	    END
	    CLOSESEQ SEQ.PTR
    RETURN


CARACTERES:
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
RETURN


END


