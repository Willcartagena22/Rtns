*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.CUS.STRING.SHORT.NAME
*-----------------------------------------------------------------------------
*Avalua el SHORT.NAME para definir valor de SHORT.NAME
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version
* GAlfaro  	09.10.2014 		Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------
*
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    
    RETURN
       
  INIT:
*-----------------------------------------------------------------------------
  
 ;*variables de trabajo 
  FN.CUS ='F.CUSTOMER'
  F.CUS	=''
 
  S_STR_SHORTNAME		=''  
  S_NAME_1				=TRIM(R.NEW(EB.CUS.NAME.1))
  S_NAME_2				=TRIM(R.NEW(EB.CUS.NAME.2))
  S_LAST_NAME_1			=TRIM(R.NEW(EB.CUS.TEXT))
  S_LAST_NAME_2			=TRIM(R.NEW(EB.CUS.FAMILY.NAME))
  
  RETURN
  
  OPENFILES:
*-----------------------------------------------------------------------------
  
  CALL OPF(FN.CUS,F.CUS)
  RETURN
      
  PROCESS:
*-----------------------------------------------------------------------------
  
  ;* Todos los componentes del nombre  
  S_STR_SHORTNAME=S_NAME_1:" ":S_NAME_2:" ":S_LAST_NAME_1:" ":S_LAST_NAME_2
	IF LEN(S_STR_SHORTNAME) LE 35 THEN 
	 	GOSUB WRT_STRING
  			RETURN 
  	END
  
  ;*Dos nombres, un apellido mas inicial de segundo apellido
  	S_STR_SHORTNAME=S_NAME_1:" ":S_NAME_2:" ":S_LAST_NAME_1:" ":LEFT(S_LAST_NAME_2,1)
	IF LEN(S_STR_SHORTNAME) LE 35 THEN
  		GOSUB WRT_STRING
  			RETURN
  	END			
  			
  	;*Dos nombres, un apellido 
  	S_STR_SHORTNAME=S_NAME_1:" ":S_NAME_2:" ":S_LAST_NAME_1
	IF LEN(S_STR_SHORTNAME) LE 35 THEN
  		GOSUB WRT_STRING
  			RETURN
  	END		
   RETURN
  WRT_STRING:
*-----------------------------------------------------------------------------
  
  	R.NEW(EB.CUS.SHORT.NAME)=S_STR_SHORTNAME
  RETURN

END
