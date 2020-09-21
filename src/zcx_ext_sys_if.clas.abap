class ZCX_EXT_SYS_IF definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants ERROR type SOTR_CONC value '000C291565CB1EDABECF2C6632AE15BE' ##NO_TEXT.
  data ERROR_INFO type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !ERROR_INFO type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXT_SYS_IF IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->ERROR_INFO = ERROR_INFO .
  endmethod.
ENDCLASS.
