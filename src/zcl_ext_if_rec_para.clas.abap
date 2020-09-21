class ZCL_EXT_IF_REC_PARA definition
  public
  final
  create public .

public section.

  methods GET_FUNCTION_PARAM
    importing
      !IV_FNAME type RS38L_FNAM .
  protected section.
private section.
ENDCLASS.



CLASS ZCL_EXT_IF_REC_PARA IMPLEMENTATION.


  method get_function_param.

  call function 'RFC_GET_FUNCTION_INTERFACE'
    exporting
      funcname                      = IV_FNAME
     LANGUAGE                      = SY-LANGU
*     NONE_UNICODE_LENGTH           = ' '
*   IMPORTING
*     REMOTE_BASXML_SUPPORTED       =
*     REMOTE_CALL                   =
*     UPDATE_TASK                   =
    tables
      params                        =
*     RESUMABLE_EXCEPTIONS          =
*   EXCEPTIONS
*     FU_NOT_FOUND                  = 1
*     NAMETAB_FAULT                 = 2
*     OTHERS                        = 3
            .
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.



  endmethod.
ENDCLASS.
