*&---------------------------------------------------------------------*
*& Include ZIN_REC_FM_PARAM_IN
*&---------------------------------------------------------------------*




   data: lcl_record_param type ref to zcl_ext_if_rec_para.
   data: lt_cstack_tab type sys_callst.

   field-symbols: <fs> type any.
   call function 'SYSTEM_CALLSTACK'
     importing
       et_callstack = lt_cstack_tab. " internal table
* l_cstack_tab 里就是abap的调用堆栈

   lcl_record_param = new zcl_ext_if_rec_para( iv_fm_name = conv rs38l_fnam( lt_cstack_tab[ 1 ]-eventname ) ).

   field-symbols: <fs_value> type any.


   loop at lcl_record_param->t_params assigning field-symbol(<fs_params>).

     assign (<fs_params>-param_name) to <fs_value> .
     if <fs_value> is assigned .
       get reference of <fs_value> into <fs_params>-val_data.
       UNASSIGN <fs_value>.
     endif.
   endloop.

   lcl_record_param->convert_in_param_to_json( ).
