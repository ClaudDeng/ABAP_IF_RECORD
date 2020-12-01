class zcl_ba_ext_if_rec_para definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_param,
        param_class type rs38l_kind,
        param_name  type parameter,
        param_txt   type paramtext,
        ref_object  type rs38l_typ,
        exid        type abaptype,
        val_data    type ref to data,
        val_json    type ref to zcl_mdp_json_node,
      end of ty_param .
    types:
      ty_t_param  type table of ty_param .

    data v_in_param_json type string .
    data o_in_param_json_node type ref to zcl_mdp_json_node.
    data v_out_param_json type string .
    data o_out_param_json_node type ref to zcl_mdp_json_node.
    data t_params type ty_t_param .
    data s_param_h type ztba_if_param_h.
    data t_param_i type table of ztba_if_param_i.
    data v_begin type i.
    data v_end type i.
    methods constructor
      importing
        !iv_fm_name type rs38l_fnam .

    methods convert_in_param_to_json.
    methods convert_out_param_to_json.
    methods save_param.
  protected section.
  private section.
    methods get_function_param.
    methods convert_to_json.
endclass.



class zcl_ba_ext_if_rec_para implementation.


  method constructor.


    try.
        s_param_h-uuid  = cl_system_uuid=>create_uuid_c26_static( ).
        s_param_h-fname = iv_fm_name.
        select single pname
            into s_param_h-fg_name
            from tfdir
            where funcname =  iv_fm_name  .
        s_param_h-uname  = sy-uname.
        s_param_h-b_datum = sy-datum.
        s_param_h-b_uzeit = sy-uzeit.
        get run time field v_begin.
      catch cx_uuid_error into data(lcl_cx).
    endtry.

    me->get_function_param(  ).

  endmethod.


  method convert_in_param_to_json.


    me->convert_to_json( ).

    o_in_param_json_node = zcl_mdp_json_node=>create_object_node( ).
    loop at t_params assigning field-symbol(<fs_param>) .
      o_in_param_json_node->object_add_child_node(
          child_key   = conv string(  <fs_param>-param_name )
          child_node  = <fs_param>-val_json
      ).
    endloop.
    v_in_param_json = o_in_param_json_node->serialize( ).

  endmethod.


  method convert_out_param_to_json.
    me->convert_to_json( ).
    o_out_param_json_node = zcl_mdp_json_node=>create_object_node( ).
    loop at t_params assigning field-symbol(<fs_param>) .
      o_out_param_json_node->object_add_child_node(
          child_key   = conv string(  <fs_param>-param_name )
          child_node  = <fs_param>-val_json
      ).
    endloop.
    v_out_param_json = o_out_param_json_node->serialize( ).
  endmethod.


  method convert_to_json.
    data: lo_type   type ref to cl_abap_typedescr,
          lo_struct type ref to cl_abap_structdescr,
          lo_table  type ref to cl_abap_tabledescr.

    field-symbols:<fs_value>  type any,
                  <fs_fld>    type any,
                  <fs_struct> type any.
    loop at t_params assigning field-symbol(<fs_param>) .
      unassign <fs_value>.
      unassign <fs_fld>.
      unassign <fs_struct>.
      clear <fs_param>-val_json.
      lo_type = cl_abap_datadescr=>describe_by_data_ref( <fs_param>-val_data ).
      case lo_type->kind.
        when 'E'. "element
          assign <fs_param>-val_data->* to <fs_value>.
          if lo_type->type_kind eq 'P'
           or  lo_type->type_kind eq 'b'.
            <fs_param>-val_json =  zcl_mdp_json_node=>create_number_node( ).
          else.
            <fs_param>-val_json =  zcl_mdp_json_node=>create_string_node(  ).
          endif.
          <fs_param>-val_json->value = <fs_value>.
        when 'S'. "Structure
          data: lcl_ele_node type ref to zcl_mdp_json_node.
          assign <fs_param>-val_data->* to <fs_value>.
          <fs_param>-val_json =  zcl_mdp_json_node=>create_object_node( ).
          lo_struct ?= lo_type.
          loop at lo_struct->components assigning field-symbol(<fs_comp>).
            unassign <fs_fld>.
            assign component   <fs_comp>-name of structure <fs_value> to <fs_fld>.
            if <fs_comp>-type_kind eq 'P'
                or  <fs_comp>-type_kind eq 'b'.
              lcl_ele_node =  zcl_mdp_json_node=>create_number_node( ).
            else.
              lcl_ele_node =  zcl_mdp_json_node=>create_string_node(  ).
            endif.
            lcl_ele_node->value = <fs_fld>.
            <fs_param>-val_json->object_add_child_node(
                   child_key   = conv string( <fs_comp>-name )
                 child_node  = lcl_ele_node      ).
          endloop.
        when 'T'. "Table
          data: lcl_object_node type ref to zcl_mdp_json_node.

          field-symbols:<fs_table> type any table.
          assign <fs_param>-val_data->* to <fs_table>.
          <fs_param>-val_json =  zcl_mdp_json_node=>create_array_node( ).

          lo_table ?= lo_type.

          loop at <fs_table> assigning <fs_struct>.
            lcl_object_node = zcl_mdp_json_node=>create_object_node( ).
            lo_struct ?= cl_abap_datadescr=>describe_by_data( p_data = <fs_struct> ).
            loop at lo_struct->components assigning <fs_comp>.
              unassign <fs_fld>.
              assign component   <fs_comp>-name of structure <fs_struct> to <fs_fld>.
              if <fs_comp>-type_kind eq 'P'
                  or  <fs_comp>-type_kind eq 'b'.
                lcl_ele_node =  zcl_mdp_json_node=>create_number_node( ).
              else.
                lcl_ele_node =  zcl_mdp_json_node=>create_string_node(  ).
              endif.
              lcl_ele_node->string_set_value( value = conv string( <fs_fld> ) ).
              lcl_object_node->object_add_child_node(
                     child_key   = conv string( <fs_comp>-name )
                   child_node  = lcl_ele_node      ).
            endloop.
            <fs_param>-val_json->array_add_child_node( child_node = lcl_object_node  ).
          endloop.
        when others.
      endcase.
    endloop.

  endmethod.


  method get_function_param.


    data: lt_params type table of rfc_funint.

    call function 'RFC_GET_FUNCTION_INTERFACE'
      exporting
        funcname      = s_param_h-fname
        language      = sy-langu
*       NONE_UNICODE_LENGTH           = ' '
*       IMPORTING
*       REMOTE_BASXML_SUPPORTED       =
*       REMOTE_CALL   =
*       UPDATE_TASK   =
      tables
        params        = lt_params
*       RESUMABLE_EXCEPTIONS          =
      exceptions
        fu_not_found  = 1
        nametab_fault = 2
        others        = 3.
    if sy-subrc <> 0.
*     Implement suitable error handling here
    endif.

    t_params = value #( for ls_params in lt_params where (  paramclass <> 'X' )
                      ( param_class = ls_params-paramclass
                        param_name = ls_params-parameter
                        param_txt = ls_params-paramtext
                        ref_object =   switch #( ls_params-fieldname
                                                 when '' then ls_params-tabname
                                                 else |{ ls_params-tabname }-{ ls_params-fieldname }|
                                                    )
                        exid = ls_params-exid )
                      ).

    loop at t_params assigning field-symbol(<fs_params>).
      if <fs_params>-param_class = 'T'.
        create data <fs_params>-val_data like table of  <fs_params>-ref_object.
        <fs_params>-param_name = |{  <fs_params>-param_name }[]|.
      else.
        create data <fs_params>-val_data like  <fs_params>-ref_object.
      endif.
    endloop.

  endmethod.


  method save_param.
    get run time field v_end.
    s_param_h-e_datum = sy-datum.
    s_param_h-e_uzeit = sy-uzeit.
    s_param_h-tu_time = v_end - v_begin.

    t_param_i = value #( ( uuid = s_param_h-uuid
                                      fname = s_param_h-fname
                                      action = |in|
                                      param = me->v_in_param_json  )
                                      ( uuid = s_param_h-uuid
                                      fname = s_param_h-fname
                                      action = |out|
                                      param = me->v_out_param_json  ) ).
    modify ztba_if_param_h from s_param_h.
    modify ztba_if_param_i from table t_param_i.
    commit work.

  endmethod.
endclass.
