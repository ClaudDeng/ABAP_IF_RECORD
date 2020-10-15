class zcl_ext_if_rec_para definition
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
    data v_in_param_json_node type ref to zcl_mdp_json_node.
    data v_out_param_json type string .
    data v_fm_name type rs38l_fnam .
    data v_fg_name type rs38l_fnam .
    data t_params type ty_t_param .

    methods constructor
      importing
        !iv_fm_name type rs38l_fnam .

    methods convert_to_json.

  protected section.
  private section.
    methods get_function_param
      importing
        !iv_fname type rs38l_fnam .
endclass.



class zcl_ext_if_rec_para implementation.


  method constructor.

    v_fm_name = iv_fm_name.
    select single pname
        into v_fg_name
        from tfdir
        where funcname =  v_fm_name
       .
    me->get_function_param( iv_fname = v_fm_name ).

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
          lcl_object_node = zcl_mdp_json_node=>create_object_node( ).
          lo_table ?= lo_type.

          loop at <fs_table> assigning <fs_struct>.
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
              lcl_ele_node->value = <fs_fld>.
              lcl_object_node->object_add_child_node(
                     child_key   = conv string( <fs_comp>-name )
                   child_node  = lcl_ele_node      ).
            endloop.

            <fs_param>-val_json->array_add_child_node( child_node = lcl_object_node  ).
          endloop.
        when others.
      endcase.

    endloop.


    v_in_param_json_node = zcl_mdp_json_node=>create_object_node( ).
    loop at t_params assigning <fs_param> .
      v_in_param_json_node->object_add_child_node(
          child_key   = conv string(  <fs_param>-param_name )
          child_node  = <fs_param>-val_json
      ).

    endloop.
    v_in_param_json = v_in_param_json_node->serialize( ).

  endmethod.


  method get_function_param.


    data: lt_params type table of rfc_funint.

    call function 'RFC_GET_FUNCTION_INTERFACE'
      exporting
        funcname      = v_fm_name
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

*    data: ls_params type ty_param.
*    loop at lt_params assigning field-symbol(<fs_param>).
*
*      ls_params-param_class = <fs_param>-paramclass .
*      ls_params-param_name = <fs_param>-parameter .
*      ls_params-param_txt = <fs_param>-paramtext .
*      ls_params-ref_object =   switch #( <fs_param>-fieldname
*                                   when '' then <fs_param>-tabname
*                                   else |{ <fs_param>-tabname }-{ <fs_param>-fieldname }|
*                                      )  .
*      ls_params-exid = <fs_param>-exid .
*      if <fs_param>-param_class = 'T'.
*        create data <fs_param>-val_data like table of  <fs_param>-ref_object.
*      else.
*        create data <fs_param>-val_data like  <fs_param>-ref_object.
*      endif.
*      append ls_params to t_params.
*      clear ls_params.

*  endloop.

  endmethod.
endclass.
