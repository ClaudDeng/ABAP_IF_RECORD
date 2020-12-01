class zcl_rep_ext_if_rec_display definition
  public
  final
  create public .

  public section.
    types:
      ty_r_fname type range of rs38l_fnam,
      ty_r_gname type range of pname,
      ty_r_datum type range of datum,
      ty_r_uzeit type range of uzeit.

    data: s_params_h type ztba_if_param_h,
          t_params_h type table of ztba_if_param_h,
          t_params_i type table of ztba_if_param_i.
    data: o_params_h_display_attr type ref to zcl_ba_alv_display_attr,
          o_params_i_display_attr type ref to zcl_ba_alv_display_attr,
          o_top_display           type ref to zcl_ba_alv_display,
          o_left_display          type ref to zcl_ba_alv_display,
          o_right_container       type ref to cl_gui_container.
    interfaces zif_ba_alv_event .

    aliases t_f4_field
      for zif_ba_alv_event~t_f4_field .
    aliases handle_f4
      for zif_ba_alv_event~handle_f4 .
    aliases handle_hotspot_click
      for zif_ba_alv_event~handle_hotspot_click .
    aliases handle_toolbar
      for zif_ba_alv_event~handle_toolbar .
    aliases handle_user_command
      for zif_ba_alv_event~handle_user_command .
    aliases register_event
      for zif_ba_alv_event~register_event .
    aliases set_t_f4_field
      for zif_ba_alv_event~set_t_f4_field .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    methods constructor.
    methods show
      importing
        iv_top_container   type ref to cl_gui_docking_container
        iv_left_container  type ref to cl_gui_docking_container
        iv_right_container type ref to cl_gui_container.
    methods refresh.


    methods change_params_i
      importing
        iv_uuid type sysuuid_c22.
    methods get_data
      importing
        ir_fname type ty_r_fname
        ir_gname type ty_r_gname
        ir_datum type ty_r_datum
        ir_uzeit type ty_r_uzeit      .
  protected section.
  private section.
    methods set_display_attr.
ENDCLASS.



CLASS ZCL_REP_EXT_IF_REC_DISPLAY IMPLEMENTATION.


  method change_params_i.
    refresh t_params_i.
    clear s_params_h.
    s_params_h = t_params_h[ uuid = iv_uuid ].
    select *
         into corresponding fields of table @t_params_i
         from ztba_if_param_i
         where uuid = @s_params_h-uuid .
  endmethod.


  method constructor.


  endmethod.


  method get_data.
    select *
      into corresponding fields of table t_params_h
      from ztba_if_param_h
      where fname in ir_fname
      and fg_name in ir_gname
      and b_datum in ir_datum
      and b_uzeit in ir_uzeit.
    if t_params_h is not initial.
      s_params_h = t_params_h[ 1 ].
      refresh t_params_i.
      select *
       into corresponding fields of table @t_params_i
       from ztba_if_param_i
       where uuid = @s_params_h-uuid
       .

    endif.

  endmethod.


  method refresh.
    o_top_display->refresh( ).
    o_left_display->refresh( ).
  endmethod.


  method set_display_attr.
    data: lt_left_exclude_fld type table of lvc_rfname.
    data: lt_fldcat_line type     zcl_ba_alv_display_attr=>ty_t_fldcat_line.

    lt_fldcat_line = value #( ( fieldname = 'UUID'  fldcat_col_name = 'hotspot'  fldcat_col_value = 'X' )
                                      ).
    o_params_h_display_attr = new zcl_ba_alv_display_attr( iv_struct_name = 'ZTBA_IF_PARAM_H' ).

    o_params_h_display_attr->change_t_fldcat(
      exporting
        it_fldcat_line = lt_fldcat_line
    ).



    lt_left_exclude_fld = value #(  ( 'UUID' )  ( 'FNAME'  )  ).

    o_params_i_display_attr = new zcl_ba_alv_display_attr( it_exclude_fld = lt_left_exclude_fld
                                                                                  iv_struct_name = 'ZTBA_IF_PARAM_I' ).


    refresh lt_fldcat_line.

    lt_fldcat_line = value #( ( fieldname = 'PARAM'  fldcat_col_name = 'COLTEXT'  fldcat_col_value = '参数值' )
                                    ( fieldname = 'PARAM'  fldcat_col_name = 'SCRTEXT_L'  fldcat_col_value = '参数值' )
                                    ( fieldname = 'PARAM'  fldcat_col_name = 'SCRTEXT_M'  fldcat_col_value = '参数值' )
                                    ( fieldname = 'PARAM'  fldcat_col_name = 'SCRTEXT_S'  fldcat_col_value = '参数值' )
                                     ( fieldname = 'PARAM'  fldcat_col_name = 'hotspot'  fldcat_col_value = 'X' )
                                         ).
    o_params_i_display_attr->change_t_fldcat(
      exporting
        it_fldcat_line = lt_fldcat_line
    ).

  endmethod.


  method show.

    set_display_attr( ).
    o_top_display =  new zcl_ba_alv_display( iv_handle = '01'
                                          iv_parent = iv_top_container
                                          iv_name = 'HEAD'
                                          io_alv_display_attr = o_params_h_display_attr
                                           io_alv_display_event =  me ).

    o_top_display->show_data(
      changing
        ct_outtab = t_params_h
    ).
    o_left_display =  new zcl_ba_alv_display( iv_handle = '02'
                                    iv_parent = iv_left_container
                                    iv_name = 'ITEM'
                                    io_alv_display_attr = o_params_i_display_attr
                                     io_alv_display_event =  me ).
    o_left_display->show_data(
      changing
        ct_outtab = t_params_i
    ).

    o_right_container = iv_right_container.

  endmethod.


  method zif_ba_alv_event~handle_hotspot_click.

    data: lcl_alv type ref to cl_gui_alv_grid.
"类也可以用CASE.
    lcl_alv ?= sender.
    case sender.
      when o_top_display->o_alv.
        refresh t_params_i.
        clear s_params_h.
        s_params_h = t_params_h[ e_row_id ].

        select *
         into corresponding fields of table @t_params_i
         from ztba_if_param_i
         where uuid = @s_params_h-uuid
         .
        o_left_display->refresh( ).
      when o_left_display->o_alv.

        data: lv_json     type string,
              lv_convert  type string,
              lo_json_ser type ref to cl_trex_json_serializer,
              lv_err_text type string,
              s_param_i   type ztba_if_param_i.

        s_param_i =  t_params_i[ e_row_id ] .
        select single param
          into @lv_json
          from ztba_if_param_i
          where uuid = @s_param_i-uuid
          and action = @s_param_i-action.

        try.
*   将JSON转换为HTML
            call transformation sjson2html source xml lv_json
                                           result xml data(lv_html).
          catch cx_xslt_runtime_error into data(lo_err).
            lv_err_text = lo_err->get_text( ).
            write: lv_err_text.
            return.
        endtry.

* 显示HTML
        lv_convert = cl_abap_codepage=>convert_from( lv_html ).
        cl_abap_browser=>show_html(
          exporting
            html_string  =   lv_convert                         " HTML String
            container    =    o_right_container                        " Container for display
        ).


    endcase.

  endmethod.


  method zif_ba_alv_event~register_event.

    set handler me->handle_hotspot_click for io_alv.

  endmethod.
ENDCLASS.
