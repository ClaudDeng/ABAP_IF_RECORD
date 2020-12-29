class zcl_rep_ext_if_rec_display_v2 definition
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
          o_left_container        type ref to cl_gui_docking_container,
          o_right_container       type ref to cl_gui_container,
          o_left_html_viewer      type ref to cl_gui_html_viewer,
          o_right_html_viewer     type ref to cl_gui_html_viewer.
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


    methods get_data
      importing
        ir_fname type ty_r_fname
        ir_gname type ty_r_gname
        ir_datum type ty_r_datum
        ir_uzeit type ty_r_uzeit      .
    methods display_json
      importing
        io_html_viewer type ref to cl_gui_html_viewer
        iv_content     type string.

  protected section.
  private section.
    methods set_display_attr.

endclass.



class zcl_rep_ext_if_rec_display_v2 implementation.


  method constructor.

  endmethod.

  method display_json.
    data:lv_convert  type string,
         lv_err_text type string,
         lv_url      type char255.
    types : begin of ty_html,
              dataset(255) type c,
            end of ty_html.

    data:  lt_data type standard table of ty_html.


    try.
*   将JSON转换为HTML
        call transformation sjson2html source xml iv_content
                                       result xml data(lv_html).
      catch cx_xslt_runtime_error into data(lo_err).
        lv_err_text = lo_err->get_text( ).
        write: lv_err_text.
        return.
    endtry.
    lv_convert = cl_abap_codepage=>convert_from( lv_html ).

    call function 'SCMS_STRING_TO_FTEXT'
      exporting
        text      = lv_convert
      tables
        ftext_tab = lt_data.

    io_html_viewer->load_data(
          exporting
            type                   = 'text'           " Type of a MIME Object
            subtype                = 'html'           " Subtype of a MIME Object
          importing
            assigned_url           = lv_url                 " URL
          changing
            data_table             =  lt_data                " data table

    ).

* 显示HTML
    call method io_html_viewer->show_url
      exporting
        url                    = lv_url
      exceptions
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        others                 = 5.
    if sy-subrc <> 0.

    endif.
    io_html_viewer->do_refresh(  ).

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

    o_right_container = iv_right_container.
    o_left_container = iv_left_container.

  endmethod.


  method zif_ba_alv_event~handle_hotspot_click.
    data: lcl_alv type ref to cl_gui_alv_grid.
    lcl_alv ?= sender.
    case sender.
      when o_top_display->o_alv.
        data: lv_in_json  type string,
              lv_out_json type string.
        s_params_h = t_params_h[ e_row_id ].
        select *
           into corresponding fields of table @t_params_i
           from ztba_if_param_i
           where uuid = @s_params_h-uuid
           .
        lv_in_json = t_params_i[ action = 'in' ]-param.
        lv_out_json = t_params_i[ action = 'out' ]-param.
        if o_left_html_viewer is initial.
          o_left_html_viewer = new cl_gui_html_viewer( o_left_container ).
        endif.
        if o_right_html_viewer is initial.
          o_right_html_viewer = new cl_gui_html_viewer( o_right_container ).
        endif.
        me->display_json(
          exporting
            io_html_viewer = o_left_html_viewer
            iv_content   = lv_in_json
        ).
        me->display_json(
            exporting
              io_html_viewer = o_right_html_viewer
              iv_content   = lv_out_json
          ).
    endcase.

  endmethod.


  method zif_ba_alv_event~register_event.

    set handler me->handle_hotspot_click for io_alv.

  endmethod.
endclass.
