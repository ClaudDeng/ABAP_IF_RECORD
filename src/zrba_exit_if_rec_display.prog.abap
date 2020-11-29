*&---------------------------------------------------------------------*
*& Report ZBA_IF_PARAM_DISPLAY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrba_exit_if_rec_display.
tables: rs38l,tfdir.
data: gt_param_h type table of ztba_if_param_h,
      gt_param_i type table of ztba_if_param_i.
data: ok_code type sy-ucomm.
data:
  gcl_docking_top    type ref to cl_gui_docking_container,
  gcl_docking_left   type ref to cl_gui_docking_container,

  gcl_rep_display    type ref to zcl_rep_ext_if_rec_display,
  gcl_rep_display_v2 type ref to zcl_rep_ext_if_rec_display_v2.


*  gcl_left_data         type ref to zcl_rep_ext_if_rec_i.

selection-screen begin of  block b01 with frame title text-001.
select-options: s_fname for rs38l-name,
                s_gname for tfdir-pname,
                s_datum for sy-datum,
                s_buezit for sy-uzeit.
selection-screen end of  block b01.

selection-screen begin of  block b02 with frame title text-002.
parameter p_comp  radiobutton group ways .
parameter p_list radiobutton group ways .

selection-screen end of  block b02.


start-of-selection.

  perform frm_init_data.
  perform frm_init_display.
  call screen 9000.
*&---------------------------------------------------------------------*
*& Form FRM_INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_init_display .


endform.
*&---------------------------------------------------------------------*
*& Form FRM_INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_init_data .

  case 'X'.
    when p_list.
      gcl_rep_display = new zcl_rep_ext_if_rec_display(  ).

      gcl_rep_display->get_data(
        exporting
          ir_fname = s_fname[]
          ir_gname = s_gname[]
          ir_datum = s_datum[]
          ir_uzeit = s_buezit[]
         ).
    when p_comp.
      gcl_rep_display_v2 = new zcl_rep_ext_if_rec_display_v2(  ).
      gcl_rep_display_v2->get_data(
        exporting
          ir_fname = s_fname[]
          ir_gname = s_gname[]
          ir_datum = s_datum[]
          ir_uzeit = s_buezit[]
         ).
  endcase.



endform.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_9000 output.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  data: lv_title type string.
  set pf-status 'STATUS' .
  set titlebar 'TITLE' with '接口日志显示'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.
  data: lv_message type bapiret2-message.
*  if gcl_pmt_show is not initial.
*    gcl_pmt_show->check_changed_data(  ).
*  endif.

  case ok_code.
    when '&BACK' or '&CANCEL'.
      clear ok_code.
      leave to screen 0.
    when '&EXIT'.
      clear ok_code.
      leave program.
    when others.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*& Module DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module display output.
  if gcl_docking_top is initial .
    create object gcl_docking_top
      exporting
        parent    = cl_gui_container=>default_screen
        extension = 150
        side      = cl_gui_docking_container=>dock_at_top.
  endif.
  if gcl_docking_left is initial .
    create object gcl_docking_left
      exporting
        parent    = cl_gui_container=>default_screen
        extension = 400
        side      = cl_gui_docking_container=>dock_at_left.
  endif.

  case 'X'.
    when p_list.
      if gcl_docking_top is not initial
        and gcl_docking_top is not initial .
        gcl_rep_display->show(
          exporting
            iv_top_container  = gcl_docking_top
            iv_left_container = gcl_docking_left
            iv_right_container =  cl_gui_container=>default_screen ).
      else.
        gcl_rep_display->refresh( ).
      endif.
    when p_comp.

      if gcl_docking_top is not initial
    and gcl_docking_top is not initial .
        gcl_rep_display_v2->show(
          exporting
            iv_top_container  = gcl_docking_top
            iv_left_container = gcl_docking_left
            iv_right_container =  cl_gui_container=>default_screen ).
      else.
        gcl_rep_display_v2->refresh( ).
      endif.
  endcase.
endmodule.
