"! <p class="shorttext synchronized" lang="en">external system interface execute record</p>
class zcl_ba_ext_if_rec_exe definition
  public
  final
  create public .

  public section.
    " <p class="shorttext synchronized">空-未执行</p>
    constants c_not_running  type zesubrc value ''.
    " <p class="shorttext synchronized">00-已正确处理完成</p>
    constants c_ok type zesubrc value '00'.
    " <p class="shorttext synchronized">10-错误处理完成，可再次处理</p>
    constants c_error_reprocess type zesubrc value '10'.
    constants c_error type zesubrc value '11'.   " 11-错误处理完成，无法再次执行
    " <p class="shorttext synchronized">20-正在处理</p>
    constants c_running type zesubrc value '20'.
    data: lv_unique_check type char1 .
    " <p class="shorttext synchronized">构造函数</p>
    " 初始化单据状态
    " @parameter iv_external_note | 外部单据ID
    " @parameter iv_fname | 函数名字
    " @parameter iv_unique | 唯一检查
    " @raising zcx_ext_sys_if | 异常
    methods constructor
      importing
        value(iv_ext_sys_id) type zeext_sys_id
        value(iv_ext_note)   type zeext_sys_note
        value(iv_fname)      type rs38l_fnam
        value(iv_unique)     type char1 default 'X'
      raising
        zcx_ba_ext_sys_if .
    methods get_external_id_status
      returning
        value(rv_subrc) type zesubrc .
    methods get_step_status
      importing
        iv_step         type zestep
      exporting
        ev_awkey        type awkey
      returning
        value(rv_subrc) type zesubrc .
    methods save_record
      importing iv_message type bapi_msg .
    methods add_step
      importing
        iv_subrc    type zesubrc optional
        iv_step     type zestep
        iv_step_txt type zestep_txt
        iv_type     type bapi_mtype
        iv_message  type bapi_msg
        iv_awkey    type awkey optional
      raising
        zcx_ba_ext_sys_if .
  protected section.
  private section.
    "! <p class="shorttext synchronized">external system note</p>
    data v_external_note type zeext_sys_note .
    "! <p class="shorttext synchronized">external system id </p>
    data v_ext_sys_id type zeext_sys_id .
    "! <p class="shorttext synchronized">function name </p>
    data v_fname type rs38l_fnam .
    "! <p class="shorttext synchronized">内存ID</p>
    data v_memory_id type char50 .
    data s_record_h type ztba_if_record_h.
    data t_record_i type table of ztba_if_record_i.


ENDCLASS.



CLASS ZCL_BA_EXT_IF_REC_EXE IMPLEMENTATION.


  method add_step.
    data: ls_record_i type ztba_if_record_i.
    data: lv_subrc type zesubrc.
    get time.

    if iv_subrc is initial.
      if iv_type eq 'S'.
        lv_subrc =  me->c_ok.
      elseif iv_type eq 'W'.
        lv_subrc =  me->c_error.
      else.
        lv_subrc = me->c_error_reprocess.
      endif.
    endif.
    ls_record_i-zsubrc      = lv_subrc.
    ls_record_i-ztype       = iv_type.
    ls_record_i-zmessage    = iv_message.
    ls_record_i-ext_sys_note = v_external_note.
    ls_record_i-ext_sys_id = v_ext_sys_id.
    ls_record_i-fname       = v_fname.
    ls_record_i-step        = iv_step.
    ls_record_i-step_txt    = iv_step_txt.
    ls_record_i-awkey       = iv_awkey.
    ls_record_i-uname       = sy-uname.
    ls_record_i-datum       = sy-datum.
    append ls_record_i to t_record_i.
    clear ls_record_i.
    "需要再次处理，则抛出异常，如果不再次处理，则直接记录错误。
    if lv_subrc = c_error_reprocess.
      raise exception type zcx_ba_ext_sys_if
        exporting
          textid     = zcx_ba_ext_sys_if=>error
          error_info = |{ iv_type }_单据{ v_external_note }执行{ iv_step_txt }失败。{ iv_message }|.
    endif.

  endmethod.


  method constructor.
    v_ext_sys_id = iv_ext_sys_id.
    v_external_note = iv_ext_note.
    v_fname = iv_fname.
    v_memory_id = |{ iv_fname }{ v_ext_sys_id }{  v_external_note }|.
    lv_unique_check = iv_unique.
    data(rv_subrc) = me->get_external_id_status( ).
    case rv_subrc.
      when c_ok or c_error. "已处理完成
        raise exception type zcx_ba_ext_sys_if
          exporting
            textid     = zcx_ba_ext_sys_if=>error
            error_info = |单据{ iv_ext_note }已经处理完成。上次处理结果：{ s_record_h-zmessage }|.
      when c_running. "正在处理
        raise exception type zcx_ba_ext_sys_if
          exporting
            textid     = zcx_ba_ext_sys_if=>error
            error_info = |单据{ iv_ext_note }正在处理。处理时间：{ s_record_h-datum date = iso  }_{ s_record_h-uzeit time = iso }|.
      when c_error_reprocess. "再次处理
      when ''. "首次处理
      when others.
    endcase.
    "首次执行或再次处理，将状态更改为正在处理
    export   subrc = c_running
              datum = sy-datum
              uzeit = sy-uzeit
              to shared buffer indx(a1) client sy-mandt id v_memory_id.
    s_record_h-zsubrc = c_running.
  endmethod.


  method get_external_id_status.
    data: lv_subrc type zesubrc,
          lv_datum type sy-datum,
          lv_uzeit type sy-uzeit.

    select single *
         into s_record_h
         from ztba_if_record_h
         where ext_sys_note = v_external_note
         and ext_sys_id = v_ext_sys_id
         and fname = v_fname
         .
    if s_record_h is not initial.
      select *
        into table t_record_i
        from ztba_if_record_i
        where ext_sys_note = v_external_note
        and ext_sys_id = v_ext_sys_id
        and fname = v_fname.
    endif.
    if lv_unique_check is initial.
      clear s_record_h.
    endif.

    import  subrc  = lv_subrc
            datum  = lv_datum
            uzeit  = lv_uzeit
      from shared buffer indx(a1) client sy-mandt id v_memory_id.

    if  lv_subrc is  not initial.
      s_record_h-zsubrc = lv_subrc.
      s_record_h-datum = lv_datum.
      s_record_h-uzeit = lv_uzeit.
    endif.
    rv_subrc = s_record_h-zsubrc.
  endmethod.


  method get_step_status.

    if line_exists( t_record_i[ step = iv_step ] ).
      rv_subrc = t_record_i[ step = iv_step ]-zsubrc.
      ev_awkey = t_record_i[ step = iv_step ]-awkey.
    else.
      rv_subrc = c_not_running.
    endif.

  endmethod.


  method save_record.
    if line_exists( t_record_i[ zsubrc = c_error_reprocess ] ).
      s_record_h-zsubrc = c_error_reprocess.
    elseif line_exists( t_record_i[ zsubrc = me->c_error ] ).
      s_record_h-zsubrc = c_error.
    else.
      s_record_h-zsubrc = c_ok.
    endif.
    get time.
    s_record_h-uname = sy-uname.
    s_record_h-datum = sy-datum.
    s_record_h-uzeit = sy-uzeit.
    s_record_h-ext_sys_id = v_ext_sys_id.
    s_record_h-ext_sys_note = v_external_note.
    s_record_h-fname       = v_fname.
    s_record_h-zmessage = iv_message.
    modify ztba_if_record_h from s_record_h.
    modify ztba_if_record_i from table t_record_i.
    commit work.
    if sy-subrc eq 0 or lv_unique_check is initial.
      delete from shared buffer indx(a1)  client sy-mandt id v_memory_id.
    endif.

  endmethod.
ENDCLASS.
