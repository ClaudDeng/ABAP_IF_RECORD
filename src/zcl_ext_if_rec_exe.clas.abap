"! <p class="shorttext synchronized" lang="en">external system interface execute record</p>
class zcl_ext_if_rec_exe definition
  public
  final
  create public .
  public section.
    "! <p class="shorttext synchronized">00-未执行</p>
    constants c_status_00 type zesubrc value '00'.
    "! <p class="shorttext synchronized">01-已正确处理完成</p>
    constants c_status_01 type zesubrc value '01'.
    "! <p class="shorttext synchronized">02-错误处理完成，无法再次执行</p>
    constants c_status_02 type zesubrc value '02'.
    "! <p class="shorttext synchronized">10-错误处理完成，可再次处理</p>
    constants c_status_10 type zesubrc value '10'.
    "! <p class="shorttext synchronized">20-正在处理</p>
    constants c_status_20 type zesubrc value '20'.



    "! <p class="shorttext synchronized" >构造函数</p>
    "! 初始化单据状态
    "! @parameter iv_ext_sys_id | 外部系统ID
    "! @parameter iv_ext_note | 外部单据号
    "! @parameter iv_fname | 函数名字
    "! @raising zcx_ext_sys_if | 异常
    methods constructor
      importing
        value(iv_ext_sys_id)  type zeext_sys_id
        value(iv_ext_note)   type zeext_sys_note
        value(iv_fname)      type rs38l_fnam
      raising
        zcx_ext_sys_if .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter rv_subrc | <p class="shorttext synchronized" lang="en"></p>
    methods get_external_id_status
      returning
        value(rv_subrc) type zesubrc .

    methods add_step
      importing
                iv_step         type zestep
                iv_step_txt     type zestep_txt
                iv_type         type bapi_mtype
                iv_message      type bapi_msg
      returning
                value(rv_subrc) type zesubrc
      raising
                zcx_ext_sys_if .
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

endclass.



class zcl_ext_if_rec_exe implementation.


  method constructor.
    v_ext_sys_id = iv_ext_sys_id.
    v_external_note = iv_ext_note.
    v_fname = iv_fname.
    v_memory_id = |{ iv_fname }{ v_ext_sys_id }{  v_external_note }|.

    data(rv_subrc) = me->get_external_id_status( ).
    case rv_subrc.
      when '01' or '02'. "01已正确处理,02 "错误处理，无法再次执行
        raise exception type zcx_ext_sys_if
          exporting
            textid     = zcx_ext_sys_if=>error
            error_info = |单据{ iv_ext_note }已经处理完成。上次处理结果：{ s_record_h-zmessage }|.
      when '20'. "正在处理
        raise exception type zcx_ext_sys_if
          exporting
            textid     = zcx_ext_sys_if=>error
            error_info = |单据{ iv_ext_note }正在处理。处理时间：{ s_record_h-datum date = iso  } { s_record_h-uzeit time = iso }|.
      when '10'. "再次处理
      when '00'. "首次处理
      when others.
    endcase.
    "首次执行或再次处理，将状态更改为正在处理
    export   subrc = me->c_status_20
             datum = sy-datum
             uzeit = sy-uzeit
             to shared buffer indx(a1) client sy-mandt id v_memory_id.
    s_record_h-zsubrc = me->c_status_20.

  endmethod.


  method get_external_id_status.
    data: lv_subrc type zesubrc,
          lv_datum type sy-datum,
          lv_uzeit type sy-uzeit.
    select single *
         into s_record_h
         from ztba_if_record_h
         where ext_sys_id = v_ext_sys_id
         and ext_sys_note = v_external_note
         and fname = v_fname
         .
    if s_record_h is not initial.
      select *
      into table t_record_i
      from ztba_if_record_i
         where ext_sys_id = v_ext_sys_id
         and ext_sys_note = v_external_note
         and fname = v_fname
      .
    endif.
    "检查该单据是否正在处理
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


  method add_step.
    data: ls_record_i type ztba_if_record_i.
    get time.
    ls_record_i-ext_sys_id = v_ext_sys_id.
    ls_record_i-ext_sys_note = v_external_note.
    ls_record_i-fname       = v_fname.
    ls_record_i-step        = iv_step.
    ls_record_i-step_txt    = iv_step_txt.
    ls_record_i-ztype       = iv_type.
    ls_record_i-zmessage    = iv_message.
    ls_record_i-uname       = sy-uname.
    ls_record_i-datum       = sy-datum.
    ls_record_i-uzeit       = sy-uzeit.
    append ls_record_i to t_record_i.
    clear ls_record_i.
    if iv_type = 'E'.
      raise exception type zcx_ext_sys_if
        exporting
          textid     = zcx_ext_sys_if=>error
          error_info = |单据{ v_external_note }第{ iv_step }({ iv_step_txt })处理失败|.
    endif.

  endmethod.

endclass.
